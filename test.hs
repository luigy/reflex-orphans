{-# LANGUAGE FlexibleContexts #-}
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import System.IO.Unsafe

import Reflex
import Reflex.Host.Class
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import Control.Monad.Ref
import Control.DeepSeq

import Reflex.Orphans

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    runSpiderEBD "Test mapDyn" $ \ e -> do
      b <- hold 1 e
      d <- mapDyn show =<< holdDyn 1 e
      return (fmap show e, fmap show b, d)
  , runSpiderEBD "Test fmap Dynamic" $ \ e -> do
      b <- hold 1 e
      d <- holdDyn 1 e
      return (fmap show e, fmap show b, fmap show d)
  , runExecuteCountTest "Test mapDyn function execution count" 1 $ \r e -> do
       d <- holdDyn 1 e
       mapDyn (\v -> unsafePerformIO $ do
                  atomicModifyIORef' r (\rv -> (rv+1, ()))
                  return v
                  ) $ d
  , runExecuteCountTest "Test fmap Dynamic function execution count" 1 $ \r e-> do
       d <- holdDyn 1 e
       return . fmap (\v -> unsafePerformIO $ do
                         atomicModifyIORef' r (\rv -> (rv+1, ()))
                         return v
                     ) $ d
  , testApplicative
  ]

eventValue :: (Reflex t, MonadReadEvent t m) => EventHandle t a -> m (Maybe a)
eventValue eh = readEvent eh >>= sequence

sameBehavior :: (Reflex t, Eq a, MonadSample t m, MonadIO m, Show a) => Behavior t a -> Behavior t a -> m ()
sameBehavior ba bb = do
      va <- sample ba
      vb <- sample bb
      liftIO $ va @=? vb

runExecuteCountTest :: TestName -> Int
                    -> (IORef Int -> Event Spider Int -> HostFrame Spider (Dynamic Spider Int))
                    -> TestTree
runExecuteCountTest nm tgtcnt frm = testCase nm . runSpiderHost $ do
    (re, rmt) <- newEventWithTriggerRef
    cr <- liftIO $ newIORef 0
    pd <- runHostFrame $ frm cr re
    ehd <- subscribeEvent . updated $ pd
    void' . sample . current $ pd
    Just rt <- readRef rmt
    forM_ [1..10] $ \nv -> do
      void' . fireEventsAndRead [rt :=> (Identity nv)] $ readEvent ehd >>= sequence
      void' . sample . current $ pd
    cc <- liftIO $ readIORef cr
    liftIO $ (tgtcnt * (10+1)) @=? cc
  where
    void' act = do
      r <- act
      r `deepseq` return ()

testApplicative :: TestTree
testApplicative = testCase "Test Applicative" . runSpiderHost $ do
    (re, rmt) <- newEventWithTriggerRef
    (pb, pd) <- runHostFrame $ do
      b <- hold 1 re
      d <- holdDyn 1 re
      return (show <$> b, show <$> d)
    ehd <- subscribeEvent . updated $ pd
    pb `sameBehavior` (current pd)
    Just rt <- readRef rmt
    forM_ [1..10] $ \nv -> do
      checkStep =<< (fireEventsAndRead [rt :=> (Identity nv)] $ do
        (,,,) <$> pure (Just . show $ nv)
              <*> eventValue ehd
              <*> sample pb
              <*> (sample . current $ pd))
      pb `sameBehavior` (current pd)
    checkStep =<< (fireEventsAndRead [] $ do
        (,,,) <$> pure Nothing
              <*> eventValue ehd
              <*> sample pb
              <*> (sample . current $ pd))
    pb `sameBehavior` (current pd)
  where
    eventValue eh = readEvent eh >>= sequence
    checkStep (tv, ved, vba, vbb) = do
      liftIO $ tv @=? ved
      liftIO $ vba @=? vbb

runSpiderEBD :: (Eq a, Show a)
        => TestName
        -> (Event Spider Int -> HostFrame Spider (Event Spider a, Behavior Spider a, Dynamic Spider a))
        -> TestTree
runSpiderEBD nm frm = testCase nm . runSpiderHost $ do
    (re, rmt) <- newEventWithTriggerRef
    (pe, pb, pd) <- runHostFrame $ frm re
    ehe <- subscribeEvent pe
    ehd <- subscribeEvent . updated $ pd
    pb `sameBehavior` (current pd)
    Just rt <- readRef rmt
    forM_ [1..10] $ \nv ->
        checkStep =<< (fireEventsAndRead [rt :=> (Identity nv)] $ do
          (,,,) <$> eventValue ehe
                <*> eventValue ehd
                <*> sample pb
                <*> (sample . current $ pd))
    pb `sameBehavior` (current pd)
    checkStep =<< (fireEventsAndRead [] $ do
          (,,,) <$> eventValue ehe
                <*> eventValue ehd
                <*> sample pb
                <*> (sample . current $ pd))
    pb `sameBehavior` (current pd)
  where
    checkStep (vea, veb, vba, vbb) = do
      liftIO $ vea @=? veb
      liftIO $ vba @=? vbb
