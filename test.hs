import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Control.Monad.Trans

import Reflex
import Reflex.Host.Class
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import Control.Monad.Ref

import Reflex.Orphans

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    runSpiderTest "Test mapDyn" $ \ e -> do
      b <- hold 1 e
      d <- mapDyn show =<< holdDyn 1 e
      return (fmap show e, fmap show b, d)
  , runSpiderTest "Test fmap Dynamic" $ \ e -> do
      b <- hold 1 e
      d <- holdDyn 1 e
      return (fmap show e, fmap show b, fmap show d)
  ]

runSpiderTest :: (Eq a, Show a)
        => TestName
        -> (Event Spider Int -> HostFrame Spider (Event Spider a, Behavior Spider a, Dynamic Spider a))
        -> TestTree
runSpiderTest nm frm = testCase nm . runSpiderHost $ do
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
    eventValue eh = readEvent eh >>= sequence
    checkStep (vea, veb, vba, vbb) = do
      liftIO $ vea @=? veb
      liftIO $ vba @=? vbb
    sameBehavior ba bb = do
      va <- sample ba
      vb <- sample bb
      liftIO $ va @=? vb
