
module Main where

import Control.Monad.Cont
import Prelude hiding (break)

fibWith2Where :: Integer -> (Integer -> Integer) -> Integer
fibWith2Where 0 c = c 0
fibWith2Where 1 c = c 1
fibWith2Where n c = fibWith2Where (n-1) d
  where
    d :: Integer -> Integer
    d x = fibWith2Where (n-2) e
      where
        e :: Integer -> Integer
        e y = c (x+y)

fibWith0Where :: Integer -> (Integer -> Integer) -> Integer
fibWith0Where 0 c = c 0
fibWith0Where 1 c = c 1
fibWith0Where n c =
    fibWith0Where (n-1) $ \x -> fibWith0Where (n-2) $ \y -> c (x+y)

fibWithLambda :: Integer -> ((Integer -> Integer) -> Integer)
fibWithLambda 0 = \c -> c 0
fibWithLambda 1 = \c -> c 1
fibWithLambda n =
    \c -> fibWithLambda (n-1) $ \x -> fibWithLambda (n-2) $ \y -> c (x+y)

fibWithDo :: Integer -> Cont String Integer
fibWithDo 0 = return 0
fibWithDo 1 = return 1
fibWithDo n = do
        x <- fibWithDo (n-1)
        y <- fibWithDo (n-2)
        return (x+y)

fibWithLambdaBad :: Integer -> ((Integer -> String) -> String)
fibWithLambdaBad 0 = \c -> c 0
fibWithLambdaBad 1 = \c -> c 1
fibWithLambdaBad n =
    -- (\c ->
    --     (fibWithLambdaBad (n-1) (\x ->
    --                             (fibWithLambdaBad (n-2) $ \y ->
    --                                                         c (x+y)))))
    (\c ->
        (fibWithLambdaBad (n-1) (\x ->
                                (const "whatwhat" $ \y ->
                                                            c (x+y)))))

fibWithDoBad :: Integer -> Cont String Integer
fibWithDoBad 0 = return 0
fibWithDoBad 1 = return 1
fibWithDoBad n = do
        x <- fibWithDo (n-1)
        -- y <- cont $ \f -> "whatwhat"
        y <- cont $ const "whatwhat"
        return (x+y)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

factCont :: Integer -> (Integer -> a) -> a
factCont 0 = \c -> c 1
factCont n = \c -> factCont (n - 1) $ \m -> c (n * m)

factRealCont :: (Eq a, Num a) => a -> Cont b a
factRealCont 0 = return 1
factRealCont n = do
        m <- factRealCont (n - 1)
        return $ m * n

loopLookForIt :: ContT () IO ()
loopLookForIt =
    for_in [0..100] $ \loop x -> do
        when (x `mod` 3 == 1) $ continue loop
        when (x `div` 17 == 2) $ break loop
        lift $ print x

loopBreakOuter :: ContT () IO ()
loopBreakOuter =
    for_in [1,2,3] $ \outer x -> do
        for_in [4,5,6] $ \inner y -> do
            lift $ print y
            break outer
        lift $ print x

data (MonadCont m) => Label m = Label { continue :: m ()
                                      , break :: m ()
                                      }

data Label' = Label' { continue' :: ContT () IO ()
                     , break' :: ContT () IO ()
                     }

for_in' :: (Monad m) => [a] -> (a -> m ()) -> m ()
for_in' xs f = mapM_ f xs

for_in'_specific :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in'_specific = flip mapM_

for_in'' :: (MonadCont m) => [a] -> (a -> m ()) -> m ()
for_in'' xs f = callCC $ \c -> mapM_ f xs

for_in''_specific :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in''_specific xs f = callCC $ \c -> mapM_ f xs

for_in''' :: (MonadCont m) => [a] -> (a -> m ()) -> m ()
for_in''' xs f = mapM_ (\x -> callCC $ \c -> f x) xs

for_in'''_specific :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in'''_specific xs f = mapM_ (\x -> callCC $ \c -> f x) xs

for_in_specific :: [a] -> (Label' -> a -> ContT () IO ()) -> ContT () IO ()
for_in_specific xs f = callCC $ \breakCont ->
    forM_ xs $ \x ->
        callCC $ \continueCont ->
            let label = Label' (continueCont ()) (breakCont ())
            in f label x

for_in :: (MonadCont m) => [a] -> (Label m -> a -> m ()) -> m ()
for_in xs f = callCC $ \breakCont ->
    mapM_ (\x -> callCC $ \continueCont -> f (Label (continueCont ()) (breakCont ())) x) xs


main :: IO ()
main = do
        print $ fibWith2Where 10 id
        print $ fibWith0Where 10 id
        print $ fibWithLambda 10 id
        print $ runCont (fibWithDo 10) show
        print $ runCont (fibWithDoBad 10) show
        print $ fibWithLambdaBad 10 show
        print $ factorial 10
        print $ factCont 10 id
        print $ runCont (factRealCont 10) id
        print $ runCont (factRealCont 10) (const "hello")
        -- runContT loopBreakForIt return
        runContT loopBreakOuter return
        runContT loopBreakOuter_specific return
