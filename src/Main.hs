{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Cont
import Data.Word
import Debug.Trace
import Prelude hiding (break)
import Text.Groom

------------------------------------------------------------------------------
-- fibonacci with continuations
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- factorial with continuations
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- for...in loops in haskell
------------------------------------------------------------------------------

-- Implement for...in loops in haskell with breaking and continuing.
loopBreakOuter :: ContT () IO ()
loopBreakOuter =
    for_in [1,2,3] $ \outer x -> do
        for_in [4,5,6] $ \inner y -> do
            lift $ print y
            break outer
        lift $ print x

loopLookForIt :: ContT () IO ()
loopLookForIt =
    for_in [0..100] $ \loop x -> do
        when (x `mod` 3 == 1) $ continue loop
        when (x `div` 17 == 2) $ break loop
        lift $ print x

-- Since continuations represent, well, "continuations" to the program
-- flow, we should have some notion of a continuation that functions as
-- break, as well as a continuation that functions as continue. We will
-- store the continuations that correspond to breaking and continuing
-- inside a loop "label", which is the first argument of our hanging
-- lambda.  It's sufficient then to call continue label or break label
-- inside the monad to extract and follow the continuation.
data Label = Label { continue :: ContT () IO ()
                   , break :: ContT () IO ()
                   }

-- If we didn't have to supply any of the continuations, this is actually
-- just a flipped mapM_.
for_in' :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in' xs f = mapM_ f xs

-- Of course, sample code, f has the type Label m -> a -> m (), so this
-- won't do! Consider the following transformation.
-- This function does the same thing as for_in', but we placed it inside
-- the continuation monad and made explicit a variable c. What does the
-- current continuation c correspond to in this context? Well, it's in the
-- very outer context, which means the "current continuation" is completely
-- out of the loop. That must mean it's the break continuation. Cool.
for_in'' :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in'' xs f = callCC $ \c -> mapM_ f xs

-- Consider this second alternative transformation.
-- This time, we've replaced f with a wrapper lambda that uses callCC
-- before actually calling f, and the current continuation results in the
-- next step of mapM_ being called. This is the continue continuation.
for_in''' :: [a] -> (a -> ContT () IO ()) -> ContT () IO ()
for_in''' xs f = mapM_ (\x -> callCC $ \c -> f x) xs

-- Now all we have to do is to stick them together using the label data
-- type.
for_in :: [a] -> (Label -> a -> ContT () IO ()) -> ContT () IO ()
for_in xs f = callCC $ \breakCont ->
    forM_ xs $ \x ->
        callCC $ \continueCont ->
            let label = Label (continueCont ()) (breakCont ())
            in f label x


-----------------------------------------------------------------------------
-- callcc examples
-----------------------------------------------------------------------------

-- Returns a string depending on the length of the name parameter.
-- If the provided string is empty, returns an error.
-- Otherwise, returns a welcome message.
whatsYourName :: String -> String
whatsYourName name =
    -- Runs an anonymous Cont block and extracts value from it with
    -- (`runCont` id). Here id is the continuation, passed to the Cont
    -- block.
    (`runCont` id) $ do
        -- Binds response to the result of the following callCC block,
        -- binds exit to the continuation.
        response <- callCC''''' $ \exit -> do
            -- Validates name. This approach illustrates advantage of
            -- using callCC over return. We pass the continuation to
            -- validateName, and ***interrupt execution of the Cont block
            -- from inside of validateName***.
            validateName name exit
            -- Returns the welcome message from the callCC block.
            -- This line is not executed if validateName fails.
            return $ "Welcome, " ++ name ++ "!"
        -- Returns from the Cont block.
        return response
  where
    validateName :: String -> (String -> Cont String ()) -> Cont String ()
    validateName name exit =
        when (null name) (exit "You forgot to tell me your name!")


callCC' :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC' f = ContT $ \c ->
                runContT (f (\a -> ContT $ \_ -> c a)) c

callCC'' :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC'' f = ContT $ \c -> do
                let result = f (\a -> ContT $ \_ -> c a)
                runContT result c

callCC''' :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC''' f = cont $ \c -> do
                let result = f (\a -> cont $ \_ -> c a)
                runCont result c

callCC'''' :: forall a b r . ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC'''' exitContFunc = cont normalFunction
  where
    normalFunction :: (a -> r) -> r
    normalFunction finalChanger = runCont resultCont finalChanger
      where
        resultCont :: Cont r a
        resultCont = exitContFunc inner
          where
            inner :: a -> Cont r b
            inner a = cont throwAwayResultFunc
              where
                throwAwayResultFunc :: (b -> r) -> r
                throwAwayResultFunc _ = finalChanger a

callCC''''' :: forall a b r . ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC''''' exitContFunc = cont normalFunction
  where
    normalFunction :: (a -> r) -> r
    normalFunction finalChanger = runCont (exitContFunc inner) finalChanger
      where
        inner :: a -> Cont r b
        inner a = cont $ \_ -> finalChanger a

type Binary = Bool

data Root = Root Tree Tree

data Tree = Node { _nodeVal :: Binary
                 , _depth :: Word8
                 , _leftTree :: Tree
                 , _rightTree :: Tree
                 }
          | Leaf { _nodeVal :: Binary
                 , _leafVal :: Word8 }
          deriving Show

one, zero :: Binary
one = True
zero = False

power :: Word8
power = 5

totalDepth :: Word8
totalDepth = 2 ^ power

createTree :: Root
createTree = fixLeafValues $ Root (createSubTree one 0) (createSubTree zero 0)
  where
    createSubTree :: Binary -> Word8 -> Tree
    createSubTree val depth =
        if depth == (totalDepth - 1)
            then (Leaf val undefined)
            else Node val depth
                    (createSubTree one (depth + 1))
                    (createSubTree zero (depth + 1))

fixLeafValues :: Root -> Root
fixLeafValues (Root leftTree rightTree) =
        Root (fixLeafValues' leftTree []) (fixLeafValues' rightTree [])
  where
    fixLeafValues' :: Tree -> [Binary] -> Tree
    fixLeafValues' (Node nodeVal depth leftTree rightTree) binaries =
        if depth >= power
            then
                Node nodeVal
                     depth
                     (fixLeafValues' leftTree (nodeVal:binaries))
                     (fixLeafValues' rightTree (nodeVal:binaries))
            else
                Node nodeVal depth (fixLeafValues' leftTree []) (fixLeafValues' rightTree [])
    fixLeafValues' (Leaf nodeVal _) binaries =
        Leaf nodeVal (computeLeafVal nodeVal (reverse binaries) (power - 1) 0)

    computeLeafVal :: Binary -> [Binary] -> Word8 -> Word8 -> Word8
    computeLeafVal leafNodeVal [] _ accum = accum + if leafNodeVal then 1 else 0
    computeLeafVal leafNodeVal (b:binaries) pow accum =
        if b
            then computeLeafVal leafNodeVal binaries (pow - 1) (accum + 2 ^ pow)
            else computeLeafVal leafNodeVal binaries (pow - 1) accum

printRight :: Tree -> IO ()
printRight (Node val depth _ rightTree) = do
        print val
        printRight rightTree
printRight (Leaf val leaf) = do
        print val
        print $ "(" ++ show leaf ++ ")"

leafValues :: Tree -> [Word8]
leafValues (Node _ _ leftTree rightTree) =
        leafValues leftTree ++ leafValues rightTree
leafValues (Leaf _ leafVal) = [leafVal]

allBoards :: [[Binary]]
allBoards = replicateM (fromIntegral totalDepth) [False, True]

flipBit :: [Binary] -> Word8 -> [Binary]
flipBit binaries bitNum =
        take bitNum' binaries ++ [not (binaries !! bitNum')] ++ drop (bitNum' + 1) binaries
  where
    bitNum' = fromIntegral bitNum

allPathsLeadToZero :: Root -> Bool
allPathsLeadToZero root = all id $ map (\board -> searchForPathToZero root board) allBoards

searchForPathToZero :: Root -> [Binary] -> Bool
searchForPathToZero root board =
        if pathValue root board == 0
            then True
            else searchForPathToZero' root board 0
  where
    searchForPathToZero' :: Root -> [Binary] -> Word8 -> Bool
    searchForPathToZero' root board depth
        | depth == totalDepth                       = False
        | pathValue root board                 == 0 = True
        | pathValue root (flipBit board depth) == 0 = True
        | otherwise =
            searchForPathToZero' root board (depth + 1)

pathValue :: Root -> [Binary] -> Word8
pathValue (Root ltree rtree) bins =
        if head bins
            then pathValueTree ltree (tail bins)
            else pathValueTree rtree (tail bins)
  where
    pathValueTree :: Tree -> [Binary] -> Word8
    pathValueTree (Node val _ leftTree rightTree) (b:binaries) =
        if b
            then pathValueTree leftTree binaries
            else pathValueTree rightTree binaries
    pathValueTree (Leaf _ leafVal) _ = leafVal

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
        print $ whatsYourName "myname"
        print $ whatsYourName ""
        let root@(Root left right) = createTree
        -- print "left"
        -- printRight $ left
        -- print "leftTree left"
        -- printRight $ _leftTree left
        -- print "_leftTree $ _leftTree left"
        -- printRight $ _leftTree $ _leftTree left
        -- print "_leftTree $ _leftTree $ _leftTree left"
        -- printRight $ _leftTree $ _leftTree $ _leftTree left
        -- print "_rightTree $ _leftTree $ _rightTree left"
        -- printRight $ _rightTree $ _leftTree $ _rightTree left
        -- print "leafvalues left"
        -- print $ leafValues left
        -- putStrLn $ groom left
        -- print "pathValue root ..."
        -- print $ pathValue root [True, True, False, False]
        -- print $ pathValue root [False, False, False, False, False, False, False, False]
        -- print "groom allBoards"
        -- putStrLn $ groom allBoards
        -- print "searchForPathToZero root ..."
        -- print $ searchForPathToZero root [False, False, False, False]
        -- print $ searchForPathToZero root [False, False, False, False, False, False, False, False]
        putStrLn $ "all paths lead to zero: " ++ show (allPathsLeadToZero root)
        return ()

