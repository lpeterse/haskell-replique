{-# LANGUAGE ScopedTypeVariables #-}
module System.Terminal.Replique.TextEdit where

import System.Terminal

putDiff :: forall m. MonadScreen m => Size -> Position -> String -> String -> m Position
putDiff size pos0 old new 
    -- Old is longer: deletion is required
    | diffLen > 0 = do
        pos1 <- skip pos0 $ length prefix
        pos2 <- put pos1 new'
        delete pos2 diffLen postfix
    -- New is longer: insertion is required
    | diffLen < 0 = do
        pos1 <- skip pos0 $ length prefix
        pos2 <- put pos1 new'
        insert pos2 (abs diffLen) postfix
    -- Length is equal: just override difference
    | otherwise = do
        pos1 <- skip pos0 $ length prefix
        put pos1 new'
    where
        (prefix, (old', new'), postfix) = diff old new
        diffLen :: Int
        diffLen = length old' - length new'
        plus :: Position -> Int -> Position
        plus (Position r c) i = Position r' c'
            where
                (r',c') = quotRem (r * width size + c + i) (width size)
        skip :: Position -> Int -> m Position
        skip pos1 i = do
            let pos2 = pos1 `plus` i
            moveFromTo pos1 pos2
            pure pos2
        put :: Position -> String -> m Position
        put pos1 xs = do
            putString xs
            pure $ pos1 `plus` length xs
        moveFromTo :: Position -> Position -> m ()
        moveFromTo from to = do
            setCursorPosition to
        insert :: Position -> Int -> String -> m Position
        insert pos1 i xs = if i >= n
            then do
                putString (take n xs)
                if l > n
                    then insert pos2 i (drop n xs)
                    else pure $ pos1 `plus` l
            else do
                insertChars i
                putString (take i xs)
                if l > n
                    then putLn >> insert pos2 i (drop n xs)
                    else pure $ pos1 `plus` i
            where
                n = width size - col pos1
                l = length xs
                pos2 = Position (row pos1 + 1) 0
        delete :: Position -> Int -> String -> m Position
        delete pos1 _ [] = do
            eraseInLine EraseForward
            eraseInDisplay EraseForward
            pure pos1
        delete pos1 i xs
            | i >= n = do -- deletion spans more than one line
                pos2 <- put pos1 (take n xs)
                delete pos2 i (drop n xs)
            | otherwise = do -- characters may be shifted in from the right
                deleteChars i
                let pos2 = pos1 `plus` (n - i)
                moveFromTo pos1 pos2
                pos3 <- put pos2 (take i $ drop (n - i) xs)
                delete pos3 i (drop n xs)
            where
                n = width size - col pos1

diff :: String -> String -> (String, (String, String), String)
diff as bs = f as bs []
    where
        -- O(n): A lot of reverse, but everything is strict and tail recursive
        f xxs@(x:xs) yys@(y:ys) prefix
            | x == y    = f xs ys $! x:prefix
            | otherwise = g (reverse prefix) (reverse xxs) (reverse yys) []
        f xs ys prefix  = g (reverse prefix) (reverse xs) (reverse ys) []
        g prefix xxs@(x:xs) yys@(y:ys) postfix
            | x == y    = g prefix xs ys $! x:postfix
            | otherwise = (prefix, (reverse xxs, reverse yys), postfix)
        g prefix xs ys postfix = (prefix, (reverse xs, reverse ys), postfix)
