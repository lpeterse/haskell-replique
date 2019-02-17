{-# LANGUAGE ScopedTypeVariables #-}
module System.Terminal.Replique.Image where

import qualified Data.Text                        as T
import           Data.Text.Prettyprint.Doc
import           System.Terminal
import           Prelude hiding (putChar)

{-
runDiff :: (MonadColorPrinter m, MonadTerminal, Eq (Annotation m)) =>
    Columns -> (Row, Column) -> SimpleDocStream (Annotation m) -> SimpleDocStream (Annotation m) -> m (Row, Column)
runDiff cols pos = f pos [] []
    where
        -- | Computes the resulting position when i characters are added.
        add :: Int -> (Row, Column) -> (Row, Column)
        add i (row, col) = quotRem (cols * row + col) cols

        f pos aa bb SFail  yy     = putDocStream   pos bb yy
        f pos aa bb SEmpty yy     = putDocStream   pos bb yy
        f pos aa bb xx     SFail  = clearDocStream pos xx
        f pos aa bb xx     SEmpty = clearDocStream pos xx

        -- Char in the old document
        f pos aa bb (SChar _ xx) (SChar d yy)
                         = putChar d >> f (add 1 pos) aa bb xx yy
        f pos aa bb (SChar c xx) (SText k s yy)
            | k == 0     = f pos aa bb (SChar c xx) yy
            | k == 1     = putText s >> f (add 1 pos) aa bb xx yy
            | otherwise  = putChar (T.head s) >> f (add 1 pos) aa bb xx (SText (k - 1) (T.tail s) yy)
        f pos aa bb (SChar c xx) (SLine i yy)
                         = putChar ' ' >> f (add 1 pos) aa bb xx (SLine (i - 1) yy)

        -- Text in the old document
        f pos aa bb (SText l t xx) (SChar d yy)
            | l == 0                              = f  pos        aa bb xx (SChar d yy) -- Text is empty
            | l == 1 && T.head t == d && aa == bb = f (add 1 pos) aa bb xx          yy  -- All equal
            | otherwise                           = putChar d                           -- Override character
                                                 >> f (add 1 pos) aa bb (SText (l - 1) (T.tail t) xx) yy
        f pos aa bb (SText l t xx) (SText k s yy) = case T.commonPrefixes t s of
            Nothing
                | T.null t  -> f pos aa bb xx (SText k s yy)
                | T.null s  -> f pos aa bb (SText k s xx) yy
                | otherwise -> putNewText        -- Nothing in common
            Just triple
                | aa /= bb  -> putNewText        -- Something in common but annotations differ
                | otherwise -> skipCommon triple -- Something in common and annotations equal
            where
                putNewText
                    | l < k     = do
                        putText (T.take l s)
                        f (add l pos) aa bb xx (SText (k - l) (T.drop l s) yy)
                    | l > k     = do
                        putText s
                        f (add k pos) aa bb (SText (l - k) (T.drop k t) xx) yy
                    | otherwise = do
                        putText s
                        f (add k pos) aa bb xx yy
                skipCommon (common, suffixT, suffixS) = do
                    let commonLen = T.length common
                    skipWithScrolling common cols (snd pos)
                    f (add commonLen pos) aa bb (SText (l - commonLen) suffixT xx) (SText (k - commonLen) suffixS yy)
        f pos aa bb (SText l t xx) (SLine j yy)
            | l < j     = putText (T.replicate l " ") >> f (add l pos) aa bb xx (SLine (j - l) yy)
            | l > j     = putText (T.replicate j " ") >> f (add j pos) aa bb (SText (l - j) (T.drop j t) xx) yy
            | otherwise = putText (T.replicate j " ") >> f (add j pos) aa bb xx yy

        -- Line indent in the old document
        f pos aa bb (SLine i xx) (SChar d yy)
            | i == 0    = f pos aa bb xx (SChar d yy)
            | i == 1    = putChar d >> f (add 1 pos) aa bb xx yy
            | otherwise = putChar d >> f (add 1 pos) aa bb (SLine (i - 1) xx) yy
        f pos aa bb (SLine i xx) (SText k s yy)
            | i == 0    = f pos aa bb xx (SText k s yy)
            | k == 0    = f pos aa bb (SLine i xx) yy
            | i < k     = putText (T.take i s) >> f (add i pos) aa bb xx (SText (k - i) (T.drop i s) yy)
            | i > k     = putText s >> f (add k pos) aa bb (SLine (i - k) xx) yy
            | otherwise = putText s >> f (add k pos) aa bb xx yy
        f pos aa bb (SLine i xx) (SLine j yy)
            | i == 0    = f pos aa bb xx (SLine j yy)
            | j == 0    = f pos aa bb (SLine i xx) yy
            | i < j     = putText (T.replicate i " ") >> f (add i pos) aa bb xx (SLine (j - i) yy)
            | i > j     = putText (T.replicate j " ") >> f (add j pos) aa bb (SLine (i - j) xx) yy
            | otherwise = putText (T.replicate i " ") >> f (add j pos) aa bb xx yy

        -- Annotations:
        --  * Keep track of the annotation stacks of the old and new document.
        --  * Push and pop in the new document result in terminal commands.
        --    NB: Attributes may only be reset when they are no longer in
        --    the stack (attributes may be nested). For colors, they need
        --    to be re-set with the nearest color in the stack.
        --  * Push and pop in the new document is only relevant for subsequent
        --    elements to see whether the annotation state is equal or the
        --    the part needs to be overriden when it contains equal characters
        --    with differing annotations.
        f pos aa bb (SAnnPush a  xx) yy
            = f pos (a:aa) bb xx yy
        f pos aa bb xx (SAnnPush b yy)
            = setAnnotation b >> f pos aa (b:bb) xx yy
        f pos aa bb (SAnnPop xx) yy = case aa of
            []     -> f pos [] bb xx yy -- should be impossible
            (c:cc) -> f pos cc bb xx yy
        f pos aa bb xx (SAnnPop yy) = case bb of
            []     -> f pos aa [] xx yy -- should be impossible
            (c:cc) -> case filter (== c) cc of -- special Eq instance!
                []    -> resetAnnotation c >> f pos aa cc xx yy
                (e:_) -> setAnnotation   e >> f pos aa cc xx yy 

        putDocStream pos _       SFail          = pure pos
        putDocStream pos _       SEmpty         = pure pos
        putDocStream pos    aa  (SChar c    xx) = putChar c                    >> putDocStream pos    aa  xx
        putDocStream pos    aa  (SText _ t  xx) = putText t                    >> putDocStream pos    aa  xx
        putDocStream pos    aa  (SLine i    xx) = putText (T.replicate i " ")  >> putDocStream pos    aa  xx
        putDocStream pos    aa  (SAnnPush a xx) = setAnnotation a              >> putDocStream pos (a:aa) xx
        putDocStream pos (a:aa) (SAnnPop    xx) = case filter (== a) aa of
                                                    []    -> resetAnnotation a >> putDocStream pos    aa  xx
                                                    (e:_) -> setAnnotation   e >> putDocStream pos    aa  xx

        clearDocStream pos _ = clearLineRight >> clearScreenBelow >> pure pos

-- | Skip the given text by moving the cursor.
--
-- Intentionally inserts characters at the end of each line
-- in order to make the terminal scroll if necessary.
-- Returns the new horizontal cursor position.
--
-- |##################_                        |
-- |^                 ^                       ^|
-- |0                col = 18          cols - 1|
-- |{_______________ cols = 43 _______________}|
--
-- |##################iiiiiiiiiiiiiiiiiiiiiiiii|
-- |iiiiiiiiiiiiiiiiiiiiiiii_                  |
-- |                        ^                  |
-- |                       col'                |
-- | i = 49                                    |
-- | col' = col + i - cols = 18 + 49 - 43 = 24 |
--
skipWithScrolling :: MonadTerminal m => T.Text -> Columns -> Column -> m ()
skipWithScrolling t cols col = f col 0
    where
        lenTotal = T.length t
        lenRem i = lenTotal - i
        f c i
            -- Skipping complete:
            --   -> Return current position.
            | i + 1 >= lenTotal = pure ()
            -- Cursor is at line end:
            --   -> Print character from text and proceed.
            | c + 1 >= cols = do
                putChar (T.index t i)
                f 0 (i + 1)
            -- Cursor would stay in the same line:
            --   -> Jump there and return.
            | c + lenRem i < cols = do
                moveCursorRight (lenRem i)
                pure ()
            -- Cursor would exceed line length:
            --   -> Jump to line end and proceed.
            | otherwise = do
                let available = cols - c - 1
                moveCursorRight available
                f (c + available) (i + available)
-}
