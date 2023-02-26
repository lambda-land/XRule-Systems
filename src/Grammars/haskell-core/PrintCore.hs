-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintCore.

module PrintCore where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsCore

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsCore.Ident where
  prt _ (AbsCore.Ident i) = doc $ showString i
instance Print (AbsCore.Module' a) where
  prt i = \case
    AbsCore.Module _ id_ tdefs vdefgs -> prPrec i 0 (concatD [doc (showString "%module"), prt 0 id_, prt 0 tdefs, prt 0 vdefgs])

instance Print (AbsCore.Tdef' a) where
  prt i = \case
    AbsCore.Data _ qualident tbinds cdefs -> prPrec i 0 (concatD [doc (showString "%data"), prt 0 qualident, prt 0 tbinds, doc (showString "="), doc (showString "{"), prt 0 cdefs, doc (showString "}")])
    AbsCore.Newtype _ qualident tbinds maybety -> prPrec i 0 (concatD [doc (showString "%newtype"), prt 0 qualident, prt 0 tbinds, prt 0 maybety])

instance Print [AbsCore.Tdef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsCore.MaybeTy' a) where
  prt i = \case
    AbsCore.JustTy _ ty -> prPrec i 0 (concatD [doc (showString "="), prt 0 ty])
    AbsCore.NoTy _ -> prPrec i 0 (concatD [])

instance Print (AbsCore.Cdef' a) where
  prt i = \case
    AbsCore.Constr _ qualident atbinds tyts -> prPrec i 0 (concatD [prt 0 qualident, prt 0 atbinds, prt 0 tyts])

instance Print [AbsCore.Tyt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCore.Tyt' a) where
  prt i = \case
    AbsCore.TT _ ty -> prPrec i 0 (concatD [prt 2 ty])

instance Print [AbsCore.Cdef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsCore.Vdefg' a) where
  prt i = \case
    AbsCore.Rec _ vdefs -> prPrec i 0 (concatD [doc (showString "%rec"), doc (showString "{"), prt 0 vdefs, doc (showString "}")])
    AbsCore.Nonrec _ vdef -> prPrec i 0 (concatD [prt 0 vdef])

instance Print [AbsCore.Vdefg' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsCore.Vdef' a) where
  prt i = \case
    AbsCore.VdefQ _ qualident ty exp -> prPrec i 0 (concatD [prt 0 qualident, doc (showString "::"), prt 0 ty, doc (showString "="), prt 0 exp])
    AbsCore.VdefU _ id_ ty exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "::"), prt 0 ty, doc (showString "="), prt 0 exp])

instance Print [AbsCore.Vdef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsCore.Exp' a) where
  prt i = \case
    AbsCore.Var _ id_ -> prPrec i 2 (concatD [prt 0 id_])
    AbsCore.Dcon _ qualident -> prPrec i 2 (concatD [prt 0 qualident])
    AbsCore.Litc _ lit -> prPrec i 2 (concatD [prt 0 lit])
    AbsCore.App _ exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, prt 2 exp2])
    AbsCore.Appt _ exp ty -> prPrec i 1 (concatD [prt 1 exp, doc (showString "@"), prt 2 ty])
    AbsCore.Lams _ binds exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 binds, doc (showString "->"), prt 0 exp])
    AbsCore.Let _ vdefg exp -> prPrec i 0 (concatD [doc (showString "%let"), prt 0 vdefg, doc (showString "%in"), prt 0 exp])
    AbsCore.Case _ exp vbind alts -> prPrec i 0 (concatD [doc (showString "%case"), prt 2 exp, doc (showString "%of"), prt 0 vbind, doc (showString "{"), prt 0 alts, doc (showString "}")])
    AbsCore.Coerce _ ty exp -> prPrec i 0 (concatD [doc (showString "%coerce"), prt 2 ty, prt 0 exp])
    AbsCore.Note _ str exp -> prPrec i 0 (concatD [doc (showString "%note"), printString str, prt 0 exp])
    AbsCore.External _ str ty -> prPrec i 0 (concatD [doc (showString "%external"), printString str, prt 0 ty])

instance Print (AbsCore.Bind' a) where
  prt i = \case
    AbsCore.Vb _ vbind -> prPrec i 0 (concatD [prt 0 vbind])
    AbsCore.Tb _ tbind -> prPrec i 0 (concatD [doc (showString "@"), prt 0 tbind])

instance Print [AbsCore.Bind' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCore.Alt' a) where
  prt i = \case
    AbsCore.Acon _ qualident atbinds vbinds exp -> prPrec i 0 (concatD [prt 0 qualident, prt 0 atbinds, prt 0 vbinds, doc (showString "->"), prt 0 exp])
    AbsCore.Alit _ lit exp -> prPrec i 0 (concatD [prt 0 lit, doc (showString "->"), prt 0 exp])
    AbsCore.Adefault _ exp -> prPrec i 0 (concatD [doc (showString "%_"), doc (showString "->"), prt 0 exp])

instance Print [AbsCore.Alt' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (AbsCore.Vbind' a) where
  prt i = \case
    AbsCore.Vbind _ id_ ty -> prPrec i 0 (concatD [doc (showString "("), prt 0 id_, doc (showString "::"), prt 0 ty, doc (showString ")")])

instance Print [AbsCore.Vbind' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCore.Tbind' a) where
  prt i = \case
    AbsCore.TbindPair _ id_ kind -> prPrec i 0 (concatD [doc (showString "("), prt 0 id_, doc (showString "::"), prt 1 kind, doc (showString ")")])
    AbsCore.TbindLift _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print [AbsCore.Tbind' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCore.ATbind' a) where
  prt i = \case
    AbsCore.ATbind _ tbind -> prPrec i 0 (concatD [doc (showString "@"), prt 0 tbind])

instance Print [AbsCore.ATbind' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCore.Ty' a) where
  prt i = \case
    AbsCore.Tvar _ id_ -> prPrec i 2 (concatD [prt 0 id_])
    AbsCore.Tcon _ qualident -> prPrec i 2 (concatD [prt 0 qualident])
    AbsCore.Tapp _ ty1 ty2 -> prPrec i 1 (concatD [prt 1 ty1, prt 2 ty2])
    AbsCore.TArrow _ ty1 ty2 -> prPrec i 0 (concatD [prt 1 ty1, doc (showString "->"), prt 0 ty2])
    AbsCore.Tforalls _ tbinds ty -> prPrec i 0 (concatD [doc (showString "%forall"), prt 0 tbinds, doc (showString "."), prt 0 ty])

instance Print (AbsCore.Kind' a) where
  prt i = \case
    AbsCore.Klifted _ -> prPrec i 1 (concatD [doc (showString "*")])
    AbsCore.Kunlifted _ -> prPrec i 1 (concatD [doc (showString "#")])
    AbsCore.Kopen _ -> prPrec i 1 (concatD [doc (showString "?")])
    AbsCore.Karrow _ kind1 kind2 -> prPrec i 0 (concatD [prt 1 kind1, doc (showString "->"), prt 0 kind2])

instance Print (AbsCore.Lit' a) where
  prt i = \case
    AbsCore.Lint _ n ty -> prPrec i 0 (concatD [doc (showString "("), prt 0 n, doc (showString "::"), prt 2 ty, doc (showString ")")])
    AbsCore.Lrational _ d ty -> prPrec i 0 (concatD [doc (showString "("), prt 0 d, doc (showString "::"), prt 2 ty, doc (showString ")")])
    AbsCore.Lchar _ c ty -> prPrec i 0 (concatD [doc (showString "("), prt 0 c, doc (showString "::"), prt 2 ty, doc (showString ")")])
    AbsCore.Lstring _ str ty -> prPrec i 0 (concatD [doc (showString "("), printString str, doc (showString "::"), prt 2 ty, doc (showString ")")])

instance Print (AbsCore.QualIdent' a) where
  prt i = \case
    AbsCore.Qual _ id_1 id_2 -> prPrec i 0 (concatD [prt 0 id_1, doc (showString "."), prt 0 id_2])
