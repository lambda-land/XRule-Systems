{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module MFL.Par
  ( happyError
  , myLexer
  , pSCPL
  , pListDefn
  , pDefn
  , pStmt2
  , pLet
  , pStmt1
  , pCaseTerm
  , pLetInStmt
  , pType
  , pTypeN
  , pListTypeN
  , pListType
  , pListUIdent
  , pCasePattern
  , pListCaseTerm
  , pExp15
  , pExp13
  , pExp12
  , pExp11
  , pExp9
  , pExp8
  , pExp4
  , pExp3
  , pExp1
  , pListPIdent
  , pTypeAnnotation
  , pExp
  , pExp2
  , pExp5
  , pExp6
  , pExp7
  , pExp10
  , pExp14
  , pListExp
  , pConstantType
  ) where

import Prelude

import qualified MFL.Abs
import MFL.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap38 = HappyWrap38 ((MFL.Abs.BNFC'Position, Char))
happyIn38 :: ((MFL.Abs.BNFC'Position, Char)) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ((MFL.Abs.BNFC'Position, Double))
happyIn39 :: ((MFL.Abs.BNFC'Position, Double)) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ((MFL.Abs.BNFC'Position, String))
happyIn40 :: ((MFL.Abs.BNFC'Position, String)) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ((MFL.Abs.BNFC'Position, MFL.Abs.TokUnit))
happyIn41 :: ((MFL.Abs.BNFC'Position, MFL.Abs.TokUnit)) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ((MFL.Abs.BNFC'Position, MFL.Abs.UIdent))
happyIn42 :: ((MFL.Abs.BNFC'Position, MFL.Abs.UIdent)) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ((MFL.Abs.BNFC'Position, MFL.Abs.PIdent))
happyIn43 :: ((MFL.Abs.BNFC'Position, MFL.Abs.PIdent)) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ((MFL.Abs.BNFC'Position, MFL.Abs.PInteger))
happyIn44 :: ((MFL.Abs.BNFC'Position, MFL.Abs.PInteger)) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ((MFL.Abs.BNFC'Position, MFL.Abs.SCPL))
happyIn45 :: ((MFL.Abs.BNFC'Position, MFL.Abs.SCPL)) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ((MFL.Abs.BNFC'Position, [MFL.Abs.Defn]))
happyIn46 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.Defn])) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ((MFL.Abs.BNFC'Position, MFL.Abs.Defn))
happyIn47 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Defn)) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ((MFL.Abs.BNFC'Position, MFL.Abs.Stmt))
happyIn48 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Stmt)) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((MFL.Abs.BNFC'Position, MFL.Abs.Let))
happyIn49 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Let)) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 ((MFL.Abs.BNFC'Position, MFL.Abs.Stmt))
happyIn50 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Stmt)) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ((MFL.Abs.BNFC'Position, MFL.Abs.CaseTerm))
happyIn51 :: ((MFL.Abs.BNFC'Position, MFL.Abs.CaseTerm)) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ((MFL.Abs.BNFC'Position, MFL.Abs.LetInStmt))
happyIn52 :: ((MFL.Abs.BNFC'Position, MFL.Abs.LetInStmt)) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ((MFL.Abs.BNFC'Position, MFL.Abs.Type))
happyIn53 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Type)) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ((MFL.Abs.BNFC'Position, MFL.Abs.TypeN))
happyIn54 :: ((MFL.Abs.BNFC'Position, MFL.Abs.TypeN)) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ((MFL.Abs.BNFC'Position, [MFL.Abs.TypeN]))
happyIn55 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.TypeN])) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ((MFL.Abs.BNFC'Position, [MFL.Abs.Type]))
happyIn56 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.Type])) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((MFL.Abs.BNFC'Position, [MFL.Abs.UIdent]))
happyIn57 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.UIdent])) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 ((MFL.Abs.BNFC'Position, MFL.Abs.CasePattern))
happyIn58 :: ((MFL.Abs.BNFC'Position, MFL.Abs.CasePattern)) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ((MFL.Abs.BNFC'Position, [MFL.Abs.CaseTerm]))
happyIn59 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.CaseTerm])) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn60 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn61 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn62 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn63 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn64 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn65 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn66 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn67 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn68 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 ((MFL.Abs.BNFC'Position, [MFL.Abs.PIdent]))
happyIn69 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.PIdent])) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 ((MFL.Abs.BNFC'Position, MFL.Abs.TypeAnnotation))
happyIn70 :: ((MFL.Abs.BNFC'Position, MFL.Abs.TypeAnnotation)) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn71 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn72 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn73 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn74 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn75 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn76 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 ((MFL.Abs.BNFC'Position, MFL.Abs.Exp))
happyIn77 :: ((MFL.Abs.BNFC'Position, MFL.Abs.Exp)) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
newtype HappyWrap78 = HappyWrap78 ((MFL.Abs.BNFC'Position, [MFL.Abs.Exp]))
happyIn78 :: ((MFL.Abs.BNFC'Position, [MFL.Abs.Exp])) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap78 x)
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> HappyWrap78
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
newtype HappyWrap79 = HappyWrap79 ((MFL.Abs.BNFC'Position, MFL.Abs.ConstantType))
happyIn79 :: ((MFL.Abs.BNFC'Position, MFL.Abs.ConstantType)) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap79 x)
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> HappyWrap79
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xf0\x0a\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\xbc\x1a\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x20\x00\xf8\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x01\x01\xc0\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x08\x08\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x40\x40\x00\xf0\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x07\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x38\x00\x20\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x01\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x0e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x70\x00\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x08\x80\x03\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x1c\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\xe0\x00\x80\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x0f\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x1e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x70\x00\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x08\x80\x03\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x1c\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\xe0\x00\x80\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x07\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x38\x00\x20\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x03\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x03\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x0e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x3c\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x20\x00\xf8\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x40\x40\x00\xf0\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x02\x02\x80\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x10\x10\x00\x7c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x1e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xf0\x00\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\xaf\x06\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xf0\x00\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x02\x02\x80\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x20\x20\x00\xf8\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x01\x01\xc0\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\xf0\x6a\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x1c\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\xe0\x00\x80\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x07\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x38\x00\x20\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x01\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x0e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x70\x00\x40\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x08\x80\x03\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x1c\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\xe0\x00\x80\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x07\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x38\x00\x20\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x01\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x0e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\xc0\x03\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x1e\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\xaf\x06\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x78\x35\x20\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x5e\x01\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x00\x5e\x01\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pSCPL_internal","%start_pListDefn_internal","%start_pDefn_internal","%start_pStmt2_internal","%start_pLet_internal","%start_pStmt1_internal","%start_pCaseTerm_internal","%start_pLetInStmt_internal","%start_pType_internal","%start_pTypeN_internal","%start_pListTypeN_internal","%start_pListType_internal","%start_pListUIdent_internal","%start_pCasePattern_internal","%start_pListCaseTerm_internal","%start_pExp15_internal","%start_pExp13_internal","%start_pExp12_internal","%start_pExp11_internal","%start_pExp9_internal","%start_pExp8_internal","%start_pExp4_internal","%start_pExp3_internal","%start_pExp1_internal","%start_pListPIdent_internal","%start_pTypeAnnotation_internal","%start_pExp_internal","%start_pExp2_internal","%start_pExp5_internal","%start_pExp6_internal","%start_pExp7_internal","%start_pExp10_internal","%start_pExp14_internal","%start_pListExp_internal","%start_pConstantType_internal","Char","Double","String","TokUnit","UIdent","PIdent","PInteger","SCPL","ListDefn","Defn","Stmt2","Let","Stmt1","CaseTerm","LetInStmt","Type","TypeN","ListTypeN","ListType","ListUIdent","CasePattern","ListCaseTerm","Exp15","Exp13","Exp12","Exp11","Exp9","Exp8","Exp4","Exp3","Exp1","ListPIdent","TypeAnnotation","Exp","Exp2","Exp5","Exp6","Exp7","Exp10","Exp14","ListExp","ConstantType","'!='","'&&'","'('","')'","'*'","'+'","'++'","','","'-'","'->'","'/'","':'","'::'","';'","'<'","'<='","'='","'=='","'>'","'>='","'False'","'True'","'['","'\\\\'","']'","'case'","'else'","'if'","'in'","'let'","'letrec'","'of'","'then'","'{'","'||'","'}'","L_charac","L_doubl","L_quoted","L_TokUnit","L_UIdent","L_PIdent","L_PInteger","%eof"]
        bit_start = st * 123
        bit_end = (st + 1) * 123
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..122]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xda\xff\xda\xff\xda\xff\x88\x00\x45\x00\x92\x00\x7a\x00\xda\xff\xb1\x00\xb1\x00\xb1\x00\xb1\x00\xef\xff\x7a\x00\x7a\x00\x01\x01\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xb8\x00\x09\x00\x37\x00\xb8\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\x01\x01\xb8\x00\x2a\x01\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x00\x00\x00\x00\x6f\x00\x4d\x01\xc0\x00\x1a\x01\x30\x00\x81\x00\x66\x00\x00\x00\x99\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\xb8\x00\x01\x01\x00\x00\x00\x00\xb8\x00\x8c\x00\x00\x00\x8b\x00\x8b\x00\x8b\x00\x8b\x00\x8b\x00\x8b\x00\x8b\x00\x8b\x00\xc4\x00\x8e\x00\xa9\x00\xa9\x00\xfc\xff\xff\xff\x01\x00\x62\x00\x23\x00\x07\x01\x1b\x00\xa9\x00\xb4\x00\xd3\x00\xb9\x00\x00\x00\x00\x00\xb9\x00\xd9\x00\xba\x00\x00\x00\x00\x00\x00\x00\xdf\x00\xe4\x00\xc8\x00\x00\x00\xc4\x00\xc4\x00\xc4\x00\x00\x00\xf6\x00\xd4\x00\xd4\x00\xd4\x00\xee\x00\xd5\x00\xd5\x00\x00\x00\xe1\x00\xe7\x00\x00\x00\xdb\x00\xdb\x00\x00\x00\x00\x00\xe7\x00\xe7\x00\x09\x01\xf4\x00\xf4\x00\xf8\x00\x08\x01\x00\x00\x00\x00\x92\x00\x14\x01\x16\x01\x24\x01\xdb\x00\xd1\x00\x32\x01\x39\x01\x56\x01\xd1\x00\xd1\x00\x49\x01\x92\x00\x7a\x00\x01\x01\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\xfa\x00\x00\x00\x00\x00\x71\x01\x60\x01\x00\x00\x7d\x01\xf2\x00\xf2\x00\x7e\x01\x00\x00\x00\x00\x00\x00\x92\x00\x81\x01\x00\x00\x1a\x01\x1a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x01\x4d\x01\x8d\x01\x8d\x01\x8d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x62\x01\x88\x00\x00\x00\x7f\x01\x7a\x00\x00\x00\x00\x00\x00\x00\x79\x01\x88\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x54\x01\x6e\x01\x6b\x00\x7b\x02\x93\x01\xbd\x01\xfd\xff\xb0\x00\x36\x00\x96\x00\x3d\x00\x05\x00\x21\x00\x8c\x01\xb7\x00\xab\x02\x51\x01\x16\x05\x19\x01\xcb\x04\xb1\x04\x13\x04\xf7\x03\xab\x03\xfe\xff\x82\x01\xed\x02\xd1\x03\x49\x04\x7d\x04\x97\x04\xd9\x03\x14\x00\x21\x01\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x03\x85\x02\x00\x00\x00\x00\x48\x01\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x0c\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\x00\x00\x00\x00\x00\x39\x03\x5f\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x01\x00\x00\x00\x00\x00\x00\xe3\x01\x00\x00\x00\x00\x00\x00\x85\x03\x7c\x00\x00\x00\x00\x00\x00\x00\x84\x00\x20\x00\x52\x00\x09\x02\xbd\x00\xf6\x02\x78\x01\x9f\x01\xd0\x02\x2f\x05\x34\x05\xee\x04\xf3\x04\x0c\x05\x11\x05\xd0\x04\xe9\x04\x63\x04\x2e\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x01\x96\x01\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x02\x00\x00\xa1\x02\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x02\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\xff\x00\x00\x00\x00\xdc\xff\x84\xff\x83\xff\x85\xff\x86\xff\x00\x00\xdb\xff\xda\xff\xd6\xff\xb0\xff\xb1\xff\xb4\xff\x8a\xff\xa6\xff\xa3\xff\x8b\xff\x9b\xff\x8c\xff\x97\xff\x8f\xff\x90\xff\x88\xff\x95\xff\x99\xff\x8e\xff\x8d\xff\x9e\xff\xaa\xff\x00\x00\x00\x00\x00\x00\xb2\xff\xb3\xff\x89\xff\x00\x00\xd7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\x00\x00\x00\x00\xb7\xff\xb8\xff\x00\x00\xba\xff\x00\x00\xd8\xff\xc6\xff\xc5\xff\xbd\xff\xc7\xff\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\xd9\xff\xbf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\xcf\xff\x00\x00\x00\x00\xce\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\xd5\xff\xd3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x92\xff\x00\x00\x00\x00\xac\xff\x00\x00\x89\xff\x89\xff\x00\x00\x87\xff\xad\xff\xaf\xff\x00\x00\x98\xff\x9a\xff\x9d\xff\x9c\xff\x9f\xff\xa1\xff\xa0\xff\xa2\xff\xa4\xff\xa5\xff\xa7\xff\xa9\xff\xa8\xff\xab\xff\xb5\xff\xca\xff\xb9\xff\xbc\xff\xc8\xff\xc2\xff\xc3\xff\xc4\xff\xbe\xff\xc9\xff\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\xcc\xff\x96\xff\xae\xff\x00\x00\x00\x00\xd0\xff\xd1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x05\x00\x2a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0d\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x14\x00\x06\x00\x12\x00\x0f\x00\x10\x00\x02\x00\x12\x00\x29\x00\x05\x00\x06\x00\x0f\x00\x10\x00\x1f\x00\x12\x00\x23\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x04\x00\x06\x00\x0c\x00\x2c\x00\x06\x00\x16\x00\x2c\x00\x09\x00\x2c\x00\x29\x00\x0f\x00\x10\x00\x01\x00\x12\x00\x2a\x00\x13\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x27\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x12\x00\x06\x00\x0d\x00\x0f\x00\x10\x00\x2c\x00\x05\x00\x29\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x2c\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x04\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x25\x00\x06\x00\x29\x00\x0f\x00\x10\x00\x1f\x00\x1e\x00\x1f\x00\x13\x00\x29\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2c\x00\x06\x00\x05\x00\x0f\x00\x10\x00\x29\x00\x09\x00\x13\x00\x14\x00\x03\x00\x0f\x00\x10\x00\x29\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x29\x00\x06\x00\x02\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x23\x00\x06\x00\x03\x00\x10\x00\x11\x00\x2c\x00\x15\x00\x16\x00\x09\x00\x29\x00\x0f\x00\x10\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x09\x00\x06\x00\x15\x00\x16\x00\x17\x00\x18\x00\x08\x00\x1a\x00\x2c\x00\x1c\x00\x29\x00\x10\x00\x15\x00\x16\x00\x17\x00\x18\x00\x05\x00\x1a\x00\x29\x00\x1c\x00\x27\x00\x1e\x00\x1f\x00\x2a\x00\x2b\x00\x03\x00\x05\x00\x2a\x00\x2c\x00\x2a\x00\x27\x00\x05\x00\x03\x00\x2a\x00\x2b\x00\x0e\x00\x29\x00\x0f\x00\x09\x00\x0e\x00\x0e\x00\x0d\x00\x1f\x00\x06\x00\x03\x00\x17\x00\x09\x00\x0d\x00\x14\x00\x15\x00\x15\x00\x16\x00\x17\x00\x18\x00\x14\x00\x15\x00\x0f\x00\x03\x00\x2c\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x17\x00\x2b\x00\x0a\x00\x03\x00\x27\x00\x0f\x00\x08\x00\x2a\x00\x2b\x00\x09\x00\x2c\x00\x2c\x00\x08\x00\x17\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x0a\x00\x2b\x00\x15\x00\x16\x00\x17\x00\x18\x00\x2c\x00\x03\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x09\x00\x2b\x00\x03\x00\x08\x00\x11\x00\x2c\x00\x2c\x00\x27\x00\x09\x00\x03\x00\x2a\x00\x2b\x00\x15\x00\x16\x00\x17\x00\x18\x00\x2a\x00\x05\x00\x0d\x00\x07\x00\x15\x00\x16\x00\x17\x00\x0b\x00\x2c\x00\x14\x00\x15\x00\x15\x00\x16\x00\x17\x00\x27\x00\x11\x00\x02\x00\x2a\x00\x2b\x00\x05\x00\x06\x00\x2c\x00\x27\x00\x2a\x00\x02\x00\x2a\x00\x2b\x00\x05\x00\x06\x00\x27\x00\x0f\x00\x10\x00\x2a\x00\x2b\x00\x13\x00\x14\x00\x16\x00\x17\x00\x18\x00\x19\x00\x2c\x00\x2c\x00\x21\x00\x20\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x27\x00\x1d\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x02\x00\x19\x00\x13\x00\x05\x00\x06\x00\x25\x00\x26\x00\x27\x00\x05\x00\x02\x00\x07\x00\x2b\x00\x05\x00\x06\x00\x0b\x00\x05\x00\x04\x00\x07\x00\x08\x00\x09\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x17\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x02\x00\x29\x00\x05\x00\x05\x00\x06\x00\x08\x00\x09\x00\x27\x00\x19\x00\x02\x00\x0a\x00\x05\x00\x05\x00\x06\x00\x08\x00\x09\x00\x04\x00\x04\x00\x02\x00\x22\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x17\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x02\x00\x0c\x00\x1b\x00\x05\x00\x06\x00\x24\x00\x0b\x00\x27\x00\x14\x00\x02\x00\x20\x00\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x17\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\x27\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\xff\xff\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x16\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\x16\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x0a\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\x17\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x16\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x16\x00\x17\x00\x18\x00\x19\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\x26\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x02\x00\xff\xff\xff\xff\x05\x00\x06\x00\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\xff\xff\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\x02\x00\x26\x00\x27\x00\x05\x00\x06\x00\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xa2\x00\xa0\x00\x51\x00\x48\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x75\x00\x28\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x5d\x00\x28\x00\xa1\x00\x67\x00\x68\x00\x2d\x00\x69\x00\x65\x00\x2e\x00\x2f\x00\x67\x00\x68\x00\x52\x00\x8e\x00\xa3\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x62\x00\x28\x00\x96\x00\xff\xff\x9a\x00\x30\x00\xff\xff\x9b\x00\xff\xff\x6a\x00\x67\x00\x68\x00\xa0\x00\xc1\x00\x48\x00\x63\x00\x6a\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x48\x00\x28\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\xa1\x00\x28\x00\x51\x00\x72\x00\x68\x00\xff\xff\x51\x00\x6a\x00\x25\x00\x26\x00\x27\x00\x6f\x00\x70\x00\xff\xff\x28\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x62\x00\x28\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x25\x00\x28\x00\x6a\x00\xa4\x00\x68\x00\xa5\x00\x7d\x00\x7e\x00\xc0\x00\x6a\x00\x8f\x00\x68\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\xff\xff\x28\x00\x80\x00\x9c\x00\x9d\x00\x29\x00\x81\x00\x9e\x00\x9f\x00\xab\x00\x8d\x00\x68\x00\x6a\x00\x96\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x6a\x00\x28\x00\xa2\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\xa3\x00\x28\x00\x42\x00\x6f\x00\xc6\x00\xff\xff\x60\x00\x61\x00\x43\x00\x6a\x00\xc2\x00\x68\x00\x42\x00\x25\x00\x26\x00\x27\x00\x65\x00\x66\x00\x43\x00\x28\x00\x44\x00\x45\x00\x46\x00\x47\x00\xaa\x00\x7b\x00\xff\xff\x7c\x00\x6a\x00\x71\x00\x44\x00\x45\x00\x46\x00\x47\x00\x51\x00\x7b\x00\x6a\x00\x7c\x00\x2c\x00\x7d\x00\x7e\x00\x48\x00\x2d\x00\x6c\x00\x73\x00\x48\x00\xff\xff\x48\x00\x2c\x00\x73\x00\x42\x00\x48\x00\x2d\x00\x74\x00\x6a\x00\x6d\x00\x43\x00\x95\x00\x8a\x00\x5c\x00\xa3\x00\x9a\x00\x6c\x00\x6e\x00\x9b\x00\x5c\x00\x5d\x00\x5e\x00\x44\x00\x45\x00\x46\x00\x47\x00\x5d\x00\xbe\x00\x6d\x00\x6c\x00\xff\xff\x25\x00\x2b\x00\x2c\x00\x6f\x00\x65\x00\x6e\x00\x2d\x00\x94\x00\x42\x00\x2c\x00\x6d\x00\x93\x00\x48\x00\x2d\x00\x43\x00\xff\xff\xff\xff\x92\x00\x6e\x00\x25\x00\x2b\x00\x2c\x00\x6f\x00\x65\x00\x91\x00\x2d\x00\x44\x00\x45\x00\x46\x00\x47\x00\xff\xff\x42\x00\x25\x00\x2b\x00\x2c\x00\x6f\x00\x65\x00\x43\x00\x2d\x00\x42\x00\x8d\x00\x8c\x00\xff\xff\xff\xff\x2c\x00\x43\x00\x42\x00\x48\x00\x2d\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x97\x00\x5c\x00\x98\x00\x44\x00\x45\x00\x46\x00\x99\x00\xff\xff\x5d\x00\xd1\x00\x44\x00\x45\x00\x46\x00\x2c\x00\x88\x00\x2d\x00\x48\x00\x2d\x00\x2e\x00\x2f\x00\xff\xff\x2c\x00\x48\x00\x2d\x00\x48\x00\x2d\x00\x2e\x00\x2f\x00\x2c\x00\x9c\x00\x9d\x00\x48\x00\x2d\x00\x9e\x00\x9f\x00\x30\x00\x31\x00\x32\x00\x58\x00\xff\xff\xff\xff\xcb\x00\xca\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x3f\x00\xc9\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x2d\x00\xc6\x00\xc5\x00\x2e\x00\x2f\x00\x25\x00\x2b\x00\x2c\x00\x97\x00\x2d\x00\x98\x00\x2d\x00\x2e\x00\x2f\x00\x99\x00\x80\x00\xc4\x00\x84\x00\x85\x00\x83\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x30\x00\x5a\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xa6\x00\x2d\x00\x65\x00\x80\x00\x2e\x00\x2f\x00\x82\x00\x83\x00\x3f\x00\xaf\x00\x2d\x00\xb0\x00\x80\x00\x2e\x00\x2f\x00\x86\x00\x83\x00\xae\x00\xd1\x00\xa2\x00\xce\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x30\x00\xbc\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xac\x00\x2d\x00\x96\x00\xd3\x00\x2e\x00\x2f\x00\xd5\x00\x7e\x00\x3f\x00\x61\x00\x2d\x00\x4f\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x30\x00\xbb\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xab\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x3f\x00\x76\x00\x77\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x76\x00\x77\x00\xcb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x76\x00\x77\x00\xbf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x76\x00\x77\x00\xcf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x76\x00\x77\x00\xce\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x30\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\xcc\x00\xa7\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x5b\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\xd3\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x30\x00\xba\x00\x79\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x30\x00\x00\x00\x4e\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\xa8\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x89\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x88\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\xc7\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x53\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x30\x00\x31\x00\x32\x00\x33\x00\x4d\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x49\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x54\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x55\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\xb0\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x4c\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\xb1\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x3d\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x56\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x3e\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x57\x00\x30\x00\x31\x00\x32\x00\x33\x00\xb3\x00\x2d\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x2d\x00\x3e\x00\x3f\x00\x2e\x00\x2f\x00\x2d\x00\x3e\x00\x3f\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\xb2\x00\x30\x00\x31\x00\x32\x00\x33\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x00\x00\x2d\x00\x3e\x00\x3f\x00\x2e\x00\x2f\x00\x2d\x00\xb7\x00\x3f\x00\x2e\x00\x2f\x00\x2d\x00\xb6\x00\x3f\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x00\x00\x30\x00\x31\x00\x59\x00\x00\x00\x00\x00\x2d\x00\xb5\x00\x3f\x00\x2e\x00\x2f\x00\x2d\x00\xb4\x00\x3f\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\xb9\x00\x00\x00\x00\x00\x30\x00\x31\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (35, 124) [
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124)
	]

happy_n_terms = 45 :: Int
happy_n_nonterms = 42 :: Int

happyReduce_35 = happySpecReduce_1  0# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Char)
	)}

happyReduce_36 = happySpecReduce_1  1# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Double)
	)}

happyReduce_37 = happySpecReduce_1  2# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), ((\(PT _ (TL s)) -> s) happy_var_1))
	)}

happyReduce_38 = happySpecReduce_1  3# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TokUnit (mkPosToken happy_var_1))
	)}

happyReduce_39 = happySpecReduce_1  4# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.UIdent (mkPosToken happy_var_1))
	)}

happyReduce_40 = happySpecReduce_1  5# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.PIdent (mkPosToken happy_var_1))
	)}

happyReduce_41 = happySpecReduce_1  6# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.PInteger (mkPosToken happy_var_1))
	)}

happyReduce_42 = happySpecReduce_1  7# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn45
		 ((fst happy_var_1, MFL.Abs.SCPLPROG (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_43 = happySpecReduce_1  8# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn46
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_44 = happySpecReduce_2  8# happyReduction_44
happyReduction_44 happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	happyIn46
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_45 = happySpecReduce_3  9# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn47
		 ((fst happy_var_1, MFL.Abs.DEFN (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_46 = happyReduce 6# 10# happyReduction_46
happyReduction_46 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut59 happy_x_5 of { (HappyWrap59 happy_var_5) -> 
	happyIn48
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.CASESTMT (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_47 = happyReduce 6# 10# happyReduction_47
happyReduction_47 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut48 happy_x_4 of { (HappyWrap48 happy_var_4) -> 
	case happyOut48 happy_x_6 of { (HappyWrap48 happy_var_6) -> 
	happyIn48
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.IFSTMT (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_48 = happySpecReduce_1  10# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn48
		 ((fst happy_var_1, MFL.Abs.BARESTMT (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_49 = happySpecReduce_1  11# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.LET (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_50 = happySpecReduce_1  11# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.LRec (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_51 = happyReduce 4# 12# happyReduction_51
happyReduction_51 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut50 happy_x_4 of { (HappyWrap50 happy_var_4) -> 
	happyIn50
		 ((fst happy_var_1, MFL.Abs.ELet (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_52 = happySpecReduce_1  12# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn50
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_53 = happySpecReduce_3  13# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn51
		 ((fst happy_var_1, MFL.Abs.CASE_STMT (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_54 = happySpecReduce_3  14# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut71 happy_x_3 of { (HappyWrap71 happy_var_3) -> 
	happyIn52
		 ((fst happy_var_1, MFL.Abs.LET_IN_STMT (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_55 = happySpecReduce_3  15# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 ((fst happy_var_1, MFL.Abs.TYPEARROW (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_56 = happySpecReduce_1  15# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn53
		 ((fst happy_var_1, MFL.Abs.TYPENext (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_57 = happySpecReduce_1  16# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn54
		 ((fst happy_var_1, MFL.Abs.TYPEUNIT (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_58 = happySpecReduce_1  16# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn54
		 ((fst happy_var_1, MFL.Abs.TYPECONST_VAR (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_59 = happySpecReduce_3  16# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn54
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TYPELIST (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_60 = happySpecReduce_3  16# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn54
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TYPEPROD (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_61 = happySpecReduce_3  16# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn54
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TYPEBRACKET (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_62 = happySpecReduce_1  16# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut79 happy_x_1 of { (HappyWrap79 happy_var_1) -> 
	happyIn54
		 ((fst happy_var_1, MFL.Abs.CONST_TYPE (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_63 = happySpecReduce_0  17# happyReduction_63
happyReduction_63  =  happyIn55
		 ((MFL.Abs.BNFC'NoPosition, [])
	)

happyReduce_64 = happySpecReduce_1  17# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_65 = happySpecReduce_3  17# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn55
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_66 = happySpecReduce_1  18# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn56
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_67 = happySpecReduce_3  18# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn56
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_68 = happySpecReduce_0  19# happyReduction_68
happyReduction_68  =  happyIn57
		 ((MFL.Abs.BNFC'NoPosition, [])
	)

happyReduce_69 = happySpecReduce_1  19# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn57
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_70 = happySpecReduce_3  19# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn57
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_71 = happySpecReduce_1  20# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TRUE_PATTERN (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_72 = happySpecReduce_1  20# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.FALSE_PATTERN (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_73 = happySpecReduce_1  21# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn59
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_74 = happySpecReduce_3  21# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn59
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_75 = happySpecReduce_1  22# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn60
		 ((fst happy_var_1, MFL.Abs.EInt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_76 = happySpecReduce_1  22# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.ETrue (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_77 = happySpecReduce_1  22# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.EFalse (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_78 = happySpecReduce_1  22# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn60
		 ((fst happy_var_1, MFL.Abs.EVar (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_79 = happySpecReduce_1  22# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn60
		 ((fst happy_var_1, MFL.Abs.EString (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_80 = happySpecReduce_3  22# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut78 happy_x_2 of { (HappyWrap78 happy_var_2) -> 
	happyIn60
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.EList (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_81 = happyReduce 4# 22# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn60
		 ((fst happy_var_1, MFL.Abs.ECall (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_82 = happySpecReduce_3  22# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	happyIn60
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_83 = happySpecReduce_2  23# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut77 happy_x_2 of { (HappyWrap77 happy_var_2) -> 
	happyIn61
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.ENeg (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_84 = happySpecReduce_3  23# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	case happyOut77 happy_x_3 of { (HappyWrap77 happy_var_3) -> 
	happyIn61
		 ((fst happy_var_1, MFL.Abs.CONSTERM (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_85 = happySpecReduce_1  23# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut77 happy_x_1 of { (HappyWrap77 happy_var_1) -> 
	happyIn61
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_86 = happySpecReduce_3  24# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	happyIn62
		 ((fst happy_var_1, MFL.Abs.EAPPEND (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_87 = happySpecReduce_3  24# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	happyIn62
		 ((fst happy_var_1, MFL.Abs.EMul (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_88 = happySpecReduce_3  24# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	case happyOut61 happy_x_3 of { (HappyWrap61 happy_var_3) -> 
	happyIn62
		 ((fst happy_var_1, MFL.Abs.EDiv (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_89 = happySpecReduce_1  24# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn62
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_90 = happySpecReduce_3  25# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	happyIn63
		 ((fst happy_var_1, MFL.Abs.EAdd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_91 = happySpecReduce_3  25# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	happyIn63
		 ((fst happy_var_1, MFL.Abs.ESub (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_92 = happySpecReduce_1  25# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn63
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_93 = happySpecReduce_3  26# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn64
		 ((fst happy_var_1, MFL.Abs.ELt (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_94 = happySpecReduce_3  26# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn64
		 ((fst happy_var_1, MFL.Abs.EGt (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_95 = happySpecReduce_3  26# happyReduction_95
happyReduction_95 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn64
		 ((fst happy_var_1, MFL.Abs.ELEq (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_96 = happySpecReduce_3  26# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	happyIn64
		 ((fst happy_var_1, MFL.Abs.EGEq (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_97 = happySpecReduce_1  26# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn64
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_98 = happySpecReduce_3  27# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	happyIn65
		 ((fst happy_var_1, MFL.Abs.EEq (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_99 = happySpecReduce_3  27# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	happyIn65
		 ((fst happy_var_1, MFL.Abs.ENEq (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_100 = happySpecReduce_1  27# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn65
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_101 = happySpecReduce_3  28# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	case happyOut73 happy_x_3 of { (HappyWrap73 happy_var_3) -> 
	happyIn66
		 ((fst happy_var_1, MFL.Abs.EAnd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_102 = happySpecReduce_1  28# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	happyIn66
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_103 = happySpecReduce_3  29# happyReduction_103
happyReduction_103 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn67
		 ((fst happy_var_1, MFL.Abs.EOr (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_104 = happySpecReduce_1  29# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	happyIn67
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_105 = happyReduce 4# 30# happyReduction_105
happyReduction_105 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	case happyOut50 happy_x_4 of { (HappyWrap50 happy_var_4) -> 
	happyIn68
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.ELambda (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_106 = happySpecReduce_1  30# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	happyIn68
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_107 = happySpecReduce_1  31# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn69
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_108 = happySpecReduce_2  31# happyReduction_108
happyReduction_108 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut69 happy_x_2 of { (HappyWrap69 happy_var_2) -> 
	happyIn69
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_109 = happySpecReduce_2  32# happyReduction_109
happyReduction_109 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn70
		 ((uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1), MFL.Abs.TYPEANNOTATION (uncurry MFL.Abs.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_110 = happySpecReduce_0  32# happyReduction_110
happyReduction_110  =  happyIn70
		 ((MFL.Abs.BNFC'NoPosition, MFL.Abs.TYPEANNOTATION_EMPTY MFL.Abs.BNFC'NoPosition)
	)

happyReduce_111 = happySpecReduce_1  33# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	happyIn71
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_112 = happySpecReduce_1  34# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn72
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_113 = happySpecReduce_1  35# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn73
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_114 = happySpecReduce_1  36# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn74
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_115 = happySpecReduce_1  37# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn75
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_116 = happySpecReduce_1  38# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn76
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_117 = happySpecReduce_1  39# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn77
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_118 = happySpecReduce_0  40# happyReduction_118
happyReduction_118  =  happyIn78
		 ((MFL.Abs.BNFC'NoPosition, [])
	)

happyReduce_119 = happySpecReduce_1  40# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn78
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_120 = happySpecReduce_3  40# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	case happyOut78 happy_x_3 of { (HappyWrap78 happy_var_3) -> 
	happyIn78
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_121 = happySpecReduce_1  41# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn79
		 ((fst happy_var_1, MFL.Abs.INTEGER (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_122 = happySpecReduce_1  41# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn79
		 ((fst happy_var_1, MFL.Abs.STRING (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_123 = happySpecReduce_1  41# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn79
		 ((fst happy_var_1, MFL.Abs.CHAR (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_124 = happySpecReduce_1  41# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn79
		 ((fst happy_var_1, MFL.Abs.DOUBLE (fst happy_var_1) (snd happy_var_1))
	)}

happyNewToken action sts stk [] =
	happyDoAction 44# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TC _) -> cont 37#;
	PT _ (TD _) -> cont 38#;
	PT _ (TL _) -> cont 39#;
	PT _ (T_TokUnit _) -> cont 40#;
	PT _ (T_UIdent _) -> cont 41#;
	PT _ (T_PIdent _) -> cont 42#;
	PT _ (T_PInteger _) -> cont 43#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pSCPL_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListDefn_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pDefn_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pStmt2_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pLet_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pStmt1_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pCaseTerm_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pLetInStmt_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pTypeN_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pListTypeN_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pListType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pListUIdent_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pCasePattern_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pListCaseTerm_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pExp15_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pExp13_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pExp12_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pExp11_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pExp9_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pExp8_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pExp4_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pExp3_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pExp1_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pListPIdent_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

pTypeAnnotation_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap70 x') = happyOut70 x} in x'))

pExp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap71 x') = happyOut71 x} in x'))

pExp2_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap72 x') = happyOut72 x} in x'))

pExp5_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap73 x') = happyOut73 x} in x'))

pExp6_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap74 x') = happyOut74 x} in x'))

pExp7_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap75 x') = happyOut75 x} in x'))

pExp10_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (let {(HappyWrap76 x') = happyOut76 x} in x'))

pExp14_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (let {(HappyWrap77 x') = happyOut77 x} in x'))

pListExp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (let {(HappyWrap78 x') = happyOut78 x} in x'))

pConstantType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 34# tks) (\x -> happyReturn (let {(HappyWrap79 x') = happyOut79 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pSCPL :: [Token] -> Err MFL.Abs.SCPL
pSCPL = fmap snd . pSCPL_internal

pListDefn :: [Token] -> Err [MFL.Abs.Defn]
pListDefn = fmap snd . pListDefn_internal

pDefn :: [Token] -> Err MFL.Abs.Defn
pDefn = fmap snd . pDefn_internal

pStmt2 :: [Token] -> Err MFL.Abs.Stmt
pStmt2 = fmap snd . pStmt2_internal

pLet :: [Token] -> Err MFL.Abs.Let
pLet = fmap snd . pLet_internal

pStmt1 :: [Token] -> Err MFL.Abs.Stmt
pStmt1 = fmap snd . pStmt1_internal

pCaseTerm :: [Token] -> Err MFL.Abs.CaseTerm
pCaseTerm = fmap snd . pCaseTerm_internal

pLetInStmt :: [Token] -> Err MFL.Abs.LetInStmt
pLetInStmt = fmap snd . pLetInStmt_internal

pType :: [Token] -> Err MFL.Abs.Type
pType = fmap snd . pType_internal

pTypeN :: [Token] -> Err MFL.Abs.TypeN
pTypeN = fmap snd . pTypeN_internal

pListTypeN :: [Token] -> Err [MFL.Abs.TypeN]
pListTypeN = fmap snd . pListTypeN_internal

pListType :: [Token] -> Err [MFL.Abs.Type]
pListType = fmap snd . pListType_internal

pListUIdent :: [Token] -> Err [MFL.Abs.UIdent]
pListUIdent = fmap snd . pListUIdent_internal

pCasePattern :: [Token] -> Err MFL.Abs.CasePattern
pCasePattern = fmap snd . pCasePattern_internal

pListCaseTerm :: [Token] -> Err [MFL.Abs.CaseTerm]
pListCaseTerm = fmap snd . pListCaseTerm_internal

pExp15 :: [Token] -> Err MFL.Abs.Exp
pExp15 = fmap snd . pExp15_internal

pExp13 :: [Token] -> Err MFL.Abs.Exp
pExp13 = fmap snd . pExp13_internal

pExp12 :: [Token] -> Err MFL.Abs.Exp
pExp12 = fmap snd . pExp12_internal

pExp11 :: [Token] -> Err MFL.Abs.Exp
pExp11 = fmap snd . pExp11_internal

pExp9 :: [Token] -> Err MFL.Abs.Exp
pExp9 = fmap snd . pExp9_internal

pExp8 :: [Token] -> Err MFL.Abs.Exp
pExp8 = fmap snd . pExp8_internal

pExp4 :: [Token] -> Err MFL.Abs.Exp
pExp4 = fmap snd . pExp4_internal

pExp3 :: [Token] -> Err MFL.Abs.Exp
pExp3 = fmap snd . pExp3_internal

pExp1 :: [Token] -> Err MFL.Abs.Exp
pExp1 = fmap snd . pExp1_internal

pListPIdent :: [Token] -> Err [MFL.Abs.PIdent]
pListPIdent = fmap snd . pListPIdent_internal

pTypeAnnotation :: [Token] -> Err MFL.Abs.TypeAnnotation
pTypeAnnotation = fmap snd . pTypeAnnotation_internal

pExp :: [Token] -> Err MFL.Abs.Exp
pExp = fmap snd . pExp_internal

pExp2 :: [Token] -> Err MFL.Abs.Exp
pExp2 = fmap snd . pExp2_internal

pExp5 :: [Token] -> Err MFL.Abs.Exp
pExp5 = fmap snd . pExp5_internal

pExp6 :: [Token] -> Err MFL.Abs.Exp
pExp6 = fmap snd . pExp6_internal

pExp7 :: [Token] -> Err MFL.Abs.Exp
pExp7 = fmap snd . pExp7_internal

pExp10 :: [Token] -> Err MFL.Abs.Exp
pExp10 = fmap snd . pExp10_internal

pExp14 :: [Token] -> Err MFL.Abs.Exp
pExp14 = fmap snd . pExp14_internal

pListExp :: [Token] -> Err [MFL.Abs.Exp]
pListExp = fmap snd . pListExp_internal

pConstantType :: [Token] -> Err MFL.Abs.ConstantType
pConstantType = fmap snd . pConstantType_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
