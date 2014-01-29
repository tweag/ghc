-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ref
-- Copyright   :  (C) 2014 EURL Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module indicate
-- package, module and name of a value. This information could be used to locate
-- the value in different processes.
--
-- Currently, the main use case for references is the StaticValues language
-- extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
#endif
module GHC.Ref
  ( Ref(..)
  , GlobalName(..)
#ifdef __GLASGOW_HASKELL__
  , deRef
#endif
  ) where

import Data.Typeable    (Typeable)

#ifdef __GLASGOW_HASKELL__
import Data.Char
import Foreign.C.String ( withCString, CString )
import GHC.Exts         ( addrToAny# )
import GHC.Ptr          ( Ptr(..), nullPtr )
import Numeric
import System.Info      ( os )
#endif


-- | A reference to a top-level value of type 'a'.
data Ref a = Ref { unRef :: GlobalName }
  deriving (Read, Show, Typeable)

-- | Global names identifying top-level values
--
-- > GlobalName package_id installed_package_id module_name value_name
--
-- In essence, a 'GlobalName' augments the information provided by
-- 'Language.Haskell.TH.Syntax.Name' with the information in the
-- @installed_package_id@ field. This field is
-- Cabal:'Distribution.Package.InstalledPackageId' and it
-- is needed to identify the package when multiple variations of it are
-- installed.
--
data GlobalName = GlobalName String String String String
  deriving (Read, Show, Typeable)

#ifdef __GLASGOW_HASKELL__
-- | An unsafe lookup function for symbolic references.
--
-- @deRef (r :: Ref a)@ returns @Nothing@ if no associated value is found for
-- @r@, and @Just v@ if @v@ is the value associated to @r@ and it has the
-- expected type @a@.
--
-- This function is unsafe because if an associated value @v@ of a different
-- type is found, then the behavior of this function is undefined.
--
-- Currently, a value is considered associated to a reference if the symbols of
-- the module producing the reference are made available at runtime.
-- This can be achieved by linking the module as part of a shared library, or by
-- loading the module using the RTS linker, or by adding the symbols of the
-- program executable to the dynamic symbol table with platform specific
-- mechanisms. In Linux, the later is done by passing
-- @-optl-Wl,--export-dynamic@ to GHC when linking the program.
--
-- This function is only available with the GHC compiler.
--
deRef :: Ref a -> IO (Maybe a)
deRef (Ref (GlobalName pkg _ m n)) = do
    let mpkg = case pkg of
                 "main" -> Nothing
                 _ -> Just pkg
    loadFunction mpkg m n

-- loadFunction__ taken from
-- @plugins-1.5.4.0:System.Plugins.Load.loadFunction__@
loadFunction :: Maybe String
             -> String
             -> String
             -> IO (Maybe a)
loadFunction mpkg m valsym = do
    let symbol = prefixUnderscore
                   ++ maybe "" (\p -> zEncodeString p ++ "_") mpkg
                   ++ zEncodeString m ++ "_" ++ zEncodeString valsym
                   ++ "_closure"
    ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
    if (ptr == nullPtr)
    then return Nothing
    else case addrToAny# addr of
           (# hval #) -> return ( Just hval )
  where
    prefixUnderscore = if elem os ["darwin","mingw32","cygwin"] then "_" else ""

foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

-----------------------------------------------------------------
-- The following definitions are copied from the zenc package. --
-----------------------------------------------------------------

type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form


zEncodeString :: UserString -> EncodedString
zEncodeString s = case maybe_tuple s of
                Just n  -> n            -- Tuples go to Z2T etc
                Nothing -> go s
          where
                go []     = []
                go (c:cs) = encode_digit_ch c ++ go' cs
                go' []     = []
                go' (c:cs) = encode_ch c ++ go' cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'

-- If a digit is at the start of a symbol then we need to encode it.
-- Otherwise package names like 9pH-0.1 give linker errors.
encode_digit_ch :: Char -> EncodedString
encode_digit_ch c | c >= '0' && c <= '9' = encode_as_unicode_char c
encode_digit_ch c | otherwise            = encode_ch c

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '('  = "ZL"   -- Needed for things like (,), and (->)
encode_ch ')'  = "ZR"   -- For symmetry with (
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"

-- Variables
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c = 'z' : if isDigit (head hex_str) then hex_str
                                                           else '0':hex_str
  where hex_str = showHex (ord c) "U"
  -- ToDo: we could improve the encoding here in various ways.
  -- eg. strings of unicode characters come out as 'z1234Uz5678U', we
  -- could remove the 'U' in the middle (the 'z' works as a separator).

{-
Tuples are encoded as
        Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts
        Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.
-}

maybe_tuple :: UserString -> Maybe EncodedString

maybe_tuple "(# #)" = Just("Z1H")
maybe_tuple ('(' : '#' : cs) = case count_commas (0::Int) cs of
                                 (n, '#' : ')' : _) -> Just ('Z' : shows (n+1) "H")
                                 _                  -> Nothing
maybe_tuple "()" = Just("Z0T")
maybe_tuple ('(' : cs)       = case count_commas (0::Int) cs of
                                 (n, ')' : _) -> Just ('Z' : shows (n+1) "T")
                                 _            -> Nothing
maybe_tuple _                = Nothing

count_commas :: Int -> String -> (Int, String)
count_commas n (',' : cs) = count_commas (n+1) cs
count_commas n cs         = (n,cs)

#endif
