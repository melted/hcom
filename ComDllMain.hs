{-# OPTIONS -#include "ComDllMain_stub.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ComDllMain
-- Copyright   :  (c) Sigbjorn Finne, sof@forkIO.com 1999-2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  sof@forkIO.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'main-Main' to link with when building a DLL containing
-- a Haskell in-proc COM component.
-- 
-----------------------------------------------------------------------------
module ComDllMain where

import Main   ( comComponents )
import ComDll ( createIComDll )
import Com    ( putMessage )
import Foreign.Ptr

newComDll :: Ptr () -> IO (Ptr (Ptr ()))
newComDll handle = do
   (ip,_) <- createIComDll handle comComponents
   return ip

foreign export ccall "newComDll" newComDll :: Ptr () -> IO (Ptr (Ptr ()))
