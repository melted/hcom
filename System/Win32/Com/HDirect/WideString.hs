{-# OPTIONS_GHC -XCPP -fglasgow-exts #-}
-- Automatically generated by HaskellDirect (ihc.exe), snapshot 171208
-- Created: 23:37 Pacific Standard Time, Wednesday 17 December, 2008
-- Command line: -fno-qualified-names -fno-imports -fno-export-lists -fkeep-hresult -fout-pointers-are-not-refs -c System/Win32/Com/HDirect/WideString.idl -o System/Win32/Com/HDirect/WideString.hs

module System.Win32.Com.HDirect.WideString where



import System.Win32.Com.HDirect.PointerPrim      ( primAllocMemory )
import System.Win32.Com.HDirect.Pointer          ( stackFrame, freeMemory )
import Foreign.Ptr      ( castPtr, nullPtr, plusPtr )
import System.Win32.Com.HDirect.HDirect
import System.IO.Unsafe ( unsafePerformIO )

type LPWSTR = WideString
type LPCWSTR = WideString

newtype WideString = WideString (Ptr Wchar_t)

lengthWideString :: WideString -> Int
lengthWideString (WideString ws) = unsafePerformIO (lenWideString ws >>= return . fromIntegral)

stackWideString :: String -> (Ptr Wchar_t -> IO a) -> IO a
stackWideString str wcont = do
    stackString str $ \ len pstr -> do
      let wlen = wideStringLen pstr
      stackFrame (sizeofWideString*(wlen+1)) $ \ pwide -> do
        primStringToWide pstr (fromIntegral len) pwide wlen 
        wcont pwide

mkWideString :: String -> WideString
mkWideString ls = unsafePerformIO (stringToWide ls)

stringToWide :: String -> IO WideString
stringToWide str =
  stackString str $ \ len pstr -> do
  let wlen = wideStringLen pstr
   -- Note: we're using the task allocator here.
  pwide <- primAllocMemory ((wlen + 1) * sizeofWideString)
  primStringToWide pstr (fromIntegral len) (castPtr pwide) wlen
  unmarshallWideString (castPtr pwide)

-- Sometimes a (wchar_t*) double up as holding a 16-bit (yep, no kidding) -
-- higher 16 bits have to be zero, lower 16 the val. We *love* this stuff.
word16ToWideString :: Word16 -> IO WideString
word16ToWideString w = 
  return (WideString (plusPtr nullPtr (fromIntegral w)))

nullWideString :: WideString
nullWideString = WideString (nullPtr)

-- Does not belong here - at all.

stackString :: String -> (Int -> Ptr Char -> IO a) -> IO a
stackString str pcont = do
   let len = 1 + fromIntegral (length str)
   stackFrame len $ \ ptr -> do
      writeString False ptr str
      pcont (fromIntegral len) (castPtr ptr)

marshallWideString :: WideString -> IO (Ptr WideString)
marshallWideString (WideString ptr) = marshallPointer ptr >>= return.castPtr

marshallWideString2 :: String -> IO (Ptr WideString)
marshallWideString2 str = do
  wstr <- stringToWide str
  marshallWideString wstr

-- using 'Ptr a' is too weak, really - but avoids trivial
-- problems wrt WideString/Wchar_t confusion. ToDo: tidy up.
unmarshallWideString :: Ptr a -> IO WideString
unmarshallWideString ptr = do
  po <- unmarshallPointer ptr
  return (WideString (castPtr po))

unmarshallWideString2 :: Ptr a -> IO String
unmarshallWideString2 ptr = do
  po <- unmarshallPointer ptr
  wideToStr (WideString (castPtr po))

-- writeWideString doesn't copy the wide string, it merely
-- fills in a pointer to the wide string.

writeWideString :: Ptr WideString -> WideString -> IO ()
writeWideString ptr (WideString pstr) = writePointer (castPtr ptr) pstr

writeWideString2 :: Ptr WideString -> String -> IO ()
writeWideString2 ptr str = do
   pwstr <- marshallWideString2 str
   writePointer (castPtr ptr) pwstr

-- is this correct?
readWideString :: Ptr WideString -> IO WideString
readWideString p = unmarshallWideString (castPtr p)

readWideString2 :: Ptr WideString -> IO String
readWideString2 p = unmarshallWideString2 (castPtr p)

freeWideString :: Ptr WideString -> IO ()
freeWideString _ = return () --(WideString pstr) = freeMemory pstr

freeWString :: WideString -> IO ()
freeWString (WideString pstr) = freeMemory pstr

sizeofWideString :: Word32
sizeofWideString = 2  -- not set in stone.

wideToStr :: WideString -> IO String
wideToStr wide = do
   pwide    <- marshallWideString wide
   (pstr,_) <- wideToString (castPtr pwide)
   unmarshallString (castPtr pstr)


wideStringLen :: Ptr Char
              -> Word32
wideStringLen str =
  unsafePerformIO (prim_System_Win32_Com_HDirect_WideString_wideStringLen str)

foreign import ccall "wideStringLen" prim_System_Win32_Com_HDirect_WideString_wideStringLen :: Ptr Char -> IO Word32
wideToString :: Ptr Wchar_t
             -> IO (Ptr Char, Int32)
wideToString wstr =
  do
    pstr <- allocBytes (fromIntegral sizeofPtr)
    o_wideToString <- prim_System_Win32_Com_HDirect_WideString_wideToString wstr pstr
    pstr <- doThenFree free readPtr pstr
    return (pstr, o_wideToString)

foreign import ccall "wideToString" prim_System_Win32_Com_HDirect_WideString_wideToString :: Ptr Word16 -> Ptr (Ptr Char) -> IO Int32
lenWideString :: Ptr Wchar_t
              -> IO Int32
lenWideString wstr =
  prim_System_Win32_Com_HDirect_WideString_lenWideString wstr

foreign import ccall "lenWideString" prim_System_Win32_Com_HDirect_WideString_lenWideString :: Ptr Word16 -> IO Int32
primStringToWide :: Ptr Char
                 -> Word32
                 -> Ptr Wchar_t
                 -> Word32
                 -> IO Int32
primStringToWide str len wstr wlen =
  prim_System_Win32_Com_HDirect_WideString_primStringToWide str
                                                            len
                                                            wstr
                                                            wlen

foreign import ccall "primStringToWide" prim_System_Win32_Com_HDirect_WideString_primStringToWide :: Ptr Char -> Word32 -> Ptr Word16 -> Word32 -> IO Int32

