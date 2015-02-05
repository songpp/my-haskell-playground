{-# LANGUAGE LambdaCase #-}
module JavaClassParser where

import           Control.Applicative             ((<$>))
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as L
import           Data.Word

data CPEntry = CClass { nameIndex :: Word16 }
             | CFieldRef { clsIndex :: Word16, ntIndex :: Word16 }
             | CMethodRef { clsIndex :: Word16, ntIndex :: Word16 }
             | CInterfaceRef { clsIndex :: Word16, ntIndex :: Word16 }
             | CString { strIndex :: Word16 }
             | CInt { bytes :: Word32 }
             | CFloat { bytes :: Word32 }
             | CLong { highBytes :: Word32, lowBytes :: Word32 }
             | CDouble { highBytes :: Word32, lowBytes :: Word32 }
             | CNameAndType { nameIndex :: Word16, descriptorIndex :: Word16 }
             | CUtf8 { len :: Word16, bs :: [Word8] }
             | CMethodHandle { refKind :: Word8, refIndex :: Word16 }
             | CMethodType { descriptorIndex :: Word16 }
             | CInvokeDynamic { bootstrapMethodAttrIndex :: Word16, ntIndex :: Word16 }
             deriving (Show, Eq)

data Version = Version { minor :: Word16, major :: Word16 } deriving (Show, Eq)

testClass = "Lambdas.class"
testClassBytes = L.readFile testClass

magicP = word32be 0xCAFEBABE

versinP = Version <$> (fromIntegral <$> anyWord16be)
                  <*> (fromIntegral <$> anyWord16be)

runParse bs = do
  case parse parseClass bs of
    e@Fail {} -> error $ show e
    Done _ v -> v

parseClass = do
  _ <- magicP
  version <- versinP
  cpoolCount <- fromIntegral <$> anyWord16be
  es <- count (cpoolCount - 1) parseEntry
  accFlag <- anyWord16be
  this <- anyWord16be
  super <- anyWord16be
  intfCount <- anyWord16be
  return (version, cpoolCount, accFlag, intfCount)

parseEntry = do
  tag <- anyWord8
  entries <- parseConstantPoolEntry tag
  return entries

parseConstantPoolEntry :: Word8 -> Parser CPEntry
parseConstantPoolEntry = \case
    1  -> parseCUtf8
    3  -> CInt <$> anyWord32be
    4  -> CFloat <$> anyWord32be
    5  -> CLong <$> anyWord32be <*> anyWord32be
    6  -> CDouble <$> anyWord32be <*> anyWord32be
    7  -> CClass <$> anyWord16be
    8  -> CString <$> anyWord16be
    9  -> CFieldRef <$> anyWord16be <*> anyWord16be
    10 -> CMethodRef <$> anyWord16be <*> anyWord16be
    11 -> CInterfaceRef <$> anyWord16be <*> anyWord16be
    12 -> CNameAndType <$> anyWord16be <*> anyWord16be
    15 -> CMethodHandle <$> anyWord8 <*> anyWord16be
    16 -> CMethodType <$> anyWord16be
    18 -> CInvokeDynamic <$> anyWord16be <*> anyWord16be
  where
    parseCUtf8 = do
      len <- anyWord16be
      bs <- count (fromIntegral len) anyWord8
      return $ CUtf8 len bs

