{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
module JavaClassParser where

import           Control.Applicative             ((<$>), (<*>))
import           Data.Array                      (Array, listArray, (!))
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString.Lazy
import           Data.Bits
import qualified Data.ByteString.Lazy            as L
import           Data.Char
import           Data.Word
import           Debug.Trace                     (trace)
import           JvmClassTypes
import qualified Text.Show.Pretty                as Pr

testClass :: FilePath
testClass = "Lambdas.class"
testClassBytes :: IO L.ByteString
testClassBytes = L.readFile testClass

magicP :: Parser Word32
magicP = word32be 0xCAFEBABE
versinP :: Parser Version
versinP = Version <$> anyWord16be <*> anyWord16be

runParse :: L.ByteString -> JavaClass
runParse bs = do
  case parse parseClass bs of
    e@Fail {} -> error $ Pr.ppShow e
    Done _ v -> v

parseClass :: Parser JavaClass
parseClass = do
  _ <- magicP
  version <- versinP
  cpool <- parseConstantPool
  accFlag <- anyWord16be
  this <- parseClassName cpool
  superClassIndex <- anyWord16be
  let superClassName = if superClassIndex == 0
                        then Nothing
                        else Just $ extractClassName cpool superClassIndex
  intfCount <- anyWord16be
  interfaces <- count (fromIntegral intfCount) (parseClassName cpool)
  fieldCount <- anyWord16be
  fields <- count (fromIntegral fieldCount) (parseClassFields cpool)
  methodCount <- anyWord16be
  methods <- count (fromIntegral methodCount) (parseClassMethods cpool)
  return $ JavaClass {
      version        = version
    , constantPool   = cpool
    , classFlags     = mkClassFlags accFlag
    , thisClassName  = this
    , superClassName = superClassName
    , interfaceNames = interfaces
    , fieldCount     = fieldCount
    , fields         = fields }

mkClassFlags :: (Bits a, Num a) => a -> ClassFlags
mkClassFlags flag = ClassFlags {
  isPublic       = 0x0001 .&. flag /= 0,
  isFinal        = 0x0010 .&. flag /= 0,
  isSuperSpecial = 0x0020 .&. flag /= 0,
  isInterface    = 0x0200 .&. flag /= 0,
  isAbstract     = 0x0400 .&. flag /= 0,
  isSynthetic    = 0x1000 .&. flag /= 0,
  isAnnotation   = 0x2000 .&. flag /= 0,
  isEnum         = 0x4000 .&. flag /= 0
}


parseClassFields :: ConstantPool -> Parser Field
parseClassFields cpool = do
    accFlag <- anyWord16be
    name <- extractUtf8 cpool <$> anyWord16be
    descriptor <- extractUtf8 cpool <$> anyWord16be
    attrCount <- anyWord16be
    attrs <- count (fromIntegral attrCount) (parseAttributes cpool)
    return $ Field accFlag name descriptor attrCount attrs

parseClassMethods :: ConstantPool -> Parser Method
parseClassMethods cpool = do
    accFlag <- anyWord16be
    name <- extractUtf8 cpool <$> anyWord16be
    descriptor <- extractUtf8 cpool <$> anyWord16be
    attrCount <- anyWord16be
    attrs <- count (fromIntegral attrCount) (parseAttributes cpool)
    return $ Method accFlag name descriptor attrCount attrs

parseAttributes :: ConstantPool -> Parser Attribute
parseAttributes cpool = do
    attrName <- extractUtf8 cpool <$> anyWord16be
    attrLen <- anyWord32be
    info <- count (fromIntegral attrLen) anyWord8
    return $ Attribute attrName attrLen info


-- | constant pool
parseConstantPool :: Parser ConstantPool
parseConstantPool = do
  cpoolCount <- anyWord16be
  let cpoolLen = cpoolCount - 1
  es <- count (fromIntegral cpoolLen) parseEntry
  return $ listArray (1, cpoolLen) es

parseEntry :: Parser ConstantPoolEntry
parseEntry = do
  tag <- anyWord8
  entries <- parseConstantPoolEntries tag
  return entries

parseConstantPoolEntries :: Word8 -> Parser ConstantPoolEntry
parseConstantPoolEntries = \case
    1  -> parseCUtf8
    3  -> ConstInt <$> anyWord32be
    4  -> ConstFloat <$> anyWord32be
    5  -> ConstLong <$> anyWord32be <*> anyWord32be
    6  -> ConstDouble <$> anyWord32be <*> anyWord32be
    7  -> ConstClass <$> anyWord16be
    8  -> ConstString <$> anyWord16be
    9  -> ConstFieldRef <$> anyWord16be <*> anyWord16be
    10 -> ConstMethodRef <$> anyWord16be <*> anyWord16be
    11 -> ConstInterfaceRef <$> anyWord16be <*> anyWord16be
    12 -> ConstNameAndType <$> anyWord16be <*> anyWord16be
    15 -> ConstMethodHandle <$> anyWord8 <*> anyWord16be
    16 -> ConstMethodType <$> anyWord16be
    18 -> ConstInvokeDynamic <$> anyWord16be <*> anyWord16be
    _  -> error "impossible const val"
  where
    parseCUtf8 :: Parser ConstantPoolEntry
    parseCUtf8 = do
      len <- anyWord16be
      bs <- count (fromIntegral len) anyWord8
      return $ ConstUtf8 (utf8BytesToString bs)


parseClassName :: ConstantPool -> Parser String
parseClassName cpool = extractClassName cpool <$> anyWord16be

extractClassName :: Array Word16 ConstantPoolEntry -> Word16 -> String
extractClassName cpool idx
  | ConstClass cix <- cpool ! idx, ConstUtf8 n <- cpool ! cix = n
  | otherwise = error "Constant Class Info expected!"

extractUtf8 :: ConstantPool -> ConstantPoolIndex -> String
extractUtf8 cpool idx
  | ConstUtf8 str <- cpool ! idx = str
  | otherwise = error "Constant Utf8 expected!"

utf8BytesToString :: [Word8] -> String
utf8BytesToString [] = []
utf8BytesToString (x : rest)
  | x .&. 0x80 == 0 = chr (fromIntegral x) : utf8BytesToString rest
utf8BytesToString (x : y : rest)
  | (x .&. 0xE0 == 0xC0) && (y .&. 0xC0 == 0x80)
  = chr i : utf8BytesToString rest
  where i = (fromIntegral x .&. 0x1F) `shift` 6 + (fromIntegral y .&. 0x3F)
utf8BytesToString (x : y : z : rest)
  | (x .&. 0xF0 == 0xE0) && (y .&. 0xC0 == 0x80) && (z .&. 0xC0 == 0x80)
  = chr i : utf8BytesToString rest
  where i = ((fromIntegral x .&. 0x0F) `shift` 12
         + (fromIntegral y .&. 0x3F) `shift` 6
         + (fromIntegral z .&. 0x3F))
utf8BytesToString _ = error "cannot parse byte array for Java String"
