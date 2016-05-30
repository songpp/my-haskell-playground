module JvmClassTypes where

import           Data.Array (Array)
import           Data.Word

type ConstantPoolIndex = Word16
type ConstantPool = Array ConstantPoolIndex ConstantPoolEntry

data ConstantPoolEntry =
    ConstClass { nameIndex :: Word16 }
  | ConstFieldRef { clsIndex :: Word16, ntIndex :: Word16 }
  | ConstMethodRef { clsIndex :: Word16, ntIndex :: Word16 }
  | ConstInterfaceRef { clsIndex :: Word16, ntIndex :: Word16 }
  | ConstString { strIndex :: Word16 }
  | ConstInt { bytes :: Word32 }
  | ConstFloat { bytes :: Word32 }
  | ConstLong { highBytes :: Word32, lowBytes :: Word32 }
  | ConstDouble { highBytes :: Word32, lowBytes :: Word32 }
  | ConstNameAndType { nameIndex :: Word16, descriptorIndex :: Word16 }
  | ConstUtf8 { content :: String }
  | ConstMethodHandle { refKind :: Word8, refIndex :: Word16 }
  | ConstMethodType { descriptorIndex :: Word16 }
  | ConstInvokeDynamic { bootstrapMethodAttrIndex :: Word16, ntIndex :: Word16 }
  deriving (Show, Eq)

data JavaType = ByteType
              | CharType
              | BooleanType
              | ShortType
              | IntType
              | FloatType
              | DoubleType
              | ClassType String
              | ArrayType JavaType
              deriving (Eq, Show, Ord)

data Version = Version { minor :: Word16, major :: Word16 } deriving (Show, Eq)


data JavaClass = JavaClass {
  version        :: Version,
  constantPool   :: ConstantPool,
  classFlags     :: ClassFlags,
  thisClassName  :: String,
  superClassName :: Maybe String,
  interfaceNames :: [String],
  fieldCount     :: Word16,
  fields         :: [Field]
} deriving (Show)

data ClassFlags = ClassFlags {
  isPublic       :: Bool,
  isFinal        :: Bool,
  isSuperSpecial :: Bool,
  isInterface    :: Bool,
  isAbstract     :: Bool,
  isSynthetic    :: Bool,
  isAnnotation   :: Bool,
  isEnum         :: Bool
  } deriving (Show)

data Field = Field {
  fieldAccFlag    :: Word16,
  fieldName       :: String,
  fieldDescriptor :: String,
  fieldAttrCount  :: Word16,
  fieldAttrs      :: [Attribute]
} deriving (Show)

data Method = Method {
  methodAccFlag   :: Word16,
  methodName      :: String,
  methodDescriptor:: String,
  methodAttrCount :: Word16,
  methodAttrs     :: [Attribute]
} deriving (Show)

data Attribute = Attribute {
  attrName :: String,
  attrLen  :: Word32,
  attrInfo :: [Word8]
} deriving (Show)

