{- This file was auto-generated from serializotron.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Serializotron_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
boolVal ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "boolVal" a) =>
  Lens.Family2.LensLike' f s a
boolVal = Data.ProtoLens.Field.field @"boolVal"
bytesVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bytesVal" a) =>
  Lens.Family2.LensLike' f s a
bytesVal = Data.ProtoLens.Field.field @"bytesVal"
constructorIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constructorIndex" a) =>
  Lens.Family2.LensLike' f s a
constructorIndex = Data.ProtoLens.Field.field @"constructorIndex"
constructorName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constructorName" a) =>
  Lens.Family2.LensLike' f s a
constructorName = Data.ProtoLens.Field.field @"constructorName"
constructorTypes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constructorTypes" a) =>
  Lens.Family2.LensLike' f s a
constructorTypes = Data.ProtoLens.Field.field @"constructorTypes"
constructors ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "constructors" a) =>
  Lens.Family2.LensLike' f s a
constructors = Data.ProtoLens.Field.field @"constructors"
core ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "core" a) =>
  Lens.Family2.LensLike' f s a
core = Data.ProtoLens.Field.field @"core"
doubleVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "doubleVal" a) =>
  Lens.Family2.LensLike' f s a
doubleVal = Data.ProtoLens.Field.field @"doubleVal"
elementType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "elementType" a) =>
  Lens.Family2.LensLike' f s a
elementType = Data.ProtoLens.Field.field @"elementType"
elements ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "elements" a) =>
  Lens.Family2.LensLike' f s a
elements = Data.ProtoLens.Field.field @"elements"
fieldLabels ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fieldLabels" a) =>
  Lens.Family2.LensLike' f s a
fieldLabels = Data.ProtoLens.Field.field @"fieldLabels"
fieldName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fieldName" a) =>
  Lens.Family2.LensLike' f s a
fieldName = Data.ProtoLens.Field.field @"fieldName"
fieldType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "fieldType" a) =>
  Lens.Family2.LensLike' f s a
fieldType = Data.ProtoLens.Field.field @"fieldType"
fields ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "fields" a) =>
  Lens.Family2.LensLike' f s a
fields = Data.ProtoLens.Field.field @"fields"
int32Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "int32Val" a) =>
  Lens.Family2.LensLike' f s a
int32Val = Data.ProtoLens.Field.field @"int32Val"
intVal ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "intVal" a) =>
  Lens.Family2.LensLike' f s a
intVal = Data.ProtoLens.Field.field @"intVal"
integerVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "integerVal" a) =>
  Lens.Family2.LensLike' f s a
integerVal = Data.ProtoLens.Field.field @"integerVal"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
list ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "list" a) =>
  Lens.Family2.LensLike' f s a
list = Data.ProtoLens.Field.field @"list"
maybe'boolVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'boolVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'boolVal = Data.ProtoLens.Field.field @"maybe'boolVal"
maybe'bytesVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'bytesVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'bytesVal = Data.ProtoLens.Field.field @"maybe'bytesVal"
maybe'core ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'core" a) =>
  Lens.Family2.LensLike' f s a
maybe'core = Data.ProtoLens.Field.field @"maybe'core"
maybe'doubleVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'doubleVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'doubleVal = Data.ProtoLens.Field.field @"maybe'doubleVal"
maybe'elementType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'elementType" a) =>
  Lens.Family2.LensLike' f s a
maybe'elementType = Data.ProtoLens.Field.field @"maybe'elementType"
maybe'fieldType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'fieldType" a) =>
  Lens.Family2.LensLike' f s a
maybe'fieldType = Data.ProtoLens.Field.field @"maybe'fieldType"
maybe'int32Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'int32Val" a) =>
  Lens.Family2.LensLike' f s a
maybe'int32Val = Data.ProtoLens.Field.field @"maybe'int32Val"
maybe'intVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'intVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'intVal = Data.ProtoLens.Field.field @"maybe'intVal"
maybe'integerVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'integerVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'integerVal = Data.ProtoLens.Field.field @"maybe'integerVal"
maybe'list ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'list" a) =>
  Lens.Family2.LensLike' f s a
maybe'list = Data.ProtoLens.Field.field @"maybe'list"
maybe'primitive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'primitive" a) =>
  Lens.Family2.LensLike' f s a
maybe'primitive = Data.ProtoLens.Field.field @"maybe'primitive"
maybe'product ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'product" a) =>
  Lens.Family2.LensLike' f s a
maybe'product = Data.ProtoLens.Field.field @"maybe'product"
maybe'reference ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'reference" a) =>
  Lens.Family2.LensLike' f s a
maybe'reference = Data.ProtoLens.Field.field @"maybe'reference"
maybe'structure ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'structure" a) =>
  Lens.Family2.LensLike' f s a
maybe'structure = Data.ProtoLens.Field.field @"maybe'structure"
maybe'sum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sum" a) =>
  Lens.Family2.LensLike' f s a
maybe'sum = Data.ProtoLens.Field.field @"maybe'sum"
maybe'textVal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'textVal" a) =>
  Lens.Family2.LensLike' f s a
maybe'textVal = Data.ProtoLens.Field.field @"maybe'textVal"
maybe'typeInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'typeInfo" a) =>
  Lens.Family2.LensLike' f s a
maybe'typeInfo = Data.ProtoLens.Field.field @"maybe'typeInfo"
maybe'unit ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unit" a) =>
  Lens.Family2.LensLike' f s a
maybe'unit = Data.ProtoLens.Field.field @"maybe'unit"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
maybe'word32Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'word32Val" a) =>
  Lens.Family2.LensLike' f s a
maybe'word32Val = Data.ProtoLens.Field.field @"maybe'word32Val"
maybe'word64Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'word64Val" a) =>
  Lens.Family2.LensLike' f s a
maybe'word64Val = Data.ProtoLens.Field.field @"maybe'word64Val"
moduleName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "moduleName" a) =>
  Lens.Family2.LensLike' f s a
moduleName = Data.ProtoLens.Field.field @"moduleName"
primitive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "primitive" a) =>
  Lens.Family2.LensLike' f s a
primitive = Data.ProtoLens.Field.field @"primitive"
product ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "product" a) =>
  Lens.Family2.LensLike' f s a
product = Data.ProtoLens.Field.field @"product"
reference ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "reference" a) =>
  Lens.Family2.LensLike' f s a
reference = Data.ProtoLens.Field.field @"reference"
referenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "referenceId" a) =>
  Lens.Family2.LensLike' f s a
referenceId = Data.ProtoLens.Field.field @"referenceId"
schemaVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "schemaVersion" a) =>
  Lens.Family2.LensLike' f s a
schemaVersion = Data.ProtoLens.Field.field @"schemaVersion"
sharedTypeInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sharedTypeInfo" a) =>
  Lens.Family2.LensLike' f s a
sharedTypeInfo = Data.ProtoLens.Field.field @"sharedTypeInfo"
sharedValues ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sharedValues" a) =>
  Lens.Family2.LensLike' f s a
sharedValues = Data.ProtoLens.Field.field @"sharedValues"
structure ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "structure" a) =>
  Lens.Family2.LensLike' f s a
structure = Data.ProtoLens.Field.field @"structure"
sum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sum" a) =>
  Lens.Family2.LensLike' f s a
sum = Data.ProtoLens.Field.field @"sum"
textVal ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "textVal" a) =>
  Lens.Family2.LensLike' f s a
textVal = Data.ProtoLens.Field.field @"textVal"
typeInfo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeInfo" a) =>
  Lens.Family2.LensLike' f s a
typeInfo = Data.ProtoLens.Field.field @"typeInfo"
typeInfoRef ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeInfoRef" a) =>
  Lens.Family2.LensLike' f s a
typeInfoRef = Data.ProtoLens.Field.field @"typeInfoRef"
typeName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeName" a) =>
  Lens.Family2.LensLike' f s a
typeName = Data.ProtoLens.Field.field @"typeName"
typeParameters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "typeParameters" a) =>
  Lens.Family2.LensLike' f s a
typeParameters = Data.ProtoLens.Field.field @"typeParameters"
unit ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "unit" a) =>
  Lens.Family2.LensLike' f s a
unit = Data.ProtoLens.Field.field @"unit"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'constructorTypes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'constructorTypes" a) =>
  Lens.Family2.LensLike' f s a
vec'constructorTypes
  = Data.ProtoLens.Field.field @"vec'constructorTypes"
vec'constructors ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'constructors" a) =>
  Lens.Family2.LensLike' f s a
vec'constructors = Data.ProtoLens.Field.field @"vec'constructors"
vec'elements ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'elements" a) =>
  Lens.Family2.LensLike' f s a
vec'elements = Data.ProtoLens.Field.field @"vec'elements"
vec'fieldLabels ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'fieldLabels" a) =>
  Lens.Family2.LensLike' f s a
vec'fieldLabels = Data.ProtoLens.Field.field @"vec'fieldLabels"
vec'fields ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'fields" a) =>
  Lens.Family2.LensLike' f s a
vec'fields = Data.ProtoLens.Field.field @"vec'fields"
vec'typeParameters ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'typeParameters" a) =>
  Lens.Family2.LensLike' f s a
vec'typeParameters
  = Data.ProtoLens.Field.field @"vec'typeParameters"
word32Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "word32Val" a) =>
  Lens.Family2.LensLike' f s a
word32Val = Data.ProtoLens.Field.field @"word32Val"
word64Val ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "word64Val" a) =>
  Lens.Family2.LensLike' f s a
word64Val = Data.ProtoLens.Field.field @"word64Val"