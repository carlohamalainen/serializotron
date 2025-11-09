{- This file was auto-generated from serializotron.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Serializotron (
        DynamicCore(), DynamicCore'Core(..), _DynamicCore'Primitive,
        _DynamicCore'Product, _DynamicCore'Sum, _DynamicCore'List,
        _DynamicCore'Unit, _DynamicCore'Reference, DynamicValue(),
        ListStructure(), ListValue(), PrimitiveType(..), PrimitiveType(),
        PrimitiveType'UnrecognizedValue, PrimitiveValue(),
        PrimitiveValue'Primitive(..), _PrimitiveValue'IntVal,
        _PrimitiveValue'DoubleVal, _PrimitiveValue'TextVal,
        _PrimitiveValue'BoolVal, _PrimitiveValue'Word64Val,
        _PrimitiveValue'Int32Val, _PrimitiveValue'Word32Val,
        _PrimitiveValue'IntegerVal, _PrimitiveValue'BytesVal,
        ProductStructure(), ProductStructure'FieldInfo(), ProductValue(),
        ReferenceValue(), SZTFile(), SZTFile'SharedTypeInfoEntry(),
        SZTFile'SharedValuesEntry(), SumStructure(), SumValue(),
        TypeInfo(), TypeStructure(), TypeStructure'Structure(..),
        _TypeStructure'Primitive, _TypeStructure'Product,
        _TypeStructure'Sum, _TypeStructure'List, _TypeStructure'Unit,
        UnitStructure(), UnitValue()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Serializotron_Fields.maybe'core' @:: Lens' DynamicCore (Prelude.Maybe DynamicCore'Core)@
         * 'Proto.Serializotron_Fields.maybe'primitive' @:: Lens' DynamicCore (Prelude.Maybe PrimitiveValue)@
         * 'Proto.Serializotron_Fields.primitive' @:: Lens' DynamicCore PrimitiveValue@
         * 'Proto.Serializotron_Fields.maybe'product' @:: Lens' DynamicCore (Prelude.Maybe ProductValue)@
         * 'Proto.Serializotron_Fields.product' @:: Lens' DynamicCore ProductValue@
         * 'Proto.Serializotron_Fields.maybe'sum' @:: Lens' DynamicCore (Prelude.Maybe SumValue)@
         * 'Proto.Serializotron_Fields.sum' @:: Lens' DynamicCore SumValue@
         * 'Proto.Serializotron_Fields.maybe'list' @:: Lens' DynamicCore (Prelude.Maybe ListValue)@
         * 'Proto.Serializotron_Fields.list' @:: Lens' DynamicCore ListValue@
         * 'Proto.Serializotron_Fields.maybe'unit' @:: Lens' DynamicCore (Prelude.Maybe UnitValue)@
         * 'Proto.Serializotron_Fields.unit' @:: Lens' DynamicCore UnitValue@
         * 'Proto.Serializotron_Fields.maybe'reference' @:: Lens' DynamicCore (Prelude.Maybe ReferenceValue)@
         * 'Proto.Serializotron_Fields.reference' @:: Lens' DynamicCore ReferenceValue@ -}
data DynamicCore
  = DynamicCore'_constructor {_DynamicCore'core :: !(Prelude.Maybe DynamicCore'Core),
                              _DynamicCore'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DynamicCore where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data DynamicCore'Core
  = DynamicCore'Primitive !PrimitiveValue |
    DynamicCore'Product !ProductValue |
    DynamicCore'Sum !SumValue |
    DynamicCore'List !ListValue |
    DynamicCore'Unit !UnitValue |
    DynamicCore'Reference !ReferenceValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'core" (Prelude.Maybe DynamicCore'Core) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'primitive" (Prelude.Maybe PrimitiveValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'Primitive x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'Primitive y__))
instance Data.ProtoLens.Field.HasField DynamicCore "primitive" PrimitiveValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'Primitive x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'Primitive y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'product" (Prelude.Maybe ProductValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'Product x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'Product y__))
instance Data.ProtoLens.Field.HasField DynamicCore "product" ProductValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'Product x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'Product y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'sum" (Prelude.Maybe SumValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'Sum x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'Sum y__))
instance Data.ProtoLens.Field.HasField DynamicCore "sum" SumValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'Sum x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'Sum y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'list" (Prelude.Maybe ListValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'List x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'List y__))
instance Data.ProtoLens.Field.HasField DynamicCore "list" ListValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'List x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'List y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'unit" (Prelude.Maybe UnitValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'Unit x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'Unit y__))
instance Data.ProtoLens.Field.HasField DynamicCore "unit" UnitValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'Unit x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'Unit y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DynamicCore "maybe'reference" (Prelude.Maybe ReferenceValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DynamicCore'Reference x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DynamicCore'Reference y__))
instance Data.ProtoLens.Field.HasField DynamicCore "reference" ReferenceValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicCore'core (\ x__ y__ -> x__ {_DynamicCore'core = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DynamicCore'Reference x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DynamicCore'Reference y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message DynamicCore where
  messageName _ = Data.Text.pack "szt.DynamicCore"
  packedMessageDescriptor _
    = "\n\
      \\vDynamicCore\DC23\n\
      \\tprimitive\CAN\SOH \SOH(\v2\DC3.szt.PrimitiveValueH\NULR\tprimitive\DC2-\n\
      \\aproduct\CAN\STX \SOH(\v2\DC1.szt.ProductValueH\NULR\aproduct\DC2!\n\
      \\ETXsum\CAN\ETX \SOH(\v2\r.szt.SumValueH\NULR\ETXsum\DC2$\n\
      \\EOTlist\CAN\EOT \SOH(\v2\SO.szt.ListValueH\NULR\EOTlist\DC2$\n\
      \\EOTunit\CAN\ENQ \SOH(\v2\SO.szt.UnitValueH\NULR\EOTunit\DC23\n\
      \\treference\CAN\ACK \SOH(\v2\DC3.szt.ReferenceValueH\NULR\treferenceB\ACK\n\
      \\EOTcore"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        primitive__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "primitive"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor PrimitiveValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'primitive")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
        product__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "product"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ProductValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'product")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
        sum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sum"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SumValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sum")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
        list__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "list"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ListValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'list")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
        unit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UnitValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'unit")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
        reference__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "reference"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ReferenceValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'reference")) ::
              Data.ProtoLens.FieldDescriptor DynamicCore
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, primitive__field_descriptor),
           (Data.ProtoLens.Tag 2, product__field_descriptor),
           (Data.ProtoLens.Tag 3, sum__field_descriptor),
           (Data.ProtoLens.Tag 4, list__field_descriptor),
           (Data.ProtoLens.Tag 5, unit__field_descriptor),
           (Data.ProtoLens.Tag 6, reference__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DynamicCore'_unknownFields
        (\ x__ y__ -> x__ {_DynamicCore'_unknownFields = y__})
  defMessage
    = DynamicCore'_constructor
        {_DynamicCore'core = Prelude.Nothing,
         _DynamicCore'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DynamicCore -> Data.ProtoLens.Encoding.Bytes.Parser DynamicCore
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "primitive"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"primitive") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "product"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"product") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "sum"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"sum") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "list"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"list") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "unit"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unit") y x)
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "reference"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"reference") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DynamicCore"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'core") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (DynamicCore'Primitive v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (DynamicCore'Product v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (DynamicCore'Sum v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (DynamicCore'List v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (DynamicCore'Unit v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (DynamicCore'Reference v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DynamicCore where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DynamicCore'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DynamicCore'core x__) ())
instance Control.DeepSeq.NFData DynamicCore'Core where
  rnf (DynamicCore'Primitive x__) = Control.DeepSeq.rnf x__
  rnf (DynamicCore'Product x__) = Control.DeepSeq.rnf x__
  rnf (DynamicCore'Sum x__) = Control.DeepSeq.rnf x__
  rnf (DynamicCore'List x__) = Control.DeepSeq.rnf x__
  rnf (DynamicCore'Unit x__) = Control.DeepSeq.rnf x__
  rnf (DynamicCore'Reference x__) = Control.DeepSeq.rnf x__
_DynamicCore'Primitive ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core PrimitiveValue
_DynamicCore'Primitive
  = Data.ProtoLens.Prism.prism'
      DynamicCore'Primitive
      (\ p__
         -> case p__ of
              (DynamicCore'Primitive p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DynamicCore'Product ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core ProductValue
_DynamicCore'Product
  = Data.ProtoLens.Prism.prism'
      DynamicCore'Product
      (\ p__
         -> case p__ of
              (DynamicCore'Product p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DynamicCore'Sum ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core SumValue
_DynamicCore'Sum
  = Data.ProtoLens.Prism.prism'
      DynamicCore'Sum
      (\ p__
         -> case p__ of
              (DynamicCore'Sum p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DynamicCore'List ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core ListValue
_DynamicCore'List
  = Data.ProtoLens.Prism.prism'
      DynamicCore'List
      (\ p__
         -> case p__ of
              (DynamicCore'List p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DynamicCore'Unit ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core UnitValue
_DynamicCore'Unit
  = Data.ProtoLens.Prism.prism'
      DynamicCore'Unit
      (\ p__
         -> case p__ of
              (DynamicCore'Unit p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DynamicCore'Reference ::
  Data.ProtoLens.Prism.Prism' DynamicCore'Core ReferenceValue
_DynamicCore'Reference
  = Data.ProtoLens.Prism.prism'
      DynamicCore'Reference
      (\ p__
         -> case p__ of
              (DynamicCore'Reference p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Serializotron_Fields.core' @:: Lens' DynamicValue DynamicCore@
         * 'Proto.Serializotron_Fields.maybe'core' @:: Lens' DynamicValue (Prelude.Maybe DynamicCore)@
         * 'Proto.Serializotron_Fields.typeInfo' @:: Lens' DynamicValue TypeInfo@
         * 'Proto.Serializotron_Fields.maybe'typeInfo' @:: Lens' DynamicValue (Prelude.Maybe TypeInfo)@
         * 'Proto.Serializotron_Fields.typeInfoRef' @:: Lens' DynamicValue Data.Word.Word32@ -}
data DynamicValue
  = DynamicValue'_constructor {_DynamicValue'core :: !(Prelude.Maybe DynamicCore),
                               _DynamicValue'typeInfo :: !(Prelude.Maybe TypeInfo),
                               _DynamicValue'typeInfoRef :: !Data.Word.Word32,
                               _DynamicValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DynamicValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DynamicValue "core" DynamicCore where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicValue'core (\ x__ y__ -> x__ {_DynamicValue'core = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DynamicValue "maybe'core" (Prelude.Maybe DynamicCore) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicValue'core (\ x__ y__ -> x__ {_DynamicValue'core = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DynamicValue "typeInfo" TypeInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicValue'typeInfo
           (\ x__ y__ -> x__ {_DynamicValue'typeInfo = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DynamicValue "maybe'typeInfo" (Prelude.Maybe TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicValue'typeInfo
           (\ x__ y__ -> x__ {_DynamicValue'typeInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DynamicValue "typeInfoRef" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DynamicValue'typeInfoRef
           (\ x__ y__ -> x__ {_DynamicValue'typeInfoRef = y__}))
        Prelude.id
instance Data.ProtoLens.Message DynamicValue where
  messageName _ = Data.Text.pack "szt.DynamicValue"
  packedMessageDescriptor _
    = "\n\
      \\fDynamicValue\DC2$\n\
      \\EOTcore\CAN\SOH \SOH(\v2\DLE.szt.DynamicCoreR\EOTcore\DC2*\n\
      \\ttype_info\CAN\STX \SOH(\v2\r.szt.TypeInfoR\btypeInfo\DC2\"\n\
      \\rtype_info_ref\CAN\ETX \SOH(\rR\vtypeInfoRef"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        core__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "core"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicCore)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'core")) ::
              Data.ProtoLens.FieldDescriptor DynamicValue
        typeInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'typeInfo")) ::
              Data.ProtoLens.FieldDescriptor DynamicValue
        typeInfoRef__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_info_ref"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"typeInfoRef")) ::
              Data.ProtoLens.FieldDescriptor DynamicValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, core__field_descriptor),
           (Data.ProtoLens.Tag 2, typeInfo__field_descriptor),
           (Data.ProtoLens.Tag 3, typeInfoRef__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DynamicValue'_unknownFields
        (\ x__ y__ -> x__ {_DynamicValue'_unknownFields = y__})
  defMessage
    = DynamicValue'_constructor
        {_DynamicValue'core = Prelude.Nothing,
         _DynamicValue'typeInfo = Prelude.Nothing,
         _DynamicValue'typeInfoRef = Data.ProtoLens.fieldDefault,
         _DynamicValue'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DynamicValue -> Data.ProtoLens.Encoding.Bytes.Parser DynamicValue
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "core"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"core") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "type_info"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeInfo") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "type_info_ref"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeInfoRef") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DynamicValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'core") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'typeInfo") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"typeInfoRef") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData DynamicValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DynamicValue'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DynamicValue'core x__)
                (Control.DeepSeq.deepseq
                   (_DynamicValue'typeInfo x__)
                   (Control.DeepSeq.deepseq (_DynamicValue'typeInfoRef x__) ())))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.elementType' @:: Lens' ListStructure TypeInfo@
         * 'Proto.Serializotron_Fields.maybe'elementType' @:: Lens' ListStructure (Prelude.Maybe TypeInfo)@ -}
data ListStructure
  = ListStructure'_constructor {_ListStructure'elementType :: !(Prelude.Maybe TypeInfo),
                                _ListStructure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ListStructure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ListStructure "elementType" TypeInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListStructure'elementType
           (\ x__ y__ -> x__ {_ListStructure'elementType = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ListStructure "maybe'elementType" (Prelude.Maybe TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListStructure'elementType
           (\ x__ y__ -> x__ {_ListStructure'elementType = y__}))
        Prelude.id
instance Data.ProtoLens.Message ListStructure where
  messageName _ = Data.Text.pack "szt.ListStructure"
  packedMessageDescriptor _
    = "\n\
      \\rListStructure\DC20\n\
      \\felement_type\CAN\SOH \SOH(\v2\r.szt.TypeInfoR\velementType"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        elementType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "element_type"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'elementType")) ::
              Data.ProtoLens.FieldDescriptor ListStructure
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, elementType__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ListStructure'_unknownFields
        (\ x__ y__ -> x__ {_ListStructure'_unknownFields = y__})
  defMessage
    = ListStructure'_constructor
        {_ListStructure'elementType = Prelude.Nothing,
         _ListStructure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ListStructure -> Data.ProtoLens.Encoding.Bytes.Parser ListStructure
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "element_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"elementType") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ListStructure"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'elementType") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ListStructure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ListStructure'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ListStructure'elementType x__) ())
{- | Fields :
     
         * 'Proto.Serializotron_Fields.elements' @:: Lens' ListValue [DynamicValue]@
         * 'Proto.Serializotron_Fields.vec'elements' @:: Lens' ListValue (Data.Vector.Vector DynamicValue)@ -}
data ListValue
  = ListValue'_constructor {_ListValue'elements :: !(Data.Vector.Vector DynamicValue),
                            _ListValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ListValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ListValue "elements" [DynamicValue] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListValue'elements (\ x__ y__ -> x__ {_ListValue'elements = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ListValue "vec'elements" (Data.Vector.Vector DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ListValue'elements (\ x__ y__ -> x__ {_ListValue'elements = y__}))
        Prelude.id
instance Data.ProtoLens.Message ListValue where
  messageName _ = Data.Text.pack "szt.ListValue"
  packedMessageDescriptor _
    = "\n\
      \\tListValue\DC2-\n\
      \\belements\CAN\SOH \ETX(\v2\DC1.szt.DynamicValueR\belements"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        elements__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "elements"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicValue)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"elements")) ::
              Data.ProtoLens.FieldDescriptor ListValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, elements__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ListValue'_unknownFields
        (\ x__ y__ -> x__ {_ListValue'_unknownFields = y__})
  defMessage
    = ListValue'_constructor
        {_ListValue'elements = Data.Vector.Generic.empty,
         _ListValue'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ListValue
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DynamicValue
             -> Data.ProtoLens.Encoding.Bytes.Parser ListValue
        loop x mutable'elements
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'elements <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'elements)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'elements") frozen'elements x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "elements"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'elements y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'elements
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'elements <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'elements)
          "ListValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'elements") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ListValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ListValue'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ListValue'elements x__) ())
newtype PrimitiveType'UnrecognizedValue
  = PrimitiveType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data PrimitiveType
  = PRIMITIVE_INT |
    PRIMITIVE_DOUBLE |
    PRIMITIVE_TEXT |
    PRIMITIVE_BOOL |
    PRIMITIVE_WORD64 |
    PRIMITIVE_INT32 |
    PRIMITIVE_WORD32 |
    PRIMITIVE_INTEGER |
    PRIMITIVE_BYTES |
    PrimitiveType'Unrecognized !PrimitiveType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum PrimitiveType where
  maybeToEnum 0 = Prelude.Just PRIMITIVE_INT
  maybeToEnum 1 = Prelude.Just PRIMITIVE_DOUBLE
  maybeToEnum 2 = Prelude.Just PRIMITIVE_TEXT
  maybeToEnum 3 = Prelude.Just PRIMITIVE_BOOL
  maybeToEnum 4 = Prelude.Just PRIMITIVE_WORD64
  maybeToEnum 5 = Prelude.Just PRIMITIVE_INT32
  maybeToEnum 6 = Prelude.Just PRIMITIVE_WORD32
  maybeToEnum 7 = Prelude.Just PRIMITIVE_INTEGER
  maybeToEnum 8 = Prelude.Just PRIMITIVE_BYTES
  maybeToEnum k
    = Prelude.Just
        (PrimitiveType'Unrecognized
           (PrimitiveType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum PRIMITIVE_INT = "PRIMITIVE_INT"
  showEnum PRIMITIVE_DOUBLE = "PRIMITIVE_DOUBLE"
  showEnum PRIMITIVE_TEXT = "PRIMITIVE_TEXT"
  showEnum PRIMITIVE_BOOL = "PRIMITIVE_BOOL"
  showEnum PRIMITIVE_WORD64 = "PRIMITIVE_WORD64"
  showEnum PRIMITIVE_INT32 = "PRIMITIVE_INT32"
  showEnum PRIMITIVE_WORD32 = "PRIMITIVE_WORD32"
  showEnum PRIMITIVE_INTEGER = "PRIMITIVE_INTEGER"
  showEnum PRIMITIVE_BYTES = "PRIMITIVE_BYTES"
  showEnum
    (PrimitiveType'Unrecognized (PrimitiveType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "PRIMITIVE_INT" = Prelude.Just PRIMITIVE_INT
    | (Prelude.==) k "PRIMITIVE_DOUBLE" = Prelude.Just PRIMITIVE_DOUBLE
    | (Prelude.==) k "PRIMITIVE_TEXT" = Prelude.Just PRIMITIVE_TEXT
    | (Prelude.==) k "PRIMITIVE_BOOL" = Prelude.Just PRIMITIVE_BOOL
    | (Prelude.==) k "PRIMITIVE_WORD64" = Prelude.Just PRIMITIVE_WORD64
    | (Prelude.==) k "PRIMITIVE_INT32" = Prelude.Just PRIMITIVE_INT32
    | (Prelude.==) k "PRIMITIVE_WORD32" = Prelude.Just PRIMITIVE_WORD32
    | (Prelude.==) k "PRIMITIVE_INTEGER"
    = Prelude.Just PRIMITIVE_INTEGER
    | (Prelude.==) k "PRIMITIVE_BYTES" = Prelude.Just PRIMITIVE_BYTES
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded PrimitiveType where
  minBound = PRIMITIVE_INT
  maxBound = PRIMITIVE_BYTES
instance Prelude.Enum PrimitiveType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum PrimitiveType: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum PRIMITIVE_INT = 0
  fromEnum PRIMITIVE_DOUBLE = 1
  fromEnum PRIMITIVE_TEXT = 2
  fromEnum PRIMITIVE_BOOL = 3
  fromEnum PRIMITIVE_WORD64 = 4
  fromEnum PRIMITIVE_INT32 = 5
  fromEnum PRIMITIVE_WORD32 = 6
  fromEnum PRIMITIVE_INTEGER = 7
  fromEnum PRIMITIVE_BYTES = 8
  fromEnum
    (PrimitiveType'Unrecognized (PrimitiveType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ PRIMITIVE_BYTES
    = Prelude.error
        "PrimitiveType.succ: bad argument PRIMITIVE_BYTES. This value would be out of bounds."
  succ PRIMITIVE_INT = PRIMITIVE_DOUBLE
  succ PRIMITIVE_DOUBLE = PRIMITIVE_TEXT
  succ PRIMITIVE_TEXT = PRIMITIVE_BOOL
  succ PRIMITIVE_BOOL = PRIMITIVE_WORD64
  succ PRIMITIVE_WORD64 = PRIMITIVE_INT32
  succ PRIMITIVE_INT32 = PRIMITIVE_WORD32
  succ PRIMITIVE_WORD32 = PRIMITIVE_INTEGER
  succ PRIMITIVE_INTEGER = PRIMITIVE_BYTES
  succ (PrimitiveType'Unrecognized _)
    = Prelude.error
        "PrimitiveType.succ: bad argument: unrecognized value"
  pred PRIMITIVE_INT
    = Prelude.error
        "PrimitiveType.pred: bad argument PRIMITIVE_INT. This value would be out of bounds."
  pred PRIMITIVE_DOUBLE = PRIMITIVE_INT
  pred PRIMITIVE_TEXT = PRIMITIVE_DOUBLE
  pred PRIMITIVE_BOOL = PRIMITIVE_TEXT
  pred PRIMITIVE_WORD64 = PRIMITIVE_BOOL
  pred PRIMITIVE_INT32 = PRIMITIVE_WORD64
  pred PRIMITIVE_WORD32 = PRIMITIVE_INT32
  pred PRIMITIVE_INTEGER = PRIMITIVE_WORD32
  pred PRIMITIVE_BYTES = PRIMITIVE_INTEGER
  pred (PrimitiveType'Unrecognized _)
    = Prelude.error
        "PrimitiveType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault PrimitiveType where
  fieldDefault = PRIMITIVE_INT
instance Control.DeepSeq.NFData PrimitiveType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Serializotron_Fields.maybe'primitive' @:: Lens' PrimitiveValue (Prelude.Maybe PrimitiveValue'Primitive)@
         * 'Proto.Serializotron_Fields.maybe'intVal' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Int.Int64)@
         * 'Proto.Serializotron_Fields.intVal' @:: Lens' PrimitiveValue Data.Int.Int64@
         * 'Proto.Serializotron_Fields.maybe'doubleVal' @:: Lens' PrimitiveValue (Prelude.Maybe Prelude.Double)@
         * 'Proto.Serializotron_Fields.doubleVal' @:: Lens' PrimitiveValue Prelude.Double@
         * 'Proto.Serializotron_Fields.maybe'textVal' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Serializotron_Fields.textVal' @:: Lens' PrimitiveValue Data.Text.Text@
         * 'Proto.Serializotron_Fields.maybe'boolVal' @:: Lens' PrimitiveValue (Prelude.Maybe Prelude.Bool)@
         * 'Proto.Serializotron_Fields.boolVal' @:: Lens' PrimitiveValue Prelude.Bool@
         * 'Proto.Serializotron_Fields.maybe'word64Val' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Word.Word64)@
         * 'Proto.Serializotron_Fields.word64Val' @:: Lens' PrimitiveValue Data.Word.Word64@
         * 'Proto.Serializotron_Fields.maybe'int32Val' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Int.Int32)@
         * 'Proto.Serializotron_Fields.int32Val' @:: Lens' PrimitiveValue Data.Int.Int32@
         * 'Proto.Serializotron_Fields.maybe'word32Val' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Word.Word32)@
         * 'Proto.Serializotron_Fields.word32Val' @:: Lens' PrimitiveValue Data.Word.Word32@
         * 'Proto.Serializotron_Fields.maybe'integerVal' @:: Lens' PrimitiveValue (Prelude.Maybe Data.Text.Text)@
         * 'Proto.Serializotron_Fields.integerVal' @:: Lens' PrimitiveValue Data.Text.Text@
         * 'Proto.Serializotron_Fields.maybe'bytesVal' @:: Lens' PrimitiveValue (Prelude.Maybe Data.ByteString.ByteString)@
         * 'Proto.Serializotron_Fields.bytesVal' @:: Lens' PrimitiveValue Data.ByteString.ByteString@ -}
data PrimitiveValue
  = PrimitiveValue'_constructor {_PrimitiveValue'primitive :: !(Prelude.Maybe PrimitiveValue'Primitive),
                                 _PrimitiveValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PrimitiveValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data PrimitiveValue'Primitive
  = PrimitiveValue'IntVal !Data.Int.Int64 |
    PrimitiveValue'DoubleVal !Prelude.Double |
    PrimitiveValue'TextVal !Data.Text.Text |
    PrimitiveValue'BoolVal !Prelude.Bool |
    PrimitiveValue'Word64Val !Data.Word.Word64 |
    PrimitiveValue'Int32Val !Data.Int.Int32 |
    PrimitiveValue'Word32Val !Data.Word.Word32 |
    PrimitiveValue'IntegerVal !Data.Text.Text |
    PrimitiveValue'BytesVal !Data.ByteString.ByteString
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'primitive" (Prelude.Maybe PrimitiveValue'Primitive) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'intVal" (Prelude.Maybe Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'IntVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'IntVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "intVal" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'IntVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'IntVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'doubleVal" (Prelude.Maybe Prelude.Double) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'DoubleVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'DoubleVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "doubleVal" Prelude.Double where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'DoubleVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'DoubleVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'textVal" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'TextVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'TextVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "textVal" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'TextVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'TextVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'boolVal" (Prelude.Maybe Prelude.Bool) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'BoolVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'BoolVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "boolVal" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'BoolVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'BoolVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'word64Val" (Prelude.Maybe Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'Word64Val x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'Word64Val y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "word64Val" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'Word64Val x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'Word64Val y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'int32Val" (Prelude.Maybe Data.Int.Int32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'Int32Val x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'Int32Val y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "int32Val" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'Int32Val x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'Int32Val y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'word32Val" (Prelude.Maybe Data.Word.Word32) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'Word32Val x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'Word32Val y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "word32Val" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'Word32Val x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'Word32Val y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'integerVal" (Prelude.Maybe Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'IntegerVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'IntegerVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "integerVal" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'IntegerVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'IntegerVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField PrimitiveValue "maybe'bytesVal" (Prelude.Maybe Data.ByteString.ByteString) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (PrimitiveValue'BytesVal x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap PrimitiveValue'BytesVal y__))
instance Data.ProtoLens.Field.HasField PrimitiveValue "bytesVal" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PrimitiveValue'primitive
           (\ x__ y__ -> x__ {_PrimitiveValue'primitive = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (PrimitiveValue'BytesVal x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap PrimitiveValue'BytesVal y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Message PrimitiveValue where
  messageName _ = Data.Text.pack "szt.PrimitiveValue"
  packedMessageDescriptor _
    = "\n\
      \\SOPrimitiveValue\DC2\EM\n\
      \\aint_val\CAN\SOH \SOH(\ETXH\NULR\ACKintVal\DC2\US\n\
      \\n\
      \double_val\CAN\STX \SOH(\SOHH\NULR\tdoubleVal\DC2\ESC\n\
      \\btext_val\CAN\ETX \SOH(\tH\NULR\atextVal\DC2\ESC\n\
      \\bbool_val\CAN\EOT \SOH(\bH\NULR\aboolVal\DC2\US\n\
      \\n\
      \word64_val\CAN\ENQ \SOH(\EOTH\NULR\tword64Val\DC2\GS\n\
      \\tint32_val\CAN\ACK \SOH(\ENQH\NULR\bint32Val\DC2\US\n\
      \\n\
      \word32_val\CAN\a \SOH(\rH\NULR\tword32Val\DC2!\n\
      \\vinteger_val\CAN\b \SOH(\tH\NULR\n\
      \integerVal\DC2\GS\n\
      \\tbytes_val\CAN\t \SOH(\fH\NULR\bbytesValB\v\n\
      \\tprimitive"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        intVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "int_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'intVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        doubleVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "double_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.DoubleField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Double)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'doubleVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        textVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "text_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'textVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        boolVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "bool_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'boolVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        word64Val__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "word64_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'word64Val")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        int32Val__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "int32_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'int32Val")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        word32Val__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "word32_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'word32Val")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        integerVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "integer_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'integerVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
        bytesVal__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "bytes_val"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'bytesVal")) ::
              Data.ProtoLens.FieldDescriptor PrimitiveValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, intVal__field_descriptor),
           (Data.ProtoLens.Tag 2, doubleVal__field_descriptor),
           (Data.ProtoLens.Tag 3, textVal__field_descriptor),
           (Data.ProtoLens.Tag 4, boolVal__field_descriptor),
           (Data.ProtoLens.Tag 5, word64Val__field_descriptor),
           (Data.ProtoLens.Tag 6, int32Val__field_descriptor),
           (Data.ProtoLens.Tag 7, word32Val__field_descriptor),
           (Data.ProtoLens.Tag 8, integerVal__field_descriptor),
           (Data.ProtoLens.Tag 9, bytesVal__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PrimitiveValue'_unknownFields
        (\ x__ y__ -> x__ {_PrimitiveValue'_unknownFields = y__})
  defMessage
    = PrimitiveValue'_constructor
        {_PrimitiveValue'primitive = Prelude.Nothing,
         _PrimitiveValue'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PrimitiveValue
          -> Data.ProtoLens.Encoding.Bytes.Parser PrimitiveValue
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "int_val"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"intVal") y x)
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Data.ProtoLens.Encoding.Bytes.wordToDouble
                                          Data.ProtoLens.Encoding.Bytes.getFixed64)
                                       "double_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"doubleVal") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "text_val"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"textVal") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "bool_val"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"boolVal") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "word64_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"word64Val") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "int32_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"int32Val") y x)
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "word32_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"word32Val") y x)
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "integer_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"integerVal") y x)
                        74
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "bytes_val"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"bytesVal") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PrimitiveValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'primitive") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (PrimitiveValue'IntVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral v)
                (Prelude.Just (PrimitiveValue'DoubleVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putFixed64
                          Data.ProtoLens.Encoding.Bytes.doubleToWord v)
                (Prelude.Just (PrimitiveValue'TextVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.Text.Encoding.encodeUtf8 v)
                (Prelude.Just (PrimitiveValue'BoolVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putVarInt (\ b -> if b then 1 else 0)
                          v)
                (Prelude.Just (PrimitiveValue'Word64Val v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt v)
                (Prelude.Just (PrimitiveValue'Int32Val v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral v)
                (Prelude.Just (PrimitiveValue'Word32Val v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                       ((Prelude..)
                          Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral v)
                (Prelude.Just (PrimitiveValue'IntegerVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.Text.Encoding.encodeUtf8 v)
                (Prelude.Just (PrimitiveValue'BytesVal v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 74)
                       ((\ bs
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                   (Prelude.fromIntegral (Data.ByteString.length bs)))
                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData PrimitiveValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PrimitiveValue'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PrimitiveValue'primitive x__) ())
instance Control.DeepSeq.NFData PrimitiveValue'Primitive where
  rnf (PrimitiveValue'IntVal x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'DoubleVal x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'TextVal x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'BoolVal x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'Word64Val x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'Int32Val x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'Word32Val x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'IntegerVal x__) = Control.DeepSeq.rnf x__
  rnf (PrimitiveValue'BytesVal x__) = Control.DeepSeq.rnf x__
_PrimitiveValue'IntVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Int.Int64
_PrimitiveValue'IntVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'IntVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'IntVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'DoubleVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Prelude.Double
_PrimitiveValue'DoubleVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'DoubleVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'DoubleVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'TextVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Text.Text
_PrimitiveValue'TextVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'TextVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'TextVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'BoolVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Prelude.Bool
_PrimitiveValue'BoolVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'BoolVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'BoolVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'Word64Val ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Word.Word64
_PrimitiveValue'Word64Val
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'Word64Val
      (\ p__
         -> case p__ of
              (PrimitiveValue'Word64Val p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'Int32Val ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Int.Int32
_PrimitiveValue'Int32Val
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'Int32Val
      (\ p__
         -> case p__ of
              (PrimitiveValue'Int32Val p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'Word32Val ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Word.Word32
_PrimitiveValue'Word32Val
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'Word32Val
      (\ p__
         -> case p__ of
              (PrimitiveValue'Word32Val p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'IntegerVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.Text.Text
_PrimitiveValue'IntegerVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'IntegerVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'IntegerVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_PrimitiveValue'BytesVal ::
  Data.ProtoLens.Prism.Prism' PrimitiveValue'Primitive Data.ByteString.ByteString
_PrimitiveValue'BytesVal
  = Data.ProtoLens.Prism.prism'
      PrimitiveValue'BytesVal
      (\ p__
         -> case p__ of
              (PrimitiveValue'BytesVal p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Serializotron_Fields.fields' @:: Lens' ProductStructure [ProductStructure'FieldInfo]@
         * 'Proto.Serializotron_Fields.vec'fields' @:: Lens' ProductStructure (Data.Vector.Vector ProductStructure'FieldInfo)@ -}
data ProductStructure
  = ProductStructure'_constructor {_ProductStructure'fields :: !(Data.Vector.Vector ProductStructure'FieldInfo),
                                   _ProductStructure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProductStructure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProductStructure "fields" [ProductStructure'FieldInfo] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductStructure'fields
           (\ x__ y__ -> x__ {_ProductStructure'fields = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ProductStructure "vec'fields" (Data.Vector.Vector ProductStructure'FieldInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductStructure'fields
           (\ x__ y__ -> x__ {_ProductStructure'fields = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProductStructure where
  messageName _ = Data.Text.pack "szt.ProductStructure"
  packedMessageDescriptor _
    = "\n\
      \\DLEProductStructure\DC27\n\
      \\ACKfields\CAN\SOH \ETX(\v2\US.szt.ProductStructure.FieldInfoR\ACKfields\SUBX\n\
      \\tFieldInfo\DC2\GS\n\
      \\n\
      \field_name\CAN\SOH \SOH(\tR\tfieldName\DC2,\n\
      \\n\
      \field_type\CAN\STX \SOH(\v2\r.szt.TypeInfoR\tfieldType"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fields__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fields"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ProductStructure'FieldInfo)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"fields")) ::
              Data.ProtoLens.FieldDescriptor ProductStructure
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fields__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProductStructure'_unknownFields
        (\ x__ y__ -> x__ {_ProductStructure'_unknownFields = y__})
  defMessage
    = ProductStructure'_constructor
        {_ProductStructure'fields = Data.Vector.Generic.empty,
         _ProductStructure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProductStructure
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ProductStructure'FieldInfo
             -> Data.ProtoLens.Encoding.Bytes.Parser ProductStructure
        loop x mutable'fields
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'fields)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'fields") frozen'fields x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "fields"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'fields y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fields
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fields)
          "ProductStructure"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fields") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ProductStructure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProductStructure'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ProductStructure'fields x__) ())
{- | Fields :
     
         * 'Proto.Serializotron_Fields.fieldName' @:: Lens' ProductStructure'FieldInfo Data.Text.Text@
         * 'Proto.Serializotron_Fields.fieldType' @:: Lens' ProductStructure'FieldInfo TypeInfo@
         * 'Proto.Serializotron_Fields.maybe'fieldType' @:: Lens' ProductStructure'FieldInfo (Prelude.Maybe TypeInfo)@ -}
data ProductStructure'FieldInfo
  = ProductStructure'FieldInfo'_constructor {_ProductStructure'FieldInfo'fieldName :: !Data.Text.Text,
                                             _ProductStructure'FieldInfo'fieldType :: !(Prelude.Maybe TypeInfo),
                                             _ProductStructure'FieldInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProductStructure'FieldInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProductStructure'FieldInfo "fieldName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductStructure'FieldInfo'fieldName
           (\ x__ y__ -> x__ {_ProductStructure'FieldInfo'fieldName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProductStructure'FieldInfo "fieldType" TypeInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductStructure'FieldInfo'fieldType
           (\ x__ y__ -> x__ {_ProductStructure'FieldInfo'fieldType = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProductStructure'FieldInfo "maybe'fieldType" (Prelude.Maybe TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductStructure'FieldInfo'fieldType
           (\ x__ y__ -> x__ {_ProductStructure'FieldInfo'fieldType = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProductStructure'FieldInfo where
  messageName _ = Data.Text.pack "szt.ProductStructure.FieldInfo"
  packedMessageDescriptor _
    = "\n\
      \\tFieldInfo\DC2\GS\n\
      \\n\
      \field_name\CAN\SOH \SOH(\tR\tfieldName\DC2,\n\
      \\n\
      \field_type\CAN\STX \SOH(\v2\r.szt.TypeInfoR\tfieldType"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fieldName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fieldName")) ::
              Data.ProtoLens.FieldDescriptor ProductStructure'FieldInfo
        fieldType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_type"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'fieldType")) ::
              Data.ProtoLens.FieldDescriptor ProductStructure'FieldInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fieldName__field_descriptor),
           (Data.ProtoLens.Tag 2, fieldType__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProductStructure'FieldInfo'_unknownFields
        (\ x__ y__
           -> x__ {_ProductStructure'FieldInfo'_unknownFields = y__})
  defMessage
    = ProductStructure'FieldInfo'_constructor
        {_ProductStructure'FieldInfo'fieldName = Data.ProtoLens.fieldDefault,
         _ProductStructure'FieldInfo'fieldType = Prelude.Nothing,
         _ProductStructure'FieldInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProductStructure'FieldInfo
          -> Data.ProtoLens.Encoding.Bytes.Parser ProductStructure'FieldInfo
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "field_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "field_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fieldType") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "FieldInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"fieldName") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'fieldType") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ProductStructure'FieldInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProductStructure'FieldInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ProductStructure'FieldInfo'fieldName x__)
                (Control.DeepSeq.deepseq
                   (_ProductStructure'FieldInfo'fieldType x__) ()))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.fields' @:: Lens' ProductValue [DynamicValue]@
         * 'Proto.Serializotron_Fields.vec'fields' @:: Lens' ProductValue (Data.Vector.Vector DynamicValue)@ -}
data ProductValue
  = ProductValue'_constructor {_ProductValue'fields :: !(Data.Vector.Vector DynamicValue),
                               _ProductValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProductValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProductValue "fields" [DynamicValue] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductValue'fields
           (\ x__ y__ -> x__ {_ProductValue'fields = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ProductValue "vec'fields" (Data.Vector.Vector DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProductValue'fields
           (\ x__ y__ -> x__ {_ProductValue'fields = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProductValue where
  messageName _ = Data.Text.pack "szt.ProductValue"
  packedMessageDescriptor _
    = "\n\
      \\fProductValue\DC2)\n\
      \\ACKfields\CAN\SOH \ETX(\v2\DC1.szt.DynamicValueR\ACKfields"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        fields__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "fields"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicValue)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"fields")) ::
              Data.ProtoLens.FieldDescriptor ProductValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, fields__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProductValue'_unknownFields
        (\ x__ y__ -> x__ {_ProductValue'_unknownFields = y__})
  defMessage
    = ProductValue'_constructor
        {_ProductValue'fields = Data.Vector.Generic.empty,
         _ProductValue'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProductValue
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DynamicValue
             -> Data.ProtoLens.Encoding.Bytes.Parser ProductValue
        loop x mutable'fields
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'fields)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'fields") frozen'fields x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "fields"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'fields y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'fields
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'fields <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'fields)
          "ProductValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'fields") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ProductValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProductValue'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ProductValue'fields x__) ())
{- | Fields :
     
         * 'Proto.Serializotron_Fields.referenceId' @:: Lens' ReferenceValue Data.Word.Word32@ -}
data ReferenceValue
  = ReferenceValue'_constructor {_ReferenceValue'referenceId :: !Data.Word.Word32,
                                 _ReferenceValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReferenceValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReferenceValue "referenceId" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReferenceValue'referenceId
           (\ x__ y__ -> x__ {_ReferenceValue'referenceId = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReferenceValue where
  messageName _ = Data.Text.pack "szt.ReferenceValue"
  packedMessageDescriptor _
    = "\n\
      \\SOReferenceValue\DC2!\n\
      \\freference_id\CAN\SOH \SOH(\rR\vreferenceId"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        referenceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "reference_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"referenceId")) ::
              Data.ProtoLens.FieldDescriptor ReferenceValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, referenceId__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReferenceValue'_unknownFields
        (\ x__ y__ -> x__ {_ReferenceValue'_unknownFields = y__})
  defMessage
    = ReferenceValue'_constructor
        {_ReferenceValue'referenceId = Data.ProtoLens.fieldDefault,
         _ReferenceValue'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReferenceValue
          -> Data.ProtoLens.Encoding.Bytes.Parser ReferenceValue
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "reference_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"referenceId") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReferenceValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"referenceId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ReferenceValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReferenceValue'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ReferenceValue'referenceId x__) ())
{- | Fields :
     
         * 'Proto.Serializotron_Fields.schemaVersion' @:: Lens' SZTFile Data.Word.Word32@
         * 'Proto.Serializotron_Fields.value' @:: Lens' SZTFile DynamicValue@
         * 'Proto.Serializotron_Fields.maybe'value' @:: Lens' SZTFile (Prelude.Maybe DynamicValue)@
         * 'Proto.Serializotron_Fields.sharedValues' @:: Lens' SZTFile (Data.Map.Map Data.Word.Word32 DynamicValue)@
         * 'Proto.Serializotron_Fields.sharedTypeInfo' @:: Lens' SZTFile (Data.Map.Map Data.Word.Word32 TypeInfo)@ -}
data SZTFile
  = SZTFile'_constructor {_SZTFile'schemaVersion :: !Data.Word.Word32,
                          _SZTFile'value :: !(Prelude.Maybe DynamicValue),
                          _SZTFile'sharedValues :: !(Data.Map.Map Data.Word.Word32 DynamicValue),
                          _SZTFile'sharedTypeInfo :: !(Data.Map.Map Data.Word.Word32 TypeInfo),
                          _SZTFile'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SZTFile where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SZTFile "schemaVersion" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'schemaVersion
           (\ x__ y__ -> x__ {_SZTFile'schemaVersion = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SZTFile "value" DynamicValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'value (\ x__ y__ -> x__ {_SZTFile'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SZTFile "maybe'value" (Prelude.Maybe DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'value (\ x__ y__ -> x__ {_SZTFile'value = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SZTFile "sharedValues" (Data.Map.Map Data.Word.Word32 DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'sharedValues
           (\ x__ y__ -> x__ {_SZTFile'sharedValues = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SZTFile "sharedTypeInfo" (Data.Map.Map Data.Word.Word32 TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'sharedTypeInfo
           (\ x__ y__ -> x__ {_SZTFile'sharedTypeInfo = y__}))
        Prelude.id
instance Data.ProtoLens.Message SZTFile where
  messageName _ = Data.Text.pack "szt.SZTFile"
  packedMessageDescriptor _
    = "\n\
      \\aSZTFile\DC2%\n\
      \\SOschema_version\CAN\SOH \SOH(\rR\rschemaVersion\DC2'\n\
      \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue\DC2C\n\
      \\rshared_values\CAN\ETX \ETX(\v2\RS.szt.SZTFile.SharedValuesEntryR\fsharedValues\DC2J\n\
      \\DLEshared_type_info\CAN\EOT \ETX(\v2 .szt.SZTFile.SharedTypeInfoEntryR\SOsharedTypeInfo\SUBR\n\
      \\DC1SharedValuesEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2'\n\
      \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue:\STX8\SOH\SUBP\n\
      \\DC3SharedTypeInfoEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2#\n\
      \\ENQvalue\CAN\STX \SOH(\v2\r.szt.TypeInfoR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        schemaVersion__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "schema_version"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"schemaVersion")) ::
              Data.ProtoLens.FieldDescriptor SZTFile
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor SZTFile
        sharedValues__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "shared_values"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SZTFile'SharedValuesEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"sharedValues")) ::
              Data.ProtoLens.FieldDescriptor SZTFile
        sharedTypeInfo__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "shared_type_info"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SZTFile'SharedTypeInfoEntry)
              (Data.ProtoLens.MapField
                 (Data.ProtoLens.Field.field @"key")
                 (Data.ProtoLens.Field.field @"value")
                 (Data.ProtoLens.Field.field @"sharedTypeInfo")) ::
              Data.ProtoLens.FieldDescriptor SZTFile
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, schemaVersion__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor),
           (Data.ProtoLens.Tag 3, sharedValues__field_descriptor),
           (Data.ProtoLens.Tag 4, sharedTypeInfo__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SZTFile'_unknownFields
        (\ x__ y__ -> x__ {_SZTFile'_unknownFields = y__})
  defMessage
    = SZTFile'_constructor
        {_SZTFile'schemaVersion = Data.ProtoLens.fieldDefault,
         _SZTFile'value = Prelude.Nothing,
         _SZTFile'sharedValues = Data.Map.empty,
         _SZTFile'sharedTypeInfo = Data.Map.empty,
         _SZTFile'_unknownFields = []}
  parseMessage
    = let
        loop :: SZTFile -> Data.ProtoLens.Encoding.Bytes.Parser SZTFile
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "schema_version"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"schemaVersion") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        26
                          -> do !(entry :: SZTFile'SharedValuesEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                           (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                               Data.ProtoLens.Encoding.Bytes.isolate
                                                                                 (Prelude.fromIntegral
                                                                                    len)
                                                                                 Data.ProtoLens.parseMessage)
                                                                           "shared_values"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"sharedValues")
                                        (\ !t -> Data.Map.insert key value t) x))
                        34
                          -> do !(entry :: SZTFile'SharedTypeInfoEntry) <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                             (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                                 Data.ProtoLens.Encoding.Bytes.isolate
                                                                                   (Prelude.fromIntegral
                                                                                      len)
                                                                                   Data.ProtoLens.parseMessage)
                                                                             "shared_type_info"
                                (let
                                   key = Lens.Family2.view (Data.ProtoLens.Field.field @"key") entry
                                   value
                                     = Lens.Family2.view (Data.ProtoLens.Field.field @"value") entry
                                 in
                                   loop
                                     (Lens.Family2.over
                                        (Data.ProtoLens.Field.field @"sharedTypeInfo")
                                        (\ !t -> Data.Map.insert key value t) x))
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SZTFile"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"schemaVersion") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (Data.Monoid.mconcat
                      (Prelude.map
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                          (Data.ProtoLens.defMessage ::
                                             SZTFile'SharedValuesEntry)))))
                         (Data.Map.toList
                            (Lens.Family2.view
                               (Data.ProtoLens.Field.field @"sharedValues") _x))))
                   ((Data.Monoid.<>)
                      (Data.Monoid.mconcat
                         (Prelude.map
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"key") (Prelude.fst _v)
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"value") (Prelude.snd _v)
                                             (Data.ProtoLens.defMessage ::
                                                SZTFile'SharedTypeInfoEntry)))))
                            (Data.Map.toList
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"sharedTypeInfo") _x))))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData SZTFile where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SZTFile'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SZTFile'schemaVersion x__)
                (Control.DeepSeq.deepseq
                   (_SZTFile'value x__)
                   (Control.DeepSeq.deepseq
                      (_SZTFile'sharedValues x__)
                      (Control.DeepSeq.deepseq (_SZTFile'sharedTypeInfo x__) ()))))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.key' @:: Lens' SZTFile'SharedTypeInfoEntry Data.Word.Word32@
         * 'Proto.Serializotron_Fields.value' @:: Lens' SZTFile'SharedTypeInfoEntry TypeInfo@
         * 'Proto.Serializotron_Fields.maybe'value' @:: Lens' SZTFile'SharedTypeInfoEntry (Prelude.Maybe TypeInfo)@ -}
data SZTFile'SharedTypeInfoEntry
  = SZTFile'SharedTypeInfoEntry'_constructor {_SZTFile'SharedTypeInfoEntry'key :: !Data.Word.Word32,
                                              _SZTFile'SharedTypeInfoEntry'value :: !(Prelude.Maybe TypeInfo),
                                              _SZTFile'SharedTypeInfoEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SZTFile'SharedTypeInfoEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SZTFile'SharedTypeInfoEntry "key" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedTypeInfoEntry'key
           (\ x__ y__ -> x__ {_SZTFile'SharedTypeInfoEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SZTFile'SharedTypeInfoEntry "value" TypeInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedTypeInfoEntry'value
           (\ x__ y__ -> x__ {_SZTFile'SharedTypeInfoEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SZTFile'SharedTypeInfoEntry "maybe'value" (Prelude.Maybe TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedTypeInfoEntry'value
           (\ x__ y__ -> x__ {_SZTFile'SharedTypeInfoEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message SZTFile'SharedTypeInfoEntry where
  messageName _ = Data.Text.pack "szt.SZTFile.SharedTypeInfoEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC3SharedTypeInfoEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2#\n\
      \\ENQvalue\CAN\STX \SOH(\v2\r.szt.TypeInfoR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor SZTFile'SharedTypeInfoEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor SZTFile'SharedTypeInfoEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SZTFile'SharedTypeInfoEntry'_unknownFields
        (\ x__ y__
           -> x__ {_SZTFile'SharedTypeInfoEntry'_unknownFields = y__})
  defMessage
    = SZTFile'SharedTypeInfoEntry'_constructor
        {_SZTFile'SharedTypeInfoEntry'key = Data.ProtoLens.fieldDefault,
         _SZTFile'SharedTypeInfoEntry'value = Prelude.Nothing,
         _SZTFile'SharedTypeInfoEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SZTFile'SharedTypeInfoEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser SZTFile'SharedTypeInfoEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SharedTypeInfoEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData SZTFile'SharedTypeInfoEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SZTFile'SharedTypeInfoEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SZTFile'SharedTypeInfoEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_SZTFile'SharedTypeInfoEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.key' @:: Lens' SZTFile'SharedValuesEntry Data.Word.Word32@
         * 'Proto.Serializotron_Fields.value' @:: Lens' SZTFile'SharedValuesEntry DynamicValue@
         * 'Proto.Serializotron_Fields.maybe'value' @:: Lens' SZTFile'SharedValuesEntry (Prelude.Maybe DynamicValue)@ -}
data SZTFile'SharedValuesEntry
  = SZTFile'SharedValuesEntry'_constructor {_SZTFile'SharedValuesEntry'key :: !Data.Word.Word32,
                                            _SZTFile'SharedValuesEntry'value :: !(Prelude.Maybe DynamicValue),
                                            _SZTFile'SharedValuesEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SZTFile'SharedValuesEntry where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SZTFile'SharedValuesEntry "key" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedValuesEntry'key
           (\ x__ y__ -> x__ {_SZTFile'SharedValuesEntry'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SZTFile'SharedValuesEntry "value" DynamicValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedValuesEntry'value
           (\ x__ y__ -> x__ {_SZTFile'SharedValuesEntry'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SZTFile'SharedValuesEntry "maybe'value" (Prelude.Maybe DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SZTFile'SharedValuesEntry'value
           (\ x__ y__ -> x__ {_SZTFile'SharedValuesEntry'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message SZTFile'SharedValuesEntry where
  messageName _ = Data.Text.pack "szt.SZTFile.SharedValuesEntry"
  packedMessageDescriptor _
    = "\n\
      \\DC1SharedValuesEntry\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2'\n\
      \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue:\STX8\SOH"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor SZTFile'SharedValuesEntry
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor SZTFile'SharedValuesEntry
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SZTFile'SharedValuesEntry'_unknownFields
        (\ x__ y__
           -> x__ {_SZTFile'SharedValuesEntry'_unknownFields = y__})
  defMessage
    = SZTFile'SharedValuesEntry'_constructor
        {_SZTFile'SharedValuesEntry'key = Data.ProtoLens.fieldDefault,
         _SZTFile'SharedValuesEntry'value = Prelude.Nothing,
         _SZTFile'SharedValuesEntry'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SZTFile'SharedValuesEntry
          -> Data.ProtoLens.Encoding.Bytes.Parser SZTFile'SharedValuesEntry
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SharedValuesEntry"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData SZTFile'SharedValuesEntry where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SZTFile'SharedValuesEntry'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SZTFile'SharedValuesEntry'key x__)
                (Control.DeepSeq.deepseq
                   (_SZTFile'SharedValuesEntry'value x__) ()))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.constructorTypes' @:: Lens' SumStructure [TypeInfo]@
         * 'Proto.Serializotron_Fields.vec'constructorTypes' @:: Lens' SumStructure (Data.Vector.Vector TypeInfo)@ -}
data SumStructure
  = SumStructure'_constructor {_SumStructure'constructorTypes :: !(Data.Vector.Vector TypeInfo),
                               _SumStructure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SumStructure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SumStructure "constructorTypes" [TypeInfo] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumStructure'constructorTypes
           (\ x__ y__ -> x__ {_SumStructure'constructorTypes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField SumStructure "vec'constructorTypes" (Data.Vector.Vector TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumStructure'constructorTypes
           (\ x__ y__ -> x__ {_SumStructure'constructorTypes = y__}))
        Prelude.id
instance Data.ProtoLens.Message SumStructure where
  messageName _ = Data.Text.pack "szt.SumStructure"
  packedMessageDescriptor _
    = "\n\
      \\fSumStructure\DC2:\n\
      \\DC1constructor_types\CAN\SOH \ETX(\v2\r.szt.TypeInfoR\DLEconstructorTypes"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        constructorTypes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constructor_types"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"constructorTypes")) ::
              Data.ProtoLens.FieldDescriptor SumStructure
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, constructorTypes__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SumStructure'_unknownFields
        (\ x__ y__ -> x__ {_SumStructure'_unknownFields = y__})
  defMessage
    = SumStructure'_constructor
        {_SumStructure'constructorTypes = Data.Vector.Generic.empty,
         _SumStructure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          SumStructure
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TypeInfo
             -> Data.ProtoLens.Encoding.Bytes.Parser SumStructure
        loop x mutable'constructorTypes
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'constructorTypes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'constructorTypes)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'constructorTypes")
                              frozen'constructorTypes x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "constructor_types"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'constructorTypes y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'constructorTypes
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'constructorTypes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'constructorTypes)
          "SumStructure"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'constructorTypes") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData SumStructure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SumStructure'_unknownFields x__)
             (Control.DeepSeq.deepseq (_SumStructure'constructorTypes x__) ())
{- | Fields :
     
         * 'Proto.Serializotron_Fields.constructorName' @:: Lens' SumValue Data.Text.Text@
         * 'Proto.Serializotron_Fields.value' @:: Lens' SumValue DynamicValue@
         * 'Proto.Serializotron_Fields.maybe'value' @:: Lens' SumValue (Prelude.Maybe DynamicValue)@
         * 'Proto.Serializotron_Fields.constructorIndex' @:: Lens' SumValue Data.Word.Word32@ -}
data SumValue
  = SumValue'_constructor {_SumValue'constructorName :: !Data.Text.Text,
                           _SumValue'value :: !(Prelude.Maybe DynamicValue),
                           _SumValue'constructorIndex :: !Data.Word.Word32,
                           _SumValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SumValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField SumValue "constructorName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumValue'constructorName
           (\ x__ y__ -> x__ {_SumValue'constructorName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SumValue "value" DynamicValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumValue'value (\ x__ y__ -> x__ {_SumValue'value = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField SumValue "maybe'value" (Prelude.Maybe DynamicValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumValue'value (\ x__ y__ -> x__ {_SumValue'value = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField SumValue "constructorIndex" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _SumValue'constructorIndex
           (\ x__ y__ -> x__ {_SumValue'constructorIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Message SumValue where
  messageName _ = Data.Text.pack "szt.SumValue"
  packedMessageDescriptor _
    = "\n\
      \\bSumValue\DC2)\n\
      \\DLEconstructor_name\CAN\SOH \SOH(\tR\SIconstructorName\DC2'\n\
      \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue\DC2+\n\
      \\DC1constructor_index\CAN\ETX \SOH(\rR\DLEconstructorIndex"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        constructorName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constructor_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"constructorName")) ::
              Data.ProtoLens.FieldDescriptor SumValue
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DynamicValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'value")) ::
              Data.ProtoLens.FieldDescriptor SumValue
        constructorIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constructor_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"constructorIndex")) ::
              Data.ProtoLens.FieldDescriptor SumValue
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, constructorName__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor),
           (Data.ProtoLens.Tag 3, constructorIndex__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _SumValue'_unknownFields
        (\ x__ y__ -> x__ {_SumValue'_unknownFields = y__})
  defMessage
    = SumValue'_constructor
        {_SumValue'constructorName = Data.ProtoLens.fieldDefault,
         _SumValue'value = Prelude.Nothing,
         _SumValue'constructorIndex = Data.ProtoLens.fieldDefault,
         _SumValue'_unknownFields = []}
  parseMessage
    = let
        loop :: SumValue -> Data.ProtoLens.Encoding.Bytes.Parser SumValue
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "constructor_name"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"constructorName") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "constructor_index"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"constructorIndex") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "SumValue"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"constructorName") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'value") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"constructorIndex") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData SumValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_SumValue'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_SumValue'constructorName x__)
                (Control.DeepSeq.deepseq
                   (_SumValue'value x__)
                   (Control.DeepSeq.deepseq (_SumValue'constructorIndex x__) ())))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.typeName' @:: Lens' TypeInfo Data.Text.Text@
         * 'Proto.Serializotron_Fields.moduleName' @:: Lens' TypeInfo Data.Text.Text@
         * 'Proto.Serializotron_Fields.constructors' @:: Lens' TypeInfo [Data.Text.Text]@
         * 'Proto.Serializotron_Fields.vec'constructors' @:: Lens' TypeInfo (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Serializotron_Fields.fieldLabels' @:: Lens' TypeInfo [Data.Text.Text]@
         * 'Proto.Serializotron_Fields.vec'fieldLabels' @:: Lens' TypeInfo (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Serializotron_Fields.structure' @:: Lens' TypeInfo TypeStructure@
         * 'Proto.Serializotron_Fields.maybe'structure' @:: Lens' TypeInfo (Prelude.Maybe TypeStructure)@
         * 'Proto.Serializotron_Fields.typeParameters' @:: Lens' TypeInfo [TypeInfo]@
         * 'Proto.Serializotron_Fields.vec'typeParameters' @:: Lens' TypeInfo (Data.Vector.Vector TypeInfo)@
         * 'Proto.Serializotron_Fields.newtypeWrapper' @:: Lens' TypeInfo TypeInfo@
         * 'Proto.Serializotron_Fields.maybe'newtypeWrapper' @:: Lens' TypeInfo (Prelude.Maybe TypeInfo)@ -}
data TypeInfo
  = TypeInfo'_constructor {_TypeInfo'typeName :: !Data.Text.Text,
                           _TypeInfo'moduleName :: !Data.Text.Text,
                           _TypeInfo'constructors :: !(Data.Vector.Vector Data.Text.Text),
                           _TypeInfo'fieldLabels :: !(Data.Vector.Vector Data.Text.Text),
                           _TypeInfo'structure :: !(Prelude.Maybe TypeStructure),
                           _TypeInfo'typeParameters :: !(Data.Vector.Vector TypeInfo),
                           _TypeInfo'newtypeWrapper :: !(Prelude.Maybe TypeInfo),
                           _TypeInfo'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeInfo where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TypeInfo "typeName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'typeName (\ x__ y__ -> x__ {_TypeInfo'typeName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "moduleName" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'moduleName
           (\ x__ y__ -> x__ {_TypeInfo'moduleName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "constructors" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'constructors
           (\ x__ y__ -> x__ {_TypeInfo'constructors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TypeInfo "vec'constructors" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'constructors
           (\ x__ y__ -> x__ {_TypeInfo'constructors = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "fieldLabels" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'fieldLabels
           (\ x__ y__ -> x__ {_TypeInfo'fieldLabels = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TypeInfo "vec'fieldLabels" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'fieldLabels
           (\ x__ y__ -> x__ {_TypeInfo'fieldLabels = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "structure" TypeStructure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'structure (\ x__ y__ -> x__ {_TypeInfo'structure = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeInfo "maybe'structure" (Prelude.Maybe TypeStructure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'structure (\ x__ y__ -> x__ {_TypeInfo'structure = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "typeParameters" [TypeInfo] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'typeParameters
           (\ x__ y__ -> x__ {_TypeInfo'typeParameters = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TypeInfo "vec'typeParameters" (Data.Vector.Vector TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'typeParameters
           (\ x__ y__ -> x__ {_TypeInfo'typeParameters = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeInfo "newtypeWrapper" TypeInfo where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'newtypeWrapper
           (\ x__ y__ -> x__ {_TypeInfo'newtypeWrapper = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TypeInfo "maybe'newtypeWrapper" (Prelude.Maybe TypeInfo) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeInfo'newtypeWrapper
           (\ x__ y__ -> x__ {_TypeInfo'newtypeWrapper = y__}))
        Prelude.id
instance Data.ProtoLens.Message TypeInfo where
  messageName _ = Data.Text.pack "szt.TypeInfo"
  packedMessageDescriptor _
    = "\n\
      \\bTypeInfo\DC2\ESC\n\
      \\ttype_name\CAN\SOH \SOH(\tR\btypeName\DC2\US\n\
      \\vmodule_name\CAN\STX \SOH(\tR\n\
      \moduleName\DC2\"\n\
      \\fconstructors\CAN\ETX \ETX(\tR\fconstructors\DC2!\n\
      \\ffield_labels\CAN\EOT \ETX(\tR\vfieldLabels\DC20\n\
      \\tstructure\CAN\ENQ \SOH(\v2\DC2.szt.TypeStructureR\tstructure\DC26\n\
      \\SItype_parameters\CAN\ACK \ETX(\v2\r.szt.TypeInfoR\SOtypeParameters\DC26\n\
      \\SInewtype_wrapper\CAN\a \SOH(\v2\r.szt.TypeInfoR\SOnewtypeWrapper"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        typeName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"typeName")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        moduleName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "module_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"moduleName")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        constructors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "constructors"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"constructors")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        fieldLabels__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "field_labels"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"fieldLabels")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        structure__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "structure"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeStructure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'structure")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        typeParameters__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_parameters"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"typeParameters")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
        newtypeWrapper__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "newtype_wrapper"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TypeInfo)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'newtypeWrapper")) ::
              Data.ProtoLens.FieldDescriptor TypeInfo
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, typeName__field_descriptor),
           (Data.ProtoLens.Tag 2, moduleName__field_descriptor),
           (Data.ProtoLens.Tag 3, constructors__field_descriptor),
           (Data.ProtoLens.Tag 4, fieldLabels__field_descriptor),
           (Data.ProtoLens.Tag 5, structure__field_descriptor),
           (Data.ProtoLens.Tag 6, typeParameters__field_descriptor),
           (Data.ProtoLens.Tag 7, newtypeWrapper__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeInfo'_unknownFields
        (\ x__ y__ -> x__ {_TypeInfo'_unknownFields = y__})
  defMessage
    = TypeInfo'_constructor
        {_TypeInfo'typeName = Data.ProtoLens.fieldDefault,
         _TypeInfo'moduleName = Data.ProtoLens.fieldDefault,
         _TypeInfo'constructors = Data.Vector.Generic.empty,
         _TypeInfo'fieldLabels = Data.Vector.Generic.empty,
         _TypeInfo'structure = Prelude.Nothing,
         _TypeInfo'typeParameters = Data.Vector.Generic.empty,
         _TypeInfo'newtypeWrapper = Prelude.Nothing,
         _TypeInfo'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeInfo
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TypeInfo
                   -> Data.ProtoLens.Encoding.Bytes.Parser TypeInfo
        loop
          x
          mutable'constructors
          mutable'fieldLabels
          mutable'typeParameters
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'constructors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'constructors)
                      frozen'fieldLabels <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'fieldLabels)
                      frozen'typeParameters <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'typeParameters)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'constructors")
                              frozen'constructors
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'fieldLabels") frozen'fieldLabels
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'typeParameters")
                                    frozen'typeParameters x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "type_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeName") y x)
                                  mutable'constructors mutable'fieldLabels mutable'typeParameters
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "module_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"moduleName") y x)
                                  mutable'constructors mutable'fieldLabels mutable'typeParameters
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getText
                                              (Prelude.fromIntegral len))
                                        "constructors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'constructors y)
                                loop x v mutable'fieldLabels mutable'typeParameters
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getText
                                              (Prelude.fromIntegral len))
                                        "field_labels"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'fieldLabels y)
                                loop x mutable'constructors v mutable'typeParameters
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "structure"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"structure") y x)
                                  mutable'constructors mutable'fieldLabels mutable'typeParameters
                        50
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "type_parameters"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'typeParameters y)
                                loop x mutable'constructors mutable'fieldLabels v
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "newtype_wrapper"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"newtypeWrapper") y x)
                                  mutable'constructors mutable'fieldLabels mutable'typeParameters
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'constructors mutable'fieldLabels mutable'typeParameters
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'constructors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
              mutable'fieldLabels <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              mutable'typeParameters <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'constructors mutable'fieldLabels
                mutable'typeParameters)
          "TypeInfo"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"typeName") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"moduleName") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.Text.Encoding.encodeUtf8 _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'constructors") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.Text.Encoding.encodeUtf8 _v))
                         (Lens.Family2.view
                            (Data.ProtoLens.Field.field @"vec'fieldLabels") _x))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view
                                (Data.ProtoLens.Field.field @"maybe'structure") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage _v))
                         ((Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                       ((Prelude..)
                                          (\ bs
                                             -> (Data.Monoid.<>)
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                          Data.ProtoLens.encodeMessage _v))
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"vec'typeParameters") _x))
                            ((Data.Monoid.<>)
                               (case
                                    Lens.Family2.view
                                      (Data.ProtoLens.Field.field @"maybe'newtypeWrapper") _x
                                of
                                  Prelude.Nothing -> Data.Monoid.mempty
                                  (Prelude.Just _v)
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                         ((Prelude..)
                                            (\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                            Data.ProtoLens.encodeMessage _v))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData TypeInfo where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeInfo'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TypeInfo'typeName x__)
                (Control.DeepSeq.deepseq
                   (_TypeInfo'moduleName x__)
                   (Control.DeepSeq.deepseq
                      (_TypeInfo'constructors x__)
                      (Control.DeepSeq.deepseq
                         (_TypeInfo'fieldLabels x__)
                         (Control.DeepSeq.deepseq
                            (_TypeInfo'structure x__)
                            (Control.DeepSeq.deepseq
                               (_TypeInfo'typeParameters x__)
                               (Control.DeepSeq.deepseq (_TypeInfo'newtypeWrapper x__) ())))))))
{- | Fields :
     
         * 'Proto.Serializotron_Fields.maybe'structure' @:: Lens' TypeStructure (Prelude.Maybe TypeStructure'Structure)@
         * 'Proto.Serializotron_Fields.maybe'primitive' @:: Lens' TypeStructure (Prelude.Maybe PrimitiveType)@
         * 'Proto.Serializotron_Fields.primitive' @:: Lens' TypeStructure PrimitiveType@
         * 'Proto.Serializotron_Fields.maybe'product' @:: Lens' TypeStructure (Prelude.Maybe ProductStructure)@
         * 'Proto.Serializotron_Fields.product' @:: Lens' TypeStructure ProductStructure@
         * 'Proto.Serializotron_Fields.maybe'sum' @:: Lens' TypeStructure (Prelude.Maybe SumStructure)@
         * 'Proto.Serializotron_Fields.sum' @:: Lens' TypeStructure SumStructure@
         * 'Proto.Serializotron_Fields.maybe'list' @:: Lens' TypeStructure (Prelude.Maybe ListStructure)@
         * 'Proto.Serializotron_Fields.list' @:: Lens' TypeStructure ListStructure@
         * 'Proto.Serializotron_Fields.maybe'unit' @:: Lens' TypeStructure (Prelude.Maybe UnitStructure)@
         * 'Proto.Serializotron_Fields.unit' @:: Lens' TypeStructure UnitStructure@ -}
data TypeStructure
  = TypeStructure'_constructor {_TypeStructure'structure :: !(Prelude.Maybe TypeStructure'Structure),
                                _TypeStructure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TypeStructure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data TypeStructure'Structure
  = TypeStructure'Primitive !PrimitiveType |
    TypeStructure'Product !ProductStructure |
    TypeStructure'Sum !SumStructure |
    TypeStructure'List !ListStructure |
    TypeStructure'Unit !UnitStructure
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'structure" (Prelude.Maybe TypeStructure'Structure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'primitive" (Prelude.Maybe PrimitiveType) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeStructure'Primitive x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeStructure'Primitive y__))
instance Data.ProtoLens.Field.HasField TypeStructure "primitive" PrimitiveType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeStructure'Primitive x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeStructure'Primitive y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault))
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'product" (Prelude.Maybe ProductStructure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeStructure'Product x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeStructure'Product y__))
instance Data.ProtoLens.Field.HasField TypeStructure "product" ProductStructure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeStructure'Product x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeStructure'Product y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'sum" (Prelude.Maybe SumStructure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeStructure'Sum x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeStructure'Sum y__))
instance Data.ProtoLens.Field.HasField TypeStructure "sum" SumStructure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeStructure'Sum x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeStructure'Sum y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'list" (Prelude.Maybe ListStructure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeStructure'List x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeStructure'List y__))
instance Data.ProtoLens.Field.HasField TypeStructure "list" ListStructure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeStructure'List x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeStructure'List y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField TypeStructure "maybe'unit" (Prelude.Maybe UnitStructure) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (TypeStructure'Unit x__val)) -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap TypeStructure'Unit y__))
instance Data.ProtoLens.Field.HasField TypeStructure "unit" UnitStructure where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TypeStructure'structure
           (\ x__ y__ -> x__ {_TypeStructure'structure = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (TypeStructure'Unit x__val)) -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap TypeStructure'Unit y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message TypeStructure where
  messageName _ = Data.Text.pack "szt.TypeStructure"
  packedMessageDescriptor _
    = "\n\
      \\rTypeStructure\DC22\n\
      \\tprimitive\CAN\SOH \SOH(\SO2\DC2.szt.PrimitiveTypeH\NULR\tprimitive\DC21\n\
      \\aproduct\CAN\STX \SOH(\v2\NAK.szt.ProductStructureH\NULR\aproduct\DC2%\n\
      \\ETXsum\CAN\ETX \SOH(\v2\DC1.szt.SumStructureH\NULR\ETXsum\DC2(\n\
      \\EOTlist\CAN\EOT \SOH(\v2\DC2.szt.ListStructureH\NULR\EOTlist\DC2(\n\
      \\EOTunit\CAN\ENQ \SOH(\v2\DC2.szt.UnitStructureH\NULR\EOTunitB\v\n\
      \\tstructure"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        primitive__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "primitive"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor PrimitiveType)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'primitive")) ::
              Data.ProtoLens.FieldDescriptor TypeStructure
        product__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "product"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ProductStructure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'product")) ::
              Data.ProtoLens.FieldDescriptor TypeStructure
        sum__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sum"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor SumStructure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'sum")) ::
              Data.ProtoLens.FieldDescriptor TypeStructure
        list__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "list"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ListStructure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'list")) ::
              Data.ProtoLens.FieldDescriptor TypeStructure
        unit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor UnitStructure)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'unit")) ::
              Data.ProtoLens.FieldDescriptor TypeStructure
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, primitive__field_descriptor),
           (Data.ProtoLens.Tag 2, product__field_descriptor),
           (Data.ProtoLens.Tag 3, sum__field_descriptor),
           (Data.ProtoLens.Tag 4, list__field_descriptor),
           (Data.ProtoLens.Tag 5, unit__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TypeStructure'_unknownFields
        (\ x__ y__ -> x__ {_TypeStructure'_unknownFields = y__})
  defMessage
    = TypeStructure'_constructor
        {_TypeStructure'structure = Prelude.Nothing,
         _TypeStructure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TypeStructure -> Data.ProtoLens.Encoding.Bytes.Parser TypeStructure
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "primitive"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"primitive") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "product"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"product") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "sum"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"sum") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "list"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"list") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "unit"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unit") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TypeStructure"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'structure") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just (TypeStructure'Primitive v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                       ((Prelude..)
                          ((Prelude..)
                             Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                          Prelude.fromEnum v)
                (Prelude.Just (TypeStructure'Product v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (TypeStructure'Sum v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (TypeStructure'List v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v)
                (Prelude.Just (TypeStructure'Unit v))
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData TypeStructure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TypeStructure'_unknownFields x__)
             (Control.DeepSeq.deepseq (_TypeStructure'structure x__) ())
instance Control.DeepSeq.NFData TypeStructure'Structure where
  rnf (TypeStructure'Primitive x__) = Control.DeepSeq.rnf x__
  rnf (TypeStructure'Product x__) = Control.DeepSeq.rnf x__
  rnf (TypeStructure'Sum x__) = Control.DeepSeq.rnf x__
  rnf (TypeStructure'List x__) = Control.DeepSeq.rnf x__
  rnf (TypeStructure'Unit x__) = Control.DeepSeq.rnf x__
_TypeStructure'Primitive ::
  Data.ProtoLens.Prism.Prism' TypeStructure'Structure PrimitiveType
_TypeStructure'Primitive
  = Data.ProtoLens.Prism.prism'
      TypeStructure'Primitive
      (\ p__
         -> case p__ of
              (TypeStructure'Primitive p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeStructure'Product ::
  Data.ProtoLens.Prism.Prism' TypeStructure'Structure ProductStructure
_TypeStructure'Product
  = Data.ProtoLens.Prism.prism'
      TypeStructure'Product
      (\ p__
         -> case p__ of
              (TypeStructure'Product p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeStructure'Sum ::
  Data.ProtoLens.Prism.Prism' TypeStructure'Structure SumStructure
_TypeStructure'Sum
  = Data.ProtoLens.Prism.prism'
      TypeStructure'Sum
      (\ p__
         -> case p__ of
              (TypeStructure'Sum p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeStructure'List ::
  Data.ProtoLens.Prism.Prism' TypeStructure'Structure ListStructure
_TypeStructure'List
  = Data.ProtoLens.Prism.prism'
      TypeStructure'List
      (\ p__
         -> case p__ of
              (TypeStructure'List p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_TypeStructure'Unit ::
  Data.ProtoLens.Prism.Prism' TypeStructure'Structure UnitStructure
_TypeStructure'Unit
  = Data.ProtoLens.Prism.prism'
      TypeStructure'Unit
      (\ p__
         -> case p__ of
              (TypeStructure'Unit p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
      -}
data UnitStructure
  = UnitStructure'_constructor {_UnitStructure'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UnitStructure where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message UnitStructure where
  messageName _ = Data.Text.pack "szt.UnitStructure"
  packedMessageDescriptor _
    = "\n\
      \\rUnitStructure"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UnitStructure'_unknownFields
        (\ x__ y__ -> x__ {_UnitStructure'_unknownFields = y__})
  defMessage
    = UnitStructure'_constructor {_UnitStructure'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UnitStructure -> Data.ProtoLens.Encoding.Bytes.Parser UnitStructure
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UnitStructure"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData UnitStructure where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_UnitStructure'_unknownFields x__) ()
{- | Fields :
      -}
data UnitValue
  = UnitValue'_constructor {_UnitValue'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UnitValue where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message UnitValue where
  messageName _ = Data.Text.pack "szt.UnitValue"
  packedMessageDescriptor _
    = "\n\
      \\tUnitValue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UnitValue'_unknownFields
        (\ x__ y__ -> x__ {_UnitValue'_unknownFields = y__})
  defMessage
    = UnitValue'_constructor {_UnitValue'_unknownFields = []}
  parseMessage
    = let
        loop :: UnitValue -> Data.ProtoLens.Encoding.Bytes.Parser UnitValue
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UnitValue"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData UnitValue where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_UnitValue'_unknownFields x__) ()
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\DC3serializotron.proto\DC2\ETXszt\"\144\ETX\n\
    \\aSZTFile\DC2%\n\
    \\SOschema_version\CAN\SOH \SOH(\rR\rschemaVersion\DC2'\n\
    \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue\DC2C\n\
    \\rshared_values\CAN\ETX \ETX(\v2\RS.szt.SZTFile.SharedValuesEntryR\fsharedValues\DC2J\n\
    \\DLEshared_type_info\CAN\EOT \ETX(\v2 .szt.SZTFile.SharedTypeInfoEntryR\SOsharedTypeInfo\SUBR\n\
    \\DC1SharedValuesEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2'\n\
    \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue:\STX8\SOH\SUBP\n\
    \\DC3SharedTypeInfoEntry\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\rR\ETXkey\DC2#\n\
    \\ENQvalue\CAN\STX \SOH(\v2\r.szt.TypeInfoR\ENQvalue:\STX8\SOH\"\132\SOH\n\
    \\fDynamicValue\DC2$\n\
    \\EOTcore\CAN\SOH \SOH(\v2\DLE.szt.DynamicCoreR\EOTcore\DC2*\n\
    \\ttype_info\CAN\STX \SOH(\v2\r.szt.TypeInfoR\btypeInfo\DC2\"\n\
    \\rtype_info_ref\CAN\ETX \SOH(\rR\vtypeInfoRef\"\157\STX\n\
    \\vDynamicCore\DC23\n\
    \\tprimitive\CAN\SOH \SOH(\v2\DC3.szt.PrimitiveValueH\NULR\tprimitive\DC2-\n\
    \\aproduct\CAN\STX \SOH(\v2\DC1.szt.ProductValueH\NULR\aproduct\DC2!\n\
    \\ETXsum\CAN\ETX \SOH(\v2\r.szt.SumValueH\NULR\ETXsum\DC2$\n\
    \\EOTlist\CAN\EOT \SOH(\v2\SO.szt.ListValueH\NULR\EOTlist\DC2$\n\
    \\EOTunit\CAN\ENQ \SOH(\v2\SO.szt.UnitValueH\NULR\EOTunit\DC23\n\
    \\treference\CAN\ACK \SOH(\v2\DC3.szt.ReferenceValueH\NULR\treferenceB\ACK\n\
    \\EOTcore\"3\n\
    \\SOReferenceValue\DC2!\n\
    \\freference_id\CAN\SOH \SOH(\rR\vreferenceId\"\182\STX\n\
    \\SOPrimitiveValue\DC2\EM\n\
    \\aint_val\CAN\SOH \SOH(\ETXH\NULR\ACKintVal\DC2\US\n\
    \\n\
    \double_val\CAN\STX \SOH(\SOHH\NULR\tdoubleVal\DC2\ESC\n\
    \\btext_val\CAN\ETX \SOH(\tH\NULR\atextVal\DC2\ESC\n\
    \\bbool_val\CAN\EOT \SOH(\bH\NULR\aboolVal\DC2\US\n\
    \\n\
    \word64_val\CAN\ENQ \SOH(\EOTH\NULR\tword64Val\DC2\GS\n\
    \\tint32_val\CAN\ACK \SOH(\ENQH\NULR\bint32Val\DC2\US\n\
    \\n\
    \word32_val\CAN\a \SOH(\rH\NULR\tword32Val\DC2!\n\
    \\vinteger_val\CAN\b \SOH(\tH\NULR\n\
    \integerVal\DC2\GS\n\
    \\tbytes_val\CAN\t \SOH(\fH\NULR\bbytesValB\v\n\
    \\tprimitive\"9\n\
    \\fProductValue\DC2)\n\
    \\ACKfields\CAN\SOH \ETX(\v2\DC1.szt.DynamicValueR\ACKfields\"\139\SOH\n\
    \\bSumValue\DC2)\n\
    \\DLEconstructor_name\CAN\SOH \SOH(\tR\SIconstructorName\DC2'\n\
    \\ENQvalue\CAN\STX \SOH(\v2\DC1.szt.DynamicValueR\ENQvalue\DC2+\n\
    \\DC1constructor_index\CAN\ETX \SOH(\rR\DLEconstructorIndex\":\n\
    \\tListValue\DC2-\n\
    \\belements\CAN\SOH \ETX(\v2\DC1.szt.DynamicValueR\belements\"\v\n\
    \\tUnitValue\"\177\STX\n\
    \\bTypeInfo\DC2\ESC\n\
    \\ttype_name\CAN\SOH \SOH(\tR\btypeName\DC2\US\n\
    \\vmodule_name\CAN\STX \SOH(\tR\n\
    \moduleName\DC2\"\n\
    \\fconstructors\CAN\ETX \ETX(\tR\fconstructors\DC2!\n\
    \\ffield_labels\CAN\EOT \ETX(\tR\vfieldLabels\DC20\n\
    \\tstructure\CAN\ENQ \SOH(\v2\DC2.szt.TypeStructureR\tstructure\DC26\n\
    \\SItype_parameters\CAN\ACK \ETX(\v2\r.szt.TypeInfoR\SOtypeParameters\DC26\n\
    \\SInewtype_wrapper\CAN\a \SOH(\v2\r.szt.TypeInfoR\SOnewtypeWrapper\"\254\SOH\n\
    \\rTypeStructure\DC22\n\
    \\tprimitive\CAN\SOH \SOH(\SO2\DC2.szt.PrimitiveTypeH\NULR\tprimitive\DC21\n\
    \\aproduct\CAN\STX \SOH(\v2\NAK.szt.ProductStructureH\NULR\aproduct\DC2%\n\
    \\ETXsum\CAN\ETX \SOH(\v2\DC1.szt.SumStructureH\NULR\ETXsum\DC2(\n\
    \\EOTlist\CAN\EOT \SOH(\v2\DC2.szt.ListStructureH\NULR\EOTlist\DC2(\n\
    \\EOTunit\CAN\ENQ \SOH(\v2\DC2.szt.UnitStructureH\NULR\EOTunitB\v\n\
    \\tstructure\"\165\SOH\n\
    \\DLEProductStructure\DC27\n\
    \\ACKfields\CAN\SOH \ETX(\v2\US.szt.ProductStructure.FieldInfoR\ACKfields\SUBX\n\
    \\tFieldInfo\DC2\GS\n\
    \\n\
    \field_name\CAN\SOH \SOH(\tR\tfieldName\DC2,\n\
    \\n\
    \field_type\CAN\STX \SOH(\v2\r.szt.TypeInfoR\tfieldType\"J\n\
    \\fSumStructure\DC2:\n\
    \\DC1constructor_types\CAN\SOH \ETX(\v2\r.szt.TypeInfoR\DLEconstructorTypes\"A\n\
    \\rListStructure\DC20\n\
    \\felement_type\CAN\SOH \SOH(\v2\r.szt.TypeInfoR\velementType\"\SI\n\
    \\rUnitStructure*\205\SOH\n\
    \\rPrimitiveType\DC2\DC1\n\
    \\rPRIMITIVE_INT\DLE\NUL\DC2\DC4\n\
    \\DLEPRIMITIVE_DOUBLE\DLE\SOH\DC2\DC2\n\
    \\SOPRIMITIVE_TEXT\DLE\STX\DC2\DC2\n\
    \\SOPRIMITIVE_BOOL\DLE\ETX\DC2\DC4\n\
    \\DLEPRIMITIVE_WORD64\DLE\EOT\DC2\DC3\n\
    \\SIPRIMITIVE_INT32\DLE\ENQ\DC2\DC4\n\
    \\DLEPRIMITIVE_WORD32\DLE\ACK\DC2\NAK\n\
    \\DC1PRIMITIVE_INTEGER\DLE\a\DC2\DC3\n\
    \\SIPRIMITIVE_BYTES\DLE\bJ\147!\n\
    \\ACK\DC2\EOT\NUL\NUL~\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\SOH\NUL\f\n\
    \'\n\
    \\STX\EOT\NUL\DC2\EOT\EOT\NUL\t\SOH\SUB\ESC Top-level SZT file format\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\EOT\b\SI\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\ENQ\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\ENQ\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\ENQ\t\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\ENQ\SUB\ESC\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\ACK\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETX\ACK\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\ACK\SI\DC4\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\ACK\ETB\CAN\n\
    \\"\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETX\a\STX.\"\NAK Deduplication table\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETX\a\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETX\a\FS)\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETX\a,-\n\
    \#\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETX\b\STX-\"\SYN Shared type metadata\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ACK\DC2\ETX\b\STX\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETX\b\CAN(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETX\b+,\n\
    \/\n\
    \\STX\EOT\SOH\DC2\EOT\f\NUL\DLE\SOH\SUB# Core dynamic value representation\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\f\b\DC4\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\r\STX\ETB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX\r\STX\r\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\r\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\r\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\SO\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX\SO\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\SO\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\SO\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\SI\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX\SI\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\SI\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\SI\EM\SUB\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\DC2\NUL\ESC\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC2\b\DC3\n\
    \\f\n\
    \\EOT\EOT\STX\b\NUL\DC2\EOT\DC3\STX\SUB\ETX\n\
    \\f\n\
    \\ENQ\EOT\STX\b\NUL\SOH\DC2\ETX\DC3\b\f\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC4\EOT!\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\DC4\EOT\DC2\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC4\DC3\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC4\US \n\
    \\v\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETX\NAK\EOT\GS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETX\NAK\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETX\NAK\DC1\CAN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETX\NAK\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETX\SYN\EOT\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ACK\DC2\ETX\SYN\EOT\f\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETX\SYN\r\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETX\SYN\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\STX\STX\ETX\DC2\ETX\ETB\EOT\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ACK\DC2\ETX\ETB\EOT\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\SOH\DC2\ETX\ETB\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ETX\ETX\DC2\ETX\ETB\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\STX\STX\EOT\DC2\ETX\CAN\EOT\ETB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ACK\DC2\ETX\CAN\EOT\r\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\SOH\DC2\ETX\CAN\SO\DC2\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\EOT\ETX\DC2\ETX\CAN\NAK\SYN\n\
    \(\n\
    \\EOT\EOT\STX\STX\ENQ\DC2\ETX\EM\EOT!\"\ESC Reference to shared value\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\ACK\DC2\ETX\EM\EOT\DC2\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\SOH\DC2\ETX\EM\DC3\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\ENQ\ETX\DC2\ETX\EM\US \n\
    \)\n\
    \\STX\EOT\ETX\DC2\EOT\RS\NUL \SOH\SUB\GS Reference to a shared value\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\RS\b\SYN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\US\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\US\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\US\t\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\US\CAN\EM\n\
    \#\n\
    \\STX\EOT\EOT\DC2\EOT#\NUL2\SOH\SUB\ETB Primitive value types\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX#\b\SYN\n\
    \\f\n\
    \\EOT\EOT\EOT\b\NUL\DC2\EOT$\STX1\ETX\n\
    \\f\n\
    \\ENQ\EOT\EOT\b\NUL\SOH\DC2\ETX$\b\DC1\n\
    \\GS\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX%\EOT\SYN\"\DLE For Int, Int64\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX%\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX%\n\
    \\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX%\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX&\EOT\SUB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETX&\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX&\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX&\CAN\EM\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\STX\DC2\ETX'\EOT\CAN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ENQ\DC2\ETX'\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\ETX'\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\ETX'\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\ETX(\EOT\SYN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\ETX(\EOT\b\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\ETX(\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\ETX(\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\EOT\DC2\ETX)\EOT\SUB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ENQ\DC2\ETX)\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\SOH\DC2\ETX)\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\EOT\ETX\DC2\ETX)\CAN\EM\n\
    \\CAN\n\
    \\EOT\EOT\EOT\STX\ENQ\DC2\ETX*\EOT\CAN\"\v For Int32\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ENQ\DC2\ETX*\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\SOH\DC2\ETX*\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ENQ\ETX\DC2\ETX*\SYN\ETB\n\
    \\EM\n\
    \\EOT\EOT\EOT\STX\ACK\DC2\ETX+\EOT\SUB\"\f For Word32\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\ENQ\DC2\ETX+\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\SOH\DC2\ETX+\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\ACK\ETX\DC2\ETX+\CAN\EM\n\
    \.\n\
    \\EOT\EOT\EOT\STX\a\DC2\ETX,\EOT\ESC\"! For arbitrary precision Integer\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\ENQ\DC2\ETX,\EOT\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\SOH\DC2\ETX,\v\SYN\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\a\ETX\DC2\ETX,\EM\SUB\n\
    \\GS\n\
    \\EOT\EOT\EOT\STX\b\DC2\ETX-\EOT\CAN\"\DLE For ByteString\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\ENQ\DC2\ETX-\EOT\t\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\SOH\DC2\ETX-\n\
    \\DC3\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\b\ETX\DC2\ETX-\SYN\ETB\n\
    \-\n\
    \\STX\EOT\ENQ\DC2\EOT5\NUL7\SOH\SUB! Product types (records, tuples)\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX5\b\DC4\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX6\STX#\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX6\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX6\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX6\CAN\RS\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX6!\"\n\
    \&\n\
    \\STX\EOT\ACK\DC2\EOT:\NUL>\SOH\SUB\SUB Sum types (constructors)\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX:\b\DLE\n\
    \,\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX;\STX\RS\"\US Stable constructor identifier\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX;\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX;\t\EM\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX;\FS\GS\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\ETX<\STX\EM\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ACK\DC2\ETX<\STX\SO\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\ETX<\SI\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\ETX<\ETB\CAN\n\
    \4\n\
    \\EOT\EOT\ACK\STX\STX\DC2\ETX=\STX\US\"' Position within TypeInfo.constructors\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ENQ\DC2\ETX=\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\ETX=\t\SUB\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\ETX=\GS\RS\n\
    \\CAN\n\
    \\STX\EOT\a\DC2\EOTA\NULC\SOH\SUB\f List types\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETXA\b\DC1\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETXB\STX%\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\EOT\DC2\ETXB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ACK\DC2\ETXB\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETXB\CAN \n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETXB#$\n\
    \E\n\
    \\STX\EOT\b\DC2\EOTF\NULH\SOH\SUB  Unit type (empty constructors)\n\
    \\"\ETB Empty - just a marker\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETXF\b\DC1\n\
    \6\n\
    \\STX\EOT\t\DC2\EOTK\NULS\SOH\SUB* Type metadata for compatibility checking\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETXK\b\DLE\n\
    \\DC4\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETXL\STX\ETB\"\a \"Foo\"\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETXL\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETXL\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETXL\NAK\SYN\n\
    \\EM\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETXM\STX\EM\"\f \"MyModule\"\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\ETXM\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETXM\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETXM\ETB\CAN\n\
    \\RS\n\
    \\EOT\EOT\t\STX\STX\DC2\ETXN\STX#\"\DC1 [\"A\", \"B\", \"C\"]\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\EOT\DC2\ETXN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\ETXN\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\ETXN\DC2\RS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\ETXN!\"\n\
    \*\n\
    \\EOT\EOT\t\STX\ETX\DC2\ETXO\STX#\"\GS Record field names in order\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\EOT\DC2\ETXO\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\ENQ\DC2\ETXO\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\SOH\DC2\ETXO\DC2\RS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\ETX\DC2\ETXO!\"\n\
    \\v\n\
    \\EOT\EOT\t\STX\EOT\DC2\ETXP\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\ACK\DC2\ETXP\STX\SI\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\SOH\DC2\ETXP\DLE\EM\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\ETX\DC2\ETXP\FS\GS\n\
    \P\n\
    \\EOT\EOT\t\STX\ENQ\DC2\ETXQ\STX(\"C Type parameters (e.g., for \"Model t v\" applied to concrete types)\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\EOT\DC2\ETXQ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\ACK\DC2\ETXQ\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\SOH\DC2\ETXQ\DC4#\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\ETX\DC2\ETXQ&'\n\
    \G\n\
    \\EOT\EOT\t\STX\ACK\DC2\ETXR\STX\US\": For newtypes: the wrapped type (e.g., UserId wraps Text)\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\ACK\DC2\ETXR\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\SOH\DC2\ETXR\v\SUB\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\ETX\DC2\ETXR\GS\RS\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOTU\NUL]\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETXU\b\NAK\n\
    \\f\n\
    \\EOT\EOT\n\
    \\b\NUL\DC2\EOTV\STX\\\ETX\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\b\NUL\SOH\DC2\ETXV\b\DC1\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETXW\EOT \n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ACK\DC2\ETXW\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETXW\DC2\ESC\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETXW\RS\US\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\SOH\DC2\ETXX\EOT!\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ACK\DC2\ETXX\EOT\DC4\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\SOH\DC2\ETXX\NAK\FS\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\SOH\ETX\DC2\ETXX\US \n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\STX\DC2\ETXY\EOT\EM\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\STX\ACK\DC2\ETXY\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\STX\SOH\DC2\ETXY\DC1\DC4\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\STX\ETX\DC2\ETXY\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\ETX\DC2\ETXZ\EOT\ESC\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ACK\DC2\ETXZ\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\ETX\SOH\DC2\ETXZ\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\ETX\ETX\DC2\ETXZ\EM\SUB\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\EOT\DC2\ETX[\EOT\ESC\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ACK\DC2\ETX[\EOT\DC1\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\EOT\SOH\DC2\ETX[\DC2\SYN\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\EOT\ETX\DC2\ETX[\EM\SUB\n\
    \\n\
    \\n\
    \\STX\ENQ\NUL\DC2\EOT_\NULi\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETX_\ENQ\DC2\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX`\STX\DC4\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX`\STX\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX`\DC2\DC3\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\ETXa\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\ETXa\STX\DC2\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\ETXa\NAK\SYN\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\ETXb\STX\NAK\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\ETXb\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\ETXb\DC3\DC4\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\ETXc\STX\NAK\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\ETXc\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\ETXc\DC3\DC4\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\ETXd\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\ETXd\STX\DC2\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\ETXd\NAK\SYN\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ENQ\DC2\ETXe\STX\SYN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\SOH\DC2\ETXe\STX\DC1\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\STX\DC2\ETXe\DC4\NAK\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ACK\DC2\ETXf\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\SOH\DC2\ETXf\STX\DC2\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\STX\DC2\ETXf\NAK\SYN\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\a\DC2\ETXg\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\a\SOH\DC2\ETXg\STX\DC3\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\a\STX\DC2\ETXg\SYN\ETB\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\b\DC2\ETXh\STX\SYN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\b\SOH\DC2\ETXh\STX\DC1\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\b\STX\DC2\ETXh\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOTk\NULr\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXk\b\CAN\n\
    \\f\n\
    \\EOT\EOT\v\ETX\NUL\DC2\EOTl\STXo\ETX\n\
    \\f\n\
    \\ENQ\EOT\v\ETX\NUL\SOH\DC2\ETXl\n\
    \\DC3\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\NUL\DC2\ETXm\EOT\SUB\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\ENQ\DC2\ETXm\EOT\n\
    \\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\SOH\DC2\ETXm\v\NAK\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\NUL\ETX\DC2\ETXm\CAN\EM\n\
    \\r\n\
    \\ACK\EOT\v\ETX\NUL\STX\SOH\DC2\ETXn\EOT\FS\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\ACK\DC2\ETXn\EOT\f\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\SOH\DC2\ETXn\r\ETB\n\
    \\SO\n\
    \\a\EOT\v\ETX\NUL\STX\SOH\ETX\DC2\ETXn\SUB\ESC\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXq\STX \n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\EOT\DC2\ETXq\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETXq\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXq\NAK\ESC\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXq\RS\US\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOTt\NULv\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXt\b\DC4\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXu\STX*\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\EOT\DC2\ETXu\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXu\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXu\DC4%\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXu()\n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOTx\NULz\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXx\b\NAK\n\
    \\v\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXy\STX\FS\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ACK\DC2\ETXy\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXy\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXy\SUB\ESC\n\
    \\DC3\n\
    \\STX\EOT\SO\DC2\EOT|\NUL~\SOH\"\a Empty\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETX|\b\NAKb\ACKproto3"