module Type.Reflection
  ( TypeRep
  , class Typeable, typeRep, typeOf
  , Same, refl, same, cast, cast'
  , module Type.Equality
  , module Type.Proxy
  ) where


import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Maybe (Maybe(..))
import Data.Foldable (intercalate)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))

import Unsafe.Coerce (unsafeCoerce)



-- Type representations --------------------------------------------------------


-- | Non-type indexed representation of types.
data TypeRep
  = Argument TypeRep
  | NoArguments
  | Constructor String TypeRep
  | NoConstructors
  | Product TypeRep TypeRep
  | Sum TypeRep TypeRep
  | Boolean
  | Int
  | Number
  | Char
  | String
  | Array TypeRep
  | Function (Array TypeRep)

derive instance eqTypeRep :: Eq TypeRep
derive instance ordTypeRep :: Ord TypeRep


instance showTypeRep :: Show TypeRep where
  show (Argument inner)        = "(Argument " <> show inner <> ")"
  show NoArguments             = "NoArguments"
  show (Constructor name prod) = "(Constructor \"" <> name <> "\" " <> show prod <> ")"
  show NoConstructors          = "NoConstructors"
  show (Product a b)           = "(Product " <> show a <> " " <> show b <> ")"
  show (Sum a b)               = "(Sum " <> show a <> " " <> show b <> ")"
  show Boolean                 = "Boolean"
  show Int                     = "Int"
  show Number                  = "Number"
  show Char                    = "Char"
  show String                  = "String"
  show (Array inner)           = "(Array " <> show inner <> ")"
  show (Function args)         = "(" <> intercalate " -> " (map show args) <> ")"



-- Typeable --------------------------------------------------------------------


class Typeable a where
  typeRep :: Proxy a -> TypeRep


typeOf :: forall a. Typeable a => a -> TypeRep
typeOf _ = typeRep (Proxy :: Proxy a)


-- Basic types --

instance
  typeableBoolean :: Typeable Boolean
  where
    typeRep _ = Boolean

else instance
  typeableInt :: Typeable Int
  where
    typeRep _ = Int

else instance
  typeableNumber :: Typeable Number
  where
    typeRep _ = Number

else instance
  typeableChar :: Typeable Char
  where
    typeRep _ = Char

else instance
  typeableString :: Typeable String
  where
    typeRep _ = String

else instance
  typeableArray :: Typeable a => Typeable (Array a)
  where
    typeRep _ = Array (typeRep (Proxy :: Proxy a))

-- else instance
--   typeableFunction :: (Typeable a, Typeable b) => Typeable (Function a b)
--   where
--     typeRep _ = Function
--       (typeRep (Proxy :: Proxy a))
--       (typeRep (Proxy :: Proxy b))


-- Generic types --

else instance
  typeableArgument :: Typeable a => Typeable (Argument a)
  where
    typeRep _ = Argument (typeRep (Proxy :: Proxy a))

else instance
  typeableNoArguments :: Typeable NoArguments
  where
    typeRep _ = NoArguments

else instance
  typeableConstructor :: (IsSymbol n, Typeable a) => Typeable (Constructor n a)
  where
    typeRep _ = Constructor
      (reflectSymbol (SProxy :: SProxy n))
      (typeRep (Proxy :: Proxy a))

else instance
  typeableNoConstructors :: Typeable NoConstructors
  where
    typeRep _ = NoConstructors

else instance
  typeableSum :: (Typeable a, Typeable b) => Typeable (Sum a b)
  where
    typeRep _ = Sum
      (typeRep (Proxy :: Proxy a))
      (typeRep (Proxy :: Proxy b))

else instance
  typeableProduct :: (Typeable a, Typeable b) => Typeable (Product a b)
  where
    typeRep _ = Product
      (typeRep (Proxy :: Proxy a))
      (typeRep (Proxy :: Proxy b))


-- Dispatch --

-- | Note: any hand made instances in other modules will overlap with this one.
else instance
  typeableGeneric :: (Generic a r, Typeable r) => Typeable a
  where
    typeRep _ = typeRep (Proxy :: Proxy r)



-- Casts -----------------------------------------------------------------------


-- | `Refl` encodes a proof that type `a` equals type `b`,
-- | making use of the `TypeEquals a b` class.
-- | A type safe cast, for example, could be written like:
-- |
-- |     cast' :: forall a b. Typeable a => Typeable b => a -> Maybe b
-- |     cast' x
-- |       | Just (Refl proof) <- same (Proxy :: Proxy a) (Proxy :: Proxy b) =
-- |           proof \_ -> Just $ to x
-- |       | otherwise = Nothing
-- |
data Same a b
  = Refl (forall p. (TypeEquals a b => Unit -> p) -> p)


instance sameShow :: Show (Same a b) where
  show (Refl _) = "Refl"


refl :: forall a. TypeEquals a a => Same a a
refl = Refl \pack -> pack unit

-- foreign import refl' :: forall a. Same a a


-- | Calculates if two proxies `a` and `b` represent the same type.
-- | If they do, we give a proof in form of `Just Refl`,
-- | otherwise we return `Nothing`.
same :: forall a b. Typeable a => Typeable b => Proxy a -> Proxy b -> Maybe (Same a b)
same a b
  | typeRep a == typeRep b = Just $ unsafeCoerce refl
  | otherwise = Nothing


-- | Similar to `same`, but taking concrete values instead of proxies.
sameOf :: forall a b. Typeable a => Typeable b => a -> b -> Maybe (Same a b)
sameOf _ _ = same (Proxy :: Proxy a) (Proxy :: Proxy b)


cast' :: forall a b. Typeable a => Typeable b => a -> Maybe b
cast' x
  | Just (Refl proof) <- same (Proxy :: Proxy a) (Proxy :: Proxy b) =
      proof \_ -> Just $ to x
  | otherwise = Nothing


cast :: forall a b. Typeable a => Typeable b => a -> Maybe b
cast x
  | typeOf x == typeRep (Proxy :: Proxy b) = Just $ unsafeCoerce x
  | otherwise = Nothing
