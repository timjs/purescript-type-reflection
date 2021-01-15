module Type.Reflection
  ( Reflection
  , class IsType
  , reflect
  , typeOf
  , Same
  , refl
  , decide
  , decideFrom
  , cast
  , module Type.Equality
  , module Type.Proxy
  ) where

import Prelude
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, NoConstructors, Product, Sum)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Equality (class TypeEquals, from, to)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---- Type representations --------------------------------------------------------
-- | Non-type indexed representation of types.
data Reflection
  = Argument Reflection
  | NoArguments
  | Constructor String Reflection
  | NoConstructors
  | Product Reflection Reflection
  | Sum Reflection Reflection
  | Boolean
  | Int
  | Number
  | Char
  | String
  | Array Reflection
  | Function Reflection Reflection

derive instance eqReflection :: Eq Reflection

derive instance ordReflection :: Ord Reflection

instance showReflection :: Show Reflection where
  show (Argument inner) = "(Argument " <> show inner <> ")"
  show NoArguments = "NoArguments"
  show (Constructor name prod) = "(Constructor \"" <> name <> "\" " <> show prod <> ")"
  show NoConstructors = "NoConstructors"
  show (Product a b) = "(Product " <> show a <> " " <> show b <> ")"
  show (Sum a b) = "(Sum " <> show a <> " " <> show b <> ")"
  show Boolean = "Boolean"
  show Int = "Int"
  show Number = "Number"
  show Char = "Char"
  show String = "String"
  show (Array inner) = "(Array " <> show inner <> ")"
  show (Function a b) = "(" <> show a <> " -> " <> show b <> ")"

---- IsType --------------------------------------------------------------------
class IsType a where
  reflect :: Proxy a -> Reflection

typeOf :: forall a. IsType a => a -> Reflection
typeOf _ = reflect (Proxy :: Proxy a)

---- Basic types ----
instance reflectBoolean ::
  IsType Boolean where
  reflect _ = Boolean
else instance reflectInt ::
  IsType Int where
  reflect _ = Int
else instance reflectNumber ::
  IsType Number where
  reflect _ = Number
else instance reflectChar ::
  IsType Char where
  reflect _ = Char
else instance reflectString ::
  IsType String where
  reflect _ = String
else instance reflectArray ::
  IsType a =>
  IsType (Array a) where
  reflect _ = Array (reflect (Proxy :: Proxy a))
else instance reflectFunction ::
  (IsType a, IsType b) =>
  IsType (Function a b) where
  reflect _ = Function (reflect (Proxy :: Proxy a)) (reflect (Proxy :: Proxy b))
---- Generic types ----
else instance reflectArgument ::
  IsType a =>
  IsType (Argument a) where
  reflect _ = Argument (reflect (Proxy :: Proxy a))
else instance reflectNoArguments ::
  IsType NoArguments where
  reflect _ = NoArguments
else instance reflectConstructor ::
  (IsSymbol s, IsType a) =>
  IsType (Constructor s a) where
  reflect _ = Constructor (reflectSymbol (SProxy :: SProxy s)) (reflect (Proxy :: Proxy a))
else instance reflectNoConstructors ::
  IsType NoConstructors where
  reflect _ = NoConstructors
else instance reflectSum ::
  (IsType a, IsType b) =>
  IsType (Sum a b) where
  reflect _ = Sum (reflect (Proxy :: Proxy a)) (reflect (Proxy :: Proxy b))
else instance reflectProduct ::
  (IsType a, IsType b) =>
  IsType (Product a b) where
  reflect _ = Product (reflect (Proxy :: Proxy a)) (reflect (Proxy :: Proxy b))
---- Dispatch ----
-- | Note: any hand made instances in other modules will overlap with this one.
else instance reflectGeneric ::
  (Generic a r, IsType r) =>
  IsType a where
  reflect _ = reflect (Proxy :: Proxy r)

---- Propositional equality ----------------------------------------------------
-- | If `Same a b` is inhabited by some terminating value, then the type `a` is the same as the type `b`.
-- | To use this equality in practice, pattern-match on the `Same a b` to get out the `Refl` constructor with a proof.
-- | When unpacking the proof, the compiler adds `TypeEquals a b` to the context.
data Same a b
  = Refl (forall p. (TypeEquals a b => Unit -> p) -> p)

instance sameShow :: Show (Same a b) where
  show (Refl _) = "Refl"

-- | Encodes a proof that type `a` is equal to itself using the `TypeEquals` class.
refl :: forall a. TypeEquals a a => Same a a
refl = Refl \pack -> pack unit

-- | Type-safe cast using propositional equality.
castWith :: forall a b. Same a b -> a -> b
castWith (Refl proof) x = proof \_ -> to x

---- Type safe casts -----------------------------------------------------------
-- | Decide at runtime if two types `a` and `b` are the same.
-- | If they do, we give a proof in form of `Just refl`,
-- | otherwise we return `Nothing`.
-- |
-- | A type safe cast, for example, could be written like:
-- |
-- |     cast :: forall a b. IsType a => IsType b => a -> Maybe b
-- |     cast x
-- |       | Just proof <- decide :: Maybe (Same a b) = Just $ castWith proof x
-- |       | otherwise = Nothing
-- |
decide :: forall a b. IsType a => IsType b => Maybe (Same a b)
decide
  | reflect (Proxy :: Proxy a) == reflect (Proxy :: Proxy b) = Just $ unsafeCoerce refl
  | otherwise = Nothing

-- | Similar to `decide`, but uses concrete values to determine the type variables.
decideFrom :: forall a b. IsType a => IsType b => a -> b -> Maybe (Same a b)
decideFrom _ _ = decide

-- | Type safe cast from type `a` to type `b` using IsType information.
cast :: forall a b. IsType a => IsType b => a -> Maybe b
cast x
  | Just proof <- (decide :: Maybe (Same a b)) = Just $ castWith proof x
  | otherwise = Nothing

-- | (Faster?) alternative to `cast`, using an unsafe coerce internally.
cast' :: forall a b. IsType a => IsType b => a -> Maybe b
cast' x
  | typeOf x == reflect (Proxy :: Proxy b) = Just $ unsafeCoerce x
  | otherwise = Nothing
