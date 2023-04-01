-- | Predicates
--
-- Intended for qualified import.

-- > import Test.Falsify.Predicate (Predicate, (.$))
-- > import qualified Test.Falsify.Predicate as P
module Test.Falsify.Predicate (
    Predicate -- opaque
    -- * Expressions
  , Expr -- opaque
  , prettyExpr
    -- * Functions
  , Fn -- opaque
  , fn
  , fnWith
  , transparent
    -- * Construction
  , unary
  , binary
    -- * Auxiliary construction
  , satisfies
  , relatedBy
    -- * Combinators
  , dot
  , on
  , flip
  , choose
  , choose_
    -- * Evaluation and partial evaluation
  , eval
  , (.$)
  , at
    -- * Specific predicates
  , eq
  , ne
  , lt
  , le
  , gt
  , ge
  , towards
  , expect
  , between
  , even
  , odd
  , elem
  ) where

import Prelude hiding (all, flip, even, odd, pred, elem)
import qualified Prelude

import Data.Bifunctor
import Data.Kind
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.SOP (NP(..), K(..), I(..), SListI)

import qualified Data.SOP as SOP

{-------------------------------------------------------------------------------
  Small expression language
-------------------------------------------------------------------------------}

-- | Variable
type Var = String

-- | Simple expression language
--
-- The internal details of this type are (currently) not exposed.
data Expr =
    -- | Variable
    Var Var

    -- | Application
  | App Expr Expr

    -- | Non-associative infix operator
  | Infix Var Expr Expr

prettyExpr :: Expr -> String
prettyExpr = go False
  where
    go ::
         Bool -- Does the context require brackets?
      -> Expr -> String
    go needsBrackets = \case
        Var x          -> x
        App e1 e2      -> parensIf needsBrackets $ intercalate " " [
                              go False e1 -- application is left associative
                            , go True  e2
                            ]
        Infix op e1 e2 -> parensIf needsBrackets $ intercalate " " [
                              go True e1
                            , op
                            , go True e2
                            ]

    parensIf :: Bool -> String -> String
    parensIf False = id
    parensIf True  = \s -> "(" ++ s ++ ")"

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- | Function (used for composition of a 'Predicate' with a function)
data Fn a b =
    -- | Function that is visible in rendered results
    Visible Var (b -> String) (a -> b)

    -- | Function that should not be visible in rendered results
    --
    -- See 'transparent' for an example.
  | Transparent (a -> b)

-- | Default constructor for a function
fn :: Show b => (Var, a -> b) -> Fn a b
fn (n, f) = fnWith (n, show, f)

-- | Generalization of 'fn' that does not depend on 'Show'
fnWith :: (Var, b -> String, a -> b) -> Fn a b
fnWith (n, r, f) = Visible n r f

-- | Function that should not be visible in any rendered failure
--
-- Consider these two predicates:
--
-- > p1, p2 :: Predicate '[Char, Char]
-- > p1 = P.eq `P.on` (P.fn "ord"    ord)
-- > p2 = P.eq `P.on` (P.transparent ord)
--
-- Both of these compare two characters on their codepoints (through 'ord'), but
-- they result in different failures. The first would give us something like
--
-- > (ord x) /= (ord y)
-- > x    : 'a'
-- > y    : 'b'
-- > ord x: 97
-- > ord y: 98
--
-- whereas the second might give us something like
--
-- > x /= y
-- > x: 'a'
-- > y: 'b'
--
-- which of these is more useful is of course application dependent.
transparent :: (a -> b) -> Fn a b
transparent = Transparent

{-------------------------------------------------------------------------------
  Decorated predicate inputs

  This is internal API.
-------------------------------------------------------------------------------}

-- | Input to a 'Predicate'
data Input x = Input {
      -- | Expression describing the input
      inputExpr :: Expr

      -- | Rendered value of the input
    , inputRendered :: String

      -- | The input proper
    , inputValue :: x
    }

-- | Apply function to an argument
--
-- If the funciton is visible, we also return the /input/ to the function
-- (so that we can render both the input and the output); we return 'Nothing'
-- for transparent functions.
applyFn :: Fn a b -> Input a -> (Input b, Maybe (Expr, String))
applyFn (Visible n r f) x = (
      Input {
          inputExpr     = App (Var n) $ inputExpr x
        , inputRendered = r $ f (inputValue x)
        , inputValue    = f $ inputValue x
        }
    , Just $ (inputExpr x, inputRendered x)
    )
applyFn (Transparent f) x = (
      Input {
          inputExpr     = inputExpr x
        , inputRendered = inputRendered x
        , inputValue    = f $ inputValue x
        }
    , Nothing
    )

{-------------------------------------------------------------------------------
  Definition

  'Predicate' is a relatively deep embedding, so that we can provide more
  powerful combinators.
-------------------------------------------------------------------------------}

-- | Error message (when the predicate fails)
type Err = String

-- | N-ary predicate
--
-- A predicate of type
--
-- > Predicate '[Int, Bool, Char, ..]
--
-- is essentially a function @Int -> Bool -> Char -> .. -> Bool@, along with
-- some metadata about that function that allows us to render it in a human
-- readable way. In particular, we construct an 'Expr' for the values that the
-- predicate has been applied to.
data Predicate :: [Type] -> Type where
  -- | Primitive generator
  Prim :: (NP I xs -> Bool) -> (NP (K Expr) xs -> Err) -> Predicate xs

  -- | Predicate that always passes
  Pass :: Predicate xs

  -- | Conjunction
  Both :: Predicate xs -> Predicate xs -> Predicate xs

  -- | Abstraction
  Lam :: (x -> Predicate xs) -> Predicate (x ': xs)

  -- | Partial application
  At :: Predicate (x : xs) -> Input x -> Predicate xs

  -- | Function compostion
  Dot :: Predicate (x : xs) -> Fn y x -> Predicate (y : xs)

  -- | Analogue of 'Prelude.on'
  On :: Predicate (x : x : xs) -> Fn y x -> Predicate (y : y : xs)

  -- | Analogue of 'Prelude.flip'
  Flip :: Predicate (x : y : zs) -> Predicate (y : x : zs)

  -- | Choice
  Choose ::
       Predicate (       a   : xs)
    -> Predicate (         b : xs)
    -> Predicate (Either a b : xs)

  -- | Predicate that ignores its argument
  Const :: Predicate xs -> Predicate (x ': xs)

instance Monoid    (Predicate a) where mempty = Pass
instance Semigroup (Predicate a) where (<>)   = Both

-- | Primitive way to construct a predicate
--
-- This is (currently) not part of the public API.
prim ::
     (NP I xs -> Bool)
     -- ^ Predicate to check
  -> (NP (K Expr) xs -> Err)
     -- ^ Produce error message, given the expressions describing the inputs
  -> Predicate xs
prim = Prim

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Unary predicate
--
-- This is essentially a function @a -> Bool@; see 'Predicate' for detailed
-- discussion.
unary ::
     (a -> Bool)    -- ^ The predicate proper
  -> (Expr -> Err)  -- ^ Error message, given 'Expr' describing the input
  -> Predicate '[a]
unary p msg =
    prim
      (\(I x :* Nil) -> p   x)
      (\(K l :* Nil) -> msg l)

-- | Binary predicate
--
-- This is essentially a function @a -> b -> Bool@; see 'Predicate' for detailed
-- discussion.
binary ::
     (a -> b -> Bool)       -- ^ The predicate proper
  -> (Expr -> Expr -> Err)  -- ^ Error message, given 'Expr' describing inputs
  -> Predicate [a, b]
binary p msg =
    prim
      (\(I  x :* I  y :* Nil) -> p    x  y)
      (\(K lx :* K ly :* Nil) -> msg lx ly)

{-------------------------------------------------------------------------------
  Auxiliary construction
-------------------------------------------------------------------------------}

-- | Specialization of 'unary' for unary relations
satisfies :: (Var, a -> Bool) -> Predicate '[a]
satisfies (n, p) =
    unary p $ \x ->
      prettyExpr $ Var "not" `App` (Var n `App` x)

-- | Specialization of 'binary' for relations
relatedBy :: (Var, a -> b -> Bool) -> Predicate [a, b]
relatedBy (n, p) =
    binary p $ \x y ->
      prettyExpr $ Var "not" `App` (Var n `App` x `App` y)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Function composition
dot :: Predicate (x : xs) -> Fn y x -> Predicate (y : xs)
dot = Dot

-- | Analogue of 'Prelude.on'
on :: Predicate (x : x : xs) -> Fn y x -> Predicate (y : y : xs)
on = On

-- | Analogue of 'Prelude.flip
flip :: Predicate (x : y : zs) -> Predicate (y : x : zs)
flip = Flip

-- | Choose between two predicates
choose ::
     Predicate (a : xs)
  -> Predicate (b : xs)
  -> Predicate (Either a b : xs)
choose = Choose

-- | Conditional
--
-- This is a variation on 'choose' that provides no evidence for which branch is
-- taken.
choose_ ::
     Predicate xs  -- ^ Predicate to evaluate if the condition is true
  -> Predicate xs  -- ^ Predicate to evaluate if the condition is false
  -> Predicate (Bool : xs)
choose_ t f =
    choose (Const t) (Const f) `dot` transparent fromBool
  where
    fromBool :: Bool -> Either () ()
    fromBool True  = Left  ()
    fromBool False = Right ()

{-------------------------------------------------------------------------------
  Failures
-------------------------------------------------------------------------------}

data Failure = Failure {
      failureErr    :: Err
    , failureInputs :: [(Expr, String)]
    }

addInputs :: [(Expr, String)] -> Failure -> Failure
addInputs new Failure{failureErr, failureInputs} = Failure{
      failureErr
    , failureInputs = new ++ failureInputs
    }

prettyFailure :: Failure -> String
prettyFailure Failure{failureErr, failureInputs} =
   unlines $ failureErr : map (uncurry renderInput) failureInputs
  where
    maxLabelLen :: Int
    maxLabelLen = maximum $ map (length . prettyExpr . fst) failureInputs

    renderInput :: Expr -> String -> String
    renderInput e v = padTo maxLabelLen (prettyExpr e) ++ ": " ++ v

    padTo :: Int -> String -> String
    padTo n xs = xs ++ replicate (n - length xs) ' '

{-------------------------------------------------------------------------------
  Generalized evaluation

  This is internal API. Only the top-level 'eval' is exported.
-------------------------------------------------------------------------------}

evalPrim ::
     SListI xs
  => (NP I xs -> Bool)
  -> (NP (K Expr) xs -> Err)
  -> NP Input xs
  -> Either Failure ()
evalPrim p err xs
  | p (SOP.hmap (I . inputValue) xs)
  = Right ()

  | otherwise
  = Left Failure {
        failureErr    = err $ SOP.hmap (K . inputExpr) xs
      , failureInputs = SOP.hcollapse $
                          SOP.hmap (\i -> K (inputExpr i, inputRendered i)) xs
      }

evalLam ::
     SListI xs
  => (x -> Predicate xs)
  -> NP Input (x : xs)
  -> Either Failure ()
evalLam f (x :* xs) =
    first (addInputs [(inputExpr x, inputRendered x)]) $
      evalAt (f $ inputValue x) xs

evalDot ::
     SListI xs
  => Predicate (x : xs)
  -> Fn y x
  -> NP Input (y : xs)
  -> Either Failure ()
evalDot p f (x :* xs) =
    first (addInputs $ catMaybes [x']) $
      evalAt p (y :* xs)
  where
    (y, x') = applyFn f x

evalOn ::
     SListI xs
  => Predicate (x : x : xs)
  -> Fn y x
  -> NP Input (y : y : xs)
  -> Either Failure ()
evalOn p f (x0 :* x1 :* xs) =
    first (addInputs $ catMaybes [x0', x1']) $
      evalAt p (y0 :* y1 :* xs)
  where
    (y0, x0') = applyFn f x0
    (y1, x1') = applyFn f x1

evalChoice ::
     SListI xs
  => Predicate (a : xs)
  -> Predicate (b : xs)
  -> NP Input (Either a b : xs)
  -> Either Failure ()
evalChoice t f (x :* xs) =
    first (addInputs [(inputExpr x, inputRendered x)]) $
      case inputValue x of
        Left  a -> evalAt t (x{inputValue = a} :* xs)
        Right b -> evalAt f (x{inputValue = b} :* xs)

evalAt :: SListI xs => Predicate xs -> NP Input xs -> Either Failure ()
evalAt (Prim p err) xs = evalPrim p err xs
evalAt Pass         _  = return ()
evalAt (Both p1 p2) xs = evalAt p1 xs >> evalAt p2 xs
evalAt (Lam f)      xs = evalLam f xs
evalAt (p `At` x)   xs = evalAt p (x :* xs)
evalAt (p `Dot` f)  xs = evalDot p f xs
evalAt (p `On` f)   xs = evalOn  p f xs
evalAt (Flip p)     xs = let (x :* y :* zs) = xs in evalAt p (y :* x :* zs)
evalAt (Choose l r) xs = evalChoice l r xs
evalAt (Const p)    xs = evalAt p (SOP.tl xs)

{-------------------------------------------------------------------------------
  Evaluation and partial evaluation
-------------------------------------------------------------------------------}

-- | Evaluate fully applied predicate
eval :: Predicate '[] -> Either Err ()
eval p = first prettyFailure $ evalAt p Nil

-- | Infix version of 'at'
--
-- Typical usage example:
--
-- > assert $ P.relatedBy equiv "equiv"
-- >   .$ ("x", x)
-- >   .$ ("y", y)
(.$) :: Show x => Predicate (x : xs) -> (Var, x) -> Predicate xs
p .$ (n, x) = p `at` (n, show x, x)

-- | Generation of '(.$)' that does not require a 'Show' instance
at ::
     Predicate (x : xs)
  -> (Var, String, x) -- ^ Renderded name, name for the input, and input proper
  -> Predicate xs
p `at` (n, r, x) = p `At` (Input (Var n) r x)

{-------------------------------------------------------------------------------
  Specific predicates
-------------------------------------------------------------------------------}

-- | Construct predicate corresponding to some infix operator
--
-- This is an internal auxiliary.
binaryInfix ::
     Var  -- ^ Infix operator corresponding to the relation /NOT/ holding
  -> (a -> b -> Bool) -> Predicate [a, b]
binaryInfix op f = binary f $ \x y -> prettyExpr (Infix op x y)

eq, ne :: Eq a => Predicate [a, a]
eq = binaryInfix "/=" (==)
ne = binaryInfix "==" (/=)

lt, le, gt, ge :: Ord a => Predicate [a, a]
lt = binaryInfix ">=" (<)
le = binaryInfix ">"  (<=)
gt = binaryInfix "<=" (>)
ge = binaryInfix "<"  (>=)

-- | Check that values get closed to the specified target
towards :: forall a. (Show a, Ord a, Num a) => a -> Predicate [a, a]
towards = \target -> pred .$ ("target", target)
  where
    pred :: Predicate [a, a, a]
    pred = Lam (\target -> ge `on` fn ("distanceToTarget", distanceTo target))

    distanceTo :: a -> a -> a
    distanceTo target x
      | x <= target = target - x
      | otherwise   = x - target

expect :: (Show a, Eq a) => a -> Predicate '[a]
expect x = eq .$ ("expected", x)

between :: (Show a, Ord a) => a -> a -> Predicate '[a]
between lo hi = mconcat [
           le .$ ("lo", lo)
    , flip le .$ ("hi", hi)
    ]

even, odd :: Integral a => Predicate '[a]
even = satisfies ("even", Prelude.even)
odd  = satisfies ("odd ", Prelude.odd)

elem :: Eq a => Predicate '[[a], a]
elem = flip $ binaryInfix ("`notElem`") Prelude.elem