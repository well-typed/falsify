{-# LANGUAGE OverloadedStrings #-}

-- | Predicates
--
-- Intended for qualified import.
--
-- > import Test.Falsify
-- > import qualified Test.Falsify.Predicate as P
--
-- = Motivation
--
-- Testing libraries must have a way to assert and check intended-to-be-true
-- facts. For example, suppose we have
--
-- > x, y :: Int
-- > x = 5
-- > y = 10
--
-- and we want to assert that @x@ and @y@ are equal. The simplest form that this
-- might take is simply a boolean predicate; for example, @tasty-hunit@ offers
-- [@assertBool@](https://hackage-content.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html#v:assertBool),
-- and in @QuickCheck@ we have a
-- [@Testable@](https://hackage-content.haskell.org/package/QuickCheck-2.18.0.0/docs/Test-QuickCheck.html#t:Testable)
-- instance for 'Bool'. This allows us to write
--
-- > test_hunit_bool :: HUnit.Assertion
-- > test_hunit_bool = HUnit.assertBool "uhoh" $ x == y
-- >
-- > test_qc_bool :: QuickCheck.Property
-- > test_qc_bool = QuickCheck.property $ x == y
--
-- However, when such a property fails we don't get very useful output; we are
-- merely told /that/ the property failed. Both @tasty-hunit@ and @QuickCheck@
-- offer limited support for producing nicer test ouput; in the specific case of
-- equality, we can write
--
-- > test_hunit_equal :: HUnit.Assertion
-- > test_hunit_equal = HUnit.assertEqual "uhoh" x y
-- >
-- > test_qc_equal :: QuickCheck.Property
-- > test_qc_equal = QuickCheck.property $ x QuickCheck.=== y
--
-- instead, which would produce output
--
-- > uhoh
-- > expected: 5
-- >  but got: 10 (expected failure)
--
-- and
--
-- > *** Failed! Falsified (after 1 test):
-- > 5 /= 10
--
-- respectively, where we are not only told that the property failed, but also
-- /how/; in this case, what the values of @x@ and @y@ are. /Predicates/ in
-- @falsify@ as a generalization of this concept.
--
-- = Introduction to predicates
--
-- Think of a predicate of type 'Predicate' @'[a, b, ..]@ as a function
-- @a -> b -> .. -> Bool@, which can additionally produce useful test output
-- when the predicate does not hold. In order be able to produce that output, a
-- predicate is equipped with a function to generate a description of the
-- failure, given a description of the inputs; those inputs are described by
-- /expressions/ ('Expr'). For example, here is a very simple way in which we
-- might define a predicate to check that its argument is 'even':
--
-- > even1 :: Integral a => Predicate '[a]
-- > even1 = P.unary even $ \a -> "not even: " ++ P.prettyExpr a
--
-- When a predicate is applied to an argument, it must be told how to /name/
-- that argument, how to /render/ the argument, and the /value/ of the argument.
-- For example,
--
-- > test_even1 :: Property ()
-- > test_even1 = assert $ even1 `P.at` ("x", show x, x)
--
-- will result in
--
-- >  even1:   FAIL
-- >    failed after 0 shrinks
-- >    not even: x
-- >    x: 5
--
-- Typically we will use 'show' to render the argument, in which case we can
-- use '.$':
--
-- > test_even2 :: Property ()
-- > test_even2 = assert $ even1 .$ ("x", x)
--
-- This scales nicely to any number of arguments; for example, to come back the
-- equality example from the previous section:
--
-- > test_equal :: Property ()
-- > test_equal = assert $
-- >     P.eq .$ ("x", x)
-- >          .$ ("y", y)
--
-- will produce
--
-- > equal:   FAIL
-- >   failed after 0 shrinks
-- >   x /= y
-- >   x: 5
-- >   y: 10
--
-- = Compositionality
--
-- Suppose that we want to verify that @x@ and @y@ have the same polarity (both
-- are even or both are odd). We /could/ use 'eq' again:
--
-- > test_samePolarity1 :: Property ()
-- > test_samePolarity1 = assert $
-- >     P.eq .$ ("even x", even x)
-- >          .$ ("even y", even y)
--
-- but if we run that, we get
--
-- >  samePolarity1: FAIL
-- >    failed after 0 shrinks
-- >    even x /= even y
-- >    even x: False
-- >    even y: True
--
-- In order to debug the problem, we might like to the value of @x@ and @y@, not
-- just whether they are even or not. We could instead define a custom predicate
-- specifically for this purpose:
--
-- > samePolarity :: Integral a => Predicate '[a, a]
-- > samePolarity =
-- >     P.binary
-- >       (\a b -> even a == even b)
-- >       (\a b -> P.prettyExpr a ++ " and " ++ P.prettyExpr b ++ " have different polarity")
-- >
-- > test_samePolarity2 :: Property ()
-- > test_samePolarity2 = assert $ samePolarity .$ ("x", x) .$ ("y", y)
--
-- which would produce
--
-- > samePolarity2: FAIL
-- >   failed after 0 shrinks
-- >   x and y have different polarity
-- >   x: 5
-- >   y: 10
--
-- but now we have the opposite problem: we see the values of @x@ and @y@, but
-- not their polarity. Fortunately, we can take advantage of the
-- compositionality of predicates, and state very directly that the function
-- 'even', applied 'on' both arguments, must produce the same result:
--
-- > samePolarity' :: Integral a => Predicate '[a, a]
-- > samePolarity' = P.eq `P.on` P.fn ("even", even)
-- >
-- > test_samePolarity3 :: Property ()
-- > test_samePolarity3 = assert $
-- >     samePolarity'
-- >       .$ ("x", x)
-- >       .$ ("y", y)
--
-- produces
--
-- > samePolarity3: FAIL
-- >   failed after 0 shrinks
-- >   (even x) /= (even y)
-- >   x     : 5
-- >   y     : 10
-- >   even x: False
-- >   even y: True
--
-- Occassionally it is useful to suppress the results of functions applications;
-- for example, suppose we have
--
-- > newtype T = WrapT Int
-- >   deriving stock (Show)
-- >
-- > unwrapT :: T -> Int
-- > unwrapT (WrapT a) = a
--
-- and we want to check whether @X 5@ and @X 10@ have the same polarity; in this
-- case, there isn't much point explicitly including the output of @unwrapT@,
-- so we can suppress it with 'transparent':
--
-- > test_samePolarity4 :: Property ()
-- > test_samePolarity4 = assert $
-- >     samePolarity' `P.on` P.transparent unwrapT
-- >       .$ ("x", WrapT 5)
-- >       .$ ("y", WrapT 10)
--
-- = N-ary predicates
--
-- Most predicates are either 'unary' or 'binary'; these can usually easily be
-- defined using 'satisfies' and 'relatedBy' respectively, which will take care
-- of producing a nice error message. In the general case you can construct
-- predicates of arbitrary arity using 'lam', 'pass' and 'fail', though in that
-- case you will be responsible for constructing your own error messages.
--
-- For example, suppose we have a "real" implementation of some kind of security
-- policy implementation as well as a "model" implementation:
--
-- > applyReal, applyModel :: Policy -> Operation -> Resource -> Actor -> Bool
--
-- Then we could define a predicate that compares these two as follows:
--
-- > realVsModel :: Predicate '[Policy, Operation, Resource, Actor]
-- > realVsModel = P.lam $ \p -> P.lam $ \o -> P.lam $ \r -> P.lam $ \a ->
-- >     let real  = applyReal  p o r a
-- >         model = applyModel p o r a
-- >     in if real == model then
-- >          P.pass
-- >        else
-- >          P.fail $ "real says " ++ show real ++ ", model says " ++ show model
--
-- Such a predicate can then be used like any other, and rendering of the
-- arguments /to/ the predicate is handled automatically. For example:
--
-- > test_realVsModel :: Property ()
-- > test_realVsModel = assert $
-- >     realVsModel
-- >       .$ ( "policy"    , policy    )
-- >       .$ ( "operation" , operation )
-- >       .$ ( "resource"  , resource  )
-- >       .$ ( "actor"     , actor     )
--
-- (Usually of course these inputs would be randomly generated.) This property
-- might result in
--
-- > realVsModel: FAIL
-- >   failed after 0 shrinks
-- >   real says False, model says True
-- >   policy   : "strict"
-- >   operation: "delete"
-- >   resource : "db"
-- >   actor    : "joe"
module Test.Falsify.Predicate (
    Predicate -- opaque
    -- * Expressions
  , Expr -- opaque
  , prettyExpr
    -- * Functions
  , Fn     -- opaque
  , FnName -- opaque
  , fn
  , fnWith
  , transparent
    -- * Construction
  , pass
  , fail
  , unary
  , binary
    -- * Auxiliary construction
  , satisfies
  , relatedBy
    -- * Combinators
  , dot
  , split
  , on
  , flip
  , matchEither
  , matchBool
  , lam
    -- * Evaluation and partial evaluation
  , VarName -- opaque
  , Err
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
  , pairwise
  ) where

import Prelude hiding (all, flip, even, odd, pred, elem, fail)
import qualified Prelude

import Data.Bifunctor
import Data.Kind
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.SOP (NP(..), K(..), I(..), SListI)
import Data.String

import qualified Data.SOP as SOP

{-------------------------------------------------------------------------------
  Small expression language
-------------------------------------------------------------------------------}

-- | Variable
newtype VarName = WrapVarName{
      unwrapVarName :: String
    }
  deriving newtype (IsString)

-- | Simple expression language
--
-- The internal details of this type are (currently) not exposed.
data Expr =
    -- | Variable
    Var VarName

    -- | Function
    --
    -- We distinguish between v'Var' and v'Fn' only for improved readability.
  | Fn FnName

    -- | Application
  | App Expr Expr

    -- | Non-associative infix operator
  | Infix FnName Expr Expr

-- | Pretty-print expression
prettyExpr :: Expr -> String
prettyExpr = go False
  where
    go ::
         Bool -- Does the context require brackets?
      -> Expr -> String
    go needsBrackets = \case
        Var x          -> unwrapVarName x
        Fn f           -> unwrapFnName f
        App e1 e2      -> parensIf needsBrackets $ intercalate " " [
                              go False e1 -- application is left associative
                            , go True  e2
                            ]
        Infix op e1 e2 -> parensIf needsBrackets $ intercalate " " [
                              go True e1
                            , unwrapFnName op
                            , go True e2
                            ]

    parensIf :: Bool -> String -> String
    parensIf False = id
    parensIf True  = \s -> "(" ++ s ++ ")"

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- | Function name
newtype FnName = WrapFnName{
      unwrapFnName :: String
    }
  deriving newtype (IsString)

-- | Function (used for composition of a 'Predicate' with a function)
data Fn a b =
    -- | Function that is visible in rendered results
    Visible FnName (b -> String) (a -> b)

    -- | Function that should not be visible in rendered results
    --
    -- See 'transparent' for an example.
  | Transparent (a -> b)

-- | Default constructor for a function
fn :: Show b => (FnName, a -> b) -> Fn a b
fn (n, f) = fnWith (n, show, f)

-- | Generalization of 'fn' that does not depend on 'Show'
fnWith :: (FnName, b -> String, a -> b) -> Fn a b
fnWith (n, r, f) = Visible n r f

-- | Function that should not be visible in any rendered failure
--
-- Consider these two predicates:
--
-- > p1, p2 :: Predicate '[Char, Char]
-- > p1 = P.eq `P.on` (P.fn "ord"    ord)
-- > p2 = P.eq `P.on` (P.transparent ord)
--
-- Both of these compare two characters on their codepoints (through @ord@), but
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
          inputExpr     = App (Fn n) $ inputExpr x
        , inputRendered = r $ f (inputValue x)
        , inputValue    = f $ inputValue x
        }
    , Just $ renderInput x
    )
applyFn (Transparent f) x = (
      Input {
          inputExpr     = inputExpr x
        , inputRendered = inputRendered x
        , inputValue    = f $ inputValue x
        }
    , Nothing
    )

renderInput :: Input x -> (Expr, String)
renderInput x = (inputExpr x, inputRendered x)

renderInputs :: SListI xs => NP Input xs -> [(Expr, String)]
renderInputs xs = SOP.hcollapse $ SOP.hmap (K . renderInput) xs

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

  -- | Predicate that always fails
  Fail :: Err -> Predicate xs

  -- | Conjunction
  Both :: Predicate xs -> Predicate xs -> Predicate xs

  -- | Abstraction
  Lam :: (Input x -> Predicate xs) -> Predicate (x ': xs)

  -- | Partial application
  At :: Predicate (x : xs) -> Input x -> Predicate xs

  -- | Function compostion
  Dot :: Predicate (x' : xs) -> Fn x x' -> Predicate (x : xs)

  -- | Analogue of '(Control.Arrow.***)'
  Split :: Predicate (x' : y' : xs) -> (Fn x x', Fn y y') -> Predicate (x : y : xs)

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

-- | Constant 'True'
pass :: Predicate xs
pass = Pass

-- | Constant 'False'
fail :: Err -> Predicate xs
fail = Fail

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
satisfies :: (FnName, a -> Bool) -> Predicate '[a]
satisfies (n, p) =
    unary p $ \x ->
      prettyExpr $ Fn "not" `App` (Fn n `App` x)

-- | Specialization of 'binary' for relations
relatedBy :: (FnName, a -> b -> Bool) -> Predicate [a, b]
relatedBy (n, p) =
    binary p $ \x y ->
      prettyExpr $ Fn "not" `App` (Fn n `App` x `App` y)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

-- | Function composition (analogue of '(.)')
dot :: Predicate (x : xs) -> Fn y x -> Predicate (y : xs)
dot = Dot

-- | Analogue of 'Control.Arrow.(***)'
split ::
     Predicate (x' : y' : xs)
  -> (Fn x x', Fn y y')
  -> Predicate (x : y : xs)
split = Split

-- | Analogue of 'Prelude.on'
on :: Predicate (x : x : xs) -> Fn y x -> Predicate (y : y : xs)
on p f = p `split` (f, f)

-- | Analogue of 'Prelude.flip'
flip :: Predicate (x : y : zs) -> Predicate (y : x : zs)
flip = Flip

-- | Match on the argument, and apply whichever predicate is applicable.
matchEither ::
     Predicate (a : xs)
  -> Predicate (b : xs)
  -> Predicate (Either a b : xs)
matchEither = Choose

-- | Conditional
--
-- This is a variation on 'matchEither' that provides no evidence for which
-- branch is taken.
matchBool ::
     Predicate xs  -- ^ Predicate to evaluate if the condition is true
  -> Predicate xs  -- ^ Predicate to evaluate if the condition is false
  -> Predicate (Bool : xs)
matchBool t f =
    matchEither (Const t) (Const f) `dot` transparent fromBool
  where
    fromBool :: Bool -> Either () ()
    fromBool True  = Left  ()
    fromBool False = Right ()

-- | Lambda abstraction
--
-- See module documentation of "Test.Falsify.Predicate" for discussion.
lam :: (x -> Predicate xs) -> Predicate (x : xs)
lam k = Lam $ \Input{inputValue} -> k inputValue

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
   unlines $ failureErr : map (uncurry padInput) failureInputs
  where
    maxLabelLen :: Int
    maxLabelLen = maximum $ map (length . prettyExpr . fst) failureInputs

    padInput :: Expr -> String -> String
    padInput e v = padTo maxLabelLen (prettyExpr e) ++ ": " ++ v

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
      , failureInputs = renderInputs xs
      }

evalLam ::
     SListI xs
  => (Input x -> Predicate xs)
  -> NP Input (x : xs)
  -> Either Failure ()
evalLam f (x :* xs) =
    first (addInputs [renderInput x]) $
      evalAt (f x) xs

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

evalSplit ::
     SListI xs
  => Predicate (x' : y' : xs)
  -> (Fn x x', Fn y y')
  -> NP Input (x : y : xs)
  -> Either Failure ()
evalSplit p (f, g) (x :* y :* xs) =
    first (addInputs $ catMaybes [inp_x, inp_y]) $
      evalAt p (x' :* y' :* xs)
  where
    (x', inp_x) = applyFn f x
    (y', inp_y) = applyFn g y

evalChoice ::
     SListI xs
  => Predicate (a : xs)
  -> Predicate (b : xs)
  -> NP Input (Either a b : xs)
  -> Either Failure ()
evalChoice t f (x :* xs) =
    first (addInputs [renderInput x]) $
      case inputValue x of
        Left  a -> evalAt t (x{inputValue = a} :* xs)
        Right b -> evalAt f (x{inputValue = b} :* xs)

evalAt :: SListI xs => Predicate xs -> NP Input xs -> Either Failure ()
evalAt (Prim p err)       xs = evalPrim p err xs
evalAt Pass               _  = return ()
evalAt (Fail err)         xs = Left $ Failure err (renderInputs xs)
evalAt (Both p1 p2)       xs = evalAt p1 xs >> evalAt p2 xs
evalAt (Lam f)            xs = evalLam f xs
evalAt (p `At` x)         xs = evalAt p (x :* xs)
evalAt (p `Dot` f)        xs = evalDot p f xs
evalAt (p `Split` (f, g)) xs = evalSplit p (f, g) xs
evalAt (Flip p)           xs = let (x :* y :* zs) = xs in
                               evalAt p (y :* x :* zs)
evalAt (Choose l r)       xs = evalChoice l r xs
evalAt (Const p)          xs = evalAt p (SOP.tl xs)

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
-- > assert $
-- >      P.relatedBy ("equiv", equiv)
-- >   .$ ("x", x)
-- >   .$ ("y", y)
(.$) :: Show x => Predicate (x : xs) -> (VarName, x) -> Predicate xs
p .$ (n, x) = p `at` (n, show x, x)

-- | Generalization of '(.$)' that does not require a 'Show' instance
at ::
     Predicate (x : xs)
  -> (VarName, String, x) -- ^ Rendered name, expression, and input proper
  -> Predicate xs
p `at` (n, r, x) = p `atExpr` (Var n, r, x)

-- | Generalization of 'at' for an arbitrary 'Expr'
--
-- This is not currently part of the public API, since we haven't yet decided
-- how exactly we want to expose 'Expr' (if at all).
atExpr ::
     Predicate (x : xs)
  -> (Expr, String, x) -- ^ Rendered name, expression, and input proper
  -> Predicate xs
p `atExpr` (e, r, x) = p `At` (Input e r x)

{-------------------------------------------------------------------------------
  Specific predicates
-------------------------------------------------------------------------------}

-- | Construct predicate corresponding to some infix operator
--
-- This is an internal auxiliary.
binaryInfix ::
     FnName  -- ^ Infix operator corresponding to the relation /NOT/ holding
  -> (a -> b -> Bool) -> Predicate [a, b]
binaryInfix op f = binary f $ \x y -> prettyExpr (Infix op x y)

-- | Equal
eq :: Eq a => Predicate [a, a]
eq = binaryInfix "/=" (==)

-- | Not equal
ne :: Eq a => Predicate [a, a]
ne = binaryInfix "==" (/=)

-- | (Strictly) less than
lt :: Ord a => Predicate [a, a]
lt = binaryInfix ">=" (<)

-- | Less than or equal to
le :: Ord a => Predicate [a, a]
le = binaryInfix ">"  (<=)

-- | (Strictly) greater than
gt :: Ord a => Predicate [a, a]
gt = binaryInfix "<=" (>)

-- | Greater than or equal to
ge :: Ord a => Predicate [a, a]
ge = binaryInfix "<"  (>=)

-- | Check that values get closed to the specified target
towards :: forall a. (Show a, Ord a, Num a) => a -> Predicate [a, a]
towards = \target -> pred .$ ("target", target)
  where
    pred :: Predicate [a, a, a]
    pred = Lam $ \target ->
        ge `on` fn ("distanceToTarget", distanceTo (inputValue target))

    distanceTo :: a -> a -> a
    distanceTo target x
      | x <= target = target - x
      | otherwise   = x - target

-- | Specialization of 'eq', useful when expecting a specific value in a test
expect :: (Show a, Eq a) => a -> Predicate '[a]
expect x = eq .$ ("expected", x)

-- | Check that @lo <= x <= hi@
between :: (Show a, Ord a) => a -> a -> Predicate '[a]
between lo hi = mconcat [
           le .$ ("lo", lo)
    , flip le .$ ("hi", hi)
    ]

-- | Number is even
even :: Integral a => Predicate '[a]
even = satisfies ("even", Prelude.even)

-- | Number is odd
odd :: Integral a => Predicate '[a]
odd  = satisfies ("odd ", Prelude.odd)

-- | Membership check
elem :: Eq a => Predicate [[a], a]
elem = flip $ binaryInfix ("`notElem`") Prelude.elem

-- | Apply predicate to every pair of consecutive elements in the list
pairwise :: forall a. Show a => Predicate [a, a] -> Predicate '[[a]]
pairwise p = Lam $ \xs ->
    foldMap
      (uncurry $ pred (inputExpr xs))
      (pairs $ zip [0..] (inputValue xs))
  where
    pairs :: forall x. [x] -> [(x, x)]
    pairs []           = []
    pairs [_]          = []
    pairs (x : y : zs) = (x, y) : pairs (y : zs)

    pred :: Expr -> (Word, a) -> (Word, a) -> Predicate '[]
    pred xs (i, x) (j, y) =
                 p
        `atExpr` (Infix "!!" xs (Var $ WrapVarName $ show i), show x, x)
        `atExpr` (Infix "!!" xs (Var $ WrapVarName $ show j), show y, y)
