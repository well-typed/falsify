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
  , alwaysPass
  , alwaysFail
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
  , pairwise
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
  Fail :: Predicate xs

  -- | Conjunction
  Both :: Predicate xs -> Predicate xs -> Predicate xs

  -- | Abstraction
  Lam :: (Input x -> Predicate xs) -> Predicate (x ': xs)

  -- | Partial application
  At :: Predicate (x : xs) -> Input x -> Predicate xs

  -- | Function compostion
  Dot :: Predicate (x' : xs) -> Fn x x' -> Predicate (x : xs)

  -- | Analogue of 'Control.Arrow.(***)'
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
alwaysPass :: Predicate xs
alwaysPass = Pass

-- | Constant 'False'
alwaysFail :: Predicate xs
alwaysFail = Fail

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
-- This is a variation on 'choose' that provides no evidence for which branch is
-- taken.
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
evalAt Fail               xs = Left $ Failure "Fail" (renderInputs xs)
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
(.$) :: Show x => Predicate (x : xs) -> (Var, x) -> Predicate xs
p .$ (n, x) = p `at` (Var n, show x, x)

-- | Generation of '(.$)' that does not require a 'Show' instance
at ::
     Predicate (x : xs)
  -> (Expr, String, x) -- ^ Renderded name, expression, and input proper
  -> Predicate xs
p `at` (e, r, x) = p `At` (Input e r x)

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
        `at` (Infix "!!" xs (Var $ show i), show x, x)
        `at` (Infix "!!" xs (Var $ show j), show y, y)

