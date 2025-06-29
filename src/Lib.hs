{-# LANGUAGE LambdaCase #-}

module Lib (brit, run) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.OldList (intercalate)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin)
import Prelude hiding (and)

data Parent = Mother | Father
  deriving (Show, Eq, Ord)

data Person = Applicant | Parent Parent Person
  deriving (Show, Eq, Ord)

data Predicate
  = IsBritish Person
  | Settled Person
  | BornBefore Int Person
  | BornInUK Person
  | BornAfter Int Person
  | Naturalised Person
  | Years3LivingInUK Person
  | IsBritOtbd Person
  | CrownService Person
  | Married Person Person
  deriving (Show, Eq, Ord)

data Document
  = BirthCertificate Person
  | MarriageCertificate Person Person
  | NaturalizationCertificate Person
  | Passport Person
  | SettledStatus Person
  | FCOLetter Person
  deriving (Show, Eq, Ord)

data Proof
  = -- Deriving britishness via a parent, and their proof of britishness
    ViaParent Person Proof
  | -- Britishness via two claims.
    And Proof Proof
  | -- Foundational evidence.
    Evidence Predicate
  deriving (Show)

data Knowledge = SureYes | SureNo | Unsure
  deriving (Eq)

type Claims = Map Predicate Knowledge

type M a = StateT Claims (LogicT IO) a

-- | Conditional logic: if condition succeeds, combine with action
ifThenElse :: (MonadLogic m) => m Proof -> m Proof -> m Proof -> m Proof
ifThenElse c a = ifte c (\x -> And x <$> a)

-- | Try first option, fall back to second if first fails
orElse :: (MonadLogic m) => m a -> m a -> m a
orElse a = ifte a pure

and :: M Proof -> M Proof -> M Proof
and a b = And <$> a <*> b

-- | Ask a question about a predicate. If the answer is `SureYes`, this is
-- recorded for future questions.
question :: Predicate -> M Knowledge
question q = do
  s <- get
  case Map.lookup q s of
    Just k -> pure k
    Nothing -> do
      l <- liftIO (putStrLn ("? : " <> disp q) >> getLine)
      let k = case l of
            "y" -> SureYes
            "n" -> SureNo
            _ -> Unsure
      modify (Map.insert q k)
      pure k

-- | Ask about a predicate, failing if the user says no.
check :: Predicate -> M ()
check q = do
  k <- question q
  guard (k == SureYes)

-- | Require a predicate to be true and return it as evidence
evidence :: Predicate -> M Proof
evidence p = check p >> pure (Evidence p)

-- | Is a person british?
brit :: Person -> M Proof
brit p =
  do
    check (IsBritish p)
    byBirth <|> naturalised
  where
    byBirth =
      ifThenElse
        (evidence (BornInUK p))
        (britBornInUk p)
        (britBornAbroad p)
    naturalised = evidence (Naturalised p)

-- | UK-born citizenship (pre-1983 or via parent)
britBornInUk :: Person -> M Proof
britBornInUk p = evidence (BornBefore 1983 p) `orElse` britBornInUkViaParent p

-- | UK-born citizenship via parent (used for post-1983 births)
britBornInUkViaParent :: Person -> M Proof
britBornInUkViaParent p = viaParent p (\parent -> brit parent `orElse` settled parent)

-- | Establish citizenship via a parent's status
--
-- Handles different rules for maternal vs paternal citizenship:
-- * Maternal: always available
-- * Paternal: automatic after 2006, requires marriage before 2006
viaParent :: Person -> (Person -> M Proof) -> M Proof
viaParent p cond = viaMother <|> viaFather
  where
    via parent = ViaParent (Parent parent p) <$> cond (Parent parent p)
    viaMother = via Mother
    viaFather =
      ifThenElse
        (evidence (BornAfter 2006 p))
        (via Father)
        (married (Parent Father p) (Parent Mother p) `and` via Father)

-- | British citizenship for those born abroad
britBornAbroad :: Person -> M Proof
britBornAbroad p =
  viaParent p $ \parent ->
    britOtbd parent `orElse` (brit parent `and` evidence (Years3LivingInUK parent))

-- | British otherwise than by descent (BOTD)
britOtbd :: Person -> M Proof
britOtbd p = do
  a <- question (IsBritOtbd p)
  guard (a /= SureNo)
  evidence (Naturalised p) `orElse` britOtbdUkBorn `orElse` bornCrownService p
  where
    britOtbdUkBorn = evidence (BornInUK p) `and` britBornInUk p

bornCrownService :: Person -> M Proof
bornCrownService p = viaParent p $ \parent -> do
  check (CrownService parent)
  brit parent `and` evidence (CrownService parent)

-- | Establish that two people were married at time of birth
married :: Person -> Person -> M Proof
married p q = evidence (Married p q)

-- | Establish that a person had settled status at time of birth
settled :: Person -> M Proof
settled p = evidence (Settled p)

-- | Run the citizenship determination and display results
--
-- Shows all possible proofs of citizenship and the documents required
-- for each proof, including unconditionally required documents.
run :: M Proof -> IO ()
run m = do
  hSetBuffering stdin LineBuffering
  res <- observeAllT (evalStateT m Map.empty)
  putStrLn $ "Applicant has " <> show (length res) <> " proof(s) of britishness:"
  forM_ res $ \r -> do
    putStrLn "\n-----------\n"
    putStrLn (disp r)
    putStrLn "Possible doc sets:"
    let docSets = nub (observeAll (docs r))
    forM_ docSets $ \ds -> putStrLn ("- " <> dispDocSet ds)
    putStrLn "\nThese documents are unconditionally required:"
  where
    -- putStrLn (dispDocSet (foldr1 Set.intersection docSets))

    dispDocSet ds = intercalate ", " (disp <$> Set.toList ds)

-- | Generate all possible document sets that could satisfy a proof
--
-- For each proof structure, determines what documents would be needed
-- to provide evidence for all the predicates involved.
docs :: Proof -> Logic (Set Document)
docs = \case
  ViaParent (Parent _ p) proof -> (<>) (Set.singleton (BirthCertificate p)) <$> docs proof
  ViaParent _ proof -> docs proof
  And proof1 proof2 -> do
    docs1 <- docs proof1
    docs2 <- docs proof2
    pure (docs1 <> docs2)
  Evidence predicate -> predicateDocs predicate
  where
    predicateDocs :: Predicate -> Logic (Set Document)
    predicateDocs = \case
      IsBritish _ -> mempty
      Settled p -> pure (Set.singleton (SettledStatus p))
      BornBefore _ p -> pure (Set.singleton (BirthCertificate p))
      BornInUK p -> pure (Set.singleton (BirthCertificate p))
      BornAfter _ p -> pure (Set.singleton (BirthCertificate p))
      Naturalised p -> pure (Set.singleton (NaturalizationCertificate p))
      Years3LivingInUK _ -> mempty
      IsBritOtbd _ -> mempty
      Married p1 p2 -> pure (Set.singleton (MarriageCertificate p1 p2))
      CrownService p -> pure (Set.singleton (FCOLetter p))

-- * Display

class Disp a where
  disp :: a -> String

instance Disp Parent where
  disp = show

instance Disp Person where
  disp Applicant = "Applicant"
  disp (Parent t p) = disp p <> "'s " <> disp t

instance Disp Predicate where
  disp = \case
    IsBritish p -> disp p <> " is british"
    Settled p -> disp p <> " was settled at time of birth"
    BornBefore year p -> disp p <> " was born before " <> show year
    BornInUK p -> disp p <> " was born in UK"
    BornAfter year p -> disp p <> " was born after " <> show year
    Naturalised p -> disp p <> " was naturalized"
    Years3LivingInUK p -> disp p <> " lived in UK for 3+ years"
    IsBritOtbd p -> disp p <> " is british otherwise than by descent"
    Married p1 p2 -> disp p1 <> " was married to " <> disp p2 <> " at time of birth"
    CrownService p -> disp p <> " was in crown service at time of birth"

instance Disp Document where
  disp = \case
    BirthCertificate p -> "Birth certificate for " <> disp p
    MarriageCertificate p1 p2 -> "Marriage certificate for " <> disp p1 <> " and " <> disp p2
    NaturalizationCertificate p -> "Naturalization certificate for " <> disp p
    Passport p -> "Passport for " <> disp p
    SettledStatus p -> "Settled status document for " <> disp p
    FCOLetter p -> "FCO Letter of crown service for " <> disp p

instance Disp Proof where
  disp = \case
    ViaParent parentType proof -> "Via " <> disp parentType <> "'s britishness:\n" <> indentLines "  " (disp proof)
    And proof1 proof2 ->
      "• "
        <> indentBulletLines (disp proof1)
        <> "• "
        <> indentBulletLines (disp proof2)
    Evidence predicate -> disp predicate
    where
      indentLines prefix text = unlines $ map (prefix <>) $ lines text
      indentBulletLines text =
        let textLines = lines text
         in case textLines of
              [] -> ""
              (firstLine : restLines) -> firstLine <> "\n" <> unlines (map ("  " <>) restLines)
