{-# LANGUAGE LambdaCase #-}

-- | British citizenship determination system
--
-- This module implements the logic for determining British citizenship eligibility
-- based on UK nationality law. It handles various scenarios including:
--
-- * Birth in the UK before/after 1983
-- * Birth abroad with British parents
-- * Naturalization
-- * Settlement status
-- * Parental citizenship transmission
--
-- The system generates proofs of citizenship and determines what documents
-- are required to support each proof.
module Lib (brit, run) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.List (nub)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.OldList (intercalate)

class Disp a where
  disp :: a -> String

data ParentType = Mother | Father
  deriving (Show, Eq, Ord)

instance Disp ParentType where
  disp = show

data Person = Applicant | Parent ParentType Person
  deriving (Show, Eq, Ord)

instance Disp Person where
  disp Applicant = "Applicant"
  disp (Parent t p) = disp p <> "'s " <> disp t

data Predicate
  = IsBritish Person
  | Settled Person
  | BornBefore Int Person
  | BornInUK Person
  | BornAfter Int Person
  | Naturalized Person
  | Years3LivingInUK Person
  | IsBritOtbd Person
  | Married Person Person
  deriving (Show, Eq, Ord)

instance Disp Predicate where
  disp = \case
    IsBritish p -> disp p <> " is british"
    Settled p -> disp p <> " was settled at time of birth"
    BornBefore year p -> disp p <> " was born before " <> show year
    BornInUK p -> disp p <> " was born in UK"
    BornAfter year p -> disp p <> " was born after " <> show year
    Naturalized p -> disp p <> " was naturalized"
    Years3LivingInUK p -> disp p <> " lived in UK for 3+ years"
    IsBritOtbd p -> disp p <> " is british otherwise than by descent"
    Married p1 p2 -> disp p1 <> " was married to " <> disp p2 <> " at time of birth"

data DocumentType
  = BirthCertificate Person
  | MarriageCertificate Person Person
  | NaturalizationCertificate Person
  | Passport Person
  | SettledStatus Person
  deriving (Show, Eq, Ord)

instance Disp DocumentType where
  disp = \case
    BirthCertificate p -> "Birth certificate for " <> disp p
    MarriageCertificate p1 p2 -> "Marriage certificate for " <> disp p1 <> " and " <> disp p2
    NaturalizationCertificate p -> "Naturalization certificate for " <> disp p
    Passport p -> "Passport for " <> disp p
    SettledStatus p -> "Settled status document for " <> disp p

data Proof
  = ViaParent Person Proof
  | And Proof Proof
  | Evidence Predicate
  deriving (Show)

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

type Claims = Set Predicate

-- | Monad for citizenship determination with backtracking and state
type M a = LogicT (StateT Claims IO) a

-- | Response to a question about a predicate
data Knowledge = SureYes | SureNo | Unsure

-- | Ask a question about a predicate and get user input
ask :: Predicate -> M Knowledge
ask q = do
  s <- get
  if q `elem` s
    then pure SureYes
    else do
      l <- liftIO $ do
        putStrLn ("? : " <> disp q)
        getLine
      case l of
        "y" -> do
          modify (Set.insert q)
          pure SureYes
        "n" -> pure SureNo
        _ -> pure Unsure

-- | Check that a predicate is true, failing if the user says no
check :: Predicate -> M ()
check q = do
  s <- get
  if q `elem` s
    then pure ()
    else do
      l <- liftIO $ do
        putStrLn ("? : " <> disp q)
        getLine
      guard (l == "y")
      modify (Set.insert q)

-- | Determine if a person is British and generate proof
-- 
-- This implements the main logic of British citizenship law:
-- 1. If born in UK, check if before 1983 (automatic) or via parent
-- 2. If born abroad, check parental citizenship or naturalization
brit :: Person -> M Proof
brit p =
  do
    check (IsBritish p)
    cut
      (askRequired (BornInUK p))
      (britBornInUk p)
      (britBornAbroad p)

-- | Logic for UK-born citizenship (post-1983 or via parent)
britBornInUk :: Person -> M Proof  
britBornInUk p = askRequired (BornBefore 1983 p) `orElse` britBornInUkViaParent p

-- | UK-born citizenship via parent (used for post-1983 births)
britBornInUkViaParent :: Person -> M Proof
britBornInUkViaParent p = viaParent p (\parent -> brit parent `orElse` settled parent)

-- | Conditional logic: if condition succeeds, combine with action
cut :: (MonadLogic m) => m Proof -> m Proof -> m Proof -> m Proof
cut c a = ifte c (\x -> And x <$> a)

-- | Try first option, fall back to second if first fails
orElse :: (MonadLogic m) => m a -> m a -> m a
orElse a = ifte a pure

-- | Require a predicate to be true and return it as evidence
askRequired :: Predicate -> M Proof
askRequired p = check p >> pure (Evidence p)

-- | Establish citizenship via a parent's status
-- 
-- Handles different rules for maternal vs paternal citizenship:
-- * Maternal: always available
-- * Paternal: automatic after 2006, requires marriage before 2006
viaParent :: Person -> (Person -> M Proof) -> M Proof
viaParent p cond =
  go Mother
    <|> cut
      (askRequired (BornAfter 2006 p))
      (go Father)
      (And <$> married (Parent Father p) (Parent Mother p) <*> go Father)
  where
    go parent = do
      ViaParent (Parent parent p) <$> cond (Parent parent p)

-- | British citizenship for those born abroad
--
-- Two main routes:
-- 1. Parent is British otherwise than by descent (BOTD)
-- 2. Parent is British and lived in UK for 3+ years
britBornAbroad :: Person -> M Proof
britBornAbroad p =
  viaParent p britOtbd
    `orElse` viaParent p (\parent -> brit parent >> askRequired (Years3LivingInUK parent))

-- | British otherwise than by descent (BOTD)
--
-- Someone is BOTD if they are:
-- 1. Born in UK (with same rules as main citizenship)
-- 2. Naturalized as British citizen
britOtbd :: Person -> M Proof
britOtbd p =
  do
    r <- ask (IsBritOtbd p)
    case r of
      SureNo -> mempty
      _ -> britOtbdUkBorn <|> askRequired (Naturalized p)
  where
    -- Reuse the same UK-born logic as the main brit function
    britOtbdUkBorn = And <$> askRequired (BornInUK p) <*> britBornInUk p

-- | Establish that two people were married at time of birth
married :: Person -> Person -> LogicT (StateT Claims IO) Proof
married p q = askRequired (Married p q)

-- | Establish that a person had settled status at time of birth
settled :: Person -> M Proof
settled p = askRequired (Settled p)

-- | Run the citizenship determination and display results
--
-- Shows all possible proofs of citizenship and the documents required
-- for each proof, including unconditionally required documents.
run :: M Proof -> IO ()
run m = do
  res <- evalStateT (observeAllT m) Set.empty
  putStrLn $ "Applicant has " <> show (length res) <> " proof(s) of britishness:"
  forM_ res $ \r -> do
    putStrLn "\n-----------\n"
    putStrLn (disp r)
    putStrLn "Possible doc sets:"
    let docSets = nub (observeAll (docs r))
    forM_ docSets $ \ds -> putStrLn ("- " <> dispDocSet ds)
    putStrLn "\nThese documents are unconditionally required:"
    putStrLn (dispDocSet (foldr1 Set.intersection docSets))
  where
    dispDocSet ds = intercalate ", " (disp <$> Set.toList ds)

-- | Generate all possible document sets that could satisfy a proof
--
-- For each proof structure, determines what documents would be needed
-- to provide evidence for all the predicates involved.
docs :: Proof -> Logic (Set DocumentType)
docs = \case
  ViaParent (Parent _ p) proof -> (<>) (Set.singleton (BirthCertificate p)) <$> docs proof
  ViaParent _ proof -> docs proof
  And proof1 proof2 -> do
    docs1 <- docs proof1
    docs2 <- docs proof2
    pure (docs1 <> docs2)
  Evidence predicate -> predicateDocs predicate
  where
    predicateDocs :: Predicate -> Logic (Set DocumentType)
    predicateDocs = \case
      IsBritish _ -> mempty
      Settled p -> pure (Set.singleton (SettledStatus p))
      BornBefore _ p -> pure (Set.singleton (BirthCertificate p))
      BornInUK p -> pure (Set.singleton (BirthCertificate p))
      BornAfter _ p -> pure (Set.singleton (BirthCertificate p))
      Naturalized p -> pure (Set.singleton (NaturalizationCertificate p))
      Years3LivingInUK _ -> mempty
      IsBritOtbd _ -> mempty
      Married p1 p2 -> pure (Set.singleton (MarriageCertificate p1 p2))
