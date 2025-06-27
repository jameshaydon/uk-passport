{-# LANGUAGE LambdaCase #-}

module Lib (brit, run) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import Data.List (nub)
import Data.Set (Set)
import Data.Set qualified as Set

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
  deriving (Show, Eq, Ord)

instance Disp DocumentType where
  disp = \case
    BirthCertificate p -> "Birth certificate for " <> disp p
    MarriageCertificate p1 p2 -> "Marriage certificate for " <> disp p1 <> " and " <> disp p2
    NaturalizationCertificate p -> "Naturalization certificate for " <> disp p
    Passport p -> "Passport for " <> disp p

data Proof
  = ViaParent ParentType Proof
  | And Proof Proof
  | Evidence Predicate
  deriving (Show)

instance Disp Proof where
  disp = \case
    ViaParent parentType proof -> "Via " <> disp parentType <> "'s britishness:\n" <> indentLines "  " (disp proof)
    And proof1 proof2 ->
      "• "
        <> indentBulletLines (disp proof1)
        <> "\n• "
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

type M a = LogicT (StateT Claims IO) a

data Knowledge = SureYes | SureNo | Unsure

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

brit :: Person -> M Proof
brit p =
  do
    check (IsBritish p)
    cut
      (askRequired (BornInUK p))
      (askRequired (BornBefore 1983 p) `orElse` britBornInUkViaParent p)
      (britBornAbroad p)

britBornInUkViaParent :: Person -> M Proof
britBornInUkViaParent p = viaParent p (\parent -> brit parent `orElse` settled parent)

cut :: (MonadLogic m) => m Proof -> m Proof -> m Proof -> m Proof
cut c a = ifte c (\x -> And x <$> a)

orElse :: (MonadLogic m) => m a -> m a -> m a
orElse a = ifte a pure

askRequired :: Predicate -> M Proof
askRequired p = check p >> pure (Evidence p)

viaParent :: Person -> (Person -> M Proof) -> M Proof
viaParent p cond =
  go Mother
    <|> ( cut
            (askRequired (BornAfter 2006 p))
            (go Father)
            (And <$> married (Parent Father p) (Parent Mother p) <*> go Father)
        )
  where
    go parent = do
      ViaParent parent <$> cond (Parent parent p)

britBornAbroad :: Person -> M Proof
britBornAbroad p =
  viaParent p britOtbd
    `orElse` viaParent p (\parent -> brit parent >> askRequired (Years3LivingInUK parent))

britOtbd :: Person -> M Proof
britOtbd p =
  do
    r <- ask (IsBritOtbd p)
    case r of
      SureNo -> mempty
      _ -> britOtbdUkBorn <|> askRequired (Naturalized p)
  where
    britOtbdUkBorn =
      And
        <$> askRequired (BornInUK p)
        <*> ( askRequired (BornBefore 1983 p)
                `orElse` britBornInUkViaParent p
            )

married :: Person -> Person -> LogicT (StateT Claims IO) Proof
married p q = askRequired (Married p q)

settled :: Person -> M Proof
settled p = askRequired (Settled p)

run :: M Proof -> IO ()
run m = do
  res <- evalStateT (observeAllT m) Set.empty
  putStrLn $ "Applicant has " <> show (length res) <> " proof(s) of britishness:\n\n"
  forM_ res $ \r -> do
    putStrLn "-----------"
    putStrLn (disp r)
    putStrLn "\nPossible doc sets:"
    let docSets = nub (observeAll (docs r))
    forM_ docSets $ \ds -> print ds
    putStrLn "These documents are unconditionally required:"
    print (foldr1 Set.intersection docSets)

docs :: Proof -> Logic (Set DocumentType)
docs = \case
  ViaParent _ proof -> docs proof
  And proof1 proof2 -> do
    docs1 <- docs proof1
    docs2 <- docs proof2
    pure (docs1 <> docs2)
  Evidence predicate -> predicateDocs predicate
  where
    predicateDocs :: Predicate -> Logic (Set DocumentType)
    predicateDocs = \case
      IsBritish p -> pure (Set.singleton (Passport p)) <|> pure (Set.singleton (Passport p))
      Settled p -> pure (Set.singleton (Passport p))
      BornBefore _ p -> pure (Set.singleton (BirthCertificate p))
      BornInUK p -> pure (Set.singleton (BirthCertificate p))
      BornAfter _ p -> pure (Set.singleton (BirthCertificate p))
      Naturalized p -> pure (Set.singleton (NaturalizationCertificate p))
      Years3LivingInUK p -> pure (Set.singleton (Passport p))
      IsBritOtbd p -> pure (Set.singleton (Passport p)) <|> pure (Set.singleton (Passport p))
      Married p1 p2 -> pure (Set.singleton (MarriageCertificate p1 p2))
