{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad(replicateM)
import           Data.Attoparsec.Text(Parser)
import           Data.Attoparsec.Text.Lazy(Result(..),parse)
import           Data.Char(isPrint)
import           Data.QIF
import           Data.String(fromString)
import qualified Data.Text as S
import           Data.Text.Lazy(Text)
import           Data.Text.Lazy.Builder(Builder, toLazyText)
import           Data.Time(Day(..), fromGregorian, addDays)
import           Lens.Micro(set)
import           Test.QuickCheck
import           Test.Tasty(TestTree, testGroup, defaultMain)
import           Test.Tasty.HUnit(testCase, (@=?))
import           Test.Tasty.QuickCheck(testProperty)

import Debug.Trace

main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    testBasics
  , testBlocks
  , testLists
  ]

-- -----------------------------------------------------------------------------

testBasics :: TestTree
testBasics =
  testGroup "Basic constants" [
    testQuantities
  , testDates
  , testSecurityType
  ]

testQuantities :: TestTree
testQuantities =
  testGroup "Simple Quantities" [
    testGroup "Currency" [
      testCase "1.25"   (1.25  @=? (runP parseCurrency "1.25"))
    , testCase "$1.25"  (1.25  @=? (runP parseCurrency "$1.25"))
    , testCase "-1.25"  (-1.25 @=? (runP parseCurrency "-1.25"))
    , testCase "-$1.25" (-1.25 @=? (runP parseCurrency "-$1.25"))
    , testCase "-$1.2"  (-1.2  @=? (runP parseCurrency "-$1.2"))
    , testCase "-$1."   (-1    @=? (runP parseCurrency "-$1."))
    , testCase "-1"     (-1    @=? (runP parseCurrency "-1"))
    , testCase "-1.2"   (-1.2  @=? (runP parseCurrency "-1.2"))
    , testCase "-1.0"   (-1    @=? (runP parseCurrency "-1.0"))
    , testCase "1.1"    (1.1   @=? (runP parseCurrency "1.1"))
    , testCase "1."     (1     @=? (runP parseCurrency "1."))
    , testProperty "Currency Serialization" currencySerialization
    ],
    testGroup "Share Quantities" [
      testCase "-3.0351" (-3.0351 @=? (runP parseShareQuantity "-3.0351"))
    , testCase "-3.035"  (-3.035  @=? (runP parseShareQuantity "-3.035"))
    , testCase "-3.03"   (-3.03   @=? (runP parseShareQuantity "-3.03"))
    , testCase "-3.3"    (-3.3    @=? (runP parseShareQuantity "-3.3"))
    , testCase "-3."     (-3      @=? (runP parseShareQuantity "-3."))
    , testCase "-3"      (-3      @=? (runP parseShareQuantity "-3"))
    , testCase "3.0351"  (3.0351  @=? (runP parseShareQuantity "3.0351"))
    , testCase "3.035"   (3.035   @=? (runP parseShareQuantity "3.035"))
    , testCase "3.03"    (3.03    @=? (runP parseShareQuantity "3.03"))
    , testCase "3.3"     (3.3     @=? (runP parseShareQuantity "3.3"))
    , testCase "3."      (3       @=? (runP parseShareQuantity "3."))
    , testCase "3"       (3       @=? (runP parseShareQuantity "3"))
    , testProperty "Share quantity serialization"
        (standardSerializer parseShareQuantity renderShareQuantity)
    ]
  ]

testDates :: TestTree
testDates = testGroup "Dates" [
    testCase "12/12/15" (fromGregorian 2015 12 12 @=? runP parseDate "12/12/15")
  , testCase "5/14/15"  (fromGregorian 2015 5  14 @=? runP parseDate "5/14/15")
  , testCase "3/4/00"   (fromGregorian 2000 3  4  @=? runP parseDate "3/4/00")
  , testCase "10/5/13"  (fromGregorian 2013 10 5  @=? runP parseDate "10/5/13")
  , testProperty "Date serialization"
      (standardSerializer parseDate renderDate)
  ]

testSecurityType :: TestTree
testSecurityType = testGroup "Security types" [
    testCase "Stock Option"
      (StockOption   @=? runP parseSecurityType "Stock Option")
  , testCase "Bond"
      (Bond          @=? runP parseSecurityType "Bond")
  , testCase "CD"
      (CD            @=? runP parseSecurityType "CD")
  , testCase "Mutual Fund"
      (MutualFund    @=? runP parseSecurityType "Mutual Fund")
  , testCase "Index"
      (Index         @=? runP parseSecurityType "Index")
  , testCase "ETF"
      (ETF           @=? runP parseSecurityType "ETF")
  , testCase "Money Market Fund"
      (MoneyMarket   @=? runP parseSecurityType "Money Market Fund")
  , testCase "Precious Metal"
      (PreciousMetal @=? runP parseSecurityType "Precious Metal")
  , testCase "Commodity"
      (Commodity     @=? runP parseSecurityType "Commodity")
  , testCase "Stock"
      (Stock         @=? runP parseSecurityType "Stock")
  , testProperty "Security type serialization"
      (standardSerializer parseSecurityType renderSecurityType)
  ]

-- -----------------------------------------------------------------------------

testBlocks :: TestTree
testBlocks =
  testGroup "Standard Blocks" [
    testSecurity
  , testAccount
  , testCategory
  , testTransaction
  , testInvTransaction
  ]

testSecurity :: TestTree
testSecurity =
  testGroup "Security Block" [
    testCase "Alaska Airlines"
      (security "Alaska Air Group, Inc" "ALK" Stock Nothing @=?
        runP parseSecurity "NAlaska Air Group, Inc\nSALK\nTStock\n^\n")
  , testCase "Vanguard S&P"
      (security "Vanguard S&P 500 Index,Investor Shares" "VFINX"
                MutualFund Nothing
         @=?
        runP parseSecurity
          "NVanguard S&P 500 Index,Investor Shares\nSVFINX\nTMutual Fund\n^\n")
  , testCase "Vanguard ETF"
      (security "Vanguard ETF" "VET" ETF (Just "House") @=?
        runP parseSecurity "NVanguard ETF\nSVET\nTETF\nGHouse\n^\n")
  , testProperty "Security block serialization"
      (standardSerializer parseSecurity renderSecurity)
  ]

security :: S.Text -> S.Text -> SecurityType -> Maybe S.Text -> Security
security n t y g =
  set secName   n $
  set secTicker t $
  set secType   y $
  set secGoal   g emptySecurity

testAccount :: TestTree
testAccount =
  testGroup "Accounts" [
    testCase "Umpqua Account"
      (account "Umpqua Bank" BankAccount "Checking" Nothing Nothing 1234.12 @=?
        runP parseAccount "NUmpqua Bank\nDChecking\nX\nTBank\nB$1234.12\n^\n")
  , testCase "Blue Bank"
      (account "Blue Bank" CreditCardAccount "" (Just 40)
               (Just (fromGregorian 2012 2 14)) (-520)
        @=?
         runP parseAccount
          "NBlue Bank\nD\nX\nTCCard\n/2/14/12\nL40.0\nB-$520.\n^\n")
  , testProperty "Account serialization"
      (standardSerializer parseAccount renderAccount)
  , testProperty "Account header serialization"
      (standardSerializer parseAccountHeader renderAccountHeader)
  ]

account :: S.Text -> AccountType -> S.Text ->
           Maybe Currency -> Maybe Day -> Currency ->
           Account
account n t d cl bd b =
  set accountName        n  $
  set accountType        t  $
  set accountDescription d  $
  set accountCreditLimit cl $
  set accountBalanceDate bd $
  set accountBalance     b  emptyAccount

testCategory :: TestTree
testCategory =
  testGroup "Categories" [
    testCase "Auto"
      (category "Auto" "Automobile-related expenses" Expense False Nothing Nothing @=?
        runP parseCategory "NAuto\nDAutomobile-related expenses\nE\n^\n")
  , testCase "Registration"
      (category "Registration" "" Expense True Nothing (Just 535) @=?
        runP parseCategory "NRegistration\nD\nT\nE\nR535\n^\n")
  , testCase "Salary"
      (category "Salary" "Income" Income True Nothing Nothing @=?
        runP parseCategory "NSalary\nDIncome\nT\nI\n^\n")
  , testCase "Dinner"
      (category "Dinner" "Yum yum" Expense False (Just 200) Nothing @=?
        runP parseCategory "NDinner\nDYum yum\nE\nB200.\n^\n")
  , testProperty "Category serialization"
      (standardSerializer parseCategory renderCategory)
  ]

category :: S.Text -> S.Text -> CategoryKind -> Bool ->
            Maybe Currency -> Maybe Word ->
            Category
category n d k itr ba tsi =
  set catName            n   $
  set catDescription     d   $
  set catKind            k   $
  set catIsTaxRelated    itr $
  set catBudgetAmount    ba  $
  set catTaxScheduleInfo tsi emptyCategory

testTransaction :: TestTree
testTransaction =
  testGroup "Transactions" [
    testCase "Roost"
      (transaction (fromGregorian 2012 2 14) "Roost" "Yay fun" (-25) Nothing
                   (Just "Dining") True False [] @=?
        (runP parseTransaction
          "D2/14/12\nPRoost\nMYay fun\nT-25\nCX\nLDining\n^\n"))
  , testCase "Uncleared Roost"
      (transaction (fromGregorian 2012 2 14) "Roost" "Yay fun" (-25) Nothing
                   (Just "Dining") False False [] @=?
        (runP parseTransaction
          "D2/14/12\nPRoost\nMYay fun\nT-25.\nLDining\n^\n"))
  , testCase "Business Roost"
      (transaction (fromGregorian 2012 2 14) "Roost" "Yay fun" (-25) Nothing
                   (Just "Dining") True True [] @=?
        (runP parseTransaction
          "D2/14/12\nPRoost\nMYay fun\nT-25.00\nCX\nF\nLDining\n^\n"))
  , testCase "Paycheck"
      (transaction (fromGregorian 2018 3 4) "Galois" "" 50 Nothing Nothing
                   True False [set entryAmount (-30) $
                               set entryCategory "Checking" emptySplitItem,
                               set entryAmount (-20) $
                               set entryCategory "Savings" emptySplitItem] @=?
        (runP parseTransaction
         "D3/4/18\nPGalois\nM\nT50.0\nCX\nSChecking\nE\n$-30.0\nSSavings\nE\n$-20\n^\n"))
  , testProperty "Transaction serialization"
      (standardSerializer parseTransaction renderTransaction)
  ]

transaction :: Day -> S.Text -> S.Text -> Currency ->
               Maybe Word -> Maybe S.Text -> Bool -> Bool -> [SplitItem] ->
               Transaction
transaction d p m a n c l r s =
  set entDate         d $
  set entParty        p $
  set entMemo         m $
  set entAmount       a $
  set entNumber       n $
  set entCategory     c $
  set entCleared      l $
  set entReimbursable r $
  set entSplits       s emptyTransaction

testInvTransaction :: TestTree
testInvTransaction =
  testGroup "Interest Transactions" [
    testCase "Dividend"
      (Dividend (set tradeSecurity "SEC" $
                 set tradeSharePrice Nothing $
                 set tradeQuantity (Just 0) $
                 set tradeCommission (Just 0) $
                 set tradeTotalAmount 0 (emptyTrade (fromGregorian 2065 7 7)))
        @=?
         (runP parseInvTransaction
            "D7/7/65\nNDiv\nYSEC\nQ0\nO0.00\nT0\n^\n"))
  , testCase "Dividend, with share price"
      (Dividend (set tradeSecurity "SEC" $
                 set tradeSharePrice (Just 0) $
                 set tradeQuantity (Just 0) $
                 set tradeCommission (Just 0) $
                 set tradeTotalAmount 0 (emptyTrade (fromGregorian 2065 7 7)))
        @=?
         (runP parseInvTransaction
            "D7/7/65\nNDiv\nYSEC\nI0\nQ0\nO0.00\nT0\n^\n"))
  , testProperty "Interest transaction serialization"
      (standardSerializer parseInvTransaction renderInvTransaction)
  ]

-- -----------------------------------------------------------------------------

testLists :: TestTree
testLists =
  testGroup "Lists of structures" [
    testProperty  "Account List"
      (standardSerializer parseAccountList renderAccountList)
  , testProperty  "Category List"
      (standardSerializer parseCategoryList renderCategoryList)
  , testProperty  "Investment Entries"
      (standardSerializer parseInvestmentEntries renderInvestmentEntries)
  , testProperty  "Bank Entries"
      (standardSerializer parseBankEntryList renderBankEntryList)
  , testProperty  "Credit Card Entries"
      (standardSerializer parseCreditCardEntryList renderCreditCardEntryList)
  , testProperty  "Asset Entries"
      (standardSerializer parseAssetEntryList renderAssetEntryList)
  , testProperty  "Liability Entries"
      (standardSerializer parseLiabilityEntryList renderLiabilityEntryList)
  , testProperty  "Security list"
      (standardSerializer parseSecurityList renderSecurityList)
  ]

-- -----------------------------------------------------------------------------

currencySerialization :: Bool -> Currency -> Property
currencySerialization doDollar amt =
  amt === (runP parseCurrency (runR (renderCurrency doDollar) amt))

standardSerializer :: (Eq a, Show a) =>
                      Parser a -> (a -> Builder) ->
                      a ->
                      Property
standardSerializer parser renderer x =
  x   === (runP parser (runR renderer x))

-- -----------------------------------------------------------------------------

runP :: Parser a -> Text -> a
runP parser txt = go (parse parser txt)
 where
  go (Fail _ _ err) = error ("Parser error: " ++ err)
  go (Done _ res)   = res

runR :: (a -> Builder) -> a -> Text
runR builder v = toLazyText (builder v)

-- -----------------------------------------------------------------------------

instance Arbitrary Day where
  arbitrary =
    do let base = fromGregorian 2000 1 1
       x <- choose (0, 36524)
       return (addDays x base)

instance Arbitrary SecurityType where
  arbitrary = elements [ Stock, Bond, CD, MutualFund, Index, ETF, MoneyMarket,
                         PreciousMetal, Commodity, StockOption, Other ]

instance Arbitrary Security where
  arbitrary =
    do n <- arbitrary
       t <- arbitrary
       y <- arbitrary
       g <- arbitrary
       return $ set secName   n
              $ set secTicker t
              $ set secType   y
              $ set secGoal   g emptySecurity

instance Arbitrary S.Text where
  arbitrary =
    do len  <- choose (0,150)
       chrs <- replicateM len (suchThat arbitrary isPrint)
       return (fromString chrs)

instance Arbitrary AccountType where
  arbitrary = elements [ BankAccount, CashAccount, CreditCardAccount,
                         InvestmentAccount, AssetAccount, LiabilityAccount ]

instance Arbitrary Account where
  arbitrary =
    do (n, t, d, cl, bd, b) <- arbitrary
       return $ set accountName        n
              $ set accountType        t
              $ set accountDescription d
              $ set accountCreditLimit cl
              $ set accountBalanceDate bd
              $ set accountBalance     b emptyAccount

instance Arbitrary Category where
  arbitrary =
    do k <- elements [Income, Expense]
       (n, d, it, ba, ts) <- arbitrary
       return $ set catName            n
              $ set catDescription     d
              $ set catKind            k
              $ set catIsTaxRelated    it
              $ set catBudgetAmount    ba
              $ set catTaxScheduleInfo ts emptyCategory

instance Arbitrary TradeInfo where
  arbitrary =
    do (d, s, p, q, c, a) <- arbitrary
       day <- arbitrary
       return $ set tradeDate        d
              $ set tradeSecurity    s
              $ set tradeSharePrice  p
              $ set tradeQuantity    q
              $ set tradeCommission  c
              $ set tradeTotalAmount a (emptyTrade day)

instance Arbitrary TransferInfo where
  arbitrary =
    do (d, s, m, a, c, t, p) <- arbitrary
       day <- arbitrary
       return $ set transDate        d
              $ set transSummary     s
              $ set transMemo        m
              $ set transAmount      a
              $ set transCleared     c
              $ set transAccount     t
              $ set transSplits      p (emptyTransfer day)

instance Arbitrary InvTransaction where
  arbitrary = oneof [ Buy      <$> arbitrary
                    , Sell     <$> arbitrary
                    , Transfer <$> arbitrary
                    , Dividend <$> arbitrary
                    , Interest <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary SplitItem where
  arbitrary =
    do (m, a, c) <- arbitrary
       return $ set entryMemo     m
              $ set entryAmount   a
              $ set entryCategory c emptySplitItem

instance Arbitrary Transaction where
  arbitrary =
    do (d,p,m,a,n,c,l,r,s) <- arbitrary
       return $ set entDate         d
              $ set entParty        p
              $ set entMemo         m
              $ set entAmount       a
              $ set entNumber       n
              $ set entCategory     c
              $ set entCleared      l
              $ set entReimbursable r
              $ set entSplits       s emptyTransaction

instance Arbitrary QIF where
  arbitrary =
    do (a, c, s, i, n) <- arbitrary
       return $ set qifAccounts               a
              $ set qifCategories             c
              $ set qifSecurities             s
              $ set qifInvestmentTransactions i
              $ set qifNormalTransactions     n emptyQIF

