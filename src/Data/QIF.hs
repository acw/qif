{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |A module for parsing or rendering QIF files.
--
-- QIF is a fairly braindead format designed for transfering financial data
-- between applications. If you're writing a new financial application, you
-- might want to find a newer format to share with other applications (if you
-- can find one), and you definitely shouldn't use this as the database. For one
-- thing, this format uses two-digit years, which is just kind of lazy. Also, it
-- enforces absolutely no consistency constraints in terms of cross-references
-- or transaction sums.
--
-- To parse a QIF file, I suggest using "Data.Attoparsec.Text.Lazy" and a lazy
-- 'Text' data structure, as follows:
--
-- @
--    do txt <- Text.pack `fmap` readFile "my.qif"
--       case parse parseQIF txt of
--         Fail around _ err ->
--           fail ("Parse error (" ++ err ++ ") around :" ++
--                 show (Text.take 10 around))
--         Done _ res ->
--           somethingInteresting res
-- @
--
-- To render a QIF file, you can run the builders from "Data.Text.LazyBuilder"
-- directly, as so:
--
-- @
--    Data.Text.Lazy.IO.writeFile "my.qif" (toLazyText (renderQIF myQIF))
-- @
--
module Data.QIF(
         QIF,           emptyQIF
       ,                qifAccounts,              qifCategories
       ,                qifSecurities,            qifInvestmentTransactions
       ,                qifNormalTransactions
       ,                parseQIF,                 renderQIF
       -- * Various lists in QIF
       ,                parseAccountList,         renderAccountList
       ,                parseCategoryList,        renderCategoryList
       ,                parseBankEntryList,       renderBankEntryList
       ,                parseInvestmentEntries,   renderInvestmentEntries
       ,                parseCashEntryList,       renderCashEntryList
       ,                parseCreditCardEntryList, renderCreditCardEntryList
       ,                parseAssetEntryList,      renderAssetEntryList
       ,                parseLiabilityEntryList,  renderLiabilityEntryList
       ,                parseSecurityList,        renderSecurityList
       -- * Account Information
       , Account,         emptyAccount
       ,                  accountName,           accountType
       ,                  accountDescription,    accountCreditLimit
       ,                  accountBalanceDate,    accountBalance
       ,                  parseAccount,          renderAccount
       , AccountType(..), parseAccountType,      renderAccountType
       ,                  parseShortAccountType, renderShortAccountType
       ,                  parseAccountHeader,    renderAccountHeader
       -- * Category Information
       , CategoryKind(..)
       , Category,      emptyCategory
       ,                catName,              catDescription
       ,                catKind,              catIsTaxRelated
       ,                catBudgetAmount,      catTaxScheduleInfo
       ,                parseCategory,        renderCategory
       -- * Transaction Information
       , SplitItem,        emptySplitItem
       ,                   entryMemo,         entryAmount
       ,                   entryCategory
       ,                   parseSplit,        renderSplit
       -- ** Standard Transactions (Bank, Credit Card, etc.)
       , Transaction,      emptyTransaction
       ,                   entDate,           entParty
       ,                   entMemo,           entAmount
       ,                   entNumber,         entCategory
       ,                   entCleared,        entReimbursable
       ,                   entSplits
       ,                   parseTransaction,  renderTransaction
       -- ** Investment Account Transactions
       -- *** Trade Information
       , TradeInfo,        emptyTrade
       ,                   tradeDate,         tradeSecurity
       ,                   tradeSharePrice,   tradeQuantity
       ,                   tradeCommission,   tradeTotalAmount
       -- *** Transfer Information
       , TransferInfo,     emptyTransfer
       ,                   transDate,         transSummary
       ,                   transMemo,         transAmount
       ,                   transCleared,      transAccount
       ,                   transSplits
       -- *** Actual Investment Actions
       , InvTransaction(..)
       ,                   invEntDate
       ,                   parseInvTransaction, renderInvTransaction
       -- * Security Types
       , SecurityType(..), parseSecurityType, renderSecurityType
       , Security,         emptySecurity
       ,                   secName,           secTicker
       ,                   secType,           secGoal
       ,                   parseSecurity,     renderSecurity
       -- * Fixed-width quantities
       , Currency,      parseCurrency,      renderCurrency
       , ShareQuantity, parseShareQuantity, renderShareQuantity
       ,                parseQuantity,      renderQuantity
       -- * Old-school dates
       ,                parseDate,          renderDate
       )
 where

import           Control.Monad(when)
import           Data.Attoparsec.Text(Parser,char,choice,decimal,digit,
                                      endOfInput,endOfLine,inClass,many',
                                      many1,option,satisfy,string)
import qualified Data.Attoparsec.Text as Atto
import           Data.Char(digitToInt,isPrint)
import           Data.Fixed(Fixed,HasResolution(..),E2)
import           Data.Monoid((<>))
import qualified Data.Text as Text
import           Data.Text(Text)
import           Data.Text.Lazy.Builder(Builder,singleton,fromString,fromText)
import           Data.Time(Day(..),fromGregorian,toGregorian)
import           Data.Time.Format(defaultTimeLocale,parseTimeM,formatTime)
import           Data.Word(Word)
import           Lens.Micro(Lens',ASetter',lens,set,over)
import           Lens.Micro.Extras(view)
import           Lens.Micro.TH(makeLensesWith,lensRules,generateSignatures)

-- Fixed-width Quantities: Currency and Share Counts ---------------------------

data E4
instance HasResolution E4 where
  resolution _ = 10000

-- |A fixed width implementation of currency, based on the U.S. dollar. Future
-- versions of this library that wish to support other currencies may wish to
-- change this, or to abstract the rest of the library over a currency type.
type Currency      = Fixed E2

-- |A fixed-width implementation of quantities for shares. So far, I have seen
-- sites report share quantities to up to four decimal points, henced the value.
type ShareQuantity = Fixed E4

-- |Parse a fixed-width number. Should parse negative values, as well. This does
-- support QIF's annoying "5." notation, as well.
parseQuantity :: HasResolution a => Parser (Fixed a)
parseQuantity =
  do finally <- option id (char '-' >> return negate)
     leader   <- digitToNum `fmap` digit
     firstbit <- beforePoint leader
     option (finally firstbit) $
       do _      <- char '.'
          posres <- afterPoint firstbit 10
          return (finally posres)
 where 
  beforePoint acc =
    option acc $
      choice [
        do _ <- char ','
           beforePoint acc
      , do x <- digitToNum `fmap` digit
           beforePoint ((acc * 10) + x)
      ]
  afterPoint acc place =
    option acc $
      do d <- digitToNum `fmap` digit
         afterPoint (acc + (d / place)) (place * 10)

-- |Render a quantity. As opposed to the parser, this output function will
-- always represent numbers to their full precision.
renderQuantity :: HasResolution a => Fixed a -> Builder
renderQuantity = fromString . show

-- |Parse a currency. This is slightly differentiated from 'parseQuantity' in
-- that it will happily ignore a dollar sign placed in the correct location.
-- Note that this will support negative amounts written as either \"-$500\" or
-- as \"$-500\".
parseCurrency :: Parser Currency
parseCurrency =
  do finally <- option id  (char '-' *> return negate) -- kept to deal with -$1
     _       <- option '$' (char '$')
     amount  <- parseQuantity
     return (finally amount)

-- |Render a currency. The boolean state whether or not to include a dollar
-- sign. When dollar signs are included, negatives are written as \"-$500\"
-- rather than \"$-500\".
renderCurrency :: Bool -> Currency -> Builder
renderCurrency showDollar x =
  neg <> (if showDollar then singleton '$' else mempty) <> renderQuantity x'
 where
  (x',neg) = if x < 0 then (negate x, singleton '-') else (x,mempty)


-- |Parse a share quantity. Currently an alias for 'parseQuantity'.
parseShareQuantity :: Parser ShareQuantity
parseShareQuantity = parseQuantity

-- |Render a share quantity. Currently an alias for 'renderQuantity'.
renderShareQuantity :: ShareQuantity -> Builder
renderShareQuantity = renderQuantity

digitToNum :: Num a => Char -> a
digitToNum = fromIntegral . digitToInt

-- Account Types ---------------------------------------------------------------

-- |The type of an account; should be fairly self-explanatory.
data AccountType = BankAccount
                 | CashAccount
                 | CreditCardAccount
                 | InvestmentAccount
                 | AssetAccount
                 | LiabilityAccount
 deriving (Eq, Read, Show)

-- |Parse a fully-rendered account type (e.g, \"!Type:Bank\"), used for
-- section headings.
parseAccountType :: Parser AccountType
parseAccountType = string "!Type:" *> parseShortAccountType

-- |Parse the short version of an account type (e.g., "Bank"), which is used in
-- a couple different places.
parseShortAccountType :: Parser AccountType
parseShortAccountType =
  choice [ string "Bank"  *> return BankAccount
         , string "Cash"  *> return CashAccount
         , string "CCard" *> return CreditCardAccount
         , string "Invst" *> return InvestmentAccount
         , string "Oth A" *> return AssetAccount
         , string "Oth L" *> return LiabilityAccount
         ]

-- |Render a fully-rendered account type (e.g., \"!Type:Bank\"), used for
-- section headings.
renderAccountType :: AccountType -> Builder
renderAccountType acc = fromText "!Type:" <> renderShortAccountType acc

-- |Render the short version of an account type (e.g., \"Bank\").
renderShortAccountType :: AccountType -> Builder
renderShortAccountType BankAccount       = fromText "Bank"
renderShortAccountType CashAccount       = fromText "Cash"
renderShortAccountType CreditCardAccount = fromText "CCard"
renderShortAccountType InvestmentAccount = fromText "Invst"
renderShortAccountType AssetAccount      = fromText "Oth A"
renderShortAccountType LiabilityAccount  = fromText "Oth L"

-- Accounts --------------------------------------------------------------------

-- |An account in the QIF file. This same structure applies for all the account
-- types.
data Account = Account {
       _accountName        :: Text
     , _accountType        :: AccountType
     , _accountDescription :: Text
     , _accountCreditLimit :: Maybe Currency
     , _accountBalanceDate :: Maybe Day
     , _accountBalance     :: Currency
     }
 deriving (Eq, Show)

makeLensesWith (set generateSignatures False lensRules) ''Account

-- |The name of the account
accountName        :: Lens' Account Text
-- |The type of the account
accountType        :: Lens' Account AccountType
-- |The description of the account; in my limited experience this can (and most
-- likely will) be empty.
accountDescription :: Lens' Account Text
-- |For accounts with limits, the credit limit for the account.
accountCreditLimit :: Lens' Account (Maybe Currency)
-- |The date at which the balance in the next field was current.
accountBalanceDate :: Lens' Account (Maybe Day)
-- |The current balance.
accountBalance     :: Lens' Account Currency

-- |A blank account. Defaults to 'BankAccount' for the type, with the obvious
-- zeros, empty strings, and Nothings elsewhere.
emptyAccount :: Account
emptyAccount = Account {
    _accountName        = ""
  , _accountType        = BankAccount
  , _accountDescription = ""
  , _accountCreditLimit = Nothing
  , _accountBalanceDate = Nothing
  , _accountBalance     = 0
  }

-- |Parse an account.
parseAccount :: Parser Account
parseAccount = go emptyAccount
 where
  go base =
    do label <- satisfy (inClass "NTDL/$BX^")
       case label of
         'N' -> getP go base accountName        parseString
         'T' -> getP go base accountType        parseShortAccountType
         'D' -> getP go base accountDescription parseString
         'L' -> getP go base accountCreditLimit (Just `fmap` parseCurrency)
         '/' -> getP go base accountBalanceDate (Just `fmap` parseDate)
         '$' -> getP go base accountBalance     parseCurrency
         'B' -> getP go base accountBalance     parseCurrency
         'X' -> many1 endOfLine *> go base
         '^' -> many1 endOfLine *> return base
         _   -> fail "Unknown, out of scope account label."

getP :: (s -> Parser s) -> s -> ASetter' s a -> Parser a -> Parser s
getP go base field getter =
  do x <- getter
     _ <- many1 endOfLine
     go (set field x base)

getP' :: (s -> Parser s) -> s -> ASetter' s (Maybe a) -> Parser a -> Parser s
getP' go base field getter =
    do x <- option Nothing (Just `fmap` getter)
       _ <- many1 endOfLine
       go (set field x base)

parseString :: Parser Text
parseString = Atto.takeWhile (not . inClass "\r\n")

-- |Render an account.
renderAccount :: Account -> Builder
renderAccount acc = mconcat [
    put  'N' acc accountName        fromText
  , put  'D' acc accountDescription fromText
  , put  'T' acc accountType        renderShortAccountType
  , put  'B' acc accountBalance     (renderCurrency True)
  , putm 'L' acc accountCreditLimit (renderCurrency True)
  , putm '/' acc accountBalanceDate renderDate
  , singleton '^' <> singleton '\n'
  ]

put :: Char -> s -> Lens' s a -> (a -> Builder) -> Builder
put label acc field builder =
  singleton label <> builder (view field acc) <> singleton '\n'

putm :: Char -> s -> Lens' s (Maybe a) -> (a -> Builder) -> Builder
putm label acc field builder =
  case view field acc of
    Just x  -> singleton label <> builder x <> singleton '\n'
    Nothing -> mempty

putm' :: Char -> s -> Lens' s (Maybe a) -> (a -> Builder) -> Builder
putm' label acc field builder =
  case view field acc of
    Just x  -> singleton label <> builder x <> singleton '\n'
    Nothing -> singleton label <>              singleton '\n'

-- Dates -----------------------------------------------------------------------

-- |Parse a date, using old-school, incredibly unwise, \"mm/dd/yy\" formats. To
-- simplify my life, this assumes that all dates start in 2000, rather than in
-- 1970 or some other date. Thus, if you have data going back before 2000,
-- you'll need to post-process this to the appropriate date, by subtracting 100
-- appropriately. Hopefully by 2100 noone will be using QIF anymore, and this
-- won't matter.
parseDate :: Parser Day
parseDate =
  do str <- Atto.takeWhile isPrint
     intime <- parseTimeM False defaultTimeLocale "%-m/%e/%y" (Text.unpack str)
     let (year, mon, day) = toGregorian intime
     if year < 2000
        then return (fromGregorian (2000 + (year `mod` 100)) mon day)
        else return intime

-- |Render the date in QIF's silly format.
renderDate :: Day -> Builder
renderDate = fromString . formatTime defaultTimeLocale "%-m/%e/%y"

-- -----------------------------------------------------------------------------

-- |Parse the list of accounts associated with this QIF file.
parseAccountList :: Parser [Account]
parseAccountList =
  do _    <- string "!Option:AutoSwitch" *> many1 endOfLine
     _    <- string "!Account"           *> many1 endOfLine
     accs <- many' parseAccount
     _    <- string "!Clear:AutoSwitch"  *> many1 endOfLine
     return accs

-- |Render the list of accounts associated with this QIF file.
renderAccountList :: [Account] -> Builder
renderAccountList accs =
  fromString "!Option:AutoSwitch\n" <>
  fromString "!Account\n"           <>
  mconcat (map renderAccount accs)  <>
  fromString "!Clear:AutoSwitch\n"

-- Categories ------------------------------------------------------------------

-- |Information about a category that one might mark a transaction against.
data Category = Category {
       _catName            :: Text
     , _catDescription     :: Text
     , _catKind            :: CategoryKind
     , _catIsTaxRelated    :: Bool
     , _catBudgetAmount    :: Maybe Currency
     , _catTaxScheduleInfo :: Maybe Word
     }
 deriving (Eq, Show)

-- |Whether a category is an income category or an expense category.
data CategoryKind = Income | Expense
 deriving (Eq, Show)

-- |A blank category. We default categories to 'Expense'.
emptyCategory :: Category
emptyCategory = Category {
    _catName            = ""
  , _catDescription     = ""
  , _catKind            = Expense
  , _catIsTaxRelated    = False
  , _catBudgetAmount    = Nothing
  , _catTaxScheduleInfo = Nothing
  }

makeLensesWith (set generateSignatures False lensRules) ''Category

-- |The name of the category.
catName            :: Lens' Category Text
-- |A description of the category in question. Often empty.
catDescription     :: Lens' Category Text
-- |The kind of category; 'Expense' or 'Income'
catKind            :: Lens' Category CategoryKind
-- |Whether or not this category might be tax-related.
catIsTaxRelated    :: Lens' Category Bool
-- |A budget amount, if a budget has been established and published.
catBudgetAmount    :: Lens' Category (Maybe Currency)
-- |A number describing the tax schedule to look at.
catTaxScheduleInfo :: Lens' Category (Maybe Word)

-- |Parse a category.
parseCategory :: Parser Category
parseCategory = go emptyCategory
 where
  go base =
   do label <- satisfy (inClass "NDTIEBR^")
      case label of
        'N' -> getP go base catName            parseString
        'D' -> getP go base catDescription     parseString
        'B' -> getP go base catBudgetAmount    (Just `fmap` parseCurrency)
        'R' -> getP go base catTaxScheduleInfo (Just `fmap` decimal)
        --
        'T' -> many1 endOfLine *> go (set catIsTaxRelated True    base)
        'I' -> many1 endOfLine *> go (set catKind         Income  base)
        'E' -> many1 endOfLine *> go (set catKind         Expense base)
        --
        '^' -> many1 endOfLine *> return base
        _   -> fail "Unknown, out of scope category label."

-- |Render a category.
renderCategory :: Category -> Builder
renderCategory cat = mconcat [
    put  'N' cat catName         fromText
  , put  'D' cat catDescription  fromText
  , if _catIsTaxRelated cat then fromString "T\n" else mempty
  , if _catKind cat == Income   then fromString "I\n" else "E\n"
  , putm 'B' cat catBudgetAmount    (renderCurrency True)
  , putm 'R' cat catTaxScheduleInfo (fromString . show)
  , fromString "^\n"
  ]

-- Category Lists --------------------------------------------------------------

-- |Parse the list of categories (and the header for said list).
parseCategoryList :: Parser [Category]
parseCategoryList =
  do _    <- string "!Type:Cat"
     _    <- many1 endOfLine
     many' parseCategory

-- |Render the header for the list of categories followed by each of the
-- categories.
renderCategoryList :: [Category] -> Builder
renderCategoryList cats =
  fromString "!Type:Cat\n" <> mconcat (map renderCategory cats)


-- Account Headers -------------------------------------------------------------

-- |Sections full of transactions start with the header demarcating what account
-- the transactions are in regard to. This parses that header, returning the
-- account. Note that if you were expecting to be a somewhat reasonable
-- standard, and just reference a previously-defined account, you're in for a
-- disappointment. This is a completely fresh 'Account' structure, and you'll
-- have to match things up (and merge any differences) yourself.
parseAccountHeader :: Parser Account
parseAccountHeader =
  do _ <- string "!Account"
     _ <- many1  endOfLine
     parseAccount

-- |Render the header that should proceed any list of transactions.
renderAccountHeader :: Account -> Builder
renderAccountHeader acc = fromString "!Account\n" <> renderAccount acc

-- Bank Entries ----------------------------------------------------------------

-- |When a single transaction is split across a couple categories, this is your
-- friend.
data SplitItem = SplitItem {
       _entryMemo     :: Text
     , _entryAmount   :: Currency
     , _entryCategory :: Text
     }
 deriving (Eq, Show)

-- |An empty 'SplitItem'. No texts, no money. So sad.
emptySplitItem :: SplitItem
emptySplitItem = SplitItem "" 0 ""

makeLensesWith (set generateSignatures False lensRules) ''SplitItem

-- |Any memo taken as part of this split.
entryMemo :: Lens' SplitItem Text
-- |The amount of money in this split.
entryAmount :: Lens' SplitItem Currency
-- |The category associated with this split.
entryCategory :: Lens' SplitItem Text

-- |A normal transaction, that doesn't include an action in the stock market.
data Transaction = Transaction {
       _entDate         :: Day
     , _entParty        :: Text
     , _entMemo         :: Text
     , _entAmount       :: Currency
     , _entNumber       :: Maybe Word
     , _entCategory     :: Maybe Text
     , _entCleared      :: Bool
     , _entReimbursable :: Bool
     , _entSplits       :: [SplitItem]
     }
 deriving (Eq, Show)

-- |A transaction with no real data, that happened to occur on January 1st,
-- 2000. Happy new year!
emptyTransaction :: Transaction
emptyTransaction =
  Transaction (fromGregorian 2000 1 1) "" "" 0 Nothing Nothing False False []

makeLensesWith (set generateSignatures False lensRules) ''Transaction

-- |The date of the transaction.
entDate :: Lens' Transaction Day
-- |The other party to the transaction.
entParty :: Lens' Transaction Text
-- |Any memos taken about the transaction.
entMemo :: Lens' Transaction Text
-- |The total amount of the transaction.
entAmount :: Lens' Transaction Currency
-- |The check or other number, as appropriate.
entNumber :: Lens' Transaction (Maybe Word)
-- |The category associated with the transaction, if provided.
entCategory :: Lens' Transaction (Maybe Text)
-- |Whether or not this transaction has cleared.
entCleared :: Lens' Transaction Bool
-- |Whether or not this transaction is reimbursable.
entReimbursable :: Lens' Transaction Bool
-- |Any splits assocaited with this transaction.
entSplits :: Lens' Transaction [SplitItem]

-- |Parse a transaction. Note that this function only does parsing, not
-- consistency checking. Thus, you may end up with a transaction whose splits do
-- not sum to the total transaction amount, or is missing a category, etc.
parseTransaction :: Parser Transaction
parseTransaction = go emptyTransaction
 where
  go base =
   do label <- satisfy (inClass "DPMTUCLNSF^")
      case label of
        'D' -> getP go base entDate      parseDate
        'P' -> getP go base entParty     parseString
        'M' -> getP go base entMemo      parseString
        'T' -> getP go base entAmount    parseCurrency
        'U' -> getP go base entAmount    parseCurrency
        'C' -> getP go base entCleared   parseCleared
        'L' -> getP go base entCategory  (Just `fmap` parseString)
        'N' -> getP go base entNumber    (Just `fmap` decimal)
        --
        'S' -> do cat <-parseString <* many1 endOfLine
                  ent <- parseSplit (emptySplitItem{ _entryCategory = cat })
                  go (over entSplits (++ [ent]) base)
        --
        'F' -> many1 endOfLine *> go (set entReimbursable True base)
        '^' -> many1 endOfLine *> return base
        _   -> fail "Unknown, out of scope bank entry label."

-- |Render a transaction. This function assumes that you have performed any
-- consistency checking you're going to do before writing out this transaction.
-- It won't do any for you.
renderTransaction :: Transaction -> Builder
renderTransaction be =
  put  'D' be entDate     renderDate                                     <>
  put  'P' be entParty    fromText                                       <>
  put  'M' be entMemo     fromText                                       <>
  put  'T' be entAmount   (renderCurrency False)                         <>
  putm 'N' be entNumber   (fromString . show)                            <>
  put  'C' be entCleared  renderCleared                                  <>
  putm 'L' be entCategory fromText                                       <>
  (if view entReimbursable be then singleton 'F' <> newline else mempty) <>
  mconcat (map renderSplit (view entSplits be))                          <>
  singleton '^' <> newline
 where

-- |Parse a split. Note that some banking programs may end up emitting empty
-- splits, and we don't do anything about that. So you might want to check if
-- what you get back is 'emptyTransaction', or something morally similar.
parseSplit :: SplitItem -> Parser SplitItem
parseSplit base = option base $
  do label <- satisfy (inClass "E$")
     case label of
       'E' -> getP parseSplit base entryMemo   parseString
       '$' -> getP parseSplit base entryAmount parseCurrency
       _   -> fail "Unknown, out of scope split entry label."

-- |Render a split. Please be sensible in what you emit; this function won't
-- check your work for you.
renderSplit :: SplitItem -> Builder
renderSplit s =
  put 'S' s entryCategory fromText                <>
  put 'E' s entryMemo     fromText                <>
  put '$' s entryAmount   (renderCurrency False)

parseCleared :: Parser Bool
parseCleared = option False (char 'X' *> return True)

renderCleared :: Bool -> Builder
renderCleared False = mempty
renderCleared True  = fromText "X"

newline :: Builder
newline = singleton '\n'

-- Bank Entry Lists ------------------------------------------------------------

-- |Parse a list of bank transactions. You should probably call this directly
-- after 'parseAccountHeader' and discovering that it's a 'Bank' account. You
-- should also not trust the results of this, as it does no consistency checking
-- on your behalf.
parseBankEntryList :: Parser [Transaction]
parseBankEntryList =
  do _ <- string "!Type:Bank" >> many1 endOfLine
     many' parseTransaction

-- |Render a list of bank transactions. Please do any consistency checking you
-- want before calling this. You probably also want to have called
-- 'renderAccountHeader' with an appropriate 'Bank' account before calling this
-- one.
renderBankEntryList :: [Transaction] -> Builder
renderBankEntryList ls =
  fromText "!Type:Bank" <> newline <> mconcat (map renderTransaction ls)

-- Investment Entries ---------------------------------------------------------

-- |Information about a given trade.
data TradeInfo = TradeInfo {
       _tradeDate        :: Day
     , _tradeSecurity    :: Text
     , _tradeSharePrice  :: Maybe Currency
     , _tradeQuantity    :: Maybe ShareQuantity
     , _tradeCommission  :: Maybe Currency
     , _tradeTotalAmount :: Currency
     }
 deriving (Eq, Show)

-- |Build an empty trade made on a given day.
emptyTrade :: Day -> TradeInfo
emptyTrade day = TradeInfo day "" Nothing Nothing Nothing 0

makeLensesWith (set generateSignatures False lensRules) ''TradeInfo

-- |The date of the trade.
tradeDate :: Lens' TradeInfo Day
-- |The security this trade was about. Note that while we probably should be
-- doing some input validation on this, we're not. So if you're consuming this
-- value, be a bit paranoid.
tradeSecurity :: Lens' TradeInfo Text
-- |The share price of the security during the trade, if provided.
tradeSharePrice :: Lens' TradeInfo (Maybe Currency)
-- |The amount of the share traded, if provided.
tradeQuantity :: Lens' TradeInfo (Maybe ShareQuantity)
-- |The annoying commission taken out of the trade, if provided. Note that QIF
-- does differentiate between Nothing and (Just 0.00), for some reason.
tradeCommission :: Lens' TradeInfo (Maybe Currency)
-- |The total amount of the trade.
tradeTotalAmount :: Lens' TradeInfo Currency

-- |Information about a transfer into an investment account. This probably looks
-- like a normal transaction in a non-investment account, and each one probably
-- has a sibling that is exactly that.
data TransferInfo = TransferInfo {
       _transDate    :: Day
     , _transSummary :: Text
     , _transMemo    :: Text
     , _transAmount  :: Currency
     , _transCleared :: Bool
     , _transAccount :: Text
     , _transSplits  :: [SplitItem]
     }
 deriving (Eq, Show)

-- |An empty transfer that occurred on the given day.
emptyTransfer :: Day -> TransferInfo
emptyTransfer day = TransferInfo day "" "" 0 False "" []

makeLensesWith (set generateSignatures False lensRules) ''TransferInfo

-- |The date of the transfer.
transDate :: Lens' TransferInfo Day
-- |A summary of the transfer. Sometimes the other party in the transfer, or
-- just a short name, and sometimes blank.
transSummary :: Lens' TransferInfo Text
-- |A memo or note about the transaction. Often blank, in our limited
-- experience.
transMemo :: Lens' TransferInfo Text
-- |The amount of the transfer.
transAmount :: Lens' TransferInfo Currency
-- |Whether or not the transfer has cleared.
transCleared :: Lens' TransferInfo Bool
-- |The account with which this transaction took place ... usually. Sometimes
-- this is empty. Make of that as you will.
transAccount :: Lens' TransferInfo Text
-- |Any splits associated with the transaction.
transSplits :: Lens' TransferInfo [SplitItem]

-- |An action in an investment account. These are the ones I've seen in QIF
-- files shown to me. If you run into other ones, please file a bug or submit a
-- patch.
data InvTransaction = Buy           TradeInfo
                    | Sell          TradeInfo
                    | Transfer      TransferInfo
                    | Dividend      TradeInfo
                    | Interest Text TradeInfo
 deriving (Eq, Show)

-- |The date of an investment account action, regardless of what kind of
-- transaction it was.
invEntDate :: Lens' InvTransaction Day
invEntDate = lens dget dset
 where
  dget :: InvTransaction -> Day
  dget (Buy        tinfo)   = view tradeDate tinfo
  dget (Sell       tinfo)   = view tradeDate tinfo
  dget (Transfer   tinfo)   = view transDate tinfo
  dget (Dividend   tinfo)   = view tradeDate tinfo
  dget (Interest _ tinfo)   = view tradeDate tinfo
  dset :: InvTransaction -> Day -> InvTransaction
  dset (Buy        tinfo) x = Buy        (set  tradeDate x tinfo)
  dset (Sell       tinfo) x = Sell       (set  tradeDate x tinfo)
  dset (Transfer   tinfo) x = Transfer   (set  transDate x tinfo)
  dset (Dividend   tinfo) x = Dividend   (set  tradeDate x tinfo)
  dset (Interest a tinfo) x = Interest a (set  tradeDate x tinfo)

-- |Parse an investment transaction. Like it's sister function,
-- 'parseTransaction', this function doesn't do any semantic validation. So it's
-- possible that the date in the transaction doesn't make any sense. So ...
-- that's on you.
parseInvTransaction :: Parser InvTransaction
parseInvTransaction =
  do date <- char 'D' *> parseDate <* many1 endOfLine
     choice [ Transfer `fmap` tranTransaction (emptyTransfer date)
            , Buy      `fmap` buyTransaction  (emptyTrade    date)
            , Sell     `fmap` sellTransaction (emptyTrade    date)
            , Dividend `fmap` divTransaction  (emptyTrade    date)
            ,                 intTransaction  (emptyTrade    date)
            ]
 where
  tranTransaction base =
    do label <- satisfy (inClass "PMTCLNS^")
       case label of
         'P' -> getP tranTransaction base transSummary parseString
         'M' -> getP tranTransaction base transMemo    parseString
         'T' -> getP tranTransaction base transAmount  parseCurrency
         '$' -> getP tranTransaction base transAmount  parseCurrency
         'C' -> getP tranTransaction base transCleared parseCleared
         'L' -> getP tranTransaction base transAccount parseString
         'N' -> many1 digit *> many1 endOfLine *> tranTransaction base
         'S' -> do cat <- parseString <* many1 endOfLine
                   ent <- parseSplit (emptySplitItem{ _entryCategory = cat })
                   tranTransaction (over transSplits (++ [ent]) base)
         '^' -> many1 endOfLine *> return base
         _   -> fail "Unknown, out of scope transfer investment transaction"
  --
  buyTransaction  base = string "NBuy"  >> many1 endOfLine >> trade base
  sellTransaction base = string "NSell" >> many1 endOfLine >> trade base
  divTransaction  base = string "NDiv"  >> many1 endOfLine >> trade base
  trade base =
    do label <- satisfy (inClass "YIQOTN^")
       case label of
         'Y' -> getP  trade base tradeSecurity    parseString
         'I' -> getP' trade base tradeSharePrice  parseCurrency
         'Q' -> getP' trade base tradeQuantity    parseQuantity
         'O' -> getP' trade base tradeCommission  parseCurrency
         'T' -> getP  trade base tradeTotalAmount parseCurrency
         '^' -> many1 endOfLine *> return base
         _   -> fail "Unknown, out of scope trade investment transaction"
  --
  intTransaction base =
    do label <- char 'N' *> parseString <* many1 endOfLine
       when (label `elem` ["Buy","Sell"]) $ fail "Shouldn't be here."
       tr <- trade base
       return (Interest label tr)

-- |Render an investment transaction. As you might expect, this doesn't check
-- your work. So be careful.
renderInvTransaction :: InvTransaction -> Builder
renderInvTransaction ent =
  singleton 'D' <> renderDate (view invEntDate ent) <> singleton '\n' <>
    case ent of
      Buy        t -> renderTradeInfo    "Buy"  t
      Sell       t -> renderTradeInfo    "Sell" t
      Transfer   t -> renderTransferInfo        t
      Dividend   t -> renderTradeInfo    "Div"  t
      Interest n t -> renderTradeInfo    n      t

renderTradeInfo :: Text -> TradeInfo -> Builder
renderTradeInfo name t =
  singleton 'N' <> fromText name <> newline         <>
  put   'Y' t tradeSecurity    fromText               <>
  putm' 'I' t tradeSharePrice  (renderCurrency False) <>
  putm' 'Q' t tradeQuantity    renderShareQuantity    <>
  putm' 'O' t tradeCommission  (renderCurrency False) <>
  put   'T' t tradeTotalAmount (renderCurrency False) <>
  singleton '^' <> newline

renderTransferInfo :: TransferInfo -> Builder
renderTransferInfo t =
  put 'P' t transSummary fromText                <>
  put 'M' t transMemo    fromText                <>
  put 'T' t transAmount  (renderCurrency False)  <>
  put 'C' t transCleared renderCleared           <>
  put 'L' t transAccount fromText                <>
  mconcat (map renderSplit (view transSplits t)) <>
  singleton '^' <> newline

-- Investment Entry Lists ------------------------------------------------------

-- |Parse a list of investment entries. You probably should've called
-- 'parseAccountHeader' right before this and found an investment account.
parseInvestmentEntries :: Parser [InvTransaction]
parseInvestmentEntries =
  do _ <- string "!Type:Invst" >> many1 endOfLine
     many' parseInvTransaction

-- |Render a list of investment transactions. You should probably have just
-- called 'renderAccountHeader' with an investment account.
renderInvestmentEntries :: [InvTransaction] -> Builder
renderInvestmentEntries ents =
  fromText "!Type:Invst" <> newline <> mconcat (map renderInvTransaction ents)


-- Cash Entry Lists ------------------------------------------------------------

-- |Parse a list of cash transactions. You should probably have just called
-- 'parseAccountHeader' and found a 'Cash' account. You should probably also
-- be a bit paranoid about checking over the date you read, as we perform no
-- semantic checks on your behalf.
parseCashEntryList :: Parser [Transaction]
parseCashEntryList =
  do _ <- string "!Type:Cash" >> many1 endOfLine
     many' parseTransaction

-- |Render a list of cash transactions. You should have just called
-- 'renderAccountHeader' with a 'Cash' account.
renderCashEntryList :: [Transaction] -> Builder
renderCashEntryList ls =
  fromText "!Type:Cash" <> newline <> mconcat (map renderTransaction ls)

-- Credit Card Entry Lists -----------------------------------------------------

-- |Parse a list of credit card transactions. You should probably have just
-- called 'parseAccountHeader' and found a 'Cash' account. You should probably
-- also be a bit paranoid about checking over the date you read, as we perform
-- no semantic checks on your behalf.
parseCreditCardEntryList :: Parser [Transaction]
parseCreditCardEntryList =
  do _ <- string "!Type:CCard" >> many1 endOfLine
     many' parseTransaction

-- |Render a list of credit card transactions. You should have just called
-- 'renderAccountHeader' with a 'CreditCard' account.
renderCreditCardEntryList :: [Transaction] -> Builder
renderCreditCardEntryList ls =
  fromText "!Type:CCard" <> newline <> mconcat (map renderTransaction ls)

-- Asset Entry Lists -----------------------------------------------------------

-- |Parse a list of transactions in an asset account. Again, you probably should
-- have just called 'parseAccountHeader' and found an 'Asset' account, and you
-- should make sure to do any data validation you care about. Because this
-- library just doesn't care.
parseAssetEntryList :: Parser [Transaction]
parseAssetEntryList =
  do _ <- string "!Type:Oth A" >> many1 endOfLine
     many' parseTransaction

-- |Render a list of transactions on an asset. Did you call
-- 'renderAccountHeader' before this with an asset account? You should have!
renderAssetEntryList :: [Transaction] -> Builder
renderAssetEntryList ls =
  fromText "!Type:Oth A" <> newline <> mconcat (map renderTransaction ls)

-- Liability Entry Lists -------------------------------------------------------

-- |Last one! Parse a list of transactions about a liability. Probably a loan,
-- which you may or may not regret. You *will* regret it, however, if you didn't
-- call 'parseAccountHeader' first and find a 'Liability' account. You will also
-- regret it if you don't do some input validation on what you get from this
-- function.
parseLiabilityEntryList :: Parser [Transaction]
parseLiabilityEntryList =
  do _ <- string "!Type:Oth L" >> many1 endOfLine
     many' parseTransaction

-- |Render a list of transactions about a liability, probably right after you
-- called 'renderAccountHeader' with a liability account.
renderLiabilityEntryList :: [Transaction] -> Builder
renderLiabilityEntryList ls =
  fromText "!Type:Oth L" <> newline <> mconcat (map renderTransaction ls)

-- Security Types --------------------------------------------------------------

-- |The kinds of securities QIF files will reference.
data SecurityType = Stock | Bond | CD | MutualFund | Index | ETF | MoneyMarket
                  | PreciousMetal | Commodity | StockOption | Other
 deriving (Eq, Show)

-- |Parse a security type.
parseSecurityType :: Parser SecurityType
parseSecurityType = choice [
    -- these are intentionally out of order; "Stock Option" *MUST* precede
    -- "Stock", or this will shortcut
    string "Stock Option"      *> return StockOption
  , string "Bond"              *> return Bond
  , string "CD"                *> return CD
  , string "Mutual Fund"       *> return MutualFund
  , string "Index"             *> return Index
  , string "ETF"               *> return ETF
  , string "Money Market Fund" *> return MoneyMarket
  , string "Precious Metal"    *> return PreciousMetal
  , string "Commodity"         *> return Commodity
  , string "Stock"             *> return Stock
  , string "Other"             *> return Other
  ]

-- |Render a security type.
renderSecurityType :: SecurityType -> Builder
renderSecurityType st =
  case st of
    Stock         -> fromText "Stock"
    Bond          -> fromText "Bond"
    CD            -> fromText "CD"
    MutualFund    -> fromText "Mutual Fund"
    Index         -> fromText "Index"
    ETF           -> fromText "ETF"
    MoneyMarket   -> fromText "Money Market Fund"
    PreciousMetal -> fromText "Precious Metal"
    Commodity     -> fromText "Commodity"
    StockOption   -> fromText "Stock Option"
    Other         -> fromText "Other"

-- Securities ------------------------------------------------------------------

-- |The information QIF keeps about a security.
data Security = Security {
       _secName   :: Text
     , _secTicker :: Text
     , _secType   :: SecurityType
     , _secGoal   :: Maybe Text
     }
 deriving (Eq, Show)

-- |An empty security, forlorn and alone, with no name, no ticker, and no goals.
-- Definitely a stock, though.
emptySecurity :: Security
emptySecurity = Security "" "" Stock Nothing

makeLensesWith (set generateSignatures False lensRules) ''Security

-- |The name of the security.
secName :: Lens' Security Text
-- |The ticker symbol for the security. If I was a better person this would do
-- some validation on the input.
secTicker :: Lens' Security Text
-- |The type of security.
secType :: Lens' Security SecurityType
-- |The goal for the security. I think this is for things like "Buying a house"
-- or "Saving for college", but I've never actually seen this used in the wild.
secGoal :: Lens' Security (Maybe Text)

-- |Parse a security. Performs no validation that the name makes sense, the
-- ticker makes sense, or that the two go together. Good luck with that.
parseSecurity :: Parser Security
parseSecurity = go emptySecurity
 where
  go base =
    do label <- satisfy (inClass "NSTG^")
       case label of
         'N' -> getP go base secName   parseString
         'S' -> getP go base secTicker parseString
         'T' -> getP go base secType   parseSecurityType
         'G' -> getP go base secGoal   (Just `fmap` parseString)
         '^' -> many1 endOfLine *> return base
         _   -> fail "Unknown, out of scope security label."

-- |Render a security. You should probably make sure that your data structure
-- makes sense before you write it, but that's your thing. This function won't
-- judget you.
renderSecurity :: Security -> Builder
renderSecurity s =
  put  'N' s secName   fromText           <>
  put  'S' s secTicker fromText           <>
  put  'T' s secType   renderSecurityType <>
  putm 'G' s secGoal   fromText           <>
  singleton '^' <> newline

-- Securities Lists ------------------------------------------------------------

-- |Parse a list of securities out of the QIF file.
parseSecurityList :: Parser [Security]
parseSecurityList =
  do _ <- string "!Type:Security" >> many1 endOfLine
     many' parseSecurity

-- |Render a list of securities.
renderSecurityList :: [Security] -> Builder
renderSecurityList ls =
  fromText "!Type:Security" <> newline <> mconcat (map renderSecurity ls)

-- Securities Lists ------------------------------------------------------------

-- |The semantic content of a QIF file. (Explicitly this: very little semantic
-- processing has gone into this data structure, and it could contain semantic
-- errors in the underlying file. Checking for these things is your job.)
data QIF = QIF {
       _qifAccounts               :: [Account]
     , _qifCategories             :: [Category]
     , _qifSecurities             :: [Security]
     , _qifInvestmentTransactions :: [(Account, [InvTransaction])]
     , _qifNormalTransactions     :: [(Account, [Transaction])]
     }
 deriving (Eq, Show)

-- |An empty QIF file.
emptyQIF :: QIF
emptyQIF = QIF [] [] [] [] []

makeLensesWith (set generateSignatures False lensRules) ''QIF

-- |The accounts associated with the QIF file. We hope. You might expect that
-- there would be an invariant that 'qifAccounts' would be the same as 'map'
-- 'fst' 'qifInvestmentTransactions' '++' 'map' 'fst' 'qifNormalTransactions'.
-- I would, and it'd be nice if you tried to maintain that in your code. But,
-- unfortuntely, there's nothing in the QIF file format that requires this. So
-- you should probably be careful, and make sure you handle the case in which
-- this item mentions accounts not seen anywhere else, and the case in which
-- 'qifNormalTransactions' and 'qifInvestmentTransactions' suddenly invent new
-- accounts.
qifAccounts :: Lens' QIF [Account]
-- |The list of categories saved in this QIF file. Like 'qifAccounts', there
-- doesn't seem to be anything enforcing consistency in the actual QIF file. So
-- you may find that this list mentions categories not referenced elsewhere --
-- which is not necessarily too surprising -- but also that there may be
-- transactions that mention new categories unlisted in this field.
qifCategories :: Lens' QIF [Category]
-- |A cached list of securities. As with the other fields, be warned, as this is
-- not required to be complete, as far as I can tell.
qifSecurities :: Lens' QIF [Security]
-- |A list of investment accounts and the transactions associated with those
-- accounts. Typically each 'Account' should reference an account in
-- 'qifAccount' and include exactly the same date, but there's nothing in the
-- file structure that enforces this invariant.
qifInvestmentTransactions :: Lens' QIF [(Account, [InvTransaction])]
-- |A list of non-investment accounts and the transactions associated with them.
-- Again, one might expect that each 'Account' here should reference an account
-- in 'qifAccount', and contain exactly the same data, but there's nothing in
-- the file structure that enforces this constraint.
qifNormalTransactions :: Lens' QIF [(Account, [Transaction])] 

-- |Parse a QIF file. This function is purely a syntactic parse, and makes no
-- attempt to verify that the data it parses makes sense. So please be a bit
-- paranoid with all the numbers and strings you receive, and perform any
-- validation you need on your own. Also, this function assumes that it is
-- parsing only a QIF file, and that it should run to the end of the input.
parseQIF :: Parser QIF
parseQIF = go emptyQIF
 where
  go base = choice [ add' base qifAccounts   parseAccountList
                   , add' base qifCategories parseCategoryList
                   , add' base qifSecurities parseSecurityList
                   , getTransactions base
                   , endOfInput >> return base
                   ]
  --
  add' :: QIF -> ASetter' QIF [a] -> Parser [a] -> Parser QIF
  add' base field getter =
    do list <- getter
       go (over field (++ list) base)
  --
  getTransactions base =
    do acc <- parseAccountHeader
       case view accountType acc of
         BankAccount       ->
           getts parseBankEntryList qifNormalTransactions base acc
         CashAccount       ->
           getts parseCashEntryList qifNormalTransactions base acc
         CreditCardAccount ->
           getts parseCreditCardEntryList qifNormalTransactions base acc
         InvestmentAccount ->
           getts parseInvestmentEntries qifInvestmentTransactions base acc
         AssetAccount      ->
           getts parseAssetEntryList qifNormalTransactions base acc
         LiabilityAccount  ->
           getts parseLiabilityEntryList qifNormalTransactions base acc
  --
  getts :: Parser [a] -> ASetter' QIF [(Account,[a])] ->
           QIF -> Account ->
           Parser QIF
  getts listParser field base account =
    do list <- listParser
       go (over field (++ [(account, list)]) base)

-- |Render out a QIF File. Because it's the order I've seen in my early example
-- QIF files, this renders in the following order: account list, category list,
-- investment accounts and their transactions, non-investment accounts and their
-- transactions, and then security lists.
renderQIF :: QIF -> Builder
renderQIF qif =
  renderAccountList (view qifAccounts qif) <>
  renderCategoryList (view qifCategories qif) <>
  mconcat
    (map (\ (acc,trans) -> renderAccountHeader acc <> renderInvestmentEntries trans)
         (view qifInvestmentTransactions qif)) <>
  mconcat
    (map (\ (acc,trans) -> renderAccountHeader acc <>
            case view accountType acc of
              BankAccount       -> renderBankEntryList trans
              CashAccount       -> renderCashEntryList trans
              CreditCardAccount -> renderCreditCardEntryList trans
              InvestmentAccount -> error "Investment account in normal list?"
              AssetAccount      -> renderAssetEntryList trans
              LiabilityAccount  -> renderLiabilityEntryList trans)
          (view qifNormalTransactions qif)) <>
  renderSecurityList (view qifSecurities qif)
