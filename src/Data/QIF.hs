{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.QIF
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
import           Lens.Micro.TH(makeLenses)

-- Fixed-width Quantities: Currency and Share Counts ---------------------------

data E4
instance HasResolution E4 where
  resolution _ = 10000

type Currency      = Fixed E2
type ShareQuantity = Fixed E4

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

renderQuantity :: HasResolution a => Fixed a -> Builder
renderQuantity = fromString . show

parseCurrency :: Parser Currency
parseCurrency =
  do finally <- option id  (char '-' *> return negate) -- kept to deal with -$1
     _       <- option '$' (char '$')
     amount  <- parseQuantity
     return (finally amount)

renderCurrency :: Bool -> Currency -> Builder
renderCurrency showDollar x =
  neg <> (if showDollar then singleton '$' else mempty) <> renderQuantity x'
 where
  (x',neg) = if x < 0 then (negate x, singleton '-') else (x,mempty)

parseShareQuantity :: Parser ShareQuantity
parseShareQuantity = parseQuantity

renderShareQuantity :: ShareQuantity -> Builder
renderShareQuantity = renderQuantity

digitToNum :: Num a => Char -> a
digitToNum = fromIntegral . digitToInt

-- Account Types ---------------------------------------------------------------

data AccountType = BankAccount
                 | CashAccount
                 | CreditCardAccount
                 | InvestmentAccount
                 | AssetAccount
                 | LiabilityAccount
 deriving (Eq, Read, Show)

parseAccountType :: Parser AccountType
parseAccountType = string "!Type:" *> parseShortAccountType

parseShortAccountType :: Parser AccountType
parseShortAccountType =
  choice [ string "Bank"  *> return BankAccount
         , string "Cash"  *> return CashAccount
         , string "CCard" *> return CreditCardAccount
         , string "Invst" *> return InvestmentAccount
         , string "Oth A" *> return AssetAccount
         , string "Oth L" *> return LiabilityAccount
         ]

renderAccountType :: AccountType -> Builder
renderAccountType acc = fromText "!Type:" <> renderShortAccountType acc

renderShortAccountType :: AccountType -> Builder
renderShortAccountType BankAccount       = fromText "Bank"
renderShortAccountType CashAccount       = fromText "Cash"
renderShortAccountType CreditCardAccount = fromText "CCard"
renderShortAccountType InvestmentAccount = fromText "Invst"
renderShortAccountType AssetAccount      = fromText "Oth A"
renderShortAccountType LiabilityAccount  = fromText "Oth L"

-- Accounts --------------------------------------------------------------------

data Account = Account {
       _accountName        :: Text
     , _accountType        :: AccountType
     , _accountDescription :: Text
     , _accountCreditLimit :: Maybe Currency
     , _accountBalanceDate :: Maybe Day
     , _accountBalance     :: Currency
     }
 deriving (Eq, Show)

pennies :: Currency -> Integer
pennies x = truncate (x * 100.0)

makeLenses ''Account

emptyAccount :: Account
emptyAccount = Account {
    _accountName        = ""
  , _accountType        = BankAccount
  , _accountDescription = ""
  , _accountCreditLimit = Nothing
  , _accountBalanceDate = Nothing
  , _accountBalance     = 0
  }

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

parseDate :: Parser Day
parseDate =
  do str <- Atto.takeWhile isPrint
     intime <- parseTimeM False defaultTimeLocale "%-m/%e/%y" (Text.unpack str)
     let (year, mon, day) = toGregorian intime
     if year < 2000
        then return (fromGregorian (2000 + (year `mod` 100)) mon day)
        else return intime

renderDate :: Day -> Builder
renderDate = fromString . formatTime defaultTimeLocale "%-m/%e/%y"

-- -----------------------------------------------------------------------------

parseAccountList :: Parser [Account]
parseAccountList =
  do _    <- string "!Option:AutoSwitch" *> many1 endOfLine
     _    <- string "!Account"           *> many1 endOfLine
     accs <- many' parseAccount
     _    <- string "!Clear:AutoSwitch"  *> many1 endOfLine
     return accs

renderAccountList :: [Account] -> Builder
renderAccountList accs =
  fromString "!Option:AutoSwitch\n" <>
  fromString "!Account\n"           <>
  mconcat (map renderAccount accs)  <>
  fromString "!Clear:AutoSwitch\n"

-- Categories ------------------------------------------------------------------

data Category = Category {
       _catName            :: Text
     , _catDescription     :: Text
     , _catKind            :: CategoryKind
     , _catIsTaxRelated    :: Bool
     , _catBudgetAmount    :: Maybe Currency
     , _catTaxScheduleInfo :: Maybe Word
     }
 deriving (Eq, Show)

data CategoryKind = Income | Expense
 deriving (Eq, Show)

emptyCategory :: Category
emptyCategory = Category {
    _catName            = ""
  , _catDescription     = ""
  , _catKind            = Expense
  , _catIsTaxRelated    = False
  , _catBudgetAmount    = Nothing
  , _catTaxScheduleInfo = Nothing
  }

makeLenses ''Category

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

parseCategoryList :: Parser [Category]
parseCategoryList =
  do _    <- string "!Type:Cat"
     _    <- many1 endOfLine
     many' parseCategory

renderCategoryList :: [Category] -> Builder
renderCategoryList cats =
  fromString "!Type:Cat\n" <> mconcat (map renderCategory cats)


-- Account Headers -------------------------------------------------------------

parseAccountHeader :: Parser Account
parseAccountHeader =
  do _ <- string "!Account"
     _ <- many1  endOfLine
     parseAccount

renderAccountHeader :: Account -> Builder
renderAccountHeader acc = fromString "!Account\n" <> renderAccount acc

-- Bank Entries ----------------------------------------------------------------

data SplitItem = SplitItem {
       _entryMemo     :: Text
     , _entryAmount   :: Currency
     , _entryCategory :: Text
     }
 deriving (Eq, Show)

emptySplitItem :: SplitItem
emptySplitItem = SplitItem "" 0 ""

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

emptyTransaction :: Transaction
emptyTransaction =
  Transaction (fromGregorian 2000 1 1) "" "" 0 Nothing Nothing False False []

makeLenses ''SplitItem
makeLenses ''Transaction

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

parseSplit :: SplitItem -> Parser SplitItem
parseSplit base = option base $
  do label <- satisfy (inClass "E$")
     case label of
       'E' -> getP parseSplit base entryMemo   parseString
       '$' -> getP parseSplit base entryAmount parseCurrency
       _   -> fail "Unknown, out of scope split entry label."

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

parseTransactionList :: Parser [Transaction]
parseTransactionList =
  do _ <- string "!Type:Bank" >> many1 endOfLine
     many' parseTransaction

renderTransactionList :: [Transaction] -> Builder
renderTransactionList ls =
  fromText "!Type:Bank" <> newline <> mconcat (map renderTransaction ls)

-- Investment Entries ---------------------------------------------------------

data TradeInfo = TradeInfo {
       _tradeDate        :: Day
     , _tradeSecurity    :: Text
     , _tradeSharePrice  :: Maybe Currency
     , _tradeQuantity    :: Maybe ShareQuantity
     , _tradeCommission  :: Maybe Currency
     , _tradeTotalAmount :: Currency
     }
 deriving (Eq, Show)

emptyTrade :: Day -> TradeInfo
emptyTrade day = TradeInfo day "" Nothing Nothing Nothing 0

makeLenses ''TradeInfo

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

emptyTransfer :: Day -> TransferInfo
emptyTransfer day = TransferInfo day "" "" 0 False "" []

makeLenses ''TransferInfo

data InvTransaction = Buy           TradeInfo
                    | Sell          TradeInfo
                    | Transfer      TransferInfo
                    | Dividend      TradeInfo
                    | Interest Text TradeInfo
 deriving (Eq, Show)

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

parseInvestmentEntries :: Parser [InvTransaction]
parseInvestmentEntries =
  do _ <- string "!Type:Invst" >> many1 endOfLine
     many' parseInvTransaction

renderInvestmentEntries :: [InvTransaction] -> Builder
renderInvestmentEntries ents =
  fromText "!Type:Invst" <> newline <> mconcat (map renderInvTransaction ents)


-- Cash Entry Lists ------------------------------------------------------------

parseCashEntryList :: Parser [Transaction]
parseCashEntryList =
  do _ <- string "!Type:Cash" >> many1 endOfLine
     many' parseTransaction

renderCashEntryList :: [Transaction] -> Builder
renderCashEntryList ls =
  fromText "!Type:Cash" <> newline <> mconcat (map renderTransaction ls)

-- Credit Card Entry Lists -----------------------------------------------------

parseCreditCardEntryList :: Parser [Transaction]
parseCreditCardEntryList =
  do _ <- string "!Type:CCard" >> many1 endOfLine
     many' parseTransaction

renderCreditCardEntryList :: [Transaction] -> Builder
renderCreditCardEntryList ls =
  fromText "!Type:CCard" <> newline <> mconcat (map renderTransaction ls)

-- Asset Entry Lists -----------------------------------------------------------

parseAssetEntryList :: Parser [Transaction]
parseAssetEntryList =
  do _ <- string "!Type:Oth A" >> many1 endOfLine
     many' parseTransaction

renderAssetEntryList :: [Transaction] -> Builder
renderAssetEntryList ls =
  fromText "!Type:Oth A" <> newline <> mconcat (map renderTransaction ls)

-- Liability Entry Lists -------------------------------------------------------

parseLiabilityEntryList :: Parser [Transaction]
parseLiabilityEntryList =
  do _ <- string "!Type:Oth L" >> many1 endOfLine
     many' parseTransaction

renderLiabilityEntryList :: [Transaction] -> Builder
renderLiabilityEntryList ls =
  fromText "!Type:Oth L" <> newline <> mconcat (map renderTransaction ls)

-- Security Types --------------------------------------------------------------

data SecurityType = Stock | Bond | CD | MutualFund | Index | ETF | MoneyMarket
                  | PreciousMetal | Commodity | StockOption | Other
 deriving (Eq, Show)

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

data Security = Security {
       _secName   :: Text
     , _secTicker :: Text
     , _secType   :: SecurityType
     , _secGoal   :: Maybe Text
     }
 deriving (Eq, Show)

emptySecurity :: Security
emptySecurity = Security "" "" Stock Nothing

makeLenses ''Security

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

renderSecurity :: Security -> Builder
renderSecurity s =
  put  'N' s secName   fromText           <>
  put  'S' s secTicker fromText           <>
  put  'T' s secType   renderSecurityType <>
  putm 'G' s secGoal   fromText           <>
  singleton '^' <> newline

-- Securities Lists ------------------------------------------------------------

parseSecurityList :: Parser [Security]
parseSecurityList =
  do _ <- string "!Type:Security" >> many1 endOfLine
     many' parseSecurity

renderSecurityList :: [Security] -> Builder
renderSecurityList ls =
  fromText "!Type:Security" <> newline <> mconcat (map renderSecurity ls)

-- Securities Lists ------------------------------------------------------------

data QIF = QIF {
       _qifAccounts          :: [Account]
     , _qifCategories        :: [Category]
     , _qifSecurities        :: [Security]
     , _qifInvestmentActions :: [(Account, [InvTransaction])]
     , _qifNormalActions     :: [(Account, [Transaction])]
     }
 deriving (Eq, Show)

emptyQIF :: QIF
emptyQIF = QIF [] [] [] [] []

makeLenses ''QIF

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
           getts parseTransactionList qifNormalActions base acc
         CashAccount       ->
           getts parseCashEntryList qifNormalActions base acc
         CreditCardAccount ->
           getts parseCreditCardEntryList qifNormalActions base acc
         InvestmentAccount ->
           getts parseInvestmentEntries qifInvestmentActions base acc
         AssetAccount      ->
           getts parseAssetEntryList qifNormalActions base acc
         LiabilityAccount  ->
           getts parseLiabilityEntryList qifNormalActions base acc
  --
  getts :: Parser [a] -> ASetter' QIF [(Account,[a])] ->
           QIF -> Account ->
           Parser QIF
  getts listParser field base account =
    do list <- listParser
       go (over field (++ [(account, list)]) base)

