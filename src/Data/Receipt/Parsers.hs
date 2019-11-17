{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Receipt.Parsers
  ( -- * Receipt Sections
    pContactInfo,
    pTransaction,

    -- * Entire Receipt
    pReceipt,
  )
where

import Data.Receipt.Types
  ( AddressLine (..),
    ContactInfo (..),
    Parser,
    Phone (..),
    Receipt (..),
    Transaction (..),
  )
import Data.Text (Text, append, pack, replicate)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)
import Prelude hiding (replicate)

-- | When separating tokens, skip leftover space characters.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany spaceChar)

-- | Reads a labelled value.
-- @
-- >>> parseTest (rLabel "Store: " (many numberChar)) "Store: 00723"
-- "00723"
-- @
rLabel :: Text -> Parser a -> Parser a
rLabel prefix parser = string prefix >> parser

-- | Parse separator for receipt sections.
pSeparator :: Parser Text
pSeparator = string $ replicate 42 "_"

-- | Parse a phone-number.
-- @
-- >>> parseTest (lexeme pPhoneNumber) $ pack "123-456-7890"
-- phoneNumber> IN: "123-456-7890"
-- phoneNumber> MATCH (COK): "123-456-7890"
-- phoneNumber> VALUE: "123-456-7890"
-- "123-456-7890"
-- @
pPhoneNumber :: Parser Phone
pPhoneNumber = dbg "phoneNumber" . try $ do
  areaCode <- count 3 digitChar
  _ <- char '-'
  centralOffice <- count 3 digitChar
  _ <- char '-'
  lineNumber <- count 4 digitChar
  return . Phone . pack $ areaCode ++ "-" ++ centralOffice ++ "-" ++ lineNumber

-- | Parse lines of the address header.
pAddressLine :: Parser AddressLine
pAddressLine =
  dbg "addressLine" . try $
    AddressLine . pack
      <$> manyTill
        (alphaNumChar <|> spaceChar <|> char ',')
        (try $ count 2 spaceChar)

-- | Parse '/' separated date.
pTransactionDate :: Parser Text
pTransactionDate =
  dbg "transaction date" . try $
    pack
      <$> manyTill (numberChar <|> char '/') spaceChar

-- | Parse 12-hour time.
pTransactionTime :: Parser Text
pTransactionTime =
  dbg "transaction time" . try $ do
    time <- pack <$> manyTill (numberChar <|> char ':') spaceChar
    period <- string "PM" <|> string "AM"
    return $ time `append` period

-- | Parse transaction header.
pTransaction :: Parser Transaction
pTransaction = dbg "transaction header" . try $ do
  transactionDate <- lexeme pTransactionDate
  transactionTime <- lexeme pTransactionTime
  transactionTransactionID <-
    dbg "transID" . lexeme
      $ rLabel "Trans.: "
      $ pack <$> many numberChar
  transactionStoreID <-
    dbg "storeID" . lexeme
      $ rLabel "Store: "
      $ pack <$> many numberChar
  transactionRegister <-
    dbg "regID" . lexeme
      $ rLabel "Reg.: "
      $ pack <$> many numberChar
  transactionCashier <-
    dbg "cashierID" . lexeme
      $ rLabel "Cashier: "
      $ pack <$> many alphaNumChar
  transactionID <-
    dbg "transaction ID" . lexeme
      $ rLabel "Sales: "
      $ pack <$> many alphaNumChar
  transactionMemberID <-
    dbg "memberID" . lexeme
      $ rLabel "Member ID: "
      $ pack <$> many numberChar
  _ <- lexeme $ string "Sale"
  return Transaction {..}

-- | Parse `ContactInfo`.
pContactInfo :: Parser ContactInfo
pContactInfo = do
  (contactAddress, contactPhone) <-
    someTill_
      (lexeme pAddressLine)
      (lexeme pPhoneNumber)
  return $ ContactInfo {..}

-- | Parse `Receipt`. Consumes all input.
pReceipt :: Parser Receipt
pReceipt = do
  skipMany spaceChar
  receiptContact <- pContactInfo
  _ <- lexeme pSeparator
  receiptTransaction <- lexeme pTransaction
  _ <- lexeme pSeparator
  _ <-
    lexeme
      $ rLabel "Employee Number: "
      $ many (numberChar <|> char '*')
  _ <- takeRest
  return $ Receipt {..}

-- * Discounts

data Purchase
  = Purchase
      { purchaseName :: Text,
        purchaseQuantity :: Int,
        purchaseID :: Text,
        purcahseDiscount :: Maybe [Discount]
      }

data DiscountType :: * where
  DealDiscount :: DiscountType
  EmployeeDiscount :: DiscountType
  deriving (Show)

data Discount
  = Discount
      { discountType :: DiscountType,
        discountAmount :: Text,
        discountText :: Text
      }
  deriving (Show)

-- | Parse `Purchase` with optional `Discount`s applied.
pPurchase :: Parser Text
pPurchase = dbg "purchase" . try $ do
  undefined

-- | Parse a `Discount`.
pDiscount :: Parser Discount
pDiscount = dbg "discount" . try $ do
  discountType <-
    choice
      [ EmployeeDiscount <$ string "Item Discount ",
        DealDiscount <$ string "Deal Discount Amt."
      ]
  case discountType of
    EmployeeDiscount -> do
      discountText <- lexeme pPercent
      discountAmount <- lexeme pAmount
      _ <- lexeme $ string "Employee Discount"
      return Discount {..}
    DealDiscount -> do
      discountAmount <- lexeme pAmount
      discountText <- lexeme pDealText
      return Discount {..}
  where
    pDealText :: Parser Text
    pDealText =
      pack
        <$> manyTill
          (alphaNumChar <|> char '.' <|> char '%')
          (count 2 spaceChar)
    pPercent :: Parser Text
    pPercent =
      pack
        <$> manyTill
          (numberChar <|> char '.' <|> char '%')
          spaceChar
    pAmount :: Parser Text
    pAmount =
      pack
        <$> between
          (char '(')
          (char ')')
          (many (numberChar <|> char '.'))
