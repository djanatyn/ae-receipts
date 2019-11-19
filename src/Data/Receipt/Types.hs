{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}

module Data.Receipt.Types
  ( -- * Parser
    Parser,

    -- * Contact Header
    AddressLine (..),
    Address,
    Phone (..),
    ContactInfo (..),

    -- * Transaction Header
    Transaction (..),

    -- * Purchases
    DiscountType (..),
    Discount (..),
    Purchase (..),

    -- * Receipt
    Receipt (..),
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

newtype AddressLine = AddressLine Text deriving (Show)

type Address = [AddressLine]

newtype Phone = Phone Text deriving (Show)

data ContactInfo
  = ContactInfo
      { contactPhone :: Phone,
        contactAddress :: Address
      }
  deriving (Show)

data Transaction
  = Transaction
      { transactionDate :: Text,
        transactionTime :: Text,
        transactionTransactionID :: Text,
        transactionStoreID :: Text,
        transactionRegister :: Text,
        transactionCashier :: Text,
        transactionID :: Text,
        transactionMemberID :: Text
      }
  deriving (Show)

data Purchase
  = Purchase
      { purchaseName :: Text,
        purchaseSalePrice :: Text,
        purchaseID :: Text,
        purchaseQuantity :: Text,
        purchasePrice :: Text,
        purchaseDiscount :: Maybe [Discount]
      }
  deriving (Show)

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

data Receipt
  = Receipt
      { receiptContact :: ContactInfo,
        receiptTransaction :: Transaction,
        receiptPurchases :: [Purchase]
      }
  deriving (Show)
