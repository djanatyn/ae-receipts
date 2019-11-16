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

data Receipt
  = Receipt
      { receiptContact :: ContactInfo,
        receiptTransaction :: Transaction
      }
  deriving (Show)
