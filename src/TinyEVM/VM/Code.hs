module TinyEVM.VM.Code
  ( -- * The @Code@ type
    Code(..)
    -- * Operations
  , encode
  , decode
  ) where

import Data.Aeson (FromJSON, parseJSON, withText)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as ByteString

import TinyEVM.VM.Instruction (Instruction, InvalidOpcode)
import qualified TinyEVM.VM.Instruction as Instruction

-- | Code is just a list of instructions.
newtype Code = Code { unCode :: [Instruction] }
  deriving (Eq, Show)

instance FromJSON Code where
  parseJSON = withText "Text" parse
    where
      parse :: Text -> Parser Code
      parse s = either (fail . show) return (decode $ toBytes s)

      toBytes :: Text -> [Word8]
      toBytes = ByteString.unpack . encodeUtf8

-- | Encodes a sequence of VM instructions.
encode :: Code -> [Word8]
encode = concatMap Instruction.encode . unCode

-- | Decodes a VM bytecode into sequence of instructions.
decode :: [Word8] -> Either InvalidOpcode Code
decode bytecode = Code . reverse <$> go [] bytecode
  where
    go :: [Instruction] -> [Word8] -> Either InvalidOpcode [Instruction]
    go acc [] = Right acc
    go acc (b:bs) = do
      (instr, rest) <- Instruction.decodeOne b bs
      go (instr:acc) rest
