module LazyBoy.Video where

import Data.Bits
import LazyBoy.Cpu

lcdControlAddress :: Address
lcdControlAddress = 0xFF40

--
getLcdControl :: Cpu s Operand
getLcdControl = readMemory lcdControlAddress

-- 
setLcdControl :: Operand -> Cpu s ()
setLcdControl = writeMemory lcdControlAddress

--
getLcdEnabled :: Cpu s Bool
getLcdEnabled = readMemory lcdControlAddress >>= getOperandFlagBit 7

--
setLcdEnabled :: Bool -> Cpu s ()
setLcdEnabled = readMemory lcdControlAddress >>= writeMemory lcdControlAddress . (.|.) x

--
getWindowDisplayEnabled :: Cpu s Bool
getWindowDisplayEnabled = readMemory lcdControlAddress >>= getOperandFlagBit 5

--
--  Bit 7 - LCD Display Enable             (0=Off, 1=On)
--  Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 5 - Window Display Enable          (0=Off, 1=On)
--  Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
--  Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
--  Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
--  Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)