module LazyBoy.Instructions where

import Data.Bits
import Data.STRef
import LazyBoy.Cpu

getRegByOpCode :: OpCode -> Register s
getRegByOpCode op
	| op .&. 0xF == 0x00 = bReg
	| op .&. 0xF == 0x01 = cReg
	| op .&. 0xF == 0x02 = dReg
	| op .&. 0xF == 0x03 = eReg
	| op .&. 0xF == 0x04 = hReg
	| op .&. 0xF == 0x05 = lReg
	| op .&. 0xF == 0x06 = hReg
	| op .&. 0xF == 0x07 = aReg
	| op .&. 0xF == 0x08 = bReg
	| op .&. 0xF == 0x09 = cReg
	| op .&. 0xF == 0x0A = dReg
	| op .&. 0xF == 0x0B = eReg
	| op .&. 0xF == 0x0C = hReg
	| op .&. 0xF == 0x0D = lReg
	| op .&. 0xF == 0x0E = hReg
	| otherwise          = aReg

execute :: OpCode -> Cpu s Int
execute op 
	| op == 0x00 = nop
	| op == 0x01 = fetchWideImmediate >>= ldWI bReg cReg
	| op == 0x02 = ldWM bReg cReg
	| op == 0x03 = incW bReg cReg
	| op == 0x04 = incR bReg
	| op == 0x05 = decR bReg
	| op == 0x06 = fetchImmediate >>= ldI bReg
	| op == 0x07 = undefined
	| op == 0x08 = undefined
	| op == 0x09 = getBc >>= addHL
	| op == 0x0A = getBc >>= ldAM
	| op == 0x0B = decW bReg cReg
	| op == 0x0C = incR cReg
	| op == 0x0D = decR cReg
	| op == 0x0E = fetchImmediate >>= ldI cReg
	| op == 0x0F = undefined
	| op == 0x11 = fetchWideImmediate >>= ldWI dReg eReg
	| op == 0x12 = ldWM dReg eReg
	| op == 0x13 = incW dReg eReg
	| op == 0x14 = incR dReg
	| op == 0x15 = decR dReg
	| op == 0x16 = fetchImmediate >>= ldI dReg
	| op == 0x19 = getDe >>= addHL
	| op == 0x1A = getDe >>= ldAM
	| op == 0x1B = decW dReg eReg
	| op == 0x1C = incR eReg
	| op == 0x1D = decR eReg
	| op == 0x1E = fetchImmediate >>= ldI eReg
	| op == 0x21 = fetchWideImmediate >>= ldWI hReg lReg
	| op == 0x22 = ldWM hReg lReg >>= (\x -> alterHl (+ 1) >> return x)
	| op == 0x23 = incW hReg lReg
	| op == 0x24 = incR hReg
	| op == 0x25 = decR hReg
	| op == 0x26 = fetchImmediate >>= ldI hReg
	| op == 0x29 = getHl >>= addHL
	| op == 0x2A = getHl >>= ldAM >>= (\x -> alterHl (+ 1) >> return x)
	| op == 0x2B = decW hReg lReg
	| op == 0x2C = incR lReg
	| op == 0x2D = decR lReg
	| op == 0x2E = fetchImmediate >>= ldI lReg
	| op == 0x39 = getSp >>= addHL
	| op == 0x46 = ldRM bReg
	| op <= 0x47 = ldRR bReg $ getRegByOpCode op
	| op == 0x4E = ldRM cReg
	| op <= 0x4F = ldRR cReg $ getRegByOpCode op
	| op == 0x56 = ldRM dReg
	| op <= 0x57 = ldRR dReg $ getRegByOpCode op
	| op == 0x5E = ldRM eReg
	| op <= 0x5F = ldRR eReg $ getRegByOpCode op
	| op == 0x66 = ldRM hReg
	| op <= 0x67 = ldRR hReg $ getRegByOpCode op
	| op == 0x6E = ldRM lReg
	| op <= 0x6F = ldRR lReg $ getRegByOpCode op
	| op == 0x76 = halt
	| op <= 0x77 = ldMR $ getRegByOpCode op
	| op == 0x7E = ldRM aReg
	| op <= 0x7F = ldRR aReg $ getRegByOpCode op
	| op == 0x86 = addM
	| op <= 0x87 = addR $ getRegByOpCode op
	| op == 0x8E = adcM
	| op <= 0x8F = adcR $ getRegByOpCode op
	| op == 0x96 = subM
	| op <= 0x97 = subR $ getRegByOpCode op
	| op == 0x9E = sbcM
	| op <= 0x9F = sbcR $ getRegByOpCode op
	| op == 0xA6 = andM
	| op <= 0xA7 = andR $ getRegByOpCode op
	| op == 0xAE = xorM
	| op <= 0xAF = xorR $ getRegByOpCode op
	| op == 0xB6 = orM
	| op <= 0xB7 = orR $ getRegByOpCode op
	| op == 0xCB = fetch >>= executeCB

executeCB :: OpCode -> Cpu s Int
executeCB = undefined

nop :: Cpu s Int
nop = return 4

addR :: Register s -> Cpu s Int
addR x = getVar x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

addM :: Cpu s Int
addM = getHl >>= readMemory >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 8

adcR :: Register s -> Cpu s Int
adcR x = getVar x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

adcM :: Cpu s Int
adcM = getHl >>= readMemory >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 8

addHL :: WideOperand -> Cpu s Int
addHL x = alterHl_ (+ x) >> return 8

subR :: Register s -> Cpu s Int
subR x = getVar x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

subM :: Cpu s Int
subM = getHl >>= readMemory >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 8

sbcR :: Register s -> Cpu s Int
sbcR x = getVar x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

sbcM :: Cpu s Int
sbcM = getHl >>= readMemory >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 8

andR :: Register s -> Cpu s Int
andR x = getVar x >>= alterAccumulator . (.&.) >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag True >> return 4

andM :: Cpu s Int
andM = getHl >>= readMemory >>= alterAccumulator . (.&.) >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag True >> return 8

xorR :: Register s -> Cpu s Int
xorR x = getVar x >>= alterAccumulator . xor >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag False >> return 4

xorM :: Cpu s Int
xorM = getHl >>= readMemory >>= alterAccumulator . xor >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag False >> return 8

orR :: Register s -> Cpu s Int
orR x = getVar x >>= alterAccumulator . (.|.) >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag False >> return 4

orM :: Cpu s Int
orM = getHl >>= readMemory >>= alterAccumulator . (.|.) >>= checkAndSetZeroFlag >> setCarryFlag False >> setHalfCarryFlag False >> return 8

halt :: Cpu s Int
halt = return 4

ldWI :: Register s -> Register s -> WideOperand -> Cpu s Int
ldWI x y v = setWideVar x y v >> return 12

ldWM :: Register s -> Register s -> Cpu s Int
ldWM x y = getWideVar x y >>= (\addr -> getVar aReg >>= writeMemory addr) >> return 8

incW :: Register s -> Register s -> Cpu s Int
incW x y = alterWideVar_ x y (+ 1) >> return 8

decW :: Register s -> Register s -> Cpu s Int
decW x y = alterWideVar_ x y (flip (-) 1) >> return 8

incR :: Register s -> Cpu s Int
incR x = alterVar x (+ 1) >>= checkAndSetZeroFlag >> return 4

decR :: Register s -> Cpu s Int
decR x = alterVar x (flip (-) 1) >>= checkAndSetZeroFlag >> return 4

ldI :: Register s -> Operand -> Cpu s Int
ldI x v = setVar x v >> return 8

ldAM :: Address -> Cpu s Int
ldAM x = readMemory x >>= setVar aReg >> return 8

ldRR :: Register s -> Register s -> Cpu s Int
ldRR x y = getVar y >>= setVar x >> return 4

ldRM :: Register s -> Cpu s Int
ldRM x = getHl >>= readMemory >>= setVar x >> return 8

ldMR :: Register s -> Cpu s Int
ldMR x = getHl >>= (\addr -> getVar x >>= writeMemory addr) >> return 8

swap :: Operand -> Operand
swap x = ((x .&. 0x0F) `shiftL` 4) .|. ((x .&. 0xF0) `shiftR` 4)

swapR :: Register s -> Cpu s Int
swapR x = alterVar x swap >> setCarryFlag False >> setHalfCarryFlag False >> return 8

swapM :: Cpu s Int
swapM = getHl >>= flip alterMemory swap >> setCarryFlag False >> setHalfCarryFlag False >> return 16