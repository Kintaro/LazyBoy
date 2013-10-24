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

getVarByOpCode :: OpCode -> Cpu s Operand
getVarByOpCode = getVar . getRegByOpCode

execute :: OpCode -> Cpu s Int
execute op 
	| op == 0x00 = nop
	| op == 0x01 = fetchWideImmediate >>= ldWI bReg cReg
	| op == 0x02 = ldWM bReg cReg
	| op == 0x03 = incW bReg cReg
	| op == 0x04 = incR bReg
	| op == 0x05 = decR bReg
	| op == 0x06 = fetchImmediate >>= ldI bReg
	| op == 0x07 = rlca
	| op == 0x08 = fetchWideImmediate >>= ldMSP
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
	| op == 0x18 = fetchImmediate >>= jr True
	| op == 0x19 = getDe >>= addHL
	| op == 0x1A = getDe >>= ldAM
	| op == 0x1B = decW dReg eReg
	| op == 0x1C = incR eReg
	| op == 0x1D = decR eReg
	| op == 0x1E = fetchImmediate >>= ldI eReg
	| op == 0x20 = getZeroFlag >>= (\z -> fetchImmediate >>= jr (not z))
	| op == 0x21 = fetchWideImmediate >>= ldWI hReg lReg
	| op == 0x22 = ldWM hReg lReg >>= (\x -> alterHl (+ 1) >> return x)
	| op == 0x23 = incW hReg lReg
	| op == 0x24 = incR hReg
	| op == 0x25 = decR hReg
	| op == 0x26 = fetchImmediate >>= ldI hReg
	| op == 0x28 = getZeroFlag >>= (\z -> fetchImmediate >>= jr z)
	| op == 0x29 = getHl >>= addHL
	| op == 0x2A = getHl >>= ldAM >>= (\x -> alterHl (+ 1) >> return x)
	| op == 0x2B = decW hReg lReg
	| op == 0x2C = incR lReg
	| op == 0x2D = decR lReg
	| op == 0x2E = fetchImmediate >>= ldI lReg
	| op == 0x38 = getCarryFlag >>= (\c -> fetchImmediate >>= jr (not c))
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
	| op == 0x86 = getHl >>= readMemory >>= add
	| op <= 0x87 = getVarByOpCode op >>= add
	| op == 0x8E = getHl >>= readMemory >>= adc
	| op <= 0x8F = getVarByOpCode op >>= adc
	| op == 0x96 = getHl >>= readMemory >>= sub
	| op <= 0x97 = getVarByOpCode op >>= sub
	| op == 0x9E = getHl >>= readMemory >>= sbc
	| op <= 0x9F = getVarByOpCode op >>= sbc 
	| op == 0xA6 = andM
	| op <= 0xA7 = andR $ getRegByOpCode op
	| op == 0xAE = xorM
	| op <= 0xAF = xorR $ getRegByOpCode op
	| op == 0xB6 = orM
	| op <= 0xB7 = orR $ getRegByOpCode op
	| op == 0xC0 = getZeroFlag >>= \z -> ret (20, 8) (not z)
	| op == 0xC1 = pop bReg cReg
	| op == 0xC2 = getZeroFlag >>= (\z -> fetchWideImmediate >>= jp (16, 12) (not z))
	| op == 0xC3 = fetchWideImmediate >>= jp (16, 16) True
	| op == 0xC4 = getZeroFlag >>= (\z -> fetchWideImmediate >>= call (24, 12) (not z))
	| op == 0xC5 = getBc >>= push
	| op == 0xC6 = fetchImmediate >>= add
	| op == 0xC7 = rst 0x00
	| op == 0xC8 = getZeroFlag >>= \z -> ret (20, 8) z
	| op == 0xC9 = ret (16, 16) True
	| op == 0xCA = getZeroFlag >>= (\z -> fetchWideImmediate >>= jp (16, 12) z)
	| op == 0xCB = fetch >>= executeCB
	| op == 0xCC = getZeroFlag >>= (\z -> fetchWideImmediate >>= call (24, 12) z)
	| op == 0xCD = fetchWideImmediate >>= call (24, 24) True
	| op == 0xCF = rst 0x08
	| op == 0xD0 = getCarryFlag >>= \c -> ret (20, 8) (not c)
	| op == 0xD1 = pop dReg eReg
	| op == 0xD2 = getCarryFlag >>= (\c -> fetchWideImmediate >>= jp (16, 12) (not c))
	| op == 0xD4 = getCarryFlag >>= (\c -> fetchWideImmediate >>= call (24, 12) (not c))
	| op == 0xD5 = getDe >>= push
	| op == 0xD7 = rst 0x10
	| op == 0xD8 = getCarryFlag >>= \c -> ret (20, 8) c
	| op == 0xD8 = reti
	| op == 0xDA = getCarryFlag >>= (\c -> fetchWideImmediate >>= jp (16, 12) c)
	| op == 0xDC = getCarryFlag >>= (\c -> fetchWideImmediate >>= call (24, 12) c)
	| op == 0xDF = rst 0x18
	| op == 0xE1 = pop hReg lReg
	| op == 0xE5 = getHl >>= push
	| op == 0xE7 = rst 0x20
	| op == 0xE9 = getHl >>= jp (4, 4) True
	| op == 0xEF = rst 0x28
	| op == 0xF1 = pop aReg fReg
	| op == 0xF5 = getWideVar aReg fReg >>= push
	| op == 0xF7 = rst 0x30
	| op == 0xFF = rst 0x38

executeCB :: OpCode -> Cpu s Int
executeCB = undefined

{- Arithmetic -}

add :: Operand -> Cpu s Int
add x = return x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

adc :: Operand -> Cpu s Int
adc x = return x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

addHL :: WideOperand -> Cpu s Int
addHL x = alterHl_ (+ x) >> return 8

sub :: Operand -> Cpu s Int
sub x = return x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

sbc :: Operand -> Cpu s Int
sbc x = return x >>= alterAccumulator . (+) >>= checkAndSetZeroFlag >> return 4

incW :: Register s -> Register s -> Cpu s Int
incW x y = alterWideVar_ x y (+ 1) >> return 8

decW :: Register s -> Register s -> Cpu s Int
decW x y = alterWideVar_ x y (flip (-) 1) >> return 8

incR :: Register s -> Cpu s Int
incR x = alterVar x (+ 1) >>= checkAndSetZeroFlag >> return 4

decR :: Register s -> Cpu s Int
decR x = alterVar x (flip (-) 1) >>= checkAndSetZeroFlag >> return 4

swap :: Operand -> Operand
swap x = ((x .&. 0x0F) `shiftL` 4) .|. ((x .&. 0xF0) `shiftR` 4)

swapR :: Register s -> Cpu s Int
swapR x = alterVar x swap >> setCarryFlag False >> setHalfCarryFlag False >> return 8

swapM :: Cpu s Int
swapM = getHl >>= flip alterMemory swap >> setCarryFlag False >> setHalfCarryFlag False >> return 16

cpl :: Cpu s Int
cpl = alterVar_ aReg (`xor` 0xFF) >> return 4

{- Logical -} 

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

{- Rotate / Shift -}

rlca :: Cpu s Int
rlca = alterVar_ aReg (`shiftL` 1) >> return 4

rrca :: Cpu s Int
rrca = alterVar_ aReg (`shiftR` 1) >> return 4

{- Loadcommands -}

ldWI :: Register s -> Register s -> WideOperand -> Cpu s Int
ldWI x y v = setWideVar x y v >> return 12

ldWM :: Register s -> Register s -> Cpu s Int
ldWM x y = getWideVar x y >>= (\addr -> getVar aReg >>= writeMemory addr) >> return 8

ldMSP :: Address -> Cpu s Int
ldMSP x = getSp >>= writeWideMemory x >> return 20

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

{- 16 Bit Loadcommands -}

pop :: Register s -> Register s -> Cpu s Int
pop x y = getSp >>= readWideMemory >>= setWideVar x y >> alterSp (+ 2) >> return 12

push :: WideOperand -> Cpu s Int
push x = alterSp (flip (-) 2) >>= writeWideMemory x >> return 16

{- Jump commands -}

jr :: Bool -> Operand -> Cpu s Int
jr f r = if f then alterPc (+ fromIntegral r) >> return 12 else return 8

jp :: (Int, Int) -> Bool -> Address -> Cpu s Int
jp (a, b) f r = if f then setPc r >> return a else return b

ret :: (Int, Int) -> Bool -> Cpu s Int
ret (a, b) f = if f then getSp >>= readWideMemory >>= setPc >> alterSp (+ 2) >> return a else return b

reti :: Cpu s Int
reti = return 16

call :: (Int, Int) -> Bool -> Address -> Cpu s Int
call (a, b) f r = if f then alterSp (flip (-) 2) >>= (\addr -> getPc >>= writeWideMemory addr) >> setPc r >> return a else return b

rst :: Address -> Cpu s Int
rst r = call (16, 16) True r

{- CPU control -}

nop :: Cpu s Int
nop = return 4

ccf :: Cpu s Int
ccf = alterCarryFlag_ not >> return 4

scf :: Cpu s Int
scf = setCarryFlag True >> return 4