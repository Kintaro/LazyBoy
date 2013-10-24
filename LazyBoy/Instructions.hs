module LazyBoy.Instructions where

import Debug.Trace
import Prelude hiding (and, or)
import Data.Bits hiding (bit)
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
	| op == 0x0F = rrca
	| op == 0x11 = fetchWideImmediate >>= ldWI dReg eReg
	| op == 0x12 = ldWM dReg eReg
	| op == 0x13 = incW dReg eReg
	| op == 0x14 = incR dReg
	| op == 0x15 = decR dReg
	| op == 0x16 = fetchImmediate >>= ldI dReg
	| op == 0x17 = rla
	| op == 0x18 = fetchImmediate >>= jr True
	| op == 0x19 = getDe >>= addHL
	| op == 0x1A = getDe >>= ldAM
	| op == 0x1B = decW dReg eReg
	| op == 0x1C = incR eReg
	| op == 0x1D = decR eReg
	| op == 0x1E = fetchImmediate >>= ldI eReg
	| op == 0x1F = rra
	| op == 0x20 = getZeroFlag >>= (\z -> fetchImmediate >>= jr (not z))
	| op == 0x21 = fetchWideImmediate >>= ldWI hReg lReg
	| op == 0x22 = ldWM hReg lReg >>= (\x -> alterHl (+ 1) >> return x)
	| op == 0x23 = incW hReg lReg
	| op == 0x24 = incR hReg
	| op == 0x25 = decR hReg
	| op == 0x26 = fetchImmediate >>= ldI hReg
	| op == 0x28 = getZeroFlag >>= (\z -> fetchImmediate >>= jr z)
	| op == 0x29 = getHl >>= addHL
	| op == 0x2A = getHl >>= ldRM aReg >> alterHl_ (1 +) >> return 8
	| op == 0x2B = decW hReg lReg
	| op == 0x2C = incR lReg
	| op == 0x2D = decR lReg
	| op == 0x2E = fetchImmediate >>= ldI lReg
	| op == 0x38 = getCarryFlag >>= (\c -> fetchImmediate >>= jr (not c))
	| op == 0x39 = getSp >>= addHL
	| op == 0x3A = getHl >>= ldRM aReg >> alterHl_ (flip (-) 1) >> return 8
	| op == 0x46 = getHl >>= ldRM bReg
	| op <= 0x47 = ldRR bReg $ getRegByOpCode op
	| op == 0x4E = getHl >>= ldRM cReg
	| op <= 0x4F = ldRR cReg $ getRegByOpCode op
	| op == 0x56 = getHl >>= ldRM dReg
	| op <= 0x57 = ldRR dReg $ getRegByOpCode op
	| op == 0x5E = getHl >>= ldRM eReg
	| op <= 0x5F = ldRR eReg $ getRegByOpCode op
	| op == 0x66 = getHl >>= ldRM hReg
	| op <= 0x67 = ldRR hReg $ getRegByOpCode op
	| op == 0x6E = getHl >>= ldRM lReg
	| op <= 0x6F = ldRR lReg $ getRegByOpCode op
	| op == 0x76 = halt
	| op <= 0x77 = getHl >>= \hl -> ldMR hl (getRegByOpCode op)
	| op == 0x7E = getHl >>= ldRM aReg
	| op <= 0x7F = ldRR aReg $ getRegByOpCode op
	| op == 0x86 = getHl >>= readMemory >>= add
	| op <= 0x87 = getVarByOpCode op >>= add
	| op == 0x8E = getHl >>= readMemory >>= adc
	| op <= 0x8F = getVarByOpCode op >>= adc
	| op == 0x96 = getHl >>= readMemory >>= sub
	| op <= 0x97 = getVarByOpCode op >>= sub
	| op == 0x9E = getHl >>= readMemory >>= sbc
	| op <= 0x9F = getVarByOpCode op >>= sbc 
	| op == 0xA6 = getHl >>= readMemory >>= and'
	| op <= 0xA7 = getVarByOpCode op >>= and'
	| op == 0xAE = getHl >>= readMemory >>= xor'
	| op <= 0xAF = getVarByOpCode op >>= xor'
	| op == 0xB6 = getHl >>= readMemory >>= or'
	| op <= 0xB7 = getVarByOpCode op >>= or'
	| op == 0xB6 = getHl >>= readMemory >>= cp >> return 8
	| op <= 0xBF = getVarByOpCode op >>= cp 
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
	| op == 0xCE = fetchImmediate >>= adc
	| op == 0xCF = rst 0x08
	| op == 0xD0 = getCarryFlag >>= \c -> ret (20, 8) (not c)
	| op == 0xD1 = pop dReg eReg
	| op == 0xD2 = getCarryFlag >>= (\c -> fetchWideImmediate >>= jp (16, 12) (not c))
	| op == 0xD4 = getCarryFlag >>= (\c -> fetchWideImmediate >>= call (24, 12) (not c))
	| op == 0xD5 = getDe >>= push
	| op == 0xD6 = fetchImmediate >>= sub
	| op == 0xD7 = rst 0x10
	| op == 0xD8 = getCarryFlag >>= \c -> ret (20, 8) c
	| op == 0xD8 = reti
	| op == 0xDA = getCarryFlag >>= (\c -> fetchWideImmediate >>= jp (16, 12) c)
	| op == 0xDC = getCarryFlag >>= (\c -> fetchWideImmediate >>= call (24, 12) c)
	| op == 0xCE = fetchImmediate >>= sbc
	| op == 0xDF = rst 0x18
	| op == 0xE0 = fetchImmediate >>= \n -> ldMR (0xFF00 + fromIntegral n) aReg >> return 12
	| op == 0xE1 = pop hReg lReg
	| op == 0xE2 = getVar cReg >>= \n -> ldMR (0xFF00 + fromIntegral n) aReg >> return 12
	| op == 0xE5 = getHl >>= push
	| op == 0xE6 = fetchImmediate >>= and'
	| op == 0xE7 = rst 0x20
	| op == 0xE9 = getHl >>= jp (4, 4) True
	| op == 0xEE = fetchImmediate >>= xor'
	| op == 0xEF = rst 0x28
	| op == 0xF0 = fetchImmediate >>= \n -> ldRM aReg (0xFF00 + fromIntegral n) >> return 12
	| op == 0xF1 = pop aReg fReg
	| op == 0xF2 = getVar cReg >>= \n -> ldRM aReg (0xFF00 + fromIntegral n) >> return 12
	| op == 0xF3 = di
	| op == 0xF5 = getWideVar aReg fReg >>= push
	| op == 0xF6 = fetchImmediate >>= or'
	| op == 0xF7 = rst 0x30
	| op == 0xF8 = getSp >>= (\sp -> fetchImmediate >>= \x -> setHl (sp + fromIntegral x)) >> return 12
	| op == 0xF9 = getHl >>= setSp >> return 8
	| op == 0xFB = ei
	| op == 0xFE = fetchImmediate >>= cp >> return 8
	| op == 0xFF = rst 0x38

executeCB :: OpCode -> Cpu s Int
executeCB op 
	| op == 0x06 = undefined
	| op <= 0x07 = rlc (getRegByOpCode op)
	| op == 0x0E = undefined
	| op <= 0x0F = rrc (getRegByOpCode op)
	| op == 0x16 = undefined
	| op <= 0x17 = rl (getRegByOpCode op)
	| op == 0x1E = undefined
	| op <= 0x1F = rr (getRegByOpCode op)
	| op == 0x26 = undefined
	| op <= 0x27 = sla (getRegByOpCode op)
	| op == 0x2E = undefined
	| op <= 0x2F = sra (getRegByOpCode op)
	| op == 0x36 = getHl >>= flip alterMemory swap >> return 16
	| op <= 0x37 = swapR $ getRegByOpCode op
	| op == 0x3E = undefined
	| op <= 0x3F = srl (getRegByOpCode op)
	| op == 0x46 = getHl >>= readMemory >>= bit 0 >> return 12
	| op <= 0x47 = getVarByOpCode op >>= bit 0
	| op == 0x4E = getHl >>= readMemory >>= bit 1 >> return 12
	| op <= 0x4F = getVarByOpCode op >>= bit 1
	| op == 0x56 = getHl >>= readMemory >>= bit 2 >> return 12
	| op <= 0x57 = getVarByOpCode op >>= bit 2
	| op == 0x5E = getHl >>= readMemory >>= bit 3 >> return 12
	| op <= 0x5F = getVarByOpCode op >>= bit 3
	| op == 0x66 = getHl >>= readMemory >>= bit 4 >> return 12
	| op <= 0x67 = getVarByOpCode op >>= bit 4
	| op == 0x6E = getHl >>= readMemory >>= bit 5 >> return 12
	| op <= 0x6F = getVarByOpCode op >>= bit 5
	| op == 0x76 = getHl >>= readMemory >>= bit 6 >> return 12
	| op <= 0x77 = getVarByOpCode op >>= bit 6
	| op == 0x7E = getHl >>= readMemory >>= bit 7 >> return 12
	| op <= 0x7F = getVarByOpCode op >>= bit 7
	| op == 0x86 = getHl >>= (\addr -> alterMemory addr (res 0)) >> return 16
	| op <= 0x87 = alterVar_ (getRegByOpCode op) (res 0) >> return 8
	| op == 0x8E = getHl >>= (\addr -> alterMemory addr (res 1)) >> return 16
	| op <= 0x8F = alterVar_ (getRegByOpCode op) (res 1) >> return 8
	| op == 0x96 = getHl >>= (\addr -> alterMemory addr (res 2)) >> return 16
	| op <= 0x97 = alterVar_ (getRegByOpCode op) (res 2) >> return 8
	| op == 0x9E = getHl >>= (\addr -> alterMemory addr (res 3)) >> return 16
	| op <= 0x9F = alterVar_ (getRegByOpCode op) (res 3) >> return 8
	| op == 0xA6 = getHl >>= (\addr -> alterMemory addr (res 4)) >> return 16
	| op <= 0xA7 = alterVar_ (getRegByOpCode op) (res 4) >> return 8
	| op == 0xAE = getHl >>= (\addr -> alterMemory addr (res 5)) >> return 16
	| op <= 0xAF = alterVar_ (getRegByOpCode op) (res 5) >> return 8
	| op == 0xB6 = getHl >>= (\addr -> alterMemory addr (res 6)) >> return 16
	| op <= 0xB7 = alterVar_ (getRegByOpCode op) (res 6) >> return 8
	| op == 0xBE = getHl >>= (\addr -> alterMemory addr (res 7)) >> return 16
	| op <= 0xBF = alterVar_ (getRegByOpCode op) (res 7) >> return 8
	| op == 0xC6 = getHl >>= (\addr -> alterMemory addr (set 0)) >> return 16
	| op <= 0xC7 = alterVar_ (getRegByOpCode op) (set 0) >> return 8
	| op == 0xCE = getHl >>= (\addr -> alterMemory addr (set 1)) >> return 16
	| op <= 0xCF = alterVar_ (getRegByOpCode op) (set 1) >> return 8
	| op == 0xD6 = getHl >>= (\addr -> alterMemory addr (set 2)) >> return 16
	| op <= 0xD7 = alterVar_ (getRegByOpCode op) (set 2) >> return 8
	| op == 0xDE = getHl >>= (\addr -> alterMemory addr (set 3)) >> return 16
	| op <= 0xDF = alterVar_ (getRegByOpCode op) (set 3) >> return 8
	| op == 0xE6 = getHl >>= (\addr -> alterMemory addr (set 4)) >> return 16
	| op <= 0xE7 = alterVar_ (getRegByOpCode op) (set 4) >> return 8
	| op == 0xEE = getHl >>= (\addr -> alterMemory addr (set 5)) >> return 16
	| op <= 0xEF = alterVar_ (getRegByOpCode op) (set 5) >> return 8
	| op == 0xF6 = getHl >>= (\addr -> alterMemory addr (set 6)) >> return 16
	| op <= 0xF7 = alterVar_ (getRegByOpCode op) (set 6) >> return 8
	| op == 0xFE = getHl >>= (\addr -> alterMemory addr (set 7)) >> return 16
	| op <= 0xFF = alterVar_ (getRegByOpCode op) (set 7) >> return 8

{- Arithmetic -}

add :: Operand -> Cpu s Int
add x = (alterAccumulator . (+)) x >>= checkAndSetZeroFlag >> return 4

adc :: Operand -> Cpu s Int
adc x = (alterAccumulator . (+)) x >>= checkAndSetZeroFlag >> return 4

addHL :: WideOperand -> Cpu s Int
addHL x = alterHl_ (+ x) >> return 8

sub :: Operand -> Cpu s Int
sub x = (alterAccumulator . (-)) x >>= checkAndSetZeroFlag >> return 4

sbc :: Operand -> Cpu s Int
sbc x = (alterAccumulator . (-)) x >>= checkAndSetZeroFlag >> return 4

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

cpl :: Cpu s Int
cpl = alterVar_ aReg (`xor` 0xFF) >> return 4

{- Logical -} 

and' :: Operand -> Cpu s Int
and' x = (alterAccumulator . (.&.)) x >>= checkAndSetZeroFlag >> setAddFlag False >> setCarryFlag False >> setHalfCarryFlag True >> return 4

xor' :: Operand -> Cpu s Int
xor' x = (alterAccumulator . xor) x >>= checkAndSetZeroFlag >> setAddFlag False >> setCarryFlag False >> setHalfCarryFlag False >> return 4

or' :: Operand -> Cpu s Int
or' x = (alterAccumulator . (.|.)) x >>= checkAndSetZeroFlag >> setAddFlag False >> setCarryFlag False >> setHalfCarryFlag False >> return 4

halt :: Cpu s Int
halt = return 4

cp :: Operand -> Cpu s Int
cp x = getVar aReg >>= (\a -> case () of
			_ | a - x == 0 -> setVar fReg 0xC0
			  | a - x <  0 -> setVar fReg 0x50
			  | otherwise  -> setVar fReg 0x60) >> return 4

{- Rotate / Shift -}

shiftWithCarry :: Register s -> Int -> (Operand -> Int -> Operand) -> Cpu s Int
shiftWithCarry r t f = getVar r >>= (\a -> setCarryFlag (testBit a t)) >> alterVar r (`f` 1) >>= checkAndSetZeroFlag >> setAddFlag False >> setHalfCarryFlag False >> return 8

shiftThroughCarry :: Register s -> Int -> Int -> (Operand -> Int -> Operand) -> Cpu s Int
shiftThroughCarry r t s f = getCarryFlag >>= (\c -> getVar r >>= (\a -> setCarryFlag (testBit a t)) >> alterVar_ r (`f` 1) >> alterVar r (\x -> if c then setBit x s else clearBit x s)) >>= checkAndSetZeroFlag >> setAddFlag False >> setHalfCarryFlag False >> return 8

rlca :: Cpu s Int
rlca = shiftWithCarry aReg 7 shiftL >> return 4

rla :: Cpu s Int
rla = shiftThroughCarry aReg 7 0 shiftL >> return 4

rrca :: Cpu s Int
rrca = shiftWithCarry aReg 0 shiftR >> return 4

rra :: Cpu s Int
rra = shiftThroughCarry aReg 0 7 shiftR >> return 4

rlc :: Register s -> Cpu s Int
rlc x = shiftWithCarry x 7 shiftL 

rl :: Register s -> Cpu s Int
rl x = shiftThroughCarry x 7 0 shiftL

rrc :: Register s -> Cpu s Int
rrc x = shiftWithCarry x 0 shiftR

rr :: Register s -> Cpu s Int
rr x = shiftThroughCarry x 0 7 shiftR

sla :: Register s -> Cpu s Int
sla x = shiftWithCarry x 7 shiftL >> alterVar_ x (`clearBit` 0) >> return 8

sra :: Register s -> Cpu s Int
sra x = getVar x >>= (\s -> shiftWithCarry x 7 shiftL >> alterVar_ x (`clearBit` 0) >> alterVar_ x (\c -> if testBit s 7 then setBit c 7 else clearBit c 7)) >> return 8

srl :: Register s -> Cpu s Int
srl x = shiftWithCarry x 0 shiftR >> alterVar_ x (`clearBit` 7) >> return 8

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

ldRM :: Register s -> Address -> Cpu s Int
ldRM x a = readMemory a >>= setVar x >> return 8

ldMR :: Address -> Register s -> Cpu s Int
ldMR a x = getVar x >>= writeMemory a >> return 8

{- 16 Bit Loadcommands -}

pop :: Register s -> Register s -> Cpu s Int
pop x y = getSp >>= readWideMemory >>= setWideVar x y >> alterSp (+ 2) >> return 12

push :: WideOperand -> Cpu s Int
push x = alterSp (flip (-) 2) >>= writeWideMemory x >> return 16

{- Bit operations -}

set :: Int -> Operand -> Operand
set n r = setBit r n

res :: Int -> Operand -> Operand
res n r = clearBit r n 

bit :: Int -> Operand -> Cpu s Int
bit n r = setHalfCarryFlag True >> setAddFlag False >> setZeroFlag (testBit r n) >> return 8

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
rst = call (16, 16) True

{- CPU control -}

nop :: Cpu s Int
nop = return 4

ccf :: Cpu s Int
ccf = alterCarryFlag_ not >> return 4

scf :: Cpu s Int
scf = setCarryFlag True >> return 4

di :: Cpu s Int
di = setVar ime False >> return 4

ei :: Cpu s Int 
ei = setVar ime True >> return 4