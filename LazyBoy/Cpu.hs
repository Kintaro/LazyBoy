{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module LazyBoy.Cpu where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.ST as STArr
import Data.Bits as B hiding (bit)
import Data.STRef
import Data.Word as W

-- Cpu is handled as a state monad
newtype Cpu s a = Cpu { runCpu :: CpuEnv s -> ST s a }

-- 
instance Monad (Cpu s) where
	return x = Cpu $ \_ -> return x
	m >>= f  = Cpu $ \r -> do
		a <- runCpu m r
		runCpu (f a) r

--
instance MonadReader (CpuEnv s) (Cpu s) where
	ask       = Cpu return
	local f m = Cpu $ runCpu m . f

--
instance Functor (Cpu s) where
	fmap f m = do
		!res <- m
		return $ f res

--
instance Applicative (Cpu s) where
	pure  = return
	(<*>) = ap

-- The Cpu environment consists of
-- 8 8-bit registers which can be combined
-- into 16-bit registers (high/low),
-- a 16-bit stack pointer and a
-- 16-bit program counter.
-- All enclosed in a state thread s
data CpuEnv s = CpuEnv
	{
	    aReg   :: STRef s Operand
	   ,bReg   :: STRef s Operand
	   ,cReg   :: STRef s Operand
	   ,dReg   :: STRef s Operand
	   ,eReg   :: STRef s Operand
	   ,hReg   :: STRef s Operand
	   ,lReg   :: STRef s Operand
	   ,fReg   :: STRef s Status
	   ,sp     :: STRef s Address
	   ,pc     :: STRef s Address
	   ,ime    :: STRef s Bool
	   ,cartridgeMem :: Memory s
	   ,videoRam     :: Memory s
	   ,bankRam      :: Memory s
	   ,internalRam  :: Memory s
	   ,spriteRam    :: Memory s
	   ,ioPorts      :: Memory s
	   ,internalRam2 :: Memory s
	   ,intRegister  :: Memory s
	}


type Operand     = W.Word8
type WideOperand = W.Word16
type OpCode      = W.Word8
type Address     = W.Word16
type Pixel       = W.Word32 
type Status      = W.Word8
type Memory s    = STUArray s Address Operand
type Register s  = (CpuEnv s -> STRef s Operand)

toAddress :: Operand -> Address
toAddress = fromIntegral

-- Write an operand to a position, given by an address, into the 
-- memory array extracted from the Cpu environment
writeArr  :: (CpuEnv s -> Memory s) -> Address -> Operand -> Cpu s ()
writeArr f a o = Cpu $ \r -> writeArray (f r) a o

-- Fetch an operand from a position, given by an address, from the array
readArr   :: (CpuEnv s -> Memory s) -> Address -> Cpu s Operand
readArr  f a   = Cpu $ \r -> readArray (f r) a

-- 
getVar    :: (CpuEnv s -> STRef s a) -> Cpu s a
getVar   f     = Cpu $ \r -> readSTRef (f r)

--
setVar    :: (CpuEnv s -> STRef s a) -> a -> Cpu s ()
setVar   f a   = Cpu $ \r -> writeSTRef (f r) a

--
alterVar  :: (CpuEnv s -> STRef s a) -> (a -> a) -> Cpu s a
alterVar f g   = Cpu $ \r -> let r' = f r in modifySTRef r' g >> readSTRef r'

--
alterVar_ :: (CpuEnv s -> STRef s a) -> (a -> a) -> Cpu s ()
alterVar_ f g  = Cpu $ \r -> modifySTRef (f r) g

--
getAccumulator :: Cpu s Operand
getAccumulator = getVar aReg

--
setAccumulator :: Operand -> Cpu s ()
setAccumulator = setVar aReg

--
alterAccumulator :: (Operand -> Operand) -> Cpu s Operand
alterAccumulator = alterVar aReg

--
baseSp :: Address
baseSp = 0x0100

--
getSp :: Cpu s Address
getSp = getVar sp

--
setSp :: Address -> Cpu s ()
setSp = setVar sp

--
alterSp :: (Address -> Address) -> Cpu s Address
alterSp = alterVar sp

--
getPc :: Cpu s Address
getPc = getVar pc

--
setPc :: Address -> Cpu s ()
setPc = setVar pc

--
alterPc :: (Address -> Address) -> Cpu s Address
alterPc = alterVar pc

--
getWideVar :: (CpuEnv s -> STRef s Operand) -> (CpuEnv s -> STRef s Operand) -> Cpu s WideOperand
getWideVar x y = do
	x' <- fromIntegral <$> getVar x
	y' <- fromIntegral <$> getVar y
	return $ (x' `shiftL` 8) .|. y'

--
setWideVar :: (CpuEnv s -> STRef s Operand) -> (CpuEnv s -> STRef s Operand) -> WideOperand -> Cpu s ()
setWideVar x y v = do
	let x' = fromIntegral (v `shiftR` 8) :: Operand
	let y' = fromIntegral (v .&. 0xFF) :: Operand
	setVar x x' >> setVar y y'

--
alterWideVar :: (CpuEnv s -> STRef s Operand) -> (CpuEnv s -> STRef s Operand) -> (WideOperand -> WideOperand) -> Cpu s WideOperand
alterWideVar x y f = do
	x' <- getWideVar x y
	setWideVar x y (f x') >> getWideVar x y

--
alterWideVar_ :: (CpuEnv s -> STRef s Operand) -> (CpuEnv s -> STRef s Operand) -> (WideOperand -> WideOperand) -> Cpu s ()
alterWideVar_ x y f = do
	x' <- getWideVar x y
	setWideVar x y (f x')

--
getBc :: Cpu s WideOperand
getBc = getWideVar bReg cReg

--
setBc :: WideOperand -> Cpu s ()
setBc = setWideVar bReg cReg

--
alterBc :: (WideOperand -> WideOperand) -> Cpu s WideOperand
alterBc = alterWideVar bReg cReg

--
alterBc_ :: (WideOperand -> WideOperand) -> Cpu s ()
alterBc_ = alterWideVar_ bReg cReg

--
getDe :: Cpu s WideOperand
getDe = getWideVar dReg eReg

--
setDe :: WideOperand -> Cpu s ()
setDe = setWideVar dReg eReg

--
alterDe :: (WideOperand -> WideOperand) -> Cpu s WideOperand
alterDe = alterWideVar dReg eReg

--
alterDe_ :: (WideOperand -> WideOperand) -> Cpu s ()
alterDe_ = alterWideVar_ dReg eReg

--
getHl :: Cpu s WideOperand
getHl = getWideVar hReg lReg

--
setHl :: WideOperand -> Cpu s ()
setHl = setWideVar hReg lReg

--
alterHl :: (WideOperand -> WideOperand) -> Cpu s WideOperand
alterHl = alterWideVar hReg lReg

--
alterHl_ :: (WideOperand -> WideOperand) -> Cpu s ()
alterHl_ = alterWideVar_ hReg lReg

--
getFlag :: Operand -> Cpu s Bool
getFlag mask = do
	x <- getVar fReg
	return $ (x .&. mask) /= 0x0

--
setFlag :: Operand -> Bool -> Cpu s ()
setFlag mask f = do
	x <- getVar fReg
	setVar fReg $ if f then x .|. mask else x .&. complement mask

--
getOperandFlagBit :: (CpuEnv s -> STRef s Operand) -> Int -> Cpu s Bool
getOperandFlagBit r x = (`testBit` x) <$> getVar r

--
getZeroFlag :: Cpu s Bool
getZeroFlag = getOperandFlagBit fReg 7 

--
setZeroFlag :: Bool -> Cpu s ()
setZeroFlag = setFlag 0x80

--
alterZeroFlag :: (Bool -> Bool) -> Cpu s Bool
alterZeroFlag f = getZeroFlag >>= setZeroFlag . f >> getZeroFlag

--
alterZeroFlag_ :: (Bool -> Bool) -> Cpu s ()
alterZeroFlag_ f = getZeroFlag >>= setZeroFlag . f

--
getAddFlag :: Cpu s Bool
getAddFlag = getOperandFlagBit fReg 6

--
setAddFlag :: Bool -> Cpu s ()
setAddFlag = setFlag 0x40

--
alterAddFlag :: (Bool -> Bool) -> Cpu s Bool
alterAddFlag f = getAddFlag >>= setAddFlag . f >> getAddFlag

--
alterAddFlag_ :: (Bool -> Bool) -> Cpu s ()
alterAddFlag_ f = getAddFlag >>= setAddFlag . f

--
getCarryFlag :: Cpu s Bool
getCarryFlag = getOperandFlagBit fReg 4

--
setCarryFlag :: Bool -> Cpu s ()
setCarryFlag = setFlag 0x10

--
alterCarryFlag_ :: (Bool -> Bool) -> Cpu s ()
alterCarryFlag_ f = getCarryFlag >>= setCarryFlag . f

--
setHalfCarryFlag :: Bool -> Cpu s ()
setHalfCarryFlag = setFlag 0x40

checkAndSetZeroFlag :: Operand -> Cpu s ()
checkAndSetZeroFlag x = setZeroFlag $ x == 0

--
pushOperand :: Operand -> Cpu s ()
pushOperand op = do
	stackValue <- getSp
	setSp $ stackValue - 1
	writeMemory (baseSp + stackValue) op

--
pullOperand :: Cpu s Operand 
pullOperand = do
	stackValue <- alterSp (+ 1)
	readMemory $ baseSp + stackValue

--
readMemory :: Address -> Cpu s Operand
readMemory addr
	| addr < 0x8000 = readArr cartridgeMem addr
	| addr < 0xA000 = readArr videoRam addr
	| addr < 0xC000 = readArr bankRam addr
	| addr < 0xE000 = readArr internalRam addr
	| addr < 0xFE00 = readArr internalRam addr
	| addr < 0xFEA0 = readArr spriteRam addr
	| addr < 0xFF3C = readArr ioPorts addr
	| addr < 0xFFFF = readArr internalRam2 addr
	| otherwise     = readArr intRegister addr

--
readWideMemory :: Address -> Cpu s WideOperand
readWideMemory addr = do
	h <- readMemory $ baseSp + addr
	l <- readMemory $ baseSp + addr + 1
	return $ fromIntegral $ (h `shiftL` 8) .|. fromIntegral l

--
writeMemory :: Address -> Operand -> Cpu s ()
writeMemory addr op
	| addr < 0x8000 = writeArr cartridgeMem addr op
	| addr < 0xA000 = writeArr videoRam addr op
	| addr < 0xC000 = writeArr bankRam addr op
	| addr < 0xE000 = writeArr internalRam addr op
	| addr < 0xFE00 = writeArr internalRam addr op
	| addr < 0xFEA0 = writeArr spriteRam addr op
	| addr < 0xFF3C = writeArr ioPorts addr op
	| addr < 0xFFFF = writeArr internalRam2 addr op
	| otherwise     = writeArr intRegister addr op

--
writeWideMemory :: Address -> WideOperand -> Cpu s ()
writeWideMemory addr op =
	let h = fromIntegral $ (op .&. 0xF0) `shiftR` 8
	    l = fromIntegral $ op .&. 0x0F
	    in writeMemory addr h >> writeMemory (addr + 1) l

--
alterMemory :: Address -> (Operand -> Operand) -> Cpu s Operand
alterMemory addr f = do
	res <- f <$> readMemory addr
	writeMemory addr res
	return res

--
writeMemoryRegion :: Address -> [Operand] -> Cpu s ()
writeMemoryRegion addr [op]     = writeMemory addr op
writeMemoryRegion addr (op:ops) = writeMemory addr op >> writeMemoryRegion (addr + 1) ops

--
readMemoryRegion :: Address -> WideOperand -> Cpu s [Operand]
readMemoryRegion addr n = sequence $ readMemoryRegion' addr n
	where readMemoryRegion' _    0 = []
	      readMemoryRegion' addr' n' = readMemory addr' : readMemoryRegion' (addr' + 1) (n' - 1) 

--
fetch :: Cpu s OpCode 
fetch = getPc >>= readMemory >>= (\x -> alterPc (+ 1) >> return x)

--
fetchImmediate :: Cpu s Operand 
fetchImmediate = getPc >>= readMemory >>= (\x -> alterPc (+ 1) >> return x)

--
fetchWideImmediate :: Cpu s WideOperand
fetchWideImmediate = do
	op <- getPc
	l <- readMemory op 
	u <- readMemory $ op + 1
	setPc $ op + 2
	return $ ((fromIntegral u :: WideOperand) `shiftL` 8) .|. (fromIntegral l :: WideOperand)

--
initCpu :: ST s (CpuEnv s)
initCpu = return CpuEnv
        `ap` newSTRef 0x0                    -- a
        `ap` newSTRef 0x0                    -- b
        `ap` newSTRef 0x0                    -- c
        `ap` newSTRef 0x0                    -- d
        `ap` newSTRef 0x0                    -- e
        `ap` newSTRef 0x0                    -- h
        `ap` newSTRef 0x0                    -- l
        `ap` newSTRef 0x0                    -- f
        `ap` newSTRef 0xFF                   -- SP
        `ap` newSTRef 0x0                    -- PC
        `ap` newSTRef True                   -- IME
        `ap` newArray (0x0000, 0x7FFF) 0x0   -- Cartridge Memory
        `ap` newArray (0x8000, 0x9FFF) 0x0   -- Video Ram
        `ap` newArray (0xA000, 0xBFFF) 0x0   -- Banked Ram
        `ap` newArray (0xC000, 0xDFFF) 0x0   -- Internal Ram
        `ap` newArray (0xFE00, 0xFE9F) 0x0   -- Sprite Ram
        `ap` newArray (0xFF00, 0xFF4B) 0x0   -- I/O
        `ap` newArray (0xFF80, 0xFFFE) 0x0   -- Internal Ram 2
        `ap` newArray (0xFFFF, 0xFFFF) 0x0   -- Interrupt Register