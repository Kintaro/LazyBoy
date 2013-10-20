
import Control.Monad.ST
import Data.ByteString as B (readFile, unpack)
import System.Environment (getArgs)

import LazyBoy.Cpu

done :: IO ()
done = putStrLn "[Done]"

loadRomIntoCpu :: [Operand] -> Cpu s ()
loadRomIntoCpu d = writeMemoryRegion 0x00 d

loadRom :: String -> IO [Operand]
loadRom f = do
	input <- B.readFile f
	return $ B.unpack input

main :: IO ()
main = do
	[filename] <- getArgs

	putStr "Initializing virtual cpu..."

	let cpu = initCpu 

	done

	putStr $ "Loading ROM file " ++ show filename ++ "..."
	romData <- loadRom filename
	done

	putStr "Loading ROM data into virtual environment..."
	let loadedCpu = cpu >>= runCpu (loadRomIntoCpu romData)
	let result = runST $ loadedCpu
	done

	putStrLn $ "Loaded value: " ++ show loadedCpu