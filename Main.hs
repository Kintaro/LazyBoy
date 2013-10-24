import Control.Monad.ST
import Data.ByteString as B (readFile, unpack)
import Graphics.Gloss as G
import System.Environment (getArgs)

import LazyBoy.Cpu

done :: IO ()
done = putStrLn "[Done]"

-- Write the given ROM data into the cpu's memory starting
-- at address 0x00
loadRomIntoCpu :: [Operand] -> Cpu s ()
loadRomIntoCpu = writeMemoryRegion 0x00

-- Load the given ROM file and unpack it into single bytes
loadRom :: String -> IO [Operand]
loadRom f = B.readFile f >>= \s -> return (B.unpack s)

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
	let result = runST loadedCpu
	done

	putStrLn $ "Loaded value: " ++ show loadedCpu

	G.display (G.InWindow "LazyBoy" (800, 600) (50, 50)) (G.makeColor 1.0 0.0 0.0 0.0) G.Blank
