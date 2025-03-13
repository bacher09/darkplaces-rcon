module DRcon.ConsoleWrapper ( ConsoleWrapper, decoratedConsole ) where

import qualified GHC.IO.Device as IO
import qualified GHC.IO.Handle as IO
import qualified GHC.IO.BufferedIO as IO
import qualified GHC.IO.Buffer as IO
import qualified Foreign
import qualified Foreign.C.String as FCS
import Data.Typeable (Typeable)


newtype ConsoleWrapper = ConsoleWrapper (IO.Handle, String -> IO ())
    deriving (Typeable)

instance IO.RawIO ConsoleWrapper where
    read             = error "Raw IO is not implemented"
    readNonBlocking  = error "Raw IO is not implemented"
    write            = error "Raw IO is not implemented"
    writeNonBlocking = error "Raw IO is not implemented"

instance IO.IODevice ConsoleWrapper where
    ready _ _ _ = return False
    -- don't close real device
    close _ = return ()
    isTerminal (ConsoleWrapper (h, _)) = IO.hIsTerminalDevice h
    isSeekable (ConsoleWrapper (h, _)) = IO.hIsSeekable h
    seek (ConsoleWrapper (h, _)) mode val = IO.hSeek h mode val >> return val
    tell (ConsoleWrapper (h, _)) = IO.hTell h
    getSize (ConsoleWrapper (h, _)) = IO.hFileSize h
    setSize (ConsoleWrapper (h, _)) = IO.hSetFileSize h
    setEcho (ConsoleWrapper (h, _)) = IO.hSetEcho h
    getEcho (ConsoleWrapper (h, _)) = IO.hGetEcho h
    -- setRaw (ConsoleWrapper (h, _)) = IO.hSetRaw h
    devType _ = return IO.Stream

instance IO.BufferedIO ConsoleWrapper where
    newBuffer _ = IO.newByteBuffer 4096
    fillReadBuffer = error "Read isn't supported"
    fillReadBuffer0 = error "Read isn't supported"
    flushWriteBuffer (ConsoleWrapper (_, w)) buf = do
        let bufStart ptr = Foreign.castPtr (Foreign.plusPtr ptr (IO.bufL buf))
        let bufLen = IO.bufR buf - IO.bufL buf
        bufBytes <- Foreign.withForeignPtr (IO.bufRaw buf) (\ptr -> FCS.peekCStringLen (bufStart ptr, bufLen))
        w bufBytes
        return (buf { IO.bufL = 0, IO.bufR = 0})

    flushWriteBuffer0 dev buf = do
        newBuf <- IO.flushWriteBuffer dev buf
        return (IO.bufR buf - IO.bufL buf, newBuf)


decoratedConsole :: IO.Handle -> (String -> IO ()) -> IO IO.Handle
--decoratedConsole h w = IO.mkDuplexHandle h "" (IO.hGetEncoding h)
decoratedConsole h w = do
    enc <- IO.hGetEncoding h
    IO.mkDuplexHandle (ConsoleWrapper (h, w)) "" enc IO.noNewlineTranslation
-- decoratedConsole h w = IO.mkDuplexHandle (ConsoleWrapper (h, w)) "" Nothing IO.noNewlineTranslation
