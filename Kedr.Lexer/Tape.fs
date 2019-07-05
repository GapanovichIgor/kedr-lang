namespace Kedr
open System
open System.IO
open System.Text

open Utils

type private Tape(stream : Stream, encoding : Encoding) =
    let reader = new StreamReader(stream, encoding)

    let mutable buffer = Array.zeroCreate<char>(4096)
    let mutable bufferStart = 0
    let mutable bufferLength = 0
    let mutable headPos = -1

    member __.WindowLength =
        assert (headPos >= bufferStart)
        headPos - bufferStart + 1
        
    member __.EndReached =
        bufferLength = 0 && reader.EndOfStream

    member this.GetNext() =
        assert (not this.EndReached)
        
        headPos <- headPos + 1
        if headPos = bufferStart + bufferLength then
            let i = reader.Read()
            if i = -1 then
                None
            else
                let c = char i

                if bufferStart + bufferLength = buffer.Length then
                    if bufferStart > 0 then
                        Array.Copy(buffer, bufferStart, buffer, 0, bufferLength)
                    else
                        let newBuffer = Array.zeroCreate<char>(buffer.Length * 2)
                        Array.Copy(buffer, bufferStart, newBuffer, 0, bufferLength)
                        buffer <- newBuffer

                bufferLength <- bufferLength + 1
                buffer.[headPos] <- c

                Some c
        else
            let c = buffer.[headPos]
            Some c

    member this.RollbackAndConsume(n : int) : char[] =
        assert (n <= bufferLength)
        assert (bufferStart + n <= buffer.Length)
        
        let chars = buffer |> getSubArray bufferStart n
        
        this.RollbackAndSkip(n)
        
        chars
        
    member __.RollbackAndSkip(n : int) : unit =
        assert (n <= bufferLength)
        assert (bufferStart + n <= buffer.Length)
        
        bufferStart <- bufferStart + n
        bufferLength <- bufferLength - n
        headPos <- bufferStart - 1