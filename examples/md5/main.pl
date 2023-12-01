open punch
open libc/math
open libc/stdio
open libc/stdlib

var shifts : pointer<uint32>
var Ks : pointer<uint32>

var A : uint32
var B : uint32
var C : uint32
var D : uint32

export func main (argc: int32, argv: pointer<const pointer<const char>>) : int32
    if argc != 2 then
        puts("Usage: <filename>")
        return 1
    fi

    initShifts ()
    initKs ()
    A = cast(uint32, 0x67452301)
    B = cast(uint32, 0xefcdab89)
    C = cast(uint32, 0x98badcfe)
    D = cast(uint32, 0x10325476)

    var readSize : uint64 = cast(uint64, 0)
    var totalSize: uint64 = cast(uint64, 0)
    var bufferSize: const uint64 = cast(uint64, 64)
    var buffer : pointer<uint8> = cast(pointer<uint8>, getmem(bufferSize))
    var file : const pointer<const void> = fopen(argv[1], "rb")
    if file == cast(uint64, 0) then
        puts("Error: Can't open file")
        return 1
    fi

    readSize = fread(cast(pointer<void>, buffer), cast(uint64, 1), bufferSize, file)
    while readSize == bufferSize do
        totalSize = totalSize + readSize
        processChunk (buffer)
        readSize = fread(cast(pointer<void>, buffer), cast(uint64, 1), bufferSize, file)
    endwhile

    totalSize = totalSize + readSize
    var originalSize : uint64 = totalSize * cast(uint64, 8)

    buffer[cast(int64, readSize)] = cast(uint8, 0x80)
    readSize = readSize + cast(uint64, 1)
    totalSize = totalSize + cast(uint64, 1)

    while mod(totalSize, cast(uint32, 64)) != cast(uint64, 56) do
        if readSize == cast(uint64, 64) then
            processChunk (buffer)
            readSize = cast(uint64, 0)
        else
            buffer[cast(int64, readSize)] = cast(uint8, 0)
            readSize = readSize + cast(uint64, 1)
            totalSize = totalSize + cast(uint64, 1)
        fi
    endwhile

    cast(pointer<uint64>,buffer)[7] = originalSize
    processChunk (buffer)
    printHexNumber (A)
    printHexNumber (B)
    printHexNumber (C)
    printHexNumber (D)
    puts("")
    return 0
endfunc

func processChunk(buffer: pointer<uint8>)
    var words: const pointer<uint32> = cast(pointer<uint32>, buffer)
    var a : uint32 = A
    var b : uint32 = B
    var c : uint32 = C
    var d : uint32 = D

    for index in 0 .. 64 do
        var F : uint32 = cast(uint32, 0)
        var g : uint32 = cast(uint32, 0)

        if 0 <= index and index <= 15 then
            F = (b and c) or ((~b) and d)
            g = cast(uint32, index)
        elseif 16 <= index and index <= 31 then
            F = (d and b) or ((~d) and c)
            g = cast(uint32, 5 * index + 1)
        elseif 32 <= index and index <= 47 then
            F = b xor c xor d
            g = cast(uint32, 3 * index + 5)
        elseif 48 <= index and index <= 63 then
            F = c xor (b or (~d))
            g = cast(uint32, 7 * index)
        else
            panic("Should not happen.")
        fi

        g = mod(g, cast(uint32, 16))
        if g >= cast(uint32, 16) or g < cast(uint32, 0) then
            panic ("g out of bounds")
        fi

        F = cast(uint32, F + a + Ks[index] + words[cast(int64, g)])
        a = d
        d = c
        c = b
        b = cast(uint32, b + leftrotate(F, shifts[index]))
    endfor

    A = A + a
    B = B + b
    C = C + c
    D = D + d
endfunc

func initKs ()
    Ks = cast(pointer<uint32>, getmem(sizeof(uint32) * cast(uint64, 64)))
    for i in 0 .. 64 do
        var di: double = cast(double, i)
        var k : double = floor(pow(2.0, 32.0) * fabs(sin(di + 1.0)))
        var k : uint32 = cast(uint32, k)
        Ks[i] = k
    endfor
endfunc

func initShifts ()
    shifts = cast(pointer<uint32>, getmem(sizeof(uint32) * cast(uint64, 64)))
    shifts[0] = cast(uint32, 7)
    shifts[1] = cast(uint32,  12)
    shifts[2] = cast(uint32,  17)
    shifts[3] = cast(uint32,  22)
    shifts[4] = cast(uint32,   7)
    shifts[5] = cast(uint32,  12)
    shifts[6] = cast(uint32,  17)
    shifts[7] = cast(uint32,  22)
    shifts[8] = cast(uint32,   7)
    shifts[9] = cast(uint32,  12)
    shifts[10] = cast(uint32,  17)
    shifts[11] = cast(uint32,  22)
    shifts[12] = cast(uint32,   7)
    shifts[13] = cast(uint32,  12)
    shifts[14] = cast(uint32,  17)
    shifts[15] = cast(uint32,  22)
    shifts[16] = cast(uint32, 5)
    shifts[17] = cast(uint32,   9)
    shifts[18] = cast(uint32,  14)
    shifts[19] = cast(uint32,  20)
    shifts[20] = cast(uint32,   5)
    shifts[21] = cast(uint32,   9)
    shifts[22] = cast(uint32,  14)
    shifts[23] = cast(uint32,  20)
    shifts[24] = cast(uint32,   5)
    shifts[25] = cast(uint32,   9)
    shifts[26] = cast(uint32,  14)
    shifts[27] = cast(uint32,  20)
    shifts[28] = cast(uint32,   5)
    shifts[29] = cast(uint32,   9)
    shifts[30] = cast(uint32,  14)
    shifts[31] = cast(uint32,  20)
    shifts[32] = cast(uint32, 4)
    shifts[33] = cast(uint32,  11)
    shifts[34] = cast(uint32,  16)
    shifts[35] = cast(uint32,  23)
    shifts[36] = cast(uint32,   4)
    shifts[37] = cast(uint32,  11)
    shifts[38] = cast(uint32,  16)
    shifts[39] = cast(uint32,  23)
    shifts[40] = cast(uint32,   4)
    shifts[41] = cast(uint32,  11)
    shifts[42] = cast(uint32,  16)
    shifts[43] = cast(uint32,  23)
    shifts[44] = cast(uint32,   4)
    shifts[45] = cast(uint32,  11)
    shifts[46] = cast(uint32,  16)
    shifts[47] = cast(uint32,  23)
    shifts[48] = cast(uint32, 6)
    shifts[49] = cast(uint32,  10)
    shifts[50] = cast(uint32,  15)
    shifts[51] = cast(uint32,  21)
    shifts[52] = cast(uint32,   6)
    shifts[53] = cast(uint32,  10)
    shifts[54] = cast(uint32,  15)
    shifts[55] = cast(uint32,  21)
    shifts[56] = cast(uint32,   6)
    shifts[57] = cast(uint32,  10)
    shifts[58] = cast(uint32,  15)
    shifts[59] = cast(uint32,  21)
    shifts[60] = cast(uint32,   6)
    shifts[61] = cast(uint32,  10)
    shifts[62] = cast(uint32,  15)
    shifts[63] = cast(uint32,  21)
endfunc

func mod(value: uint32, mod: uint32) : uint32
    var a : uint32 = cast(uint32, value / mod)
    return value - (a * mod)
endfunc

func modlong(value: uint64, mod: uint64) : uint64
    var a : uint64 = value / mod
    return value - (a * mod)
endfunc

func printHexNumber(number: uint32)
    var ptr: pointer<uint8> = cast(pointer<uint8>, addrof(number))
    for i in 0 .. 4 do
        var byte: uint8 = ptr[i]
        printf("%02x", byte)
    endfor
endfunc

func leftrotate(value: uint32, times: uint32) : uint32
    return ((value << times) or (value >> (cast(uint32, 32) - times)))
endfunc

extern func printf(format: const pointer<const char>, number: uint8) : int32
endfunc
