open libc/stdlib
open libc/stdio

func getmem(size: uint64) : pointer<void>
    var result: pointer<void> = malloc (size)
    if result == cast(uint64, 0) then
        panic ("Can't allocate memory.")
    fi
    return result
endfunc

func panic(msg: const pointer<const char>)
    puts ("*** PANIC ***")
    puts (msg)
    abort ()
endfunc
