
extern func puts(string: const pointer<const char>)
endfunc

extern func fopen(filename: const pointer<const char>, mode : const pointer<const char>) : pointer<const void>
endfunc

extern func fclose(file: const pointer<const void>)
endfunc

extern func fread(buffer: const pointer<void>, size: const uint64, count: const uint64, file: const pointer<const void>) : uint64
endfunc
