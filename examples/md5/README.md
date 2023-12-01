# MD5

This is sample program written in Punchlan. It calculates and prints MD5 hash sum of input file.

## Building
To build it make sure tools

- punchc
- [nasm](https://nasm.us)
- [opt](https://llvm.org/docs/CommandGuide/opt.html) (LLVM)
- [llc](https://llvm.org/docs/CommandGuide/llc.html) (LLVM)

Are available in `PATH` environment.

Make sure
- [Visual Studio Developer command prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022#:~:text=Follow%20these%20steps%20to%20open%20Developer%20Command%20Prompt,Line%20%3E%20Developer%20Command%20Prompt%20or%20Developer%20PowerShell) is launched
to make sure `link` and default `.lib` files are available.

Last step is to make sure `link` and default `.lib` files are available.

1. Launch powershell in already running terminal window;
2. Run `.\build.ps1`;
3. Resulting exe's will be placed in current directory;

## Usage
```
Usage: <filename>
```

For example,

```
.\md5_nasm_debug.exe .\md5.exe
594e85eec6cb0951cf3cb22f964e688b
Closing file
Freeing memory
```

**P.S.** LLVM release version (which is with all LLVM optimizations) doesn't work for some reason ¯\_(ツ)_/¯

## Benchmarks

Hashing 1GB random binary file.

| Tool                     | Time (seconds) | Speed (MB/sec) |
|--------------------------|:---------------|:---------------|
| openssl md5 (reference)  | 1.260 sec      | 812 MB/sec     |
| md5_nasm_debug.exe       | 23.127 sec     | 44 MB/sec      |
| md5_nasm_release.exe     | 23.044 sec     | 44 MB/sec      |
| md5_llvm_debug.exe       | 9.801 sec      | 104 MB/sec     |
| md5_llvm_release.exe     | fails          | fails          |
