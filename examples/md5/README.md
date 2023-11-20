# MD5

This is sample program written in Punchlan. It calculates and prints MD5 hash sum of input file.

## Building
To build it make sure

- punchc
- [nasm](https://nasm.us)

Are available in `PATH` environment.

Make sure
- [Visual Studio Developer command prompt](https://learn.microsoft.com/en-us/visualstudio/ide/reference/command-prompt-powershell?view=vs-2022#:~:text=Follow%20these%20steps%20to%20open%20Developer%20Command%20Prompt,Line%20%3E%20Developer%20Command%20Prompt%20or%20Developer%20PowerShell) is launched.

Last step is to make sure `link` and default `.lib` files are available.

1. Launch powershell in already running terminal window;
2. Run `.\build.ps1`;
3. Resulting file is named `md5.exe`;

## Usage
```
Usage: <filename>
```

For example,

```
.\md5.exe .\md5.exe
594e85eec6cb0951cf3cb22f964e688b
Closing file
Freeing memory
```