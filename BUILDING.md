# Building compiler

You will need:

- [.NET 8](https://dot.net/)
- [NASM](https://nasm.us)
- [PowerShell Core](https://microsoft.com/PowerShell) (Windows builtin should do as well)

#### Steps

1. Clone this repository;
2. `cd` into it;
3. `cd src`;
4. Run command;
```
    dotnet build Punchlan.sln -c Debug -p:Platform=x64
```
5. Compiler will be located in `punchc\bin\x64\Debug\net8.0` named `punchc.exe`
