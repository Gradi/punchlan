# Punchlan

**Punchlan** (Punching language, a language to punch) -- is a small programming language & compiler for it.
I created it to learn more about compilers, languages, parsers. It is buggy. No users expected at all.

### Pros

- Static type checking (a bad one);
- Compiles to a native code (thanks to [NASM](https://nasm.us) and [LLVM](https://llvm.org));
- Written in F# to practice F# as well;
- Fast compile times?
- No comments;

### Cons

- Bad, naive, unoptimized native code;
- No comments;
- Still a lot of bugs;
- It and it's programs run only on Windows; That is becase calling convention `System V x64 ABI` is not implemented,
but LLVM's version should run on Linux (not tested).

Read [language](LANGUAGE.md) description to get better understanding about language.

Read [building](BUILDING.md) to get to know how to build compiler.

See [examples](examples/) to see code samples.

## Usage

Invoke `punchc --help` to see usage.

```
USAGE: punchc.exe [--help] -f <string> -o <string> [--backend <nasm>] [--nasm-call-conv <microsoftx64|sysvx64>]

OPTIONS:

    -f <string>           specifies main(starting) source file
    -o <string>           specifies output filename
    --backend <nasm>      specifies target codegen backend. Defaults to Nasm
    --nasm-call-conv <microsoftx64|sysvx64>
                          specifies calling convention for Nasm codegenerator. Defaults to 'Microsoft x64'
    --help                display this list of options.
```
