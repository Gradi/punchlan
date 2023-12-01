$ErrorActionPreference='Stop'
$PSNativeCommandUseErrorActionPreference=$true

function rmfile($filename) {
    if (Test-Path $filename -PathType Leaf) {
        rm $filename
    }
}

function deldir($dirname) {
    if (Test-Path $dirname -PathType Container) {
        rm -Force -Recurse $dirname
    }
}

function link_debug {
    Param
    (
        [string[]]$objects,
        [string]$pdb,
        [string]$out
    )

    link /DEBUG `
         /ENTRY:mainCRTStartup `
         /MACHINE:x64 `
         /NOLOGO `
         /PDB:$pdb `
         /SUBSYSTEM:CONSOLE `
         /INCREMENTAL:NO `
         /NODEFAULTLIB `
         /DYNAMICBASE:NO `
         /LARGEADDRESSAWARE `
         /OUT:$out `
         @objects
}

function link_release {
    Param
    (
        [string[]]$objects,
        [string]$out
    )

    link /ENTRY:mainCRTStartup `
         /MACHINE:x64 `
         /NOLOGO `
         /SUBSYSTEM:CONSOLE `
         /INCREMENTAL:NO `
         /NODEFAULTLIB `
         /DYNAMICBASE:NO `
         /LARGEADDRESSAWARE `
         /OUT:$out `
         @objects
}

function checkecode {
    $code=$LASTEXITCODE
    if ($code -ne 0) {
        Write-Error "Last command failed."
    }
}

echo 'Cleaning stuff'
deldir .\objs\
rmfile .\md5_nasm_debug.exe
rmfile .\md5_nasm_release.exe
rmfile .\md5_llvm_debug.exe
rmfile .\md5_llvm_release.exe

echo 'mkdir objs'
mkdir .\objs

echo 'Compiling'
punchc --backend nasm -f .\main.pl -o .\objs\md5_nasm.asm
checkecode
punchc --backend llvm -f .\main.pl -o .\objs\md5_llvm_debug.ll
checkecode

echo 'Running NASM'
nasm -w+all -g -f win64 -o .\objs\md5_nasm.obj .\objs\md5_nasm.asm
checkecode

echo 'Linking NASM version'
link_debug -pdb .\objs\md5_nasm_debug.pdb -out md5_nasm_debug.exe -objects .\objs\md5_nasm.obj,libucrtd.lib,libvcruntimed.lib,libcmtd.lib,kernel32.lib
checkecode
link_release -out md5_nasm_release.exe -objects .\objs\md5_nasm.obj,libucrt.lib,libvcruntime.lib,libcmt.lib,kernel32.lib
checkecode

echo 'Verifying LLVM code'
opt -p verify -o .\objs\tmp.bc .\objs\md5_llvm_debug.ll
checkecode

echo 'Optimizing LLVM code'
opt --O3 -o .\objs\md5_llvm_release.bc .\objs\md5_llvm_debug.ll
checkecode

echo 'Compiling LLVM code'
llc -O0 --filetype=obj -o .\objs\md5_llvm_debug.obj .\objs\md5_llvm_debug.ll
checkecode
llc -O3 --filetype=obj -o .\objs\md5_llvm_release.obj .\objs\md5_llvm_release.bc
checkecode

echo 'Linking LLVM version'
link_debug -pdb .\objs\md5_llvm_debug.pdb -out md5_llvm_debug.exe -objects .\objs\md5_llvm_debug.obj,libucrtd.lib,libvcruntimed.lib,libcmtd.lib,kernel32.lib
checkecode
link_release -out md5_llvm_release.exe -objects .\objs\md5_llvm_release.obj,libucrt.lib,libvcruntime.lib,libcmt.lib,kernel32.lib
checkecode

