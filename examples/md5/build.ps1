function rmfile($filename) {
    if (Test-Path $filename -PathType Leaf) {
        rm $filename
    }
}

# Clean stuff
rmfile .\md5.exe
rmfile .\md5.obj
rmfile .\md5.asm
rmfile .\md5.ilk
rmfile .\md5.pdb

punchc -f main.pl -o md5.asm

nasm -w+all -g -f win64 -o md5.obj md5.asm

link /DEBUG `
     /ENTRY:mainCRTStartup `
     /MACHINE:x64 `
     /NOLOGO `
     /PDB:main.pdb `
     /SUBSYSTEM:CONSOLE `
     /NODEFAULTLIB `
     /DYNAMICBASE:NO `
     /LARGEADDRESSAWARE `
     md5.obj libucrtd.lib libvcruntimed.lib libcmtd.lib kernel32.lib

