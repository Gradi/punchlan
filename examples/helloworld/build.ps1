function rmfile($filename) {
    if (Test-Path $filename -PathType Leaf) {
        rm $filename
    }
}

# Clean stuff
rmfile .\hw.exe
rmfile .\hw.obj
rmfile .\hw.asm
rmfile .\hw.ilk
rmfile .\hw.pdb

punchc -f main.pl -o hw.asm

nasm -w+all -g -f win64 -o hw.obj hw.asm

link /DEBUG `
     /ENTRY:mainCRTStartup `
     /MACHINE:x64 `
     /NOLOGO `
     /PDB:hw.pdb `
     /SUBSYSTEM:CONSOLE `
     /NODEFAULTLIB `
     /DYNAMICBASE:NO `
     /LARGEADDRESSAWARE `
     hw.obj libucrtd.lib libvcruntimed.lib libcmtd.lib kernel32.lib

