$!
$!	This command procedure will generate asm68.exe (the cross-
$!	assembler) and lnk68.exe (the cross-linker) from asm68.lib, 
$!	lnk68.lib, and p2vms.olb.
$!
$link/nouser/exe=asm68.exe/nomap asm68.lib,p2vms/lib
$link/nouser/exe=lnk68.exe/nomap lnk68.lib,p2vms/lib
$
