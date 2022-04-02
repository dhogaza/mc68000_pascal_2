$! Command file to link VMS/68K cross compiler
$!
$link/nouser/exec=pas68.exe/nomap pas68.lib,p2vms/lib
$!
