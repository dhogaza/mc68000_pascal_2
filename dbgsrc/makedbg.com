$!
$! Determine the directory to build library object modules.  
$! "[.dbglib]" may be changed to any other directory.
$!
$ assign [.dbglib] dbg68
$!
$! Make sure the configuration headers are in the above directory.
$! The command file will be aborted when one of the opfig files 
$! is not found in the lb68: directory.
$!
$ sea dbg68:opfig.pas/nolog/ou=nl: release
$ sea dbg68:opfig.sa/nolog/ou=nl: release
$!
$! Define logical symbols used to invoke the cross compiler and cross
$! assembler.
$!
$ pas68 := $sys$system:pas68.exe/nocheck/nowalk/incl=dbg68:
$ asm68 :== $sys$system:asm68.exe
$!
$! Start the compilation and assembly.
$!
$ asm68 dbg68:opfig,opdb2e/obj=dbg68:opdb2e
$ asm68 dbg68:opfig,oppr2e/obj=dbg68:oppr2e
$ asm68 dbg68:opfig,opdbstk/obj=dbg68:opdbstk
$ asm68 dbg68:opfig,opdbg/obj=dbg68:opdbg
$ pas68 dbg68:dbinit=dbnomn,dbhdr,dbprc,dbierr,dblist,dbexec,dbinit
$ pas68 dbg68:dbpars=dbnomn,dbhdr,dbprc,dbsym,dbdata,dbpars
$ pas68 dbg68:dbxflt=dbnomn,dbhdr,dbprc,ident.dat,dbxerr,dbxutil,dbxini,dbxflt
$ pas68 dbg68:dbtexp=dbnomn,dbtjphdr,dbthdr,fpphdr,dbtacc,dbtrun,dbtexp
$ pas68 dbg68:dbsing=dbtprg,fpphdr,dbsing
$ pas68 dbg68:dbdoub=dbtprg,fpphdr,dbdoub
$ pas68 dbtprg,dbnomn,fpphdr,fppsing/68881/macro
$!
$! Use a very "dirty" trick to remove the reference from fppsing and
$! fppdoub to the p_68881 - compile Pascal-2 modules with /macro switch,
$! edit the file and assembly it.
$!
$ edit fppsing.sa
'p_68881'
d
exit
$ asm68 fppsing/obj=dbg68:fppsing
$ pas68 dbtprg,dbnomn,fpphdr,fppdoub/68881/macro
$ edit fppdoub.sa
'p_68881'
d
exit
$ asm68 fppdoub/obj=dbg68:fppdoub
$ pas68 dbg68:prrun=dbnomn,prhdr,prinit,prrun
$ pas68 dbg68:mapdmp=mapdmp
$ pas68 dbg68:symdmp=symdmp
$ pas68 dbg68:mapsym=mapsym
