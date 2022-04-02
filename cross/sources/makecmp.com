$!
$!	Command file to compile and link a PASCAL-2 VMS->68K cross compiler
$!	Version = 2.3.0.1
$!
$ compile :== $sys$system:pas2.exe/nocheck/nowalk
$ compile hdr,hdrprc,m
$ compile hdr,hdrprc,ident.dat,csi
$ compile hdr,hdrprc,hdrs,ident.dat,scan
$ compile p2rdreal
$ compile hdr,hdrprc,hdra,commax,comma
$ compile hdr,hdrprc,hdra,mdax,commax,mda
$ compile hdr,hdrprc,athdr,foldcomx,foldcom
$ compile hdr,hdrprc,hdra,mdax,commax,analys
$ compile hdr,hdrprc,hdra,mdax,commax,foldcomx,body
$ compile hdr,hdrprc,hdrt,commtx,commt
$ compile hdr,hdrprc,hdrt,commtx,foldcomx,opentr,fphdr,fpcalc,travrs
$ compile hdr,hdrprc,hdrt,commtx,foldcomx,improve
$ compile hdr,hdrprc,hdrt,commtx,foldcomx,walk
$ compile hdr,hdrprc,hdrc,hdrc2,commcx,commc
$ compile hdr,hdrprc,hdrc,hdrc2,commcx,genblk
$ compile hdr,hdrprc,hdrc,hdrc2,commcx,ident.dat,commc2,putcode2,putcode
$ compile hdr,hdrprc,hdrc,hdrc2,commcx,ident.dat,code
$ compile hdr,hdrprc,ident.dat,list
$ compile hdr,hdrprc,error
$ compile hdr,hdrprc,hdrc,hdrc2,files
$ compile hdr,hdrprc,hdra,mdax,commax,pdbfiles
$ compile hdr,hdrprc,fakeutil
$ macro/nolist header
$ compile v68cio
$ macro/nolist cio
$!
$!	Compile the intermediate file dump utilties.
$!
$ compile hdr,sd
$ compile hdr,athdr,ad
$ compile hdr,hdrc,hdrc2,td
$!
$!	Now make things easy for the link phase.
$!
$ copy m.obj pas68.lib
$ append csi.obj,scan.obj,p2rdreal.obj,comma.obj,mda.obj pas68.lib
$ append foldcom.obj,analys.obj,body.obj,travrs.obj pas68.lib
$ append commc.obj,commt.obj,improve.obj,walk.obj pas68.lib
$ append genblk.obj,code.obj,list.obj,error.obj,files.obj pas68.lib
$ append pdbfiles.obj,fakeutil.obj,v68cio.obj,putcode.obj pas68.lib
$ append header.obj,cio.obj pas68.lib
$!
$!	Link the cross compiler
$!
$ link/nouser/exec=pas68.exe/nomap pas68.lib,sys$library:p2vms/lib
$!
$!	Link the intermediate file dump programs
$ link/nomap sd
$ link/nomap ad
$ link/nomap td
