$!
$! Determine the directory to build library object modules.  
$! "[.libp]" may be changed to any other directory.
$!
$ assign [.libp] lb68
$!
$! Make sure the configuration headers are in the above directory.
$! The command file will be aborted when one of the opfig files 
$! is not found in the lb68: directory.
$!
$ sea lb68:opfig.pas/nolog/ou=nl: release
$ sea lb68:opfig.sa/nolog/ou=nl: release
$!
$! Define logical symbols used to invoke the cross compiler and cross
$! assembler.
$!
$ pas68 := $sys$system:pas68.exe/nowalk/nocheck/pic/incl=lb68:
$ asm68 :== $sys$system:asm68.exe
$!
$! Start the compilation and assembly.
$!
$ asm68 lb68:opfig,linkde/obj=lb68:linkde
$ asm68 lb68:opfig,initia/obj=lb68:initia
$ asm68 lb68:opfig,openin/obj=lb68:openin
$ asm68 lb68:opfig,openou/obj=lb68:openou
$ asm68 lb68:opfig,bktrap/obj=lb68:bktrap
$ pas68 lb68:opstri=opstri
$ pas68 lb68:opstrf=opstrf
$ pas68 lb68:opstrd=opstrd
$ pas68 lb68:opstr=opstr
$ asm68 lb68:opfig,opstrr/obj=lb68:opstrr
$ asm68 lb68:opfig,opster/obj=lb68:opster
$ pas68 lb68:oprdf=oprdf
$ pas68 lb68:opwtf1=opwtf1
$ pas68 lb68:opwtf2=opwtf2
$ pas68 lb68:opftoc=opftoc
$ pas68 lb68:opctof=opctof
$ pas68 lb68:oprdd=oprdd
$ pas68 lb68:opwtd1=opwtd1
$ pas68 lb68:opwtd2=opwtd2
$ pas68 lb68:opdtoc=opdtoc
$ pas68 lb68:opctod=opctod
$ pas68 lb68:fexp=fexp
$ pas68 lb68:flog=flog
$ pas68 lb68:fsin=fsin
$ pas68 lb68:fatn=fatn
$ asm68 lb68:opfig,ffloat/obj=lb68:ffloat
$ asm68 lb68:opfig,ftrunc/obj=lb68:ftrunc
$ asm68 lb68:opfig,fround/obj=lb68:fround
$ asm68 lb68:opfig,fcmp/obj=lb68:fcmp
$ asm68 lb68:opfig,fadd/obj=lb68:fadd
$ asm68 lb68:opfig,fmul/obj=lb68:fmul
$ asm68 lb68:opfig,fdiv/obj=lb68:fdiv
$ asm68 lb68:opfig,fsqr/obj=lb68:fsqr
$ asm68 lb68:opfig,fsqrt/obj=lb68:fsqrt
$ asm68 lb68:opfig,ftime/obj=lb68:ftime
$ asm68 lb68:opfig,opftod/obj=lb68:opftod
$ asm68 lb68:opfig,opdtof/obj=lb68:opdtof
$ asm68 lb68:opfig,opffltu/obj=lb68:opffltu
$ asm68 lb68:opfig,opdfltu/obj=lb68:opdfltu
$ pas68 lb68:dexp=dexp
$ pas68 lb68:dlog=dlog
$ pas68 lb68:dsin=dsin
$ pas68 lb68:datn=datn
$ asm68 lb68:opfig,dtime/obj=lb68:dtime
$ pas68 lb68:dsqrt=dsqrt
$ asm68 lb68:opfig,dsqr/obj=lb68:dsqr
$ asm68 lb68:opfig,dfloat/obj=lb68:dfloat
$ asm68 lb68:opfig,dcmp/obj=lb68:dcmp
$ asm68 lb68:opfig,dtrunc/obj=lb68:dtrunc
$ asm68 lb68:opfig,dround/obj=lb68:dround
$ asm68 lb68:opfig,dadd/obj=lb68:dadd
$ asm68 lb68:opfig,dadx/obj=lb68:dadx
$ asm68 lb68:opfig,dintx/obj=lb68:dintx
$ asm68 lb68:opfig,dmul/obj=lb68:dmul
$ asm68 lb68:opfig,ddiv/obj=lb68:ddiv
$ asm68 lb68:opfig,opdtox/obj=lb68:opdtox
$ asm68 lb68:opfig,opxtod/obj=lb68:opxtod
$ asm68 lb68:opfig,dswap/obj=lb68:dswap
$ asm68 lb68:opfig,fpc/obj=lb68:fpc
$ asm68 lb68:opfig,traps/obj=lb68:traps
$ asm68 lb68:opfig,nofpc/obj=lb68:nofpc
$ asm68 lb68:opfig,erhand/obj=lb68:erhand
$ pas68 lb68:pma=pma
$ asm68 lb68:opfig,dgdata/obj=lb68:dgdata
$ asm68 lb68:opfig,errmsg/obj=lb68:errmsg
$ pas68 lb68:getlno=getlno
$ asm68 lb68:opfig,term/obj=lb68:term
$ pas68 lb68:rename=rename
$ pas68 lb68:delete=delete
$ asm68 lb68:opfig,open/obj=lb68:open
$ pas68 lb68:switch=switch
$ pas68 lb68:parse=parse
$ asm68 lb68:opfig,fdefau/obj=lb68:fdefau
$ pas68 lb68:closer=closer
$ asm68 lb68:opfig,close/obj=lb68:close
$ asm68 lb68:opfig,new/obj=lb68:new
$ asm68 lb68:opfig,dispos/obj=lb68:dispos
$ asm68 lb68:opfig,getpos/obj=lb68:getpos
$ asm68 lb68:opfig,setpos/obj=lb68:setpos
$ asm68 lb68:opfig,seek/obj=lb68:seek
$ pas68 lb68:oprdi=oprdi
$ pas68 lb68:opwti=opwti
$ pas68 lb68:opctoi=opctoi
$ asm68 lb68:opfig,rdfast/obj=lb68:rdfast
$ asm68 lb68:opfig,rdchar/obj=lb68:rdchar
$ asm68 lb68:opfig,rdln/obj=lb68:rdln
$ asm68 lb68:opfig,rdstr/obj=lb68:rdstr
$ asm68 lb68:opfig,wrbool/obj=lb68:wrbool
$ asm68 lb68:opfig,wrchar/obj=lb68:wrchar
$ asm68 lb68:opfig,wrstr/obj=lb68:wrstr
$ asm68 lb68:opfig,page/obj=lb68:page
$ asm68 lb68:opfig,get/obj=lb68:get
$ asm68 lb68:opfig,put/obj=lb68:put
$ asm68 lb68:opfig,input/obj=lb68:input
$ asm68 lb68:opfig,output/obj=lb68:output
$ pas68 lb68:pack=pack
$ asm68 lb68:opfig,packer/obj=lb68:packer
$ pas68 lb68:timest1=timest1
$ asm68 lb68:opfig,timest/obj=lb68:timest
$ asm68 lb68:opfig,getcmd/obj=lb68:getcmd
$ asm68 lb68:opfig,exitst/obj=lb68:exitst
$ asm68 lb68:opfig,callta/obj=lb68:callta
$ asm68 lb68:opfig,callin/obj=lb68:callin
$ asm68 lb68:opfig,space/obj=lb68:space
$ pas68 lb68:expfnm=expfnm
$ asm68 lb68:opfig,prctyp/obj=lb68:prctyp
$ asm68 lb68:opfig,picoff/obj=lb68:picoff
$ asm68 lb68:opfig,stdio/obj=lb68:stdio
$ asm68 lb68:opfig,ioerro/obj=lb68:ioerro
$ asm68 lb68:opfig,breaks/obj=lb68:breaks
$ pas68 lb68:memmap=memmap
$ pas68 lb68:filist=filist
$ pas68 lb68:fdump=fdump
$ pas68 lb68:iodmp=iodmp
$ pas68 lb68:fhsdmp=fhsdmp
$ pas68 lb68:dmploc=dmploc
$ pas68 lb68:dmparea=dmparea
$ asm68 lb68:opfig,pdlow/obj=lb68:pdlow
$ asm68 lb68:opfig,opitoc/obj=lb68:opitoc
$ asm68 lb68:opfig,imul/obj=lb68:imul
$ asm68 lb68:opfig,idiv/obj=lb68:idiv
$ asm68 lb68:opfig,ferror/obj=lb68:ferror
$ asm68 lb68:opfig,filerr/obj=lb68:filerr
$ asm68 lb68:opfig,entlib/obj=lb68:entlib
$ asm68 lb68:opfig,fhscal/obj=lb68:fhscal
$ asm68 lb68:opfig,defwor/obj=lb68:defwor
$ asm68 lb68:opfig,defwkl/obj=lb68:defwkl
$ asm68 lb68:opfig,erstk/obj=lb68:erstk
$ asm68 lb68:opfig,txtbuf/obj=lb68:txtbuf
$ asm68 lb68:opfig,fpctrl/obj=lb68:fpctrl
$!
$! Concatenate all object modules into a library called pascalib.ro.
$!
$ copy lb68:linkde.ro,lb68:initia.ro,lb68:openin.ro lb68:pascalib.ro
$ append lb68:openou.ro,lb68:bktrap.ro,lb68:opstri.ro lb68:pascalib.ro
$ append lb68:opstrf.ro,lb68:opstrd.ro,lb68:opstr.ro lb68:pascalib.ro
$ append lb68:opster.ro,lb68:opstrr.ro,lb68:oprdf.ro lb68:pascalib.ro
$ append lb68:opwtf1.ro,lb68:opwtf2.ro,lb68:opctof.ro lb68:pascalib.ro
$ append lb68:opftoc.ro,lb68:oprdd.ro,lb68:opwtd1.ro lb68:pascalib.ro
$ append lb68:opwtd2.ro,lb68:opctod.ro,lb68:opdtoc.ro lb68:pascalib.ro
$ append lb68:fexp.ro,lb68:flog.ro,lb68:fsin.ro,lb68:fatn.ro lb68:pascalib.ro
$ append lb68:ffloat.ro,lb68:ftrunc.ro,lb68:fround.ro lb68:pascalib.ro
$ append lb68:fcmp.ro,lb68:fadd.ro,lb68:fmul.ro,lb68:fdiv.ro lb68:pascalib.ro
$ append lb68:fsqr.ro,lb68:fsqrt.ro,lb68:ftime.ro lb68:pascalib.ro
$ append lb68:opftod.ro,lb68:opdtof.ro,lb68:dexp.ro lb68:pascalib.ro
$ append lb68:dlog.ro,lb68:dsin.ro,lb68:datn.ro,lb68:dtime.ro lb68:pascalib.ro
$ append lb68:dsqrt.ro,lb68:dsqr.ro,lb68:dfloat.ro,lb68:dcmp.ro lb68:pascalib.ro
$ append lb68:dtrunc.ro,lb68:dround.ro,lb68:dadd.ro lb68:pascalib.ro
$ append lb68:dadx.ro,lb68:dintx.ro,lb68:dmul.ro,lb68:ddiv.ro lb68:pascalib.ro
$ append lb68:opdtox.ro,lb68:opxtod.ro,lb68:dswap.ro lb68:pascalib.ro
$ append lb68:traps.ro,lb68:erhand.ro,lb68:pma.ro lb68:pascalib.ro
$ append lb68:dgdata.ro,lb68:errmsg.ro,lb68:getlno.ro lb68:pascalib.ro
$ append lb68:term.ro,lb68:rename.ro,lb68:delete.ro lb68:pascalib.ro
$ append lb68:open.ro,lb68:parse.ro,lb68:switch.ro lb68:pascalib.ro
$ append lb68:fdefau.ro,lb68:closer.ro,lb68:close.ro lb68:pascalib.ro
$ append lb68:new.ro,lb68:dispos.ro,lb68:getpos.ro lb68:pascalib.ro
$ append lb68:setpos.ro,lb68:seek.ro,lb68:oprdi.ro lb68:pascalib.ro
$ append lb68:opwti.ro,lb68:opctoi.ro,lb68:rdfast.ro lb68:pascalib.ro
$ append lb68:rdchar.ro,lb68:rdln.ro,lb68:rdstr.ro lb68:pascalib.ro
$ append lb68:wrbool.ro,lb68:wrchar.ro,lb68:wrstr.ro lb68:pascalib.ro
$ append lb68:page.ro,lb68:get.ro,lb68:put.ro,lb68:input.ro lb68:pascalib.ro
$ append lb68:output.ro,lb68:pack.ro,lb68:packer.ro lb68:pascalib.ro
$ append lb68:timest.ro,lb68:getcmd.ro,lb68:exitst.ro lb68:pascalib.ro
$ append lb68:callta.ro,lb68:space.ro lb68:pascalib.ro
$! Following module is used to link overlays of VERSAdos native
$! compiler subtasks:
$!append lb68:callin.ro lb68:pascalib.ro
$ append lb68:expfnm.ro,lb68:prctyp.ro,lb68:picoff.ro lb68:pascalib.ro
$ append lb68:stdio.ro,lb68:ioerro.ro,lb68:breaks.ro lb68:pascalib.ro
$ append lb68:memmap.ro,lb68:filist.ro,lb68:fdump.ro lb68:pascalib.ro
$ append lb68:iodmp.ro,lb68:fhsdmp.ro,lb68:dmploc.ro lb68:pascalib.ro
$ append lb68:dmparea.ro,lb68:pdlow.ro,lb68:opitoc.ro lb68:pascalib.ro
$ append lb68:imul.ro,lb68:idiv.ro,lb68:ferror.ro lb68:pascalib.ro
$ append lb68:filerr.ro,lb68:entlib.ro,lb68:fhscal.ro lb68:pascalib.ro
$ append lb68:defwor.ro,lb68:defwkl.ro,lb68:erstk.ro lb68:pascalib.ro
$ append lb68:txtbuf.ro,lb68:fpctrl.ro lb68:pascalib.ro
$! Non-MC68881 library
$ append lb68:nofpc.ro lb68:pascalib.ro
$! MC68881 library
$!append lb68:fpc.ro lb68:pascalib.ro
