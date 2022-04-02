$	on control_y then goto abort
$       media := 'p1'
$       if media .eqs. "TU77" then goto check_tu77
$       if media .nes. "" then goto check_tk50
$ check_tu77:
$       media := "TU77"
$       devname := "mta1"
$       goto check_ok
$ check_tk50:
$       if media .nes. "TK50" then goto check_failed
$       devname := "mua0"
$       goto check_ok
$ check_failed:
$	write sys$output " "
$	write sys$output "       The supported devices are: TU77(default) and TK50"
$	write sys$output "       Release aborted."
$	write sys$output " "
$ exit
$ check_ok:
$       backitup := backup/block=16384/buffer_count=5
$!
$!
$	write sys$output " "
$	write sys$output " "
$	write sys$output "       VMS command file to generate a VMS/68k "
$	write sys$output "               2.3.0 "'media'" release "
$       if media .nes. "TU77" then goto init_tk50
$!
$!
$!	Get the tape ready.
$!
$	write sys$output " "
$	inquire answer "Please mount a 600' tape on the Tu77 -<CR> when ready"
$	write sys$output ""
$!
$ get_density:
$	inquire density "density:<800,1600> "
$	if density .eqs. "800" then goto good_density
$       if density .eqs. "1600" then goto good_density
$	write sys$output "...bad density"
$	goto get_density
$!
$ good_density:
$	write sys$output "initializing the tape...."
$	init 'devname':/dens='density' pascal
$	mount 'devname':/foreign
$!
$ goto again
$!
$ init_tk50:
$!
$!	Get the tape ready.
$!
$	write sys$output " "
$	inquire answer "Please load a TK-50 -<CR> when ready"
$	write sys$output ""
$!
$	write sys$output "initializing the tape...."
$ 	init 'devname': pascal
$	mount 'devname':/foreign
$!
$ goto again
$ again:
$!
$!
$!	Figure out what should be sent.
$!
$!
$	write sys$output " "
$	write sys$output "   ** PRODUCT SELECTION **	"
$  	write sys$output "---------------------------------
$	write sys$output " "
$	write sys$output "Answer YES, NO or END to the following questions."
$	write sys$output "An answer other than YES and END is the same as NO."
$	write sys$output "An answer of YES will include the item in the release."
$	write sys$output "An answer of END can be used to terminate selection process."
$	write sys$output " "
$!
$	crossbin 	= "NO"
$	cppsrc	 	= "NO"
$	cppbin	 	= "NO"
$	ola		= "NO"
$	libsrc		= "NO"
$	dbgsrc		= "NO"
$	nativesrc	= "NO"
$	crosssrc	= "NO"
$       stndalone	= "NO"
$       stndsrc  	= "NO"
$
$	opts		= "<YES,NO,END> "
$	inquire		 crossbin  "VMS->68K cross compiler binary.."'opts'
$       if crossbin .nes. "END" then goto endcrossbin
$ 	crossbin := "NO"
$       goto sel_ready
$ endcrossbin:
$	inquire		 ola	   "OL/A 1.2A......................."'opts'
$       if ola .nes. "END" then goto endola
$ 	ola := "NO"
$       goto sel_ready
$ endola:
$	inquire		 cppbin	   "CPP 1.2B binaries .............."'opts'
$       if cppbin .nes. "END" then goto endcppbin
$ 	cppbin := "NO"
$       goto sel_ready
$ endcppbin:
$	inquire		 cppsrc	   "CPP 1.2B sources ..............."'opts'
$       if cppsrc .nes. "END" then goto endcppsrc
$ 	cppsrc := "NO"
$       goto sel_ready
$ endcppsrc:
$       inquire          stndalone "SA template library binaries ..."'opts'
$       if stndalone .nes. "END" then goto endstndalone
$ 	stndalone := "NO"
$       goto sel_ready
$ endstndalone:
$	inquire          libsrc    "VERSAdos Library sources ......."'opts'
$       if libsrc .nes. "END" then goto endlibsrc
$ 	libsrc := "NO"
$       goto sel_ready
$ endlibsrc:
$       inquire          stndsrc   "SA template library sources ...."'opts'
$       if stndsrc .nes. "END" then goto endstndsrc
$ 	stndsrc := "NO"
$       goto sel_ready
$ endstndsrc:
$	inquire		 crosssrc  "VMS->68K cross compiler sources."'opts'
$       if crosssrc .nes. "END" then goto endcrosssrc
$ 	crosssrc := "NO"
$       goto sel_ready
$ endcrosssrc:
$!      inquire		 nativesrc "Native compiler sources ........"'opts'
$       if crosssrc .eqs. "YES" then crossbin := "YES"
$       if nativesrc .nes. "END" then goto endnativesrc
$ 	nativesrc := "NO"
$       goto sel_ready
$ endnativesrc:
$	inquire          dbgsrc    "Debugger sources ..............."'opts'
$       if dbgsrc .nes. "END" then goto enddbgsrc
$ 	dbgsrc := "NO"
$       goto sel_ready
$ enddbgsrc:
$ sel_ready:
$!
$	write sys$output " "
$	write sys$output "If the order doesn't fit any of the above,"
$	write sys$output "  type <ctrl>Y and see Kris."
$!
$	write sys$output " "
$	write sys$output "================================="
$	write sys$output "Items included in this order are:"
$
$	if crossbin .eq. "YES" then goto something
$	if cppsrc .eq. "YES" then goto something
$	if cppbin .eq. "YES" then goto something
$	if ola .eq. "YES" then goto something
$	if libsrc .eq. "YES" then goto something
$	if dbgsrc .eq. "YES" then goto something
$	if nativesrc .eq. "YES" then goto something
$	if crosssrc .eq. "YES" then goto something
$	if stndalone .eq. "YES" then goto something
$	if stndsrc .eq. "YES" then goto something
$	write sys$output " "
$	write sys$output "          *** NONE ***"
$	goto ask_try
$!
$ something:
$	if crossbin .eq. "YES" then write sys$output "  o VMS->68K cross compiler binary"
$	if ola .eq. "YES" then write sys$output "  o OL/A 1.2A"
$	if cppbin .eq. "YES" then write sys$output "  o CPP 1.2B binaries"
$	if stndalone .eq. "YES" then write sys$output "  o SA template library binaries"
$	if libsrc .eq. "YES" then write sys$output "  o VERSAdos Library sources"
$	if stndsrc .eq. "YES" then write sys$output "  o SA template library sources"
$	if cppsrc .eq. "YES" then write sys$output "  o CPP 1.2B sources"
$	if crosssrc .eq. "YES" then write sys$output "  o VMS->68K cross compiler sources"
$	if nativesrc .eq. "YES" then write sys$output "  o Native compiler sources"
$	if dbgsrc .eq. "YES" then write sys$output "  o Debugger sources"
$
$	write sys$output " "
$	inquire ok "Is this what you want to send <YES,NO> "
$       if ok .eq. "YES" then goto cut_tape
$!
$ ask_try:
$	write sys$output " "
$	inquire again "Try again <YES,NO> "
$	if again .eq. "YES" then goto again
$   	goto abort
$!
$!	Now actually create the save sets
$ cut_tape:
$	write sys$output "creating save sets.
$	write sys$output "------------------"
$
$	if crossbin .nes. "YES" then goto jump1
$  		 backitup work:[rel.vms68k.2301.cross.compiler...],-
		        work:[rel.vms68k.2301.cross.utility...] 'devname':pascal.bck
$	 	 write sys$output "  save set = PASCAL "
$ jump1:
$	if cppsrc .nes. "YES" then goto jump2
$		backitup work:[rel.cpp.v12b.relsrc...] 'devname':cppsrc.bck
$		write sys$output "  save set = CPPSRC"
$ jump2:
$	if ola .nes. "YES" then goto jump3
$		backitup work:[rel.ola.12a.vms.ola] 'devname':ola.bck
$		write sys$output "  save set = OLA"
$ jump3:
$	if libsrc .nes. "YES" then goto jump4
$		backitup work:[rel.vms68k.2301.libsrc...] 'devname':libsrc.bck
$		write sys$output "  save set = LIBSRC"
$ jump4:
$	if dbgsrc .nes. "YES" then goto jump5
$		backitup work:[rel.vms68k.2301.dbgsrc...] 'devname':dbgsrc.bck
$		write sys$output "  save set = DBGSRC"
$ jump5:
$	if nativesrc .nes. "YES" then goto jump6
$		backitup work:[rel.vms68k.2301.nativesrc...] 'devname':nativesrc.bck
$		write sys$output "  save set = NATIVESRC"
$ jump6:
$	if crosssrc .nes. "YES" then goto jump7
$		backitup work:[rel.vms68k.2301.cross.sources...] 'devname':crosssrc.bck
$		write sys$output "  save set = CROSSSRC"
$ jump7:
$       if stndalone .nes. "YES" then goto jump8
$               backitup work:[rel.vms68k.2301.template...] 'devname':stand.bck
$               write sys$output " save set = STAND"
$ jump8:
$       if stndsrc .nes. "YES" then goto jump9
$               backitup work:[rel.vms68k.2301.template...], -
                work:[rel.vms68k.2301.templatesrc...] -
                  'devname':standsrc.bck
$               write sys$output " save set = STANDSRC"
$ jump9:
$	if cppbin .nes. "YES" then goto jump10
$		backitup work:[rel.cpp.v12b.relbin...] 'devname':cpp.bck
$		write sys$output "  save set = CPP"
$!		backitup work:[rel.cpp.v12b.simcpp...] 'devname':simcpp.bck
$!		write sys$output "  save set = SIMCPP"
$
$ jump10:
$!
$	write sys$output " "
$	write sys$output "   o Include a copy of the vms --> 68K release notes."
$	write sys$output "   o Supply the appropriate manuals."
$	write sys$output "   o Update RDM records"
$	if crossbin .nes. "YES" then goto no_flop
$	write sys$output "   o generate floppies on the VME using 109.&.RELCROSS.CF"
$ no_flop:
$	write sys$output " "
$	write sys$output "Tape format is: BACKUP"
$	write sys$output "Tape label is : PASCAL"
$	write sys$output " "
$	write sys$output "*** VERSADOS RELEASE GENERATED"
$	dismount/nounload 'devname':
$	goto wrapup
$!
$ abort:
$	write sys$output "** Release aborted - no tape made"
$	dismount/nounload 'devname':
$ wrapup:
$	write sys$output " "
$	write sys$output "Please unload the tape."
$ exit
