$	on control_y then goto abort
$       devname := "MKA500"
$       backitup := backup/log/interchange/verify
$!
$	write sys$output " "
$	write sys$output " "
$	write sys$output "       VMS command file to generate a VMS/68k "
$	write sys$output "               2.3.0 release "
$!
$	write sys$output " "
$	inquire answer "Please load a TK-50 -<CR> when ready"
$	write sys$output ""
$!
$	write sys$output "initializing the tape...."
$ 	init 'devname': pascal
$	mount 'devname':/foreign
$!
$	write sys$output "creating save sets.
$	write sys$output "------------------"
$!
$	backup/log/interchane/verify [...]*.*; -
		'devname':pascal.bck/save_set
$	 	 write sys$output "  save set = PASCAL "
$!
$	write sys$output " "
$	write sys$output "Tape format is: BACKUP"
$	write sys$output "Tape label is : PASCAL"
$	write sys$output " "
$	dismount/unload 'devname':
$	goto wrapup
$!
$ abort:
$	write sys$output "** Release aborted - no tape made"
$	dismount/nounload 'devname':
$ wrapup:
$	write sys$output " "
$	write sys$output "Please unload the tape."
$ exit
