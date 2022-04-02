$! This command file will link the Pascal-2 utilities with the
$! Pascal Support Library supplied as P2VMS.OLB.  It assumes that
$! the library is in the default directory.
$!
$ copy [-.compiler]p2vms.olb []
$ link/nouser/nomap pasmat,p2vms/lib
$ link/nouser/nomap xref,p2vms/lib
$ link/nouser/nomap procre,p2vms/lib
$ link/nouser/nomap mapsym,p2vms/lib
$!
$ write sys$output "Pascal-2 Utilities have been built."
