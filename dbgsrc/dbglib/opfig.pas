{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: ~processor~
  System: VERSADOS

  Configuration header to compile Pascal-2 library modules

 Last modified by KRIS on 26-Nov-1990 13:43:39
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

const
{
The two operating system flags "versados" and "stndalon" are mutually
exclusive (only one may be true)
}
  versados = true; {The library is to be used under the VERSAdos system}
  stndalon = false; {The stand-alone version of the library}

  concurr = false; {The concurrent programming package is desired}
{
If "concurr" is non-zero, synchronizing code will be generated for all
modifications to system variables.  If it is zero, no locking code will
be generated.
}
  pic = false; {The library is position independent}
{
The three processor switches, "mc68000", "mc68010", and "mc68020" are
mutually exclusive (only one may be true).
}
  mc68000 = true; {The target processor is mc68000 (default exormacs)}
  mc68010 = false; {The target processor is mc68010 (default vme-10 series
                    systems)}
  mc68020 = false; {The target processor is mc68020 (default vme-131 board)}

  mc68881 = true; {The library is to support the mc68881 floating point
                   coprocessor}

  checking = true; {Enable consistency checks}
{
The constant "checking" is used to turn on and off code for runtime
consistency checks.   It is strongly recommended that this  be left on
except for the final production copy.  Any non-zero value enables checking.
}
  logging = true; {Enable event logging}
{
The constant "logging" enables the generation of  circular log entries for
synchronizing events.  This log is automatically printed when an error occurs,
and can help considerably in locating time-dependent errors.
}
  statistics = logging; {Enable statistics gathering}
{
The constant  "statistics"  is used to turn on and  off code for gathering
performance statistics.   It also determines  the size  of some of the data
structures allocated on the heap,  as space to hold the statistics is only
allocated when needed.
}
  romints = false; {Assume it's all RAM}
{
The constant  "romints"  must be set to 1 if the interrupt vectors are in
read-only memory.  This may be required if the rest of the load addresses are
to be in ROM.   In this case,  each vector which is  to be used with "waitint"
must contain the address of  a three word area which will be filled  at
runtime with "jsr return".
}
