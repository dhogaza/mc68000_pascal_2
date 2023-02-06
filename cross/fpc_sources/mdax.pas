{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler System-Specific Front End Routine Declarations

 Last modified by KRIS on 21-Nov-1990 15:19:18
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


procedure alloc(align: alignmentrange; {variable alignment}
                length: addressrange; {length of variable}
                var spacesize: addressrange; {size of data space}
                var varloc: addressrange; {loc of new variable}
                var overflowed: boolean {true if size overflowed} );

{ Allocate space for a single unpacked variable or field.

  A single field of length "length", with alignment "align",
  is added to the end of a data space which already had
  "spacesize" addressing units allocated.  The address of the
  newly allocated field is returned in "varloc", and "spacesize"
  is updated to include the new field.}

  external;


procedure allocpacked(align: alignmentrange; {variable alignment}
                      length: addressrange; {length of variable}
                      var spacesize: addressrange; {size of data space}
                      var varloc: addressrange; {loc of new variable}
                      var overflowed: boolean; {true if size overflowed}
                      var unusedspace: boolean {space skipped} );

{ Allocate space for a single packed field.

  A single field of length "length", with alignment "align",
  is added to the end of a data space which already had
  "spacesize" addressing units allocated.  The address of the
  newly allocated field is returned in "varloc", and "spacesize"
  is updated to include the new field.  "Unusedspace" is set
  if some space was left due to the allocation strategy.  This
  can be used to modify earlier fields for better access if
  desired.
}

  external;


procedure getallocdata(form: entryptr; {type being allocated}
                       varkind: nametype; {type of field or var}
                       packedresult: boolean; {result goes in packed field}
                       spacelen: addressrange; {space consumed so far}
                       var fieldlen: addressrange; {size to allocate}
                       var fieldalign: alignmentrange; {alignment for alloc}
                       var maxalign: alignmentrange {max alignment so far} );

{ Extract allocation data for a field or variable.

  This routine determines the length and alignment requirements for a
  field, variable, or parameter.  This may not be exactly the same
  as the space required for the value, since the field may be accessed
  indirectly, or other limitations may apply.

  "Form" points to the type entry for this variable, and "varkind" gives
  the usage.  "Fieldlen" and "fieldalign" must be set to define the space
  to be allocated, and "maxalign" must be updated to the alignment for
  the entire data space.  "Packedresult" and "spacelen" are included for
  information.
}

  external;


function arraysizeof(f: entryptr; {form to get size of}
                     packedresult: boolean {set if packed } ): addressrange;

{ Returns the amount of storage needed to contain a value of the type
  specified by "f", with desired modifications to ease array accessing.

  "Packedresult" determines if the size is in bits or addressing units.
}

  external;


procedure packedstore(eltloc: addressrange; {rel address of this field}
                      eltsize: addressrange; {size of this field}
                      baseloc: addressrange; {rel address of first buffer}
                      val: integer; {value to pack}
                      var pbuf1, pbuf2: integer; {packed buffers}
                      var full: boolean {buffer 1 is full} );

{ This routine stores the value "val" as a packed field.  The field address,
  in bits is "eltloc".  The result is to be returned in two integer buffers
  "pbuf1" and "pbuf2", which should be otherwise undisturbed.  The boolean
  "full" is set when "pbuf1" is full, and should be written.  "Baseloc" is
  the address (in bits) of the start of "pbuf1"
}

  external;


function roundpackedsize(spacesize: addressrange; {rounded space}
                         packedresult: boolean {true if packed record} ):
 addressrange;

{ Round length of declared type to an even multiple of bitsperunit if
  the type is packed.  Used to simplify code generator, which wishes to
  use unit-move instructions for structure assignments.
}

  external;


procedure possibletemp(off: addressrange;
                       vartype: index;
                       debugrec: integer);

{
    Purpose:
      Determine if tha var at off is eligible for assignment to register

    Inputs:
      off: offset from local data area of the possible temp.
      vartype: symbol table index to the var's type identifier.
      debugrec: offset in the symbol file where var's allocation is
                described.

    Outputs:
      If conditions are met then file "locals" has record appended.

    Algorithm:
      Straight forward.

    Sideeffects:
      None

    Last Modified: 7/16/85

}
  external;
