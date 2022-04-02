{[l-,r+,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Dump the contents of the symbol file created by the Pascal compiler.

 Last modified by KRIS on 26-Nov-1990 13:53:13
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{$double}

const

  { Symbol Table Access }

  intindex = 1; { integer form index }
  shortindex = 2; { short integer form index }
  realindex = 4; { real form index }
  doubleindex = 5; {double real index}
  charindex = 6; { char form index }
  boolindex = 7; { boolean form index }
  textindex = 8; { text file form index }
  nilindex = 9; { NIL form index }
  noneindex = 9; { error form index }

type

  double = real;

%include arccon
  
%include hstcon

%INCLUDE dbghdr;

  VAR
    filename: PACKED ARRAY [1..80] OF char;
    dbgfile: FILE OF debugrecord;
    recnumber: integer;

    { Decode special type names }


  PROCEDURE saytype(t: integer);


    BEGIN
      CASE t OF
        intindex: write('Integer');
        shortindex: write('ShortInt');
        realindex: write('Real');
        doubleindex: write('DoubleReal');
        charindex: write('Char');
        boolindex: write('Boolean');
	textindex: write('Text');
        OTHERWISE write(t: 1)
        END;
    END;

  {       Write out variable description }


  PROCEDURE dumpvar;


    BEGIN
      WITH dbgfile^ DO
        BEGIN
        CASE varalloc OF
          ptrregister:
            BEGIN
            write('Register:');
            CASE offset OF
              1: write(' A3');
              2: write(' A4');
              3: write(' A5');
              OTHERWISE write(' R??')
              END;
            END;
          genregister:
            BEGIN
            write('Register:');
	    if hostmachine = i80386 then
              CASE offset OF
                1: write('EBX');
                OTHERWISE write(' R??')
              END
            else
              if hostmachine = mc68000 then
                CASE offset OF
                  1: write(' D7');
                  2: write(' D6');
                  3: write(' D5');
                  OTHERWISE write(' R??')
                END
	      else if hostmachine = iapx86 then
                CASE offset OF
                  1: write(' BX');
                  OTHERWISE write(' R??')
                END
	      else
                CASE offset OF
                  1: write(' R12');
                  2: write(' R10');
                  3: write(' R9');
                  OTHERWISE write(' R??')
                  END;
              END;
	  realregister: ;
          absolute: write('Origin: ', offset: - 1, 'X');
          normalalloc: write('Normal: Offset: ', offset: 4);
          ownalloc: write('Own: Offset: ', offset: 4);
     	  pointeralloc: write('Pointer: Offset: ', offset: 4);
	  definealloc: write('Define: Offset: ', offset: 4);
	  usealloc: write('Use: Offset: ', offset: 4);
	  sharedalloc: write('Shared: Offset: ', offset: 4);
          END; { Case }
        write(' length: ', length: 2, ' ');
        IF vartype <= textindex THEN saytype(vartype)
        ELSE write('type: ', vartype: 1);
        END;
    END;

  {       Write out procedure description }


  PROCEDURE dumpproc;

    var idx: integer;

    BEGIN
      WITH dbgfile^ DO
        BEGIN
        writeln;
        write('            ');
        write('Id: ', id: 1, ' first statement: ', firststmt: 1,
              ' first name: ', firstname: 1);
        write(' last name: ', lastname: 1);
        writeln;
        write('            ');
        write('level: ', level: 1);
        write(' next proc: ', nextprocedure: 1);
	writeln;
        write('            ');
	write('Saved registers: ');
	if (not (hostmachine in [iapx86, i80386])) then
          for idx := 0 to maxptrregistermask do
	    if ptrregssaved[idx] then write('  A', idx:1);
	for idx := 0 to maxgenregistermask do
	  if hostmachine = mc68000 then begin
	    if genregssaved[idx] then write('  D', idx:1); end
	   else
	     if hostmachine = iapx86 then begin
	       if genregssaved[idx] then
	         if idx = 0 then write('  AX')
	           else if idx = 1 then write('  CX')
	  	     else if idx = 2 then write('  DX')
		       else if idx = 3 then write('  BX')
		         else if idx = 4 then write('  SP')
		           else if idx = 5 then write('  BP')
			     else if idx = 6 then write('  SI')
			       else write('  DI'); end
              else if hostmachine = i80386 then begin
	             if genregssaved[idx] then
	               if idx = 0 then write('  EAX')
	                 else if idx = 1 then write('  ECX')
		           else if idx = 2 then write('  EDX')
		             else if idx = 3 then write('  EBX')
		               else if idx = 4 then write('  ESP')
		                 else if idx = 5 then write('  EBP')
			           else if idx = 6 then write('  ESI')
			             else write('  EDI'); end
	    else if genregssaved[idx] then write('  R', idx:1);
        IF namekind = funcname THEN
          BEGIN
          writeln;
          write('            ');
          write('function type: ');
          saytype(functype);
          write(' function length: ', funclen: 1);
          END;
        END;
    END;

  {       Main program }


  BEGIN
    recnumber := 1;
    writeln;
    write('Debug file name: ');
    readln(filename);
    reset(dbgfile, filename, symext);
    write('Output file name: ');
    readln(filename);
    IF filename[1] <> ' ' THEN rewrite(output, filename, symdmpext);
    WHILE NOT eof(dbgfile) DO
      BEGIN
      write(recnumber: 3, ' ');
      WITH dbgfile^ DO
        CASE kind OF
          identdesc: 
            begin
            write('Ident:  ', identchars);
            write('Chainoffset:  ', chainoffset);
            end;
          symboldesc:
            BEGIN
            write('Symbol: ');
            write('name id: ', name: 1, ' ');
            writeln;
            write('            ');
            CASE namekind OF
              typename: write('type ', typeindex: 1);
              constname:
                BEGIN
                write('constant ');
                write('  type ', consttype:1);
                write('    ');
                CASE constform OF
                  ints: write('integer: ', i: 1);
                  chars: write('char: "', chr(i), '"');
                  bools:
                    BEGIN
                    write('boolean: ');
                    IF i = 0 THEN write('false')
                    ELSE write('true');
                    END;
                  scalars: write('scalar: ', i: 1);
                  reals: write('real: ', loophole(real, r));
                  ptrs: write('pointer (NIL)');
                  ELSE write('Unknown constant type')
                  END; { case constform }
                IF extendedint THEN write(' (Extended)');
                END; { constname }
              varname:
                BEGIN
                write('Variable name       ');
                dumpvar;
                END;
              param:
                BEGIN
                write('Parameter           ');
                dumpvar;
                END;
              varparam:
                BEGIN
                write('Var parameter       ');
                dumpvar;
                END;
              fieldname:
                BEGIN
                write('Field name          ');
                dumpvar;
                END;
              procparam:
                BEGIN
                write('Procedure parameter ');
                dumpvar;
                END;
              funcparam:
                BEGIN
                write('Function parameter  ');
                dumpvar;
                END;
              confparam:
                BEGIN
                write('Conformant array    ');
                dumpvar;
                END;
              varconfparam:
                BEGIN
                write('VAR conformant array');
                dumpvar;
                END;
              boundid:
                BEGIN
                write('Conf. array limit   ');
                dumpvar;
                END;
              procname:
                BEGIN
                write('Procedure name      ');
                dumpproc;
                END;
              funcname:
                BEGIN
                write('Function name       ');
                dumpproc;
                END;
              noname: write('Noname?');
              scalarname: write('Scalar name?');
              forwardproc: write('Forward procedure definition?');
              forwardfunc: write('Forward function definition?');
              externalproc: write('External procedure');
              externalfunc: write('External function');
              undeftypename: write('Undefined type name?');
              undefname: write('Undefined name?');
              standardproc: write('Standard procedure?');
              standardfunc: write('Standard function?');
              ELSE writeln('Unknown type.  ord=', ord(namekind): 1)
              END; { case namekind }
            END; { symboldesc }
          formdesc:
            BEGIN
            write('Form:   ');
            write('size: ', size: 3, ' ');
            IF packedflag THEN write('(packed) ');
            IF bitaddress THEN write('(bit) ');
            CASE typ OF
              subranges:
                BEGIN
                write('subrange: ', lowerord: 1, '..', upperord: 1, ' of ');
                saytype(parenttype);
                IF extended THEN write(' (Extended)');
                END;
              fields:
                begin
                write('field: id: ', fieldid: 1, ' level: ', fieldlevel: 1);
                writeln;
                write('            ');
                write('firstfield: ', firstfield: 1,
                      ' lastfield: ', lastfield: 1);
                end;
              arrays, conformantarrays:
                BEGIN
                IF typ = conformantarrays THEN write('conformant ');
                write('array [');
                saytype(indextype);
                write('] of ');
                saytype(elementtype);
                write('  eltsize=', elementsize: 1);
                IF typ = conformantarrays THEN
                  BEGIN
                  writeln;
                  write('Index of lower array limit: ': 50, lowbound: 1);
                  END;
                END;
              sets:
                BEGIN
                write('set of ');
                saytype(basetype);
                END;
{
              strings:
                begin
                write('string ');
                end;
}
              files:
                BEGIN
	        if recnumber = textindex then saytype(textindex)
                else 
		  begin 
	          write('file of ');
                  saytype(filebasetype);
		  end;
                END;
              ptrs:
                BEGIN
		if recnumber = nilindex then write('pointer (NIL)')
		else
		  begin
                  write('pointer to ');
                  saytype(ptrtype);
		  end;
                END;
              variantlabs: write('Variant label');
              scalars:
                write('Scalar  last ord: ', lastord: 1, '  first scalar: ',
                      firstscalar: 1);
              ints: write('Integer');
              bools: write('Boolean');
              chars: write('Char');
              reals: write('Real');
	      doubles: write('Double real');
              none: write('None?');
              ELSE write('Unknown type')
              END; { case typ }
            END; { formdesc }
          OTHERWISE writeln('???', ord(kind): 1);
          END; { case kind }
      writeln;
      get(dbgfile);
      recnumber := recnumber + 1;
      END; {while}
  END.
