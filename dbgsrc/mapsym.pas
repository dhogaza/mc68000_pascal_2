{[b+,o=80]}
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

  Produce human-readable storage map from compiler-produced symbol table
file.

 Last modified by KRIS on 21-Nov-1990 17:19:48
 Purpose:
Update release version for PU-VV0-GS0 at 2.3.0.1

}

program map;

 %include arccon;
 %include hstcon;
 %include dbghdr;

  const
    outstringlen = 15;
    indent = 2; {indent nested structures by this amount}
    globalbase = 0; {offset of first global variable}

    { Symbol Table Access }

    intindex = 1; { integer form index }
    shortintindex = 2; { shortint form index }
    realindex = 4; { real form index }
    doubleindex = 5; {double real index}
    charindex = 6; { char form index }
    boolindex = 7; { boolean form index }
    textindex = 8; { text file form index }
    nilindex = 9; { NIL form index }
    noneindex = 9; { error form index }

    maxsyms = 32000; {max number of symbol table entries expeccted}

    csiprompt = 'MAP>';

    %include csicon;

  type

    { command string interpreter parameters }

    argtype = (unknownarg, outfilearg, infilearg, malformedarg, missingarg);
    subargtype = 0..1;

    %include csityp;
    %include csipro;

  type
    debugfiletype = file of debugrecord;
    outstring = packed array [1..outstringlen] of char;
    offsetclasstype = (global, local, neither);
    typetoucharray = packed array [1..maxsyms] of boolean;

    linkrecordptr = ^linkrecord;
    linkrecord =
      record
        next: linkrecordptr;
        static: linkrecordptr;
        index: integer;
        id: integer;
      end;

  var
    mapfile: debugfiletype; {contains symbol table}
    mapidx: integer; {used to iterate through mapfile, looking for blocks}
    out: text; {human-readable (by some humans) output file}
    outcolumn: integer; { (unlimited!) column counter for output file }
    first, last: integer; {first and last names in this block}
    current: integer; {running index for finding blocks}
    typetouch: typetoucharray; {indicate if a form descriptor is used}
    symnumber: integer; {number of debug records in file}
    proclinkhead, currproclink: linkrecordptr;


  procedure exitst(i: integer);
    external; {quit the program}



{*************************************************************************

     ------ Command Argument Processing

*************************************************************************}

  %include getcs;


  procedure csi;

    type

      { Error Messages }
      csimessage = (extraexecfilemsg, { Extra input file }
                    cantopensym, { Unable to open symbol table file }
                    cantopenout, { Unable to output file }
                    malformedargmsg, { Badly formed argument }
                    missingargmsg, { Required argument missing }
                    xpanicmsg, { Unexpected condition }
                    unknownargmsg); { Unexpected argument }

    var
      argdefs: argdeftable; {valid command line arguments}
      outargname, inargname: argvalue; {hold name of executable file}
      inflg: boolean; {has executable file name been given}
      error: boolean; {was error encountered during command line processing}
      symsize, outsize: integer; {file size returned by reset/rewrite}
      i: integer;


    procedure setuperror(msg: csimessage;
                         arg: argvalue);


      begin {setuperror}
        case msg of
          extraexecfilemsg: writeln(' Extra executable file ');
          cantopensym: writeln(' Unable to open symbol table file ');
          cantopenout: writeln(' Unable to open output file ');
          malformedargmsg: writeln(' Badly formed argument ');
          missingargmsg: writeln(' Required argument missing ');
          unknownargmsg: writeln(' Unexpected argument ');
          end;
        if arg.len > 0 then write(' (', arg.txt: arg.len, ')');
        writeln;
        error := true;
      end; {setuperror}


    procedure processarg(arg: argvalue;
                         typ: argtype);


      begin { processarg }
        case typ of
          unknownarg: setuperror(unknownargmsg, arg);
          malformedarg: setuperror(malformedargmsg, arg);
          missingarg:
            if hostopsys = vms then setuperror(missingargmsg, inargname)
            else
              begin
              write(csiprompt, ' ');
              with inargname do
                begin
                len := 0;
                while not eoln do
                  begin
                  len := len + 1;
                  read(txt[len]);
                  end;
                readln;
                if len = 0 then setuperror(missingargmsg, inargname)
                else inflg := true;
                len := len + 1;
                txt[len] := ' ';
                end;
              end;
          infilearg:
            begin
            if inflg then setuperror(extraexecfilemsg, arg);
            inflg := true;
            inargname := arg;
            end;
          outfilearg: outargname := arg;
          end;
      end; { processarg }


    begin { csi }
      { Construct argument definition tables. }
      initdef(argdefs[unknownarg], '                ', 16, 0, optionalarg,
              nullarg);
      initdef(argdefs[malformedarg], '                ', 16, 0, optionalarg,
              nullarg);
      initdef(argdefs[missingarg], '                ', 16, 0, optionalarg,
              nullarg);
      initdef(argdefs[infilearg], 'input           ', 16, 1, requiredarg,
              filearg);
      initdef(argdefs[outfilearg], 'output          ', 1, 2, optionalarg,
              filearg);
      { Process command arguments. }
      error := false;
      inflg := false;
      inargname.len := 0;
      outargname.len := 0;
      getcs(argdefs, processarg);
      if error then exitst(4);
      reset(mapfile, inargname.txt, symext, symsize);
      if symsize = - 1 then
        begin
        setuperror(cantopensym, inargname);
        exitst(4);
        end
      else symnumber := symsize div size(debugrecord);
      if outargname.len = 0 then
        begin
        outargname := inargname;
        i := 1;
        while (i <= outargname.len) and (outargname.txt[i] <> '.') do
          i := i + 1;
        outargname.txt[i] := ' ';
        outargname.len := i;
        rewrite(out, outargname.txt, mapsymext, outsize);
        end
      else rewrite(out, outargname.txt, mapsymext, outsize);
      if outsize = - 1 then
        begin
        setuperror(cantopenout, outargname);
        exitst(4);
        end;
    end; { csi }



{output routines, provided to ease formatting
}


  procedure writech(ch: char);

{ Write one character to output file, counting column
}


    begin {writech}
      write(out, ch);
      outcolumn := outcolumn + 1;
    end {writech} ;


  procedure writenumber(i: integer; {integer to write}
                        extendedrange: boolean; {true if it is an "extended
                                                 range" number}
                        hex: boolean);

{ Write one number to output file, counting columns.
}

    var
      buf: array [1..12] of char; {for buffering number}
      bufcount: 0..12; {buffer index}
      negative: boolean; {true if negative number}
      uns: unsignedint; {for generating unsigned output}
      base: integer;


    begin {writenumber}
      if hex then base := 16
      else base := 10;
      bufcount := 0;
      if extendedrange then
        begin
        uns := i;
        repeat
          bufcount := bufcount + 1;
          buf[bufcount] := chr(uns mod base + ord('0'));
          if hex then
            if buf[bufcount] > '9' then
              buf[bufcount] := chr(ord(buf[bufcount]) - ord('9') + ord('@'));
          uns := uns div base;
        until uns = 0;
        end
      else
        begin
        negative := i < 0;
        i := abs(i);
        repeat
          bufcount := bufcount + 1;
          buf[bufcount] := chr(i mod base + ord('0'));
          if hex then
            if buf[bufcount] > '9' then
              buf[bufcount] := chr(ord(buf[bufcount]) - ord('9') + ord('@'));
          i := i div base;
        until i = 0;
        if negative then
          begin
          bufcount := bufcount + 1;
          buf[bufcount] := '-';
          end;
        end;
      outcolumn := outcolumn + bufcount;
      for bufcount := bufcount downto 1 do write(out, buf[bufcount]);
    end {writenumber} ;


  procedure writestring(s: outstring {blank-padded output string} );

    var
      i: 1..outstringlen; {for stripping blanks}


    begin {writestring}
      i := outstringlen;
      while (i > 1) and (s[i] = ' ') do i := i - 1;
      outcolumn := outcolumn + i + 1;
      write(out, s: i + 1);
    end {writestring} ;


  procedure writeline;

{ write eoln to output file.
}


    begin {writeline}
      writeln(out);
      outcolumn := 0;
    end {writeline} ;


  procedure tab(n: integer);

   {write "n" spaces to output file}

    var
      i: integer;


    begin
      for i := 1 to n do writech(' ');
    end {tab} ;


  function usertype(index: integer): integer;

    var
      j, last, d: integer;
      found: boolean;
      p: linkrecordptr;


    begin {Usertype}
      p := currproclink;
      d := current;
      found := false;
      usertype := 0;
      while (p <> nil) and not found do
        begin
        seek(mapfile, p^.index);
        j := mapfile^.firstname;
        if mapfile^.level = 0 then last := symnumber
        else last := mapfile^.nextprocedure;
        while (j <= last) and not found do
          begin
          seek(mapfile, j);
          with mapfile^ do
            if kind = symboldesc then
              if namekind = typename then found := index = typeindex;
          if not found then j := j + 1;
          end;
        p := p^.static;
        end; {while}
      if found then usertype := j;
      seek(mapfile, d);
    end; {usertype}




  procedure finddebugfile(var mapfile: debugfiletype;
                          index: integer);


    begin {position mapfile to record,index, and remember its }
      { number in Current }
      seek(mapfile, index);
      current := index;
    end;


  procedure writeatomictype(index: integer);


    begin {writeatomictype}
      case index of
        intindex: writestring(' Integer       ');
        shortintindex: writestring(' Shortint      ');
        boolindex: writestring(' Boolean       ');
        charindex: writestring(' Char          ');
        realindex: writestring(' Real          ');
        doubleindex: writestring(' Double        ');
        textindex: writestring(' Text          ');
        otherwise
          begin
          writeln(' Unexpected type name, index = ', index: 1);
          exitst(4);
          end;
        end
    end; {writeatomictype}


  procedure writeformdesc(index: integer; {mapfile index of form desc}
                          leftmargin: integer {for lining up output} );
    forward;


  procedure writeidentdesc(ident: integer {mapfile index pointing to name} );

   {write symbol name}

    var
      d, i: integer; {induction vars}


    begin {writeidentdesc}
      d := current;
      finddebugfile(mapfile, ident);
      if mapfile^.kind <> identdesc then
        begin
        writeln('expected "identdesc" record not found:', ident: 1);
        exitst(4);
        end;
      i := 1;
      repeat
        writech(mapfile^.identchars[i]);
        i := i + 1;
      until (i = 33) or (mapfile^.identchars[i] = ' '); {ouch, a kludge}
      finddebugfile(mapfile, d);
    end {writeidentdesc} ;


  procedure writeTypeName(typeidx, formidx: integer);

    var
      j: integer;


    begin {writeTypeName}
      writestring('Type           ');
      writech(' ');
      writeidentdesc(typeidx - 1);
      writech(' ');
      writech('=');
      writech(' ');
      writeformdesc(formidx, outcolumn);
      writech(';');
      writeline;
      writeline;
    end; {writeTypeName}




  procedure writesymboldesc(index: integer; {map file index of symbol desc}
                            bits: boolean; {true if offset field is in bits}
                            leftmargin: integer; {indent this far}
                            offsetclass: offsetclasstype; {kind of var}
                            baseoffset: integer {skew var/field offsets by this
                                                 amount} );

    var
      j: integer;


    procedure printvariablename(var mapfile: debugfiletype);


      begin {printvariablename}
        writech(' ');
        with mapfile^ do
          case namekind of
            varname:
              case offsetclass of
                local: writestring('Local          ');
                global: writestring('Global         ');
                end; {case}
            param:
              begin
              writestring('Parameter      ');
              if varalloc = pointeralloc then writestring(' (by pointer)  ');
              end;
            varparam: writestring('Var parameter  ');
            fieldname: ;
            procparam: writestring('Proc parameter ');
            funcparam: writestring('Func parameter ');
            confparam: writestring('Parameter      ');
            varconfparam: writestring('Var            ');
            boundid: writestring('Bound id       ');
            end; {case namekind}
        writech(' ');
      end; {printvariablename}


    procedure printallocation(var mapfile: debugfiletype);


      begin {printallocation}

        with mapfile^ do
          case varalloc of
            normalalloc:
              begin
              case offsetclass of
                local: writestring(' DL offset:    ');
                global: writestring(' GP offset:    ');
                otherwise writestring('    offset:    ');
                end; {case}
              if bits then
                begin
                writenumber(offset div 8 + baseoffset, false, false);
                writech('+');
                writenumber(abs(offset mod 8), false, false);
                writestring(' {bits}        ');
                writech(' ');
                end
              else writenumber(offset + baseoffset, false, false);
              end;
            ptrregister:
              begin
              writech(' ');
              writech('A');
              writenumber(offset + 2, true, false);
              end;
            genregister:
              begin
              writech(' ');
              if targetmachine = mc68000 then
                begin
                writech('D');
                CASE offset OF
                  1: writenumber(7, true, false);
                  2: writenumber(6, true, false);
                  3: writenumber(5, true, false);
                  END
                end
              else if targetmachine = iapx86 then
                begin
                writech(' ');
                CASE offset OF
                  1:
                    begin
                    writech('B');
                    writech('X');
                    end;
                  END
                end
                else if targetmachine = i80386 then begin
		  writech(' ');
		  CASE offset OF
		    1: begin
		      writech('E');
		      writech('B');
		      writech('X');
		    end;
		  END
	        end
              else
                begin
                writech('R');
                CASE offset OF
                  1: writenumber(9, true, false);
                  2: writenumber(10, true, false);
                  3: writenumber(12, true, false);
                  END;
                END;
              end;
            realregister:
              begin
              writech(' ');
              if targetmachine = mc68000 then
                begin
                writech('F');
                writech('P');
                CASE offset OF
                  1: writenumber(7, true, false);
                  2: writenumber(6, true, false);
                  3: writenumber(5, true, false);
                  END
                end;
              end;
            absolute:
              begin
              writestring(' absolute:     ');
              writenumber(offset, true, true); {not implemented - change when
                                                implemented}
              end;
            sharedalloc:
              begin
              writestring(' shared:       ');
              writenumber(offset, true, true);
              end;
            usealloc:
              begin
              writestring(' use:          ');
              writenumber(offset, true, true);
              end;
            definealloc:
              begin
              writestring(' define:       ');
              writenumber(offset, true, true);
              end;
            ownalloc:
              begin
              writestring(' OS offset:    ');
              if bits then
                begin
                writenumber(offset div 8 + baseoffset, false, false);
                writech('+');
                writenumber(abs(offset mod 8), false, false);
                writestring(' {bits}        ');
                writech(' ');
                end
              else writenumber(offset + baseoffset, false, false);
              end;
            pointeralloc:
              begin
              writestring(' DL offset:    ');
              if bits then
                begin
                writenumber(offset div 8 + baseoffset, false, false);
                writech('+');
                writenumber(abs(offset mod 8), false, false);
                writestring(' {bits}        ');
                writech(' ');
                end
              else writenumber(offset + baseoffset, false, false);
              end;
            end; {case}
      end; {printallocation}




    begin {writesymboldesc}
      finddebugfile(mapfile, index); {position debugfileptr}
      if mapfile^.kind <> symboldesc then
        begin
        writeln('Expected symboldesc no found in debugrecord: ', index: 1);
        exitst(4);
        end;
      case mapfile^.namekind of
        varname, param, varparam, fieldname, procparam, funcparam, confparam,
        varconfparam, boundid:
          begin
          printvariablename(mapfile);
          writech(' ');
          writeidentdesc(index - 1);
          writech(':');
          if mapfile^.namekind = procparam then writestring(' Procedure     ')
          else if mapfile^.namekind = funcparam then
            writestring(' Function      ')
          else if mapfile^.vartype < noneindex then
            writeatomictype(mapfile^.vartype)
          else
            begin
            j := usertype(mapfile^.vartype);
            if j = 0 then writeformdesc(mapfile^.vartype, leftmargin)
            else writeidentdesc(j - 1);
            end;
          writech(';');
          writech(' ');
          printallocation(mapfile);
          writestring(' Length:       ');
          writenumber(mapfile^.length, true, false);
          end;
        procname:
          begin
          writestring('Procedure      ');
          writech(' ');
          writeidentdesc(index - 1);
          writech(' ');
          writestring(' Entry address:');
          writech(' ');
          writenumber(mapfile^.entryaddress, true, false);
          writestring('  Local storage');
          writestring(' (units):      ');
          writenumber(mapfile^.blocksize, true, false);
          writeline;
          writeline;
          end;
        funcname:
          begin
          writestring('Function       ');
          writech(' ');
          writeidentdesc(index - 1);
          writech(':');
          writech(' ');
          writeformdesc(mapfile^.functype, outcolumn);
          writech(';');
          writestring('  Return value ');
          writestring(' size:         ');
          writech(' ');
          writenumber(mapfile^.funclen, true, false);
          writestring('  Return value ');
          writestring(' offset:       ');
          writech(' ');
          writenumber(baseoffset + mapfile^.funcoffset, true, false);
          writeline;
          writestring('Entry address: ');
          writech(' ');
          writenumber(mapfile^.entryaddress, true, false);
          writestring('  Local storage');
          writestring(' (units):      ');
          writech(' ');
          writenumber(mapfile^.blocksize, true, false);
          writeline;
          writeline;
          end;
        typename:
          begin
          writestring('?type          ');
          Writech(' ');
          writeidentdesc(mapfile^.typeindex - 1);
          writech(' ');
          writeline;
          end;
        end; {case}
    end; {writesymboldesc}




  procedure writenamelist(first: integer; {first name in list}
                          last: integer; {last name in list}
                          idnumber: integer; {id numbers to print}
                          bits: boolean; {bit offsets}
                          offsetclass: offsetclasstype; {which base reg}
                          baseoffset: integer; {skew by this amount}
                          leftmargin: integer {indention point} );

{ Write a list of name descriptions belonging to with id of "idnumber".
}

    var
      i: integer; {induction var}


    begin
      for i := first to last do
        begin
        finddebugfile(mapfile, i + 1);
        if mapfile^.kind = symboldesc then
          if (mapfile^.name = idnumber) then
            begin
            case mapfile^.namekind of
              varname, param, varparam, fieldname, confparam, varconfparam,
              boundid, procparam, funcparam:
                begin
                tab(leftmargin);
                writesymboldesc(i + 1, bits, leftmargin, offsetclass,
                                baseoffset);
                writeline;
                end;
              typename:
                if typetouch[mapfile^.typeindex] then
                  writetypename(i + 1, mapfile^.typeindex);
              otherwise {skip all others} ;
              end; {case}
            end; {if}
        end; {for}
    end {writenamelist} ;


  procedure writeformdesc; {FORWARD(index: integer; {mapfile index of form desc}
   {        leftmargin: integer {for lining up output}

  { Write a type structure.  If "usename" is true, we attempt to write
    a type name.  If we can't an attempt is made to write a reasonable
    representation of the type.
   }

    var
      d: integer; { local copy of current debugfile record}


    procedure writeformname(index: integer);

      var
        j, last, d: integer;
        found: boolean;
        p: linkrecordptr;


      begin {writeformname}
        d := current;
        p := currproclink;
        if index < noneindex then writeatomictype(index)
        else
          begin
          found := false;
          while (p <> nil) and not found do
            begin
            finddebugfile(mapfile, p^.index);
            j := mapfile^.firstname;
            if mapfile^.level = 0 then last := symnumber
            else last := mapfile^.nextprocedure;
            while (j <= last) and not found do
              begin
              finddebugfile(mapfile, j);
              with mapfile^ do
                if (kind = symboldesc) then
                  if (namekind = typename) then found := index = typeindex;
              if not found then j := j + 1;
              end; {while}
            p := p^.static;
            end; {while}
          if found then
            begin
            writeidentdesc(j - 1);
            end
          else
            begin
            writeln('Expected type name not found, index = ', index: 1);
            exitst(4);
            end;
          end;
        finddebugfile(mapfile, d);
      end; {writeformname}


    procedure printtype(var mapfile: debugfiletype);


      begin {printtype}
        case mapfile^.typ of
          ints: writestring(' Integer       ');
          bools: writestring(' Boolean       ');
          chars: writestring(' Char          ');
          reals: writestring(' Real          ');
          doubles: writestring(' Double        ');
          none: writestring(' ***undef?***  ');
          ptrs:
            begin
            writech(' ');
            writech('^');
            writeformname(mapfile^.ptrtype);
            end;
          files:
            begin
            writestring(' File of       ');
            writeformdesc(mapfile^.filebasetype, outcolumn);
            end;
          sets:
            begin
            writestring(' Set of        ');
            writeformdesc(mapfile^.basetype, outcolumn);
            end;
          scalars: writestring(' {user scalar} ');
          subranges:
            begin
            writenumber(mapfile^.lowerord, mapfile^.extended, false);
            writestring('..             ');
            writenumber(mapfile^.upperord, mapfile^.extended, false);
            end;
          fields:
            begin
            if outcolumn > 40 then writeline;
            leftmargin := outcolumn;
            writestring(' Record        ');
            writeline;
            writeline;
            writenamelist(mapfile^.firstfield, mapfile^.lastfield,
                          mapfile^.fieldid, mapfile^.bitaddress, neither, 0,
                          4);
            tab(7);
            writestring('end            ');
            end;
          arrays, conformantarrays:
            begin
            if outcolumn > 40 then writeline;
            if mapfile^.typ = conformantarrays then
              writestring(' Conformant    ');
            writech(' ');
            writestring('array[         ');
            writeformdesc(mapfile^.indextype, outcolumn);
            writestring('] of           ');
            writech(' ');
            writeformdesc(mapfile^.elementtype, outcolumn);
            end;
          strings:
            begin
            writestring(' String[       ');
            if mapfile^.bitaddress then
              writenumber(mapfile^.size div bitsperunit, false, false)
            else writenumber(mapfile^.size, false, false);
            writech(']');
            end;
          end; {case}
      end; {printtype}


    begin {writeformdesc}
      d := current;
      finddebugfile(mapfile, index); {position debugfile ptr}
      if mapfile^.kind <> formdesc then
        begin
        writeln('Expected form description missing:', index: 1);
        exitst(4);
        end;
      if mapfile^.packedflag then writestring(' Packed        ');
      printtype(mapfile);
      finddebugfile(mapfile, d); {restore debugfile ptr}
    end {writeformdesc} ;






  procedure printblock;


    begin {printblock}
      while (currproclink^.index <> current) and (currproclink <> nil) do
        currproclink := currproclink^.next;
      first := mapfile^.firstname;
      last := mapfile^.lastname;
      if mapfile^.level <> 0 then
        begin
        writeline;
        writeln(out, '*****');
        writeline;
        writesymboldesc(current, false, 0, local, 0);
        if mapfile^.name = 0 then
          writenamelist(first, last, mapfile^.id, false, global, globalbase, 0)
        else writenamelist(first, last, mapfile^.id, false, local, 0, 0);
        end;
      current := last + 2;
      finddebugfile(mapfile, current);
    end; {printblock}




  procedure findsymboldesc;


    begin {findsymboldesc}
      while not eof(mapfile) and (mapfile^.kind <> symboldesc) do
        begin
        current := current + 1;
        finddebugfile(mapfile, current);
        end;
    end; {findsymboldesc}


  procedure formtouch(index: integer);
    forward;


  procedure formtouch {(index: integer)} ;

    var
      idx: integer;
      saveidx: integer;
      idnumber: integer;


    begin {formtouch}
      typetouch[index] := true;
      finddebugfile(mapfile, index);
      with mapfile^ do
        case typ of
          ints, bools, chars, reals, doubles, subranges: ;
          {atomic, so go no fusther}
          ptrs: if not typetouch[ptrtype] then formtouch(ptrtype);
          sets:
            if not typetouch[basetype] then formtouch(basetype);
          fields:
            begin
            idnumber := mapfile^.fieldid;
            for idx := firstfield to lastfield do
              begin
              finddebugfile(mapfile, idx);
              if mapfile^.kind = symboldesc then
                if (mapfile^.namekind = fieldname) and
                   (mapfile^.name = idnumber) then
                  if not typetouch[mapfile^.vartype] then
                    formtouch(mapfile^.vartype);
              end;
            end;
          arrays, conformantarrays:
            begin
            saveidx := elementtype;
            if not typetouch[indextype] then formtouch(indextype);
            if not typetouch[saveidx] then formtouch(saveidx);
            end;
          files:
            if not typetouch[filebasetype] then formtouch(filebasetype);
          none: ;
          otherwise;
          end;
      finddebugfile(mapfile, index);
    end; {formtouch}


  procedure addproclink(idx: integer);

    var
      p: linkrecordptr;
      staticid: integer;
      staticname: integer;
      done: boolean;


    begin {addproclink}
      if proclinkhead = nil then
        begin
        new(proclinkhead);
        currproclink := proclinkhead;
        end
      else
        begin
        new(currproclink^.next);
        currproclink := currproclink^.next;
        end;
      currproclink^.next := nil;
      currproclink^.static := nil;
      staticname := mapfile^.id;
      with currproclink^ do
        begin
        index := idx;
        id := mapfile^.name;
        end;
      p := proclinkhead;
      while (p <> currproclink) do
        begin
        if (p^.static = nil) and (p^.id = staticname) then
          p^.static := currproclink;
        p := p^.next;
        end;
    end; {addproclink}


  procedure touchformdescs;

    var
      i: integer;


    begin {touchformdescs}
      for i := 1 to symnumber do typetouch[i] := false;
      i := 1;
      while not eof(mapfile) do
        begin
        finddebugfile(mapfile, i);
        case mapfile^.kind of
          identdesc: ;
          symboldesc:
            case mapfile^.namekind of
              varname, param, varparam, funcparam, confparam, varconfparam,
              boundid:
                if not typetouch[mapfile^.vartype] then
                  formtouch(mapfile^.vartype);
              procname, funcname: addproclink(i);
              otherwise;
              end;
          formdesc: ;
          end;
        i := i + 1;
        end;
      finddebugfile(mapfile, 1);
      currproclink := proclinkhead;
    end; {touchformdescs}




  begin {main}
    proclinkhead := nil;
    currproclink := nil;
    csi;
    touchformdescs;
    current := 1;
    while not eof(mapfile) do
      begin
      findsymboldesc;
      if (mapfile^.namekind in [procname, funcname]) then printblock
      else
        begin
        current := current + 1;
        finddebugfile(mapfile, current);
        end;
      end; {while}
  end.
