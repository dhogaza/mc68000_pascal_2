{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 ANALYS/BODY Output Dumper (Debugging Tool)

 Last modified by KRIS on 21-Nov-1990 15:36:00

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

program ad;

uses config, hdr, a_t;

procedure assert(s: string; b: boolean);
begin
  if not b then
  begin
    writeln('check for ', s, ' failed.');
    halt();
  end;
end;

function getintfileint: unsignedint;

{ Returns an integer passed in the intermediate file as a pair of bytes.
}

  var
    { This fudges an integer into bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (int: unsignedint);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;

    j: 1..32; {induction var}

  begin {getintfileint}
    assert('byte', tempfilebuf.intcode = literal);
    fudge.int := tempfilebuf.b;
    if fudge.int = hostfilelim then
      for j := 1 to hostintsize * hostfileunits do
        begin
        read(tempfiletwo, tempfilebuf);
        assert('byte', tempfilebuf.intcode = literal);
        fudge.byte[j] := tempfilebuf.b;
        end;
    getintfileint := fudge.int;
  end {getintfileint} ;


{ Initialization data is passed in the intermediate file as a series of
  operators.  Such operators can appear in thd midst of an expression,
  or they can be preceeded by a "begdata" statement and appear outside
  of a statement or even a block body.  They do not cause anything to
  be placed in the tree, but generate pseudo-ops directly.
}


procedure do_data_op;

{ Take a data operator and generate output code.
}

  var
    this_op: operatortype; {current operator}
    len, op1, op2: integer; {operands}
    realval: realarray; {a value if it's a real}


  begin {do_data_op}
    this_op := tempfilebuf.o;
    if this_op = drealop then
      begin
      write('drealop ');
      read(tempfiletwo, tempfilebuf);
      len := getintfileint;
      write(len:-8);
      end
    else
      begin
      len := 0;
      op1 := 0;
      op2 := 0;
      case this_op of
        daddop, dsubop, dstoreop, dendop:
          if this_op = daddop then write('daddop')
          else if this_op = dsubop then write('dsubop')
          else if this_op = dendop then write('dendop')
          else write('dstoreop');
        daddrop:
          begin
          write('daddrop ');
          read(tempfiletwo, tempfilebuf);
          len := getintfileint;
          read(tempfiletwo, tempfilebuf);
          op1 := getintfileint;
          read(tempfiletwo, tempfilebuf);
          op2 := getintfileint;
          write(len:1, ' ', op1:1, ' ', op2:1);
          end;
        dfaddrop, dintop, dstructop:
          begin
          if this_op = dfaddrop then write('dfaddrop ')
          else if this_op = dintop then write('dintop ')
          else write('dstructop ');
          read(tempfiletwo, tempfilebuf);
          len := getintfileint;
          read(tempfiletwo, tempfilebuf);
          op1 := getintfileint;
          write(len:1, ' ', op1:1);
          end;
        dstartop, dfieldop:
          begin
          if this_op = dstartop then write('dstartop ') else write('dfieldop ');
          read(tempfiletwo, tempfilebuf);
          op1 := getintfileint;
          read(tempfiletwo, tempfilebuf);
          op2 := getintfileint;
          write(op1:1, ' ', op2:1);
          end;
        dfillop:
          begin
          write('dfillop ');
          read(tempfiletwo, tempfilebuf);
          len := getintfileint;
          write(len:1);
          end;
        end;
      end;
  end; {do_data_op}

procedure process_data_inits;

  begin
    read(tempfiletwo, tempfilebuf);
    repeat
      while tempfilebuf.o <> endexpr do
        begin
        do_data_op;
        writeln;
        read(tempfiletwo, tempfilebuf);
        end;
      writeln('endexpr');
      read(tempfiletwo, tempfilebuf); {skip the endexpr}
    until tempfilebuf.s <> begdata;
  end;


procedure statement;

  var
    i: integer;


  procedure prints;


    begin {prints}
      assert('prints', tempfilebuf.intcode = stmt);
      case tempfilebuf.s of
        begblk: write('begblk');
        begcase: write('begcase');
        begcfor: write('begcfor');
        begdata:
          begin
          writeln('begdata');
          process_data_inits;
          prints; { now that all the data is gone, do the begblk }
          end;
        begelse: write('begelse');
        begfor: write('begfor');
        begexit: write('begexit');
        begif: write('begif');
        begloop: write('begloop');
        begreturn: write('begreturn');
        begrpt: write('begrpt');
        begswitch: write('begswitch');
        begwhile: write('begwhile');
        begwith: write('begwith');
        blksize: write('blksize');
        casedef: write('casedef');
        caselab: write('caselab');
        caselabrange: write('caselabrange');
        deflab: write('deflab');
        endall: write('endall');
        endblk: write('endblk');
        endcase: write('endcase');
        endcaseelt: write('endcaseelt');
        endcfor: write('endcfor');
        endelse: write('endelse');
        endfor: write('endfor');
        endloop: write('endloop');
        endrpt: write('endrpt');
        endswitch: write('endswitch');
        endthen: write('endthen');
        endwhile: write('endwhile');
        endwith: write('endwith');
        fordn: write('fordn');
        forup: write('forup');
        gotolab: write('gotolab');
        hiddenstmt: write('hiddenstmt');
        inits: write('inits');
        loopbreak: write('loopbreak');
        loopcont: write('loopcont');
        simple: write('simple');
        switchbreak: write('switchbreak');
        syscall: write('syscall');
        otherwise write('state ', ord(tempfilebuf.s): 3);
        end;

    end {prints} ;


  procedure expression;


    procedure printo;


      begin {printo}
        assert('printo', tempfilebuf.intcode = op);
        case tempfilebuf.o of
          daddop, daddrop, dfaddrop, dfieldop, dfillop, dintop, drealop,
          dstartop, dstoreop, dstructop, dsubop, dendop: ; {do nothing}

          modeqop: write('modeqop');
          muleqop: write('muleqop');
          shiftleqop: write('shiftleqop');
          shiftreqop: write('shiftreqop');
          xoreqop: write('xoreqop');
          preincop: write('preincop');
          postincop: write('postincop');
          xorop: write('xorop');
          shiftrop: write('shiftrop');
          questop: write('questop');
          pushret: write('pushret');
          tempop: write('tempop');
          addeqop: write('addeqop');
          addrop: write('addrop');
          aindxop: write('aindxop');
          andop: write('andop');
          andeqop: write('andeqop');
          oreqop: write('oreqop');
          castfptrop: write('castfptrop');
          castintop: write('castintop');
          castptrop: write('castptrop');
          castrealop: write('castrealop');
          chrstrop: write('chrstrop');
          chrstrop1: write('chrstrop1');
          commaop: write('commaop');
          clearnewop: write('clearnewop');
          arraystrop: write('arraystrop');
          arraystrop1: write('arraystrop1');
          bldfmt: write('bldfmt');
          bldnil: write('bldnil');
          bldset: write('bldset');
          call: write('call');
          callparam: write('callparam');
          cindxchkop: write('cindxchkop');
          closerangeop: write('closerangeop');
          cmoveop: write('cmoveop');
          congruchkop: write('congruchkop');
          copystackop: write('copystackop');
          dbl_to_real: write('dbl_to_real');
          decop: write('decop');
          defforindexop: write('defforindexop');
          defforlitindexop: write('defforlitindexop');
          definelazyop: write('definelazyop');
          defunsforindexop: write('defunsforindexop');
          defunsforlitindexop: write('defunsforlitindexop');
          diveqop: write('diveqop');
          divop: write('divop');
          doubleop: write('doubleop');
          dummyargop: write('dummyargop');
          dummyarg2op: write('dummyarg2op');
          endexpr: write('endexpr');
          eqlit: write('eqlit');
          eqop: write('eqop');
          filebufindrop: write('filebufindrop');
          float: write('float');
          float1: write('float1');
          float_double: write('float_double');
          fordnchkop: write('fordnchkop');
          forerrchkop: write('forerrchkop');
          forindexop: write('forindexop');
          forupchkop: write('forupchkop');
          fptrop: write('fptrop');
          geqlit: write('geqlit');
          geqop: write('geqop');
          globalop: write('globalop');
          ownop: write('ownop');
          segop: write('segop');
          extop: write('extop');
          gtrlit: write('gtrlit');
          gtrop: write('gtrop');
          incop: write('incop');
          indrop: write('indrop');
          indxchkop: write('indxchkop');
          indxop: write('indxop');
          inop: write('inop');
          intop: write('intop');
          kwoop: write('kwoop');
          leqlit: write('leqlit');
          leqop: write('leqop');
          levop: write('levop');
          lit: write('lit');
          localop: write('localop');
          loopholeop: write('loopholeop');
          lsslit: write('lsslit');
          lssop: write('lssop');
          minusop: write('minusop');
          modop: write('modop');
          movelit: write('movelit');
          moveop: write('moveop');
          mulop: write('mulop');
          negop: write('negop');
          neqlit: write('neqlit');
          neqop: write('neqop');
          newset: write('newset');
          newunsvarop: write('newunsvarop');
          newvarop: write('newvarop');
          notop: write('notop');
          openarrayop: write('openarrayop');
          originop: write('originop');
          orop: write('orop');
          paindxop: write('paindxop');
          parmop: write('parmop');
          pindxop: write('pindxop');
          plusop: write('plusop');
          ptrchkop: write('ptrchkop');
          ptrop: write('ptrop');
          pushaddr: write('pushaddr');
          pushstraddr: write('pushstraddr');
          pushcvalue: write('pushcvalue');
          pushfinal: write('pushfinal');
          pushlitvalue: write('pushlitvalue');
          pushproc: write('pushproc');
          pushfptr: write('pushfptr');
          pushvalue: write('pushvalue');
          quoop: write('quoop');
          rangechkop: write('rangechkop');
          rd: write('rd');
          realop: write('realop');
          real_to_dbl: write('real_to_dbl');
          remop: write('remop');
          reserve: write('reserve');
          restop: write('restop');
	  returnop: write('returnop');
          saveop: write('saveop');
          setbinfileop: write('setbinfileop');
          setelt: write('setelt');
          setfileaddrop: write('setfileaddrop');
          setfileop: write('setfileop');
          setinput: write('setinput');
          setpair: write('setpair');
          shiftlop: write('shiftlop');
          slashop: write('slashop');
          stddivop: write('stddivop');
          stdmodop: write('stdmodop');
          structop: write('structop');
          subeqop: write('subeqop');
          switchstack: write('switch');
          sysfn: write('sysfn');
          unscall: write('unscall');
          unscallparam: write('unscallparam');
          unsvarop: write('unsvarop');
          varop: write('varop');
          vindxop: write('vindxop');
          withop: write('withop');
          wr: write('wr');
          otherwise write('operator ', ord(tempfilebuf.o): 3);
          end;
      end {printo} ;


    procedure op;

      var
        realkluge:
          record
            case boolean of
              false: (i: packed array [0..1] of integer);
              true: (r: double);
          end;

        t: unsignedint;
        s: addressrange;
        i: 1..32;


      procedure printf;


        begin {printf}
	  assert('printf', tempfilebuf.intcode = form);
          case tempfilebuf.f of
            arrays: write('arrays');
            bools: write('bools');
            chars: write('chars');
            doubles: write('doubles');
            fields: write('fields');
            strings: write('strings');
            files: write('files');
            ints: write('ints');
            none: write('none');
            fptrs: write('fptrs');
            ptrs: write('ptrs');
            reals: write('reals');
            scalars: write('scalars');
            sets: write('sets');
            subranges: write('subranges');
            variantlabs: write('variantlabs');
            flexarrays: write('flexarrays');
            words : write('words');
            bytes : write('bytes');
            opaques : write('opaques');
            procs : write('procs');
            stringliterals : write('stringliterals');
            otherwise write('form ', ord(tempfilebuf.f): 3);
            end;
        end {printf} ;


      begin {op}
        printo;
        case tempfilebuf.o of
          daddop, daddrop, dfaddrop, dfieldop, dfillop, dintop, drealop,
          dstartop, dstoreop, dstructop, dsubop, dendop:
            do_data_op;
          addrop, aindxop, andop, bldfmt, bldset, call, callparam, cindxchkop,
          closerangeop, cmoveop, congruchkop, copystackop, decop, andeqop,
          definelazyop, divop, eqlit, eqop, filebufindrop, float, float1,
          chrstrop, chrstrop1, arraystrop, arraystrop1, fordnchkop, oreqop,
          forerrchkop, forupchkop, geqlit, geqop, gtrlit, gtrop, incop,
          indrop, indxchkop, indxop, inop, leqlit, leqop, loopholeop, lsslit,
          lssop, minusop, movelit, moveop, mulop, negop, neqlit, neqop, notop,
          orop, paindxop, pindxop, plusop, ptrchkop, pushaddr, pushcvalue,
          pushfinal, pushlitvalue, pushproc, pushstraddr, pushvalue, quoop,
          rangechkop, rd, remop, returnop, setbinfileop, setelt, setfileaddrop,
          setfileop, setpair, shiftlop, slashop, stddivop, sysfn, unscall,
          unscallparam, wr, float_double, real_to_dbl, dbl_to_real, dummyargop,
          dummyarg2op, openarrayop, addeqop, subeqop, pushfptr, vindxop,
          parmop, commaop, groupop, compop, castfptrop, castintop, castptrop,
          castrealop, diveqop, modeqop, muleqop,
          shiftleqop, shiftreqop, xoreqop, preincop, postincop, xorop,
          shiftrop, questop, pushret, tempop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' len:', getintfileint: 1);
            read(tempfiletwo, tempfilebuf);
            write(' cost:', getintfileint: 1, ' ');
            read(tempfiletwo, tempfilebuf);
            printf;
            end;

          withop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' ', getintfileint: 1);
            end;

          newvarop, newunsvarop, unsvarop, varop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' len:', getintfileint: 1);
            read(tempfiletwo, tempfilebuf);
            write(' lev:', getintfileint: 1);
            read(tempfiletwo, tempfilebuf);
            write(' indx:', getintfileint: 1);
            read(tempfiletwo, tempfilebuf);
            write(' ownvar:',boolean(getintfileint));
            end;

          forindexop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' depth:', getintfileint: 1);
            end;

          fptrop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(', proc:', getintfileint: 1);
            end;

          defforindexop, defforlitindexop, defunsforindexop,
          defunsforlitindexop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' len:', getintfileint: 1);
            read(tempfiletwo, tempfilebuf);
            write(' lev:', getintfileint: 1);
            end;

          reserve:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' len:', getintfileint: 1);
            if language = c then
              begin
              read(tempfiletwo, tempfilebuf);
              write(' struct_ret:', getintfileint: 1);
              end;
            end;

          structop, levop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' ', getintfileint: 1, ',');
            read(tempfiletwo, tempfilebuf);
            write(' ', getintfileint: 1);
            end;

          realop, doubleop:
            begin
            if language = c then
              begin
              read(tempfiletwo, tempfilebuf);
              t := getintfileint;
              write(' len:', t:1);
              end;
            s := 0;
            i := 1;
            while s < maxrealwords do
              begin
              read(tempfiletwo, tempfilebuf);
              t := getintfileint;
              write(' ', t mod $10000: -4, ' ', t div $10000: -4);
              i := i + 1;
              s := s + sizeof(integer);
              end;
            end;

          intop, ptrop, lit, originop, segop, extop:
            begin
            read(tempfiletwo, tempfilebuf);
            write(' ', getintfileint: 1);
            end;

          otherwise;
          end;
        writeln;
        read(tempfiletwo, tempfilebuf);
      end {op} ;


    begin {expression}
      while tempfilebuf.o <> endexpr do op;
      printo;
    end {expression} ;


  begin {statement}
    prints;
    case tempfilebuf.s of

      forup, fordn:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' by:', getintfileint: 1);
        end;

      syscall:
        begin
        read(tempfiletwo, tempfilebuf);
        if language <> c then
          begin
          write(' srcindex: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        if newdebugger then
          begin
          write(' stmt#: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        write(' line:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        writeln(' n:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        expression;
        end;

      endfor:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' jump-out-flag:', getintfileint: 1);
        end;

      begreturn, begcase, begfor, begif, begwhile, begwith, simple:
        begin
        read(tempfiletwo, tempfilebuf);
        if language <> c then
          begin
          write(' srcindex: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        if newdebugger then
          begin
          write(' stmt#: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        writeln(' line:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        expression
        end;

      endrpt:
        begin
        read(tempfiletwo, tempfilebuf);
        writeln;
        expression;
        end;

      begloop, begrpt:
        begin
        read(tempfiletwo, tempfilebuf);
        if language <> c then
          begin
          write(' srcindex: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        if newdebugger then
          begin
          write(' stmt#: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        write(' line:', getintfileint: 1);
        end;

      begexit:
        begin
        read(tempfiletwo, tempfilebuf);
        if language <> c then
          begin
          write(' srcindex: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        if newdebugger then
          begin
          write(' stmt#: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        write(' line:', getintfileint: 1);
        end;

      gotolab:
        begin
        read(tempfiletwo, tempfilebuf);
        if language <> c then
          begin
          write(' srcindex: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        if newdebugger then
          begin
          write(' stmt#: ',getintfileint:1);
          read(tempfiletwo, tempfilebuf);
          end;
        write(' line:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' lab:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' lev:', getintfileint: 1);
        end;

      deflab:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' lab:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' lev:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' nonlocalref:', getintfileint: 1);
        end;

      endblk:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' sym:', getintfileint: 1);
        end;

      begblk:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' line:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' ref:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' ps:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' bs:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' baseline:', getintfileint: 1);
        end;

      caselab:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' n:', getintfileint: 1)
        end;

      caselabrange:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' low:', getintfileint: 1);
        read(tempfiletwo, tempfilebuf);
        write(' high:', getintfileint: 1);
        end;

      blksize:
        begin
        read(tempfiletwo, tempfilebuf);
        write(' size:', getintfileint: 1)
        end;

      otherwise;
      end;
    read(tempfiletwo, tempfilebuf);
    writeln;
  end {statement} ;

begin {ad}
  assign(tempfiletwo, 'temptwo.tmp');
  reset(tempfiletwo);
  assign(locals, 'locals.tmp');
  reset(locals);


  read(tempfiletwo, tempfilebuf);
  while not eof(tempfiletwo) and
        (tempfilebuf.intcode <> stmt) or (tempfilebuf.s <> endall) do
    begin
    assert('statement', tempfilebuf.intcode = stmt);
    statement;
    end;

  close(tempfiletwo);
  close(output);
end {ad} .
