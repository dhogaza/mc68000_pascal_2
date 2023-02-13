{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Listing Printer

 Last modified by KRIS on 21-Nov-1990 15:34:50

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{ Pascal-2 Lister }

unit list;

interface

uses config, hdr, product, error, utils, scan, sysutils;

procedure list;

procedure openl;
procedure closel;

implementation

const
  tabch = 9; {tab character}
  formfeed = 12; {formfeed character}
  tabspace = 8; {the number of spaces a tab input becomes}

var
  curfileptr: filerememberptr;  {used to step through input files}
  thisfileptr: filerememberptr;  {the file that is open}
  lastfileptr: filerememberptr;  {the last file name printed in listing}
  buffer: packed array [1..180] of char;
  buffer_ctr: shortint;


procedure seekstringfile(n: integer {byte to access} );

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  var
    newblock: 1..maxstringblks; {block to which seeking}


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
      curstringblock := newblock;
      if needcaching then seek(stringfile, newblock)
      else
        begin
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          write('unexpected end of stringtable ');
          compilerabort(inconsistent);
          end;
        end;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;


procedure getstringfile;

{ Move the stringfile buffer pointer to the next entry.
}


  begin {getstringfile}
    if nextstringfile >= diskbufsize then
      begin
      curstringblock := curstringblock + 1;
      nextstringfile := 0;
      stringblkptr := stringblkptrtbl[curstringblock];
      if stringblkptr = nil then
        begin
        write('unexpected end of stringtable ');
        compilerabort(inconsistent);
        end;
      end
    else nextstringfile := nextstringfile + 1;
  end {getstringfile} ;


procedure changesourcename;

{ Change source[sourcelevel] to the next remembered file.
}

  var
    i: FilenameIndex; {induction on filename}


  begin {changesourcename}
    morefiles := (curfileptr <> nil);
    filename_length := 0;
    if morefiles then
      begin
      seekstringfile(stringfilecount + curfileptr^.offset - 1);
      while stringblkptr^[nextstringfile] <> 0 do
        begin
        filename_length := filename_length + 1;
        filename[filename_length] := chr(stringblkptr^[nextstringfile]);
        getstringfile;
        end;
      thisfileptr := curfileptr;
      curfileptr := curfileptr^.next;
      end;
    for i := filename_length + 1 to filenamelen do filename[i] := ' ';
  end {changesourcename} ;


procedure printlisting(var listing: text {file on which listing is output} );

  const

    leftmargin = 11; { keep this odd }
    pagelimit = 54;

  type
    sourcestates = (normal, skippingstring, skippingquote, skippingstar,
                    skippingcomment, builddirective, skippingblanks,
                    buildfilename, buildquotedfilename, skippingfilequote);

    alfa = packed array [1..10] of char; {for directives}

    save_rec =
      record
        line: integer;
        ch: char;
        endofline: boolean;
        filename_length: FilenameIndex;
        filename: FilenameBuf;
        fileptr: filerememberptr;
      end;

  var
    nextch: char; {next character to be scanned}
    sourcestate: sourcestates;
    directive: alfa;
    buildindex: integer;
    uniqueerrs: packed array [warning] of boolean {too big for a set!} ;
    e: warning; {for initializing uniqueerrs array}
    pageline, linecount, pagecount, physicalpage, nexterrline,
    nexterror: integer;
    totallines: integer;
    ch, commentch: char;
    endofline, endofinput: boolean;
    i: integer;
    topofpage, endofpage, skipmode: boolean;
    pagenextline: boolean; { eject next line }
    lineswitherrors: integer;
    errorsreported: integer; {counts errors actually showing in listing}
    hour, minute: integer;
    ampm: char;
    todaystring: packed array [1..9] of char;
    headerline: hdrline; {Site header line}
    headerlength: integer; {length of site header line}
    site1, site2: integer; {componants of site number}
    save: array [1..sourcedepth] of save_rec;


  procedure wl_line;

  { Dump out all the characters in the buffer.
  }

    begin {wl_line}
      writeln(listing, leftstr(buffer, buffer_ctr));
      buffer_ctr := 0;
    end; {wl_line}


  procedure wl_chr(c: char);

  { Copy a character to the output buffer.
  }

    begin {wl_str}
      buffer_ctr := buffer_ctr + 1;
      buffer[buffer_ctr] := c;
    end; {wl_str}


  procedure wl_str(s: string);

  { Copy a string to the output buffer.
  }
    var
      i: integer;

    begin {wl_str}
      for i := 1 to length(s) do
        if s[i] <> chr(0) then
          begin
          buffer_ctr := buffer_ctr + 1;
          buffer[buffer_ctr] := s[i];
          end;
    end; {wl_str}


  procedure wl_str_l(s: string;
                     n: integer);

  { Copy a string of n bytes to the output buffer.
  }
    var
      i: integer;

    begin {wl_str_l}
      for i := 1 to n - length(s) do
        begin
        buffer_ctr := buffer_ctr + 1;
        buffer[buffer_ctr] := ' ';
        end;

      for i := 1 to min(n, length(s)) do
        if s[i] <> chr(0) then
          begin
          buffer_ctr := buffer_ctr + 1;
          buffer[buffer_ctr] := s[i];
          end;
    end; {wl_str_l}


  procedure wl_int(v: integer;
                   n: integer);

  { Copy an integer (v) of width n to the output buffer.
  }

    var
      int_bufptr: 0..9;
      int_buffer: packed array [1..20] of char;
      u: unsignedint;
      s: packed array [1..20] of char;
      ctr: integer;
      i: integer;

    begin {wl_int}
      int_bufptr := 0;
      if v < 0 then wl_chr('-');
      u := abs(v);

      repeat
        int_bufptr := int_bufptr + 1;
        int_buffer[int_bufptr] := chr(u mod 10 + ord('0'));
        u := u div 10;
      until u = 0;

      ctr := 0;

      repeat
        ctr := ctr + 1;
        s[ctr] := int_buffer[int_bufptr];
        int_bufptr := int_bufptr - 1;
      until int_bufptr = 0;

      for i := 1 to n - ctr do
        begin
        buffer_ctr := buffer_ctr + 1;
        buffer[buffer_ctr] := ' ';
        end;

      for i := 1 to ctr do
        if s[i] <> chr(0) then
          begin
          buffer_ctr := buffer_ctr + 1;
          buffer[buffer_ctr] := s[i];
          end;
    end; {wl_int}



  procedure getch;

    var
      i: integer;


    procedure mapstate;

      var
        i: FilenameIndex; {induction on filename}


      procedure startincluding;

{ Process an include directive.
}


        begin {startincluding}
          if sourcelevel < sourcedepth then
            begin
            sourcelevel := sourcelevel + 1;
            save[sourcelevel].line := linecount;
            save[sourcelevel].ch := nextch;
            save[sourcelevel].endofline := endofline;

{ The following line replaces the file name found in the text, with the
  file name saved by SCAN at this point in the compilation.  "This point
  in the compilation is determined by simply stepping through the list of
  input files based on "filerememberlist".  Of course, this only works if
  changesourcename is used consistently for all input and include files. }

            changesourcename;

            save[sourcelevel].filename_length := filename_length;
            save[sourcelevel].filename := filename;
            save[sourcelevel].fileptr := thisfileptr;
            opens;
            nextch := ' ';
            endofline := true;
            sourcestate := normal;
            end;
        end {startincluding} ;


      begin {mapstate}
        case sourcestate of
          normal:
            case ch of
              '(':
                if nextch = '*' then
                  begin
                  sourcestate := skippingstar;
                  commentch := ')';
                  end;
              '/':
                if nextch = '*' then
                  begin
                  sourcestate := skippingstar;
                  commentch := '/';
                  end;
              '{':
                begin
                sourcestate := skippingcomment;
                commentch := ')';
                end;
              '''': sourcestate := skippingstring;
              '%':
                begin
                sourcestate := builddirective;
                buildindex := 0;
                directive := '          ';
                end;
              otherwise;
              end {case ch} ;
          skippingstring:
            if (ch = '''') then
              if (nextch = '''') then sourcestate := skippingquote
              else sourcestate := normal;
          skippingquote: sourcestate := skippingstring;
          skippingstar: sourcestate := skippingcomment;
          skippingcomment:
            if (ch = '}') or (ch = '*') and (nextch = commentch) then
              sourcestate := normal;
          builddirective:
            if ch in [chr(tabch), ' ', ';'] then
              if directive = 'INCLUDE   ' then sourcestate := skippingblanks
              else if directive = 'PAGE      ' then
                begin
                pagenextline := true;
                sourcestate := normal;
                end
              else sourcestate := normal
            else if buildindex < 10 then
              begin
              buildindex := buildindex + 1;
              if ch in ['a'..'z'] then
                directive[buildindex] := chr(ord(ch) - (ord('a') - ord('A')))
              else directive[buildindex] := ch;
              end;
          skippingblanks:
            if (ch <> ' ') and (ch <> chr(tabch)) then
              begin
              for i := 1 to filenamelen do filename[i] := ' ';
              if ch = '''' then
                begin
                filename_length := 0;
                sourcestate := buildquotedfilename;
                end
              else
                begin
                filename[1] := ch;
                filename_length := 1;
                sourcestate := buildfilename;
                end;
              end;
          buildfilename:
            if ch in [' ', ';'] then startincluding
            else if filename_length < filenamelen then
              begin
              filename_length := filename_length + 1;
              filename[filename_length] := ch;
              end;
          buildquotedfilename:
            if ch = '''' then sourcestate := skippingfilequote
            else if endofline then sourcestate := normal
            else if filename_length < filenamelen then
              begin
              filename_length := filename_length + 1;
              filename[filename_length] := ch;
              end;
          skippingfilequote: startincluding;
          end {case sourcestate} ;
      end {mapstate} ;


    begin {getch}
      ch := nextch;
      endofline := false;
      if eof(source[sourcelevel]) then
        begin
        if sourcelevel > 1 then
          begin
          nextch := save[sourcelevel].ch;
          endofline := save[sourcelevel].endofline;
          closes;
          sourcelevel := sourcelevel - 1;
          { Pop the old filename off stack }
          filename := save[sourcelevel].filename;
          filename_length := save[sourcelevel].filename_length;
          thisfileptr := save[sourcelevel].fileptr;

          for i := 1 to sourcelevel do
            save[i].line := save[i].line + linecount - save[sourcelevel +
                            1].line;
          end
        else
          begin

{ The following line replaces the file name found on the command line,
  with the file name saved by SCAN at this point in the compilation.
  "This point in the compilation is determined by simply stepping through
  the list of input files based on "filerememberlist".  Of course, this
  only works if changesourcename is used consistently for all input and
  include files.  We then use CLOSES and OPENS to get to the file by name. }

          changesourcename;
	  save[1].filename := filename;
	  save[1].filename_length := filename_length;
	  save[1].fileptr := thisfileptr;
          save[1].line := linecount;

          if morefiles then
            begin
            closes;
            opens;
            read(source[sourcelevel], nextch);
            end
          else
            begin
            endofinput := true;
            nextch := ' ';
            end;
          end;
        end
      else if eoln(source[sourcelevel]) then
        if nextch = ' ' then
          begin
          readln(source[sourcelevel]);
          if (sourcelevel = 1) or not eof(source[sourcelevel]) then
            endofline := true;
          end
        else nextch := ' '
      else read(source[sourcelevel], nextch);
      mapstate;
    end {getch} ;




  function digits(i: integer): integer;

    var
      j: integer;


    begin {digits}
      j := 1;
      while i >= 10 do
        begin
        j := j + 1;
        i := i div 10;
        end;
      digits := j;
    end {digits} ;


  procedure dateandtime;

    var
      sec, day, month, year: word; {time buffer}


    begin {dateandtime}
    {DRB timestamp(day, month, year, hour, minute, sec);
      if hour >= 12 then ampm := 'P'
      else ampm := 'A';

      case month of
        1: todaystring := '  -Jan-  ';
        2: todaystring := '  -Feb-  ';
        3: todaystring := '  -Mar-  ';
        4: todaystring := '  -Apr-  ';
        5: todaystring := '  -May-  ';
        6: todaystring := '  -Jun-  ';
        7: todaystring := '  -Jul-  ';
        8: todaystring := '  -Aug-  ';
        9: todaystring := '  -Sep-  ';
        10: todaystring := '  -Oct-  ';
        11: todaystring := '  -Nov-  ';
        12: todaystring := '  -Dec-  ';
        end {case month} ;
      if day > 9 then todaystring[1] := chr(ord('0') + day div 10);
      todaystring[2] := chr(ord('0') + day mod 10);
      year := year mod 100;
      todaystring[8] := chr(ord('0') + year div 10);
      todaystring[9] := chr(ord('0') + year mod 10);}
    end {dateandtime} ;


  procedure listtitle;

    var
      i: cmdindex; {induction variable}


    begin {listtitle}
      todaystring := '  today! ';
      if (targetopsys = unix) then
        case unixtarget of
          NEC:
            begin
            wl_str('Pascal-2 ');
            wl_str('Version ');
            wl_str(version);
            wl_str(versionlevel);
            wl_str('               ');
            wl_str(todaystring);
	    {DRB
            wl_int((hour + 11) mod 12 + 1, 4);
            wl_chr(':');
            wl_int(minute div 10, 1);
            wl_int(minute mod 10, 1);
            wl_chr(' ');
            wl_chr(ampm);
            wl_str('M ');}
            wl_str(' Page ');
            wl_int(pagecount, 1);
            wl_chr('-');
            wl_int(physicalpage, 1);
            wl_line;
	    {DRB wl_str_l(headerline, headerlength);
            wl_line;}
            wl_str_l(filename, filename_length); { Print current filename }
            lastfileptr := thisfileptr; {don't print this name again}
            wl_line;
            wl_line;
            totallines := totallines + 4;
            topofpage := false;
            end; {NEC}
          otherwise
            begin
            wl_str('Pascal-2 ');
            wl_str(systemtitle);
            wl_chr(' ');
            wl_str(version);
            wl_str(versionlevel);
            wl_chr(' ');
            wl_str(todaystring);
	    {DRB wl_int((hour + 11) mod 12 + 1, 4);
            wl_chr(':');
            wl_int(minute div 10, 1);
            wl_int(minute mod 10, 1);
            wl_chr(' ');
            wl_chr(ampm);
            wl_str('M ');
            wl_str('   Site #');
            wl_int(site1, 1);
            wl_int(-site2, 1);}
            wl_str('    Page ');
            wl_int(pagecount, 1);
            wl_chr('-');
            wl_int(physicalpage, 1);
            wl_line;
	    {DRB
            wl_str_l(headerline, headerlength);
            wl_line;}
            wl_str_l(filename, filename_length); { Print current filename }
            lastfileptr := thisfileptr; {don't print this name again}
            wl_line;
            wl_line;
            totallines := totallines + 4;
            topofpage := false;
            end; {otherwise}
        end  {case unixtarget}
      else {targetopsys <> unix}
        begin
        wl_str('Pascal-2 ');
        wl_str(systemtitle);
        wl_chr(' ');
        wl_str(version);
        wl_str(versionlevel);
        wl_chr(' ');
        wl_str(todaystring);
	{DRB
        wl_int((hour + 11) mod 12 + 1, 4);
        wl_chr(':');
        wl_int(minute div 10, 1);
        wl_int(minute mod 10, 1);
        wl_chr(' ');
        wl_chr(ampm);
        wl_str('M ');
        wl_str('   Site #');
        wl_int(site1, 1);
        wl_int(-site2, 1);}
        wl_str('    Page ');
        wl_int(pagecount, 1);
        wl_chr('-');
        wl_int(physicalpage, 1);
        wl_line;
	{DRB
        wl_str_l(headerline, headerlength);
        wl_line;}
        wl_str_l(filename, filename_length); { Print current filename }
        lastfileptr := thisfileptr; {don't print this name again}
        wl_line;
        wl_line;
        totallines := totallines + 4;
        topofpage := false;
        end; {else}
  
      if (switcheverplus[debugging] or switcheverplus[profiling]) and 
          not switcheverplus[defineswitch] then
        begin
        wl_line;
        wl_str_l('Line', leftmargin div 2);
        wl_str_l('Stmt', leftmargin div 2);
        wl_line;
        pageline := pageline + 2;
        totallines := totallines + 2;
        end;

    end {listtitle} ;


  procedure tab(tabfrom, tabto: integer);

    var
      i: integer;


    begin {tab}
      for i := tabfrom to tabto - 1 do wl_chr(' ');
    end {tab} ;


  procedure pagelisting;


    begin {pagelisting}
      if not topofpage then
        begin
        if buffer_ctr > 0 then wl_line;
	{DRB page(listing);}
	wl_chr(chr(12));
	wl_line;
        end;
      pageline := 1;
      topofpage := true;
      endofpage := false;
    end {pagelisting} ;


  procedure getnexterror;


    begin {getnexterror}
      if nexterror < lasterror then
        begin
        nexterror := nexterror + 1;
        nexterrline := errortable[nexterror].errline
        end
      else nexterrline := maxint
    end {getnexterror} ;




  procedure processline;

    var
      stmtno: integer; {statement number for current line (if any)}
      first: boolean; {first printing for this line}
      column, newcolumn: columnindex;
      temppos1, temppos2: integer; {for getpos, because statement file is
                                    packed}


    procedure printlineno;


      begin {printlineno}
	if (not topofpage) and (lastfileptr <> thisfileptr) then
	  if not fakelist and (pageline + 4 > pagelimit) then
	    begin {Start new page}
	    pagelisting;
	    physicalpage := physicalpage + 1;
	    end
	  else
	    begin {Just print new current filename}
	    if not fakelist then wl_line;
	    wl_str_l(filename, filename_length);
            wl_line;
	    wl_line;
	    lastfileptr := thisfileptr;
	    pageline := pageline + 2 + ord(not fakelist);
	    end;

        if topofpage then listtitle;

        if sourcelevel > 1 then wl_int(sourcelevel, 1)
        else wl_chr(' ');
        wl_int(linecount - save[sourcelevel].line, leftmargin div 2 - 1);
        if stmtno <> 0 then wl_int(stmtno, leftmargin div 2)
        else wl_str_l('  ', leftmargin div 2);
        wl_chr(' ');
        first := false;
      end {printlineno} ;


    function stopconditions: boolean;


      begin {stopconditions}
        stopconditions := endofinput or endofline or endofpage or
                          (column > linelen + 1);
      end {stopconditions} ;


    begin {processline}
      if pagenextline then pagelisting;
      pagenextline := false;
      endofpage := false;
      first := true;
      column := 1;
      stmtno := 0;

      while not stopconditions do
        begin
        getch;
        newcolumn := column;

        while ((ch = chr(tabch)) or (ch = ' ')) and not stopconditions do
          begin
          if (ch = chr(tabch)) and (sourcestate <> skippingstring) then
            newcolumn := ((newcolumn - 1) div tabspace) * tabspace +
                         tabspace + 1
          else newcolumn := newcolumn + 1;
          getch;
          end;

        if newcolumn <> column then
          begin
          if first then printlineno;
          tab(column + leftmargin, newcolumn + leftmargin);
          column := newcolumn;
          end;

        if (ch = chr(formfeed)) then
          begin
          if not skipmode then endofpage := true;
          end
        else
          begin
          if first then printlineno;
          wl_chr(ch);
          column := column + 1;
          end;
        end;

      { Flush out a long line -- prevents errors in linecount later.
      }
      while not (endofinput or endofline or endofpage) do getch;

      if endofpage then
        begin
        pagecount := pagecount + 1;
        physicalpage := 1;
        pagelisting;
        if ch = chr(formfeed) then processline;
        end
      else
        begin
        if first and not endofinput then printlineno;
        wl_line;

        totallines := totallines + 1;
        if (pageline > pagelimit) then
          begin
          physicalpage := physicalpage + 1;
          pagelisting;
          end;
        getch;
        linecount := linecount + 1;
        pageline := pageline + 1;
        end;
    end {processline} ;




  procedure listoneerror(err: warning {error to explain} );

{ Translate error scalar type to meaningful message on listing file.
}


    begin {listoneerror}
      case err of
        linetoolong: wl_str('Line too long');
        badchar: wl_str('Illegal character');
        missingdigits:
          wl_str('Need at least 1 digit after ''.'' or ''E''');
        octalconst:
          wl_str('Octal constants are not standard Pascal');
        nondecimalconst:
          wl_str('Non-decimal integers are not standard Pascal');
        toomanyerrors: wl_str('Too many errors!');
        badoctal: wl_str('Octal constant contains an illegal digit');
        badradix:
          wl_str('Non-decimal integer base must lie in range 2..16');
        badinteger:
          begin
          wl_str('Integers must lie in range ');
          wl_int( - targetmaxint - 1, 1);
          wl_str('..');
          wl_int(targetmaxint, 1);
          end;
        badexpon:
          begin
          wl_str('Exponent must lie in range ');
          if switcheverplus[doublereals] then
            begin
            wl_int(mindoubleexpon, 1);
            wl_str('..');
            wl_int(maxdoubleexpon, 1);
            end
          else
            begin
            wl_int(minexpon, 1);
            wl_str('..');
            wl_int(maxexpon, 1);
            end;
          end;
        zerostring: wl_str('String of length zero');
        levelerr:
          begin
          wl_str('Only ');
          wl_int(maxlevel, 1);
          wl_str(' levels of nesting allowed');
          end;
        doteoferr: wl_str('Use ''.'' after main program body');
        extraenderr:
          wl_str('Extra END following block -- Check BEGIN ... END pairing');
        extraprocerr:
          wl_str('Extra procedures found after main program body');
        extrastmterr:
          wl_str('Extra statements found after end of program');
        garbageerr:
          wl_str('Nonsense discovered after program end');
        blockstarterr:
          wl_str('Block must begin with LABEL,CONST,TYPE,VAR,PROCEDURE,FUNCTION, or BEGIN');
        scrambledblkerr:
          wl_str('Block declarations are incorrectly ordered');
        badlabelnest: wl_str('Label is target of illegal GOTO');
        nosemierr: wl_str('Use '';'' to separate statements');
        nobeginerr: wl_str('BEGIN expected');
        blockenderr: wl_str('Block ended incorrectly');
        noenderr: wl_str('END expected');
        stmtenderr: wl_str('Statement ended incorrectly');
        nountilerr: wl_str('UNTIL expected');
        badelseerr:
          wl_str(
            'Unexpected ELSE clause -- Check preceding IF for extra '';''');
        nothenerr: wl_str('THEN expected');
        nocommaerr: wl_str(''','' expected');
        nocolonerr: wl_str(''':'' expected');
        nooferr: wl_str('OF expected');
        caselabelerr: wl_str('Bad CASE label');
        caseelseerr:
          wl_str('OTHERWISE/ELSE clause in CASE not allowed');
        nodoerr: wl_str('DO expected');
        nobecomeserr: wl_str(''':='' expected');
        nodowntoerr: wl_str('TO or DOWNTO expected');
        nofilevar: wl_str('File variable expected');
        novarerr: wl_str('Identifier expected');
        badlabelerr:
          wl_str('Label must be unsigned integer constant');
        norparerr: wl_str(''')'' expected');
        badcolonerr:
          wl_str('Procedures cannot be followed by type definition');
        badparamerr: wl_str('Bad parameter element');
        notypenameerr: wl_str('Type name expected');
        nosemiprocerr: wl_str(''';'' expected after procedure body');
        nofuncass:
          wl_str('Function identifier is never assigned a value');
        badexprerr: wl_str('Badly formed expression');
        nooperr: wl_str('Binary operator expected');
        nooprnderr: wl_str('Operand expected');
        badindexerr:
          wl_str(''']'' or '','' must follow index expression');
        norbrackerr: wl_str(''']'' expected');
        badrparerr:
          wl_str('Unexpected '')'' -- Check for matching parenthesis');
        noeqlerr: wl_str('''='' expected');
        badconsterr: wl_str('Bad constant');
        nosemiheaderr: wl_str('Use '';'' to separate declarations');
        baddeclerr: wl_str('Declaration terminated incorrectly');
        badtypesyntax: wl_str('Bad type syntax');
        nolabelerr: wl_str('Integer label expected');
        nolbrackerr: wl_str('''['' expected');
        nodotdoterr: wl_str('''..'' expected');
        nolparerr: wl_str('''('' expected');
        proctablefull:
          begin
          wl_str('Too many procedures (only ');
          wl_int(proctablesize, 1);
          wl_str(' allowed)');
          end;
        undeftablefull:
          begin
          wl_str('Too many forward declarations (only ');
          wl_int(undeftablesize, 1);
          wl_str(' allowed)');
          end;
        tablefull:
          begin
          wl_str('Too many identifiers (only ');
          wl_int(hashtablesize, 1);
          wl_str(' allowed)');
          end;
        stringtableoverflow:
          wl_str('Too many strings or identifiers');
        baddirective: wl_str('Unknown directive');
        deepinclude:
          begin
          wl_str('Too many nested INCLUDE directives (only ');
          wl_int(sourcedepth - 1, 1);
          wl_str(' allowed)');
          end;
        duplicateident:
          wl_str(
           'Identifier cannot be redefined or defined after use at this level'
                );
        undefidenterr: wl_str('Undefined identifier');
        indexerror: wl_str('Array subscript out of range');
        overflow: wl_str('Integer overflow or division by zero');
        bigarrayerr: wl_str('Array exceeds addressable memory');
        rangeerror: wl_str('Assignment value out of range');
        badsubrange: wl_str('Illegal subrange');
        badindex: wl_str('Index must be non-real scalar type');
        badsetbase: wl_str('Sets must be non-real scalar type');
        badsetexpression:
          wl_str('Set is constructed of incompatible types');
        badcasetyp:
          wl_str('Case label must be non-real scalar type');
        badcaselab:
          wl_str('Case label type does not match tag field type');
        duplicatetag:
          wl_str('Tag identifier already used in this record');
        duplabeldef:
          wl_str('Label cannot be redefined at this level');
        labnotpredef:
          wl_str('Label must be declared in LABEL declaration');
        badlabeldef: wl_str('Label defined twice');
        badtagerr:
          wl_str('Tag does not appear in variant record label list');
        labelundef:
          wl_str('Declared labels must be defined in procedure body');
        fwdundef:
          wl_str('Forward procedure/function body is never defined');
        typeundef:
          wl_str('Forward type reference is never resolved');
        dupfwdparam:
          wl_str(
                'Parameter list cannot be duplicated in forward-declared procedure/function body');
        dupfwdresult:
          wl_str(
               'Function result type cannot be duplicated in forward-declared function body');
        dupforward:
          wl_str(
           'This procedure/function name has been previously declared forward'
                );
        fwdprocfuncerr:
          wl_str('This function was declared as a forward procedure');
        fwdfuncprocerr:
          wl_str('This procedure was declared as a forward function');
        badxdef:
          wl_str(
            'External procedures/functions must be defined at outermost level'
                );
        recordexpected: wl_str('Variable of type record expected');
        arrayexpected: wl_str('Variable of type array expected');
        ptrexpected:
          wl_str('File variable or pointer variable expected');
        badfunctionarg:
          wl_str(
                'Function cannot be applied to an operand of this type');
        illegalformat:
          wl_str(
                'This parameter cannot be followed by a format expression');
        badformat:
          wl_str('Format expression must be of type integer');
        badreadtype:
          wl_str('Variables of this type are not allowed in READ');
        noreadarg: wl_str('Need at least one variable to READ');
        badwritearg:
          wl_str('Variables of this type are not allowed in WRITE');
        nostringerr:
          wl_str('Packed array [1..n] of characters expected');
        filenameerr:
          wl_str('File names in RESET/REWRITE are non-standard');
        noptrvar: wl_str('Pointer variable expected');
        nofieldtype: wl_str('Field variable expected for NEW');
        badnewlabel: wl_str('Variant label is undefined');
        nowritearg: wl_str('Need at least one value to WRITE');
        toomanyargs: wl_str('Too many actual parameters');
        toofewargs: wl_str('Too few actual parameters');
        paramtypeerr:
          wl_str(
                'Actual parameter type doesn''t match formal parameter type');
        booleanexpected: wl_str('Boolean value expected');
        badarithtype:
          wl_str('Operator cannot be applied to these operand types');
        signedseterr:
          wl_str(
                'Unary ''+'' or ''-'' cannot be applied to set operands');
        badreloprnds:
          wl_str(
                'Illegal comparison of record, array, file, or pointer values'
                );
        badrealtoint:
          wl_str(
      'Can''t assign a real value to an integer variable (use TRUNC or ROUND)'
                );
        baddbltoreal:
          wl_str(
      'Can''t assign a double value to a real variable (use SNGL)'
                );
        typesincomp:
          wl_str('Operands are of differing or incompatible type');
        compilerwritererr:
          wl_str(
   'Compiler writer error -- please contact Oregon Software at (503) 245-2202'
                );
        nostrictinclusion:
          wl_str('No strict inclusion of sets allowed');
        badinoprnds: wl_str('Bad IN operands');
        badforvar:
          wl_str(
                'FOR-loop control variable must be declared at this level');
        unsupportedforvardecl:
          wl_str(
'FOR-loop control variable declared as OWN, USE, DEFINE, SHARED, OR ORIGIN');
        badfortype:
          wl_str(
     'FOR-loop control variable can only be a simple non-real scalar variable'
                );
        badforlimit:
          wl_str(
                'Expression type is incompatible with FOR index type');
        badcasetype:
          wl_str(
                'CASE selection expression must be a non-real scalar type');
        badcaselabeltype:
          wl_str(
                'CASE label does not match selection expression type');
        indexincomp:
          wl_str(
                'Index expression type does not match array declaration');
        badprocparam: wl_str('Procedure name expected');
        badfuncparam: wl_str('Function name expected');
        varparamerr:
          wl_str(
                'VAR parameters cannot be passed an expression, packed field or variant tag');
        badassignment:
          wl_str(
                'Assignment operands are of differing or incompatible types');
        cantpack: wl_str('Can''t pack unstructured or named type');
        wantvarname: wl_str('Variable name expected');
        nofilefile: wl_str('File cannot contain a file component');
        dupcaselabel: wl_str('Case label defined twice');
        badfunctype:
          wl_str('Function result must be of scalar or pointer type');
        badfuncassign: wl_str('Illegal function assignment');
        missingforindex:
          wl_str('Index variable missing in this FOR statement');
        modifiedfor:
          wl_str(
                'Reassignment of FOR-loop control variable not allowed');
        badprocfunc:
          wl_str('Only functions can be called from expressions');
        badassign: wl_str('Assignment to constants not allowed');
        norecordident: wl_str('Record identifier expected');
        unassigned:
          wl_str('Must assign value before using variable');
        badorigin: wl_str('Bad ORIGIN value');
        novaluefile:
          wl_str('Files must be passed as VAR parameters');
        dontassignfile:
          wl_str('Assignment of file variables not allowed');
        longstring:
          wl_str('String constants may not include line separator');
        bigsetbase:
          wl_str('Set types must have a base in the range 0..255');
        nottextfile:
          wl_str('Readln, writeln, eoln, and page must be applied to text file');
        obsoletecomments:
          wl_str('Non-standard comment form, please use "{" or "(*"');
        typenotallowed:
          wl_str('A type identifier is not allowed here');
        progexpected: wl_str('PROGRAM heading expected');
        badmodop:
          wl_str('The divisor of a mod must be greater than zero');
        badpackconform:
          wl_str(
                'Packed conformant array parameters cannot be nested');
        confinconsistent:
          wl_str(
       'All parameters in a single parameter section must have the same type.'
                );
        badconfactual:
          wl_str(
       'Actual parameter cannot be used with this conformant array parameter.'
                );
        bigrecorderr: wl_str('Record too large');
        bigblockerr:
          wl_str('Data declarations for this block are too large');
        biglabelerr:
          begin
          wl_str('Label must lie in range 0..');
          wl_int(maxstandardlabel, 1);
          end;
        badnumber:
          wl_str(
          'Blank characters must separate identifiers from numeric constants');
        badfornestref:
          wl_str('Nested procedure modifies index variable');
        badcasetags:
          wl_str('Variant tags do not exactly match range of tag type');
        nameundef: wl_str('PROGRAM parameter is never defined');
        filenotdeclared:
          wl_str('External file must be declared in PROGRAM statement');
        inputnotdeclared:
          wl_str('Standard file "input" must be declared by PROGRAM statement');
        outputnotdeclared:
          wl_str(
            'Standard file "output" must be declared by PROGRAM statement');
        novarianttag:
          wl_str(
        'Variant record case selector may not be passed as a VAR parameter');
        notlevel0: wl_str('Conformant arrays are not Level 0');
        toomanyelements: wl_str('Too many array elements');
        eofincomment: wl_str('End of file encountered in a comment');
        baddouble:
          wl_str('Embedded DOUBLE switch is illegal after first token');
        manyscopes:
          wl_str(
    'Too many records, or forward, external or nonpascal procedures/functions'
                );
        baduniv: wl_str('UNIV may only be used with VAR parameters');
        badstringindex:
          wl_str('STRING limit must be an integer in the range 1..255');
        stringoverflowerr: wl_str('STRING exceeds allocated size');
        bodyfounderr:
          wl_str(
                'Procedure or function bodies not allowed with "define" option.');
        manyenviron:
          wl_str('Only one environment directive allowed.');
        badenviron:
          wl_str(
        	'Environment directive must precede all declarations.');
        badoptions:
          wl_str(
     'Environment file options inconsistent with currently defined options.');
        baddefine:
          wl_str(
                'This type of declaration not allowed with "define" option.');
        badcase:
          wl_str('Embedded case switch is illegal after first token');
        toomanyextvars: wl_str('Too many external variables');
        badsharedvar: wl_str('Shared variable declaration error');
        badusedefinevar: wl_str('Use and/or define not allowed here');
        badcvtfunc: wl_str('SNGL and DBL are illegal with DOUBLE switch');
        badinterruptproc: wl_str('Illegal interrupt procedure');
        badmultidef: wl_str('Improper redefinition of use/define variable');
        otherwise
          begin
          wl_str('??? UNKNOWN ERROR REPORTED ???');
          end;
        end {case err} ;
    end {listoneerror} ;




  procedure processerrorline;

    var
      firsterror, lasterror, lastprinted: errorindex;
      errorlines: integer;


    procedure listerrors;

      var
        i, size: integer;
        linepos: 1..linelen;


      begin {listerrors}
        linepos := 1;
        tab(1, leftmargin + 1);
        for i := firsterror to lasterror do
          with errortable[i] do
            begin
            size := digits(ord(err));
            if (errcolumn >= linepos) and (errcolumn + size < linelen) then
              begin
              tab(linepos + leftmargin, errcolumn + leftmargin);
              wl_chr('^');
              wl_int(ord(err), size);
              linepos := errcolumn + size + 1;
              end;
            end;
        wl_line;
        totallines := totallines + 1;
        for i := firsterror to lasterror do
          with errortable[i] do
            if uniqueerrs[err] then
              begin
              uniqueerrs[err] := false;
              wl_str('*** ');
              wl_int(ord(err), 2);
              wl_str(': ');
              listoneerror(err);
              wl_line;
              totallines := totallines + 1;
              errorsreported := errorsreported + 1;
              end;
        wl_line;
        totallines := totallines + 1;
      end {listerrors} ;


    procedure listbrieferrors;

      var
        i: integer;


      begin {listbrieferrors}
        for i := firsterror to lasterror do
          with errortable[i] do
            if uniqueerrs[err] then
              begin
              uniqueerrs[err] := false;
              wl_str_l(filename, filename_length);
              wl_chr('(');
              wl_int(max(1, linecount - save[sourcelevel].line), 1);
              wl_str(') : ');
              listoneerror(err);
              wl_line;
              totallines := totallines + 1;
              errorsreported := errorsreported + 1;
              end;
      end {listbrieferrors} ;


    begin {processerrorline}
      lineswitherrors := lineswitherrors + 1;
      firsterror := nexterror;
      lasterror := nexterror;
      lastprinted := nexterror;
      errorlines := 1;
      uniqueerrs[errortable[firsterror].err] := true;
      getnexterror;

      while nexterrline = linecount do
        begin
        lasterror := nexterror;
        with errortable[lastprinted] do
          if errortable[nexterror].errcolumn >
             digits(ord(err)) + errcolumn then
            begin
            uniqueerrs[errortable[nexterror].err] := true;
            errorlines := errorlines + 1;
            lastprinted := nexterror;
            end;
        getnexterror;
        end;

      if pageline + errorlines + 2 > pagelimit then
        begin
        pagelisting;
        physicalpage := physicalpage + 1;
        end
      else pageline := pageline + errorlines + 2;
      if switcheverplus[editlist] then listbrieferrors
      else
        begin
        processline;
        listerrors;
        end;
    end {processerrorline} ;


  procedure sorterrortable;

    var
      sorted: boolean;
      temp: errorrecord;
      i, top: integer;


    function posgtr(i, j: integer): boolean;

{ Determines whether or not the error messages should
  be swapped  -- preserves time order if errors overlap.
}


      begin {posgtr}
        with errortable[j] do
          if errortable[i].errline = errline then
            posgtr := errortable[i].errcolumn >
                      errcolumn + digits(ord(err)) + 1
          else posgtr := errortable[i].errline > errline;
      end {posgtr} ;


    begin {sorterrortable}
      top := lasterror - 1;

      repeat
        sorted := true;
        for i := 1 to top do
          if posgtr(i, i + 1) then
            begin
            temp := errortable[i];
            errortable[i] := errortable[i + 1];
            errortable[i + 1] := temp;
            sorted := false;
            top := i - 1;
            end;
      until sorted;
    end {sorterrortable} ;




  procedure printtrailer;


    begin {printtrailer}

      { Only 3 lines are printed if errors are going to terminal.
      }
      if fakelist then pageline := pageline + 3
      else pageline := pageline + 6;

      if pageline > pagelimit then pagelisting;

      { Don't print invocation line if errors are going to terminal.
      }
      if not fakelist then
        begin
        wl_line;
        wl_str('Invocation line:');
        wl_line;
        wl_str_l(cmdline, cmdlength); { Print user's command line }
        wl_line;
        end;

      wl_line;
      if lineswitherrors = 0 then
        begin
        wl_str(' *** No lines with errors detected ***');
        wl_line;
        end
      else if lineswitherrors = 1 then
        begin
        wl_str(' *** There was 1 line with errors detected ***');
        wl_line;
        end
      else
        begin
        wl_str(' *** There were ');
        wl_int(lineswitherrors, 1);
        wl_str(' lines with errors detected ***');
        wl_line;
        end;

    end {printtrailer} ;




  procedure skipwitherrors(limit: integer);

    var
      pseudopageline: integer;


    procedure skip(limit: integer);

      var
        column: columnindex;
        endofpage: boolean;


      begin {skip}
        while linecount < limit do
          begin
          column := 1;
          endofpage := (pseudopageline > pagelimit);

          while not (endofinput or endofline or endofpage or
                (pseudopageline > pagelimit)) do
            begin
            getch;
            if ch = chr(tabch) then
              column := (column div tabspace) * tabspace + tabspace
            else if ch = chr(formfeed) then endofpage := true
            else column := column + 1;
            end;

          if endofpage then
            begin
            if (pseudopageline > pagelimit) then
              physicalpage := physicalpage + 1
            else
              begin
              physicalpage := 1;
              pagecount := pagecount + 1;
              end;
            pseudopageline := 1;
            end
          else
            begin
            getch;
            linecount := linecount + 1;
            pseudopageline := pseudopageline + 1;
            end;
          end;
      end {skip} ;


    begin {skipwitherrors}
      skipmode := true;
      pseudopageline := pageline;
      while nexterrline < limit do
        begin
        skip(nexterrline);
        processerrorline;
        end;
      skip(limit)
    end {skipwitherrors} ;


  procedure listwitherrors(limit: integer);


    procedure listsource(limit: integer);


      begin {listsource}
        while linecount < limit do processline;
      end {listsource} ;


    begin {listwitherrors}
      skipmode := false;
      while nexterrline < limit do
        begin
        listsource(nexterrline);
        processerrorline
        end;
      listsource(limit)
    end {listwitherrors} ;


  begin {printlisting}
    save[1].line := 0;
    curfile := 1;
    sourcelevel := 1;
    curstringblock := - 1;

{ The following two lines replace the file name found on the command line,
  with the file name saved by SCAN at the beginning of the compilation.
  We then use OPENS to get to the file by name. }

    curfileptr := filerememberlist;
    changesourcename;

    lastfileptr := nil;
    save[1].filename := filename;
    save[1].filename_length := filename_length;
    save[1].fileptr := thisfileptr;
    opens;
    if not newdebugger and
       ((switcheverplus[debugging] or switcheverplus[profiling]) and
       (lasterror = 0) and not switcheverplus[defineswitch]) then
      seek(stmtfile, 1);

    sourcestate := normal;
    topofpage := true;
    endofpage := false;
    pagenextline := false;
    endofline := false;
    endofinput := false;
    linecount := 1;
    pageline := 1;
    pagecount := 1;
    physicalpage := 1;
    totallines := 1;
    nexterror := 0;
    lineswitherrors := 0;
    nextch := ' ';
    getch;

    errorsreported := 0;
    for e := firstwarning to lastwarning do uniqueerrs[e] := false;

    dateandtime;
    {z_b129(headerline, headerlength, site1, site2);}
    sorterrortable;

    i := 1;
    getnexterror;
    while i <= lastlist do
      begin
      with listtable[i] do
        begin
        skipwitherrors(start);
        listwitherrors(start + count);
        end;
      i := i + 1;
      end;

    printtrailer;
    lasterror := errorsreported;
  end {printlisting} ;


procedure initlist;

{ Initializations.
}

  begin {initlist}
    buffer_ctr := 0;
  end; {initlist}

procedure openl;

{ Open listing file.
}


  begin {openl}
    getfilename(listname, false, false, filename, filename_length);
    case hostopsys of
      vdos: assign(listing, trim(string(filename)) + '.ls');
      msdos, unix, apollo: assign(listing, trim(string(filename)) + '.lst');
      otherwise assign(listing, trim(string(filename)) + '.lis');
      end {case} ;
    rewrite(listing);
  end {openl} ;

procedure closel;

{ Close listing and last source file.
}


  begin {closel}
    if not fakelist then close(listing);
    close(source[sourcelevel]);
    if not newdebugger and (lasterror = 0) and (switcheverplus[debugging] or
       switcheverplus[profiling]) then
      close(stmtfile);
  end {closel} ;


procedure list;

{ Make program listing.  Goes to standard output if no explicit
  listing or error file has been specified.
}


  begin {list}
    initlist;
    if fakelist then printlisting(output)
    else
      begin
      openl;

      { If the last region of the file is nolisted and the list
        command line option is used, this dummy listing line will
        force a fakelist of any errors in the nolisted region.
        This fix causes no harm in the normal case.
      } 
      lastlist := lastlist + 1;
      listtable[lastlist].start := lastline;
      listtable[lastlist].count := 0;

      printlisting(listing);
      end;
  end {list} ;

end.
