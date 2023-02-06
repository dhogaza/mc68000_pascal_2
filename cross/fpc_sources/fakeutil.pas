{$nomain}
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

  Pascal-2 Compiler Miscellaneous Utility Routines

 Last modified by KRIS on 21-Nov-1990 15:35:38

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

type
  descriptor =
    packed record
      len: integer;
      txt_ref: ^cmdbuffer;
    end;


function lib$get_foreign(var line1: descriptor;
                         prompt: descriptor;
                         var length1: integer): integer;
  nonpascal;


procedure sys$exit(i: integer);
  nonpascal;




procedure getcmdline(var line: cmdbuffer;
                     var length: cmdindex);
  external;


procedure getcmdline;

  var
    i, j, k: 0..cmdlinelength;
    ret_status: integer;
    line_descr: descriptor;
    linebuf: cmdbuffer;
    prompt_descr: descriptor;
    prompt_buf: cmdbuffer;
    linelength: integer;
    movem: boolean; {we can move preceding switches to the end}


  begin
    case hostopsys of
      vms:
        begin
        {$nopointer - we've used ref() to get a ptr}
        with line_descr do
          begin
          len := cmdlinelength;
          txt_ref := ref(linebuf);
          end;
        if language = c then
          begin
          prompt_buf[1] := 'O';
          prompt_buf[2] := 'R';
          prompt_buf[3] := 'C';
          prompt_buf[4] := '>';
          end
        else
          begin
          prompt_buf[1] := 'P';
          prompt_buf[2] := 'A';
          prompt_buf[3] := 'S';
          prompt_buf[4] := '>';
          end;

        with prompt_descr do
          begin
          len := 4;
          txt_ref := ref(prompt_buf);
          end;
        ret_status := lib$get_foreign(line_descr, prompt_descr, linelength);
        linelength := linelength mod 256;
        if linelength < 0 then length := length + 256; {????}
        movem := false;
        i := 1;
        while (i <= linelength) and not movem do
          if linebuf[i] = ' ' then movem := true
          else i := i + 1;
        while movem and (linebuf[1] = '/') do
          begin
          i := linelength;
          j := 0;
          repeat
            i := i + 1;
            j := j + 1;
            linebuf[i] := linebuf[j];
            linebuf[j] := ' ';
          until linebuf[j + 1] = ' ';
          linelength := i;
          repeat
            j := j + 1
          until linebuf[j] <> ' '; {must be non-space at linebuf[oldlength +
                                    1]}
          k := 0; {shift to start}
          for i := j to linelength do
            begin
            k := k + 1;
            linebuf[k] := linebuf[i];
            end;
          linelength := k;
          end;
        line := linebuf;
        length := linelength;
        {$pointer}
        end;
      end;
  end;

procedure doheap;

{ This routine sets the heap between passes on the RSX to 68k but does
  nothing here.
}


  begin
  end;


procedure callo;

{ Load the overlay containing "p" and call the routine.
}


  begin
    p;
  end;

function newok;


  begin
    newok := true;
  end;


procedure p_expfnm(var f: text; {open text file whose name we want}
                   var fn: FilenameBuf; {file name to be updated}
                   var fnlen: FilenameIndex {length to be updated} );
  external;


procedure p_expfnm;

{ Expand a filename to its full specification, in place.
  This routine can use either the file variable (f), or the filename
  and length variables (fn and fnlen) as a basis.  Whatever works.
}


  begin

    { Not yet implemented }

  end;
