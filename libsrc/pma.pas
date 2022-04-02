{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1981, 1982, 1983, 1984 1985, 1986, 1987, 1988, 
                1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Pascal Postmortem Analyzer

 Last modified by KRIS on 26-Nov-1990 14:03:52
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Pascal Postmortem Analyzer'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include opfig; {configuration file}

{ Pascal post-mortem analyzer.

  This routine gains control after an error has been fielded to print
  error messages and a walkback.

  This version is compatible with both 16 and 32 bit compilers.
}

const
  min_code_err = 1;
  max_code_err = 8;
  min_mach_err = 16;
  max_mach_err = 20;
  min_lib_err = 32;
  max_lib_err = 56;
  e_none = 56;

type
  shortint = 0..65535; {general machine word}
  address = ^shortint; {general machine address}

  {bit addressing stuff}
  bit = 0..7; {bits within a byte}
  byte = packed array [bit] of 0..1;
  bit_address =
    record
      byte_part: ^byte;
      bit_part: bit;
    end;

  longword = array [0..1] of shortint; {a 32 bit word}

  p_string =
    record
      len: 0..255; {one byte of length data}
      val: packed array [1..255] of char; {actual string}
    end;

  string_ptr = ^p_string;

  file_ptr = ^shortint; {temp kluge}

  stateptr = ^machinestate;

  machinestate = {holds the machine state}
    record
      d: array [0..7] of longword; {data registers}
      a: array [1..6] of address; {address registers}
      a0: address; {A0, stored separately}
      sr: shortint; {status register}
      ra: address; {return address}
    end; {machinestate}

  error_code = shortint; {error data encoding}
  line_number = shortint; {error line number}

  aux_kind = (string_kind, file_kind, int_kind);
  aux_code =
    record
      case aux_kind of
        string_kind: (message: string_ptr);
        file_kind: (block: file_ptr);
        int_kind: (value: longword);
    end;

  frame_ptr = ^frame; {stack frame pointer}
  frame =
    record
      old_frame: frame_ptr; {link to calling frame}
      return_pc: address; {procedure return address}
    end;

  {define the argument passed to p_pma}

  error_data =
    record
      user_stack: stateptr; {user stack pointer at error}
      code: error_code; {error which was detected}
      aux: aux_code; {auxiliary error code}
    end;

  bodykind = (nobody, p2procbody, p2funcbody, p2mainbody, m2procbody,
              m2modbody, cprocbody); {types of bodies}


procedure p_errmsg(var f: text; {file to receive message}
                   code: error_code; {error code to print}
                   aux: aux_code {aux data (if any)} );

{ Print the text of an error message.  The aux code will be used only
  if needed.
}
  external;


procedure p_getlno(pc: address; {program counter value to find}
                   var line: line_number; {resulting line number}
                   var kind: bodykind; {kind of code body}
                   var proc_name: bit_address; {start of proc name}
                   var gotname: boolean; {source name found in table}
                   var srcname: bit_address; {start of source file name}
                   var code: error_code {actual error code, if any} );

{ Find the source line number and procedure name (if any)
  for the PC address supplied.  The address is actually expected to be
  a return address, possibly just beyond the last code for the line.
}
  external;


procedure p_addrprint(var f: text; {file on which to write}
                      pc: address {error location} );

{ Print the address "pc" is explicitly as possible.
}
  external;


procedure p_pma(var state: error_data; {state at error}
                var out: text; {output file for diagnostics}
                var inp: text {input file for interaction} );

{ Pascal post-mortem analyzer.  Prints error data from the state at the
  time of the error.
}
  external;


procedure p_wtask(var out: text);
  external;


procedure p_errio;
  external;


procedure p_pma;

  type
    {the following is used with a negative subscript as a way to get at}
    {the library level}
    fakearray = array [0..0] of shortint;
    global_ptr = ^fakearray;

  var
    grossfake: integer; {used to get at the library level}
    gp: global_ptr; {used with grossfake to get at library level}
    lib_frames: shortint; {frames remaining in library code}
    liblevel: shortint; {initial library nesting count}
    diag_code: error_code; {error code from diagnostic data}
    this_frame: frame_ptr; {current frame pointer}
    this_pc: address; {current error or calling pc}
    global_frame: frame_ptr; {frame pointer value for global level}
    new_pc: address; {new pc in calling block}
    this_line: line_number; {line number found}
    this_kind: bodykind; {kind of block being processed}
    this_proc: bit_address; {start of procedure name}
    have_file: boolean; { source name was found }
    this_file: bit_address; { source file name }
    call_count: integer; {count of identical calls to this procedure}
    done: boolean; {terminate walkback}


  begin
    global_frame := loophole(frame_ptr, longword(0, 0));
    grossfake := - 1;
    if stndalon then p_errio;
    done := false;

    with state, user_stack^ do
      begin
      { Initialize to frame where error occurred}
      gp := loophole(global_ptr, a[5]);
      liblevel := gp^[grossfake];
      this_frame := loophole(frame_ptr, a[6]);
      this_pc := ra;

      if (code < min_mach_err) or (code > max_mach_err) then
        begin
        lib_frames := liblevel;
        while lib_frames > 0 do
          begin
          this_pc := this_frame^.return_pc;
          this_frame := this_frame^.old_frame;
          lib_frames := lib_frames - 1;
          end;
        end;

      p_getlno(this_pc, this_line, this_kind, this_proc, have_file, this_file,
               diag_code);
      if diag_code <> 0 then code := diag_code
      else code := code mod 256;

      if code <> e_none then writeln(out);
      p_errmsg(out, code, aux);

      write(out, 'Error occurred');

      if versados then p_wtask(out);
      write(out, ' at ');

      {Now trace back the entire stack}
      repeat
        p_addrprint(out, this_pc);

        { Now skip to the next different activation }
        if this_frame = global_frame then done := true
        else
          begin
          call_count := 0;
          repeat
            new_pc := this_frame^.return_pc;
            this_frame := this_frame^.old_frame;
            call_count := call_count + 1;
          until new_pc <> this_pc;
          this_pc := new_pc;

          if call_count > 1 then write(out, ' (', call_count: 1, ' times)');
          writeln(out);
          write(out, 'Last called from ');
          end;
      until done;
      end;
    writeln(out);
  end; {p_pma}
