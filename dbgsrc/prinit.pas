
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

  Build module list.

 Last modified by KRIS on 26-Nov-1990 13:52:38
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

procedure p_dbioinit;

  begin
    p_stdio(inp, out);
  end;


procedure p_picoff(var code, data: addressrange);
  external;


procedure buildmdllist;
    { Construct a linked list of module descriptors based on the information
      provided by the compiler via the diagnostics block.  For now, only 
      the code address boundaries and the name are known. }

  type
    address = ^integer;
    addrshort = ^shortint;

  var
    mdl: mdl_pointer;
    currentblock: unitinfoptr;
    i, longint: integer;
    a: address;
    a16: addrshort;
    done: boolean;
    delta: shortint;
    code, data: addressrange;


  begin {buildmdllist}
    mdl_list := nil;
    currentblock := p_getdia;
    done := false;
    p_picoff(code, data);
    repeat
      a := loophole(address, currentblock);
      a := loophole(address, loophole(integer, a) -2);
      a16 := loophole(addrshort, a);
      delta := a16^;
	{ this must be negative tablelen }
      if (delta >= 0) then done := true
      else
	begin
	a := loophole(address, loophole(integer, a) + delta);
	  { ensure that a is a valid address - it must not encroach into
	    user code area or wrap around into very high (FFFF something)
	    memory }
	longint := abs(delta);
	if (loophole(integer, a) <= longint) or
	   (loophole(addressrange, a) <= register.pc.addr) then done := true
	else
	  begin
	  currentblock := loophole(unitinfoptr, a);
          with currentblock^ do
            begin
            if tablelen = 24 then
              begin
              new(mdl);
              mdl^.code_start := codestart + code;
              mdl^.code_end := codestart + codelen + code;
              if (register.pc.addr >= codestart) and
                 (register.pc.addr < mdl^.code_end) then
                main_mdl := mdl;
              for i := 1 to msymbolname do mdl^.nam[i] := ' ';
              for i := 1 to unitnamelength do mdl^.nam[i] := module_name[i];
              mdl^.proctree := nil;
              mdl^.info := unloaded;
              initproctree(mdl);
              mdl^.next := mdl_list;
              mdl_list := mdl;
              end;
	    end;
	  end;
        end;
      until done;
  end; {buildmdllist}
