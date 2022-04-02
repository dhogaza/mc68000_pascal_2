{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Pascal-2 debugger target program and data access routines.

 Last modified by KRIS on 26-Nov-1990 13:45:20
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{***************************************************************************

  Fetch and stash routines

  These routines use data_array to either retrieve a value from memory or 
  or register, or to stash it.

****************************************************************************}


procedure fetch(typ: data_access_types;
                   len: integer;
                   addr: addressrec;
                   var datapiece: data_array);

  const
    ourname = 'fetch';

  type
    dataptr = ^data_array;

  var
    i: integer;
    ptr: dataptr;


  begin {fetch}
    case typ of
      ins_space, dat_space:
        begin
        ptr := loophole(dataptr, addr.addr);
        for i := 1 to len do datapiece[i] := ptr^[i];
        end;
      gen_reg:
        case addr.regoff of
          5: datapiece[1] := register.r.d5;
          6: datapiece[1] := register.r.d6;
          7: datapiece[1] := register.r.d7;
          end;
      ptr_reg:
	case addr.regoff of
	  3: datapiece[1] := register.r.a3;
	  4: datapiece[1] := register.r.a4;
	  end;
      real_reg: 
        case addr.regoff of
          5: 
             if len = 1 then d$getsgl(register.r.fp5, datapiece)
             else if len = 2 then d$getdbl(register.r.fp5, datapiece)
             else choke(ourname);
          6: 
             if len = 1 then d$getsgl(register.r.fp6, datapiece)
             else if len = 2 then d$getdbl(register.r.fp6, datapiece)
             else choke(ourname);
          7: 
             if len = 1 then d$getsgl(register.r.fp7, datapiece)
             else if len = 2 then d$getdbl(register.r.fp7, datapiece)
             else choke(ourname);
          end;
      otherwise choke(ourname);
      end;
  end; {fetch}


procedure stash (typ: data_access_types;
                    len: integer;
                    addr: addressrec;
                    var datapiece: data_array);

  const
    ourname = 'stash';

  type
    dataptr = ^data_array;

  var
    i: integer;
    ptr: dataptr;


  begin {stash}
    case typ of
      ins_space, dat_space:
        begin
        ptr := loophole(dataptr, addr.addr);
        for i := 1 to len do ptr^[i] := datapiece[i];
        end;
      ptr_reg:
	case addr.regoff of
	  3: register.r.a3 := datapiece[1];
	  4: register.r.a4 := datapiece[1];
	  end;
      gen_reg:
        case addr.regoff of
          5: register.r.d5 := datapiece[1];
          6: register.r.d6 := datapiece[1];
          7: register.r.d7 := datapiece[1];
          otherwise;
          end;
      real_reg: 
        case addr.regoff of
          5: if len = 1 then d$putsgl(register.r.fp5, datapiece)
             else if len = 2 then d$putdbl(register.r.fp5, datapiece)
             else choke(ourname);
          6: if len = 1 then d$putsgl(register.r.fp6, datapiece)
             else if len = 2 then d$putdbl(register.r.fp6, datapiece)
             else choke(ourname);
          7: if len = 1 then d$putsgl(register.r.fp7, datapiece)
             else if len = 2 then d$putdbl(register.r.fp7, datapiece)
             else choke(ourname);
          end;
      otherwise choke(ourname);
      end;
  end; {stash}



procedure p_picoff(var code, data: addressrange);
  external;


procedure getunitlist(var unitlist: unitptr);
    {Construct a list of compilation unit descriptor records based 
     on the module information passed from the compiler via the 
     daignostics section. }

  type
    address = ^integer;
    addrshort = ^shortint;

  var
    unit: unitptr;
    currentblock: unitinfoptr;
    i, longint: integer;
    a: address;
    a16: addrshort;
    done: boolean;
    delta: shortint;
    code, data: addressrange;
    data_base_rec: addressrec;


  begin {getunitlist}
    unitlist := nil;
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
            if (tablelen - externtablelen) = 28 then
              begin
              new(unit);
              unit^.code_start := codestart + code;
              unit^.code_end := codestart + codelen + code;
              if (register.pc.addr >= codestart) and
                 (register.pc.addr < unit^.code_end) then
                unit^.mainunit := true
              else unit^.mainunit := false;
              for i := 1 to msymbolname do unit^.nam[i] := ' ';
              for i := 1 to unitnamelength do unit^.nam[i] := module_name[i];
              if switches[sw_own] then unit^.data_base := data_base + data
              else 
                begin
                data_base_rec := d$glbadr;
                unit^.data_base := data_base_rec.addr;
                end;
              unit^.picoffset := data;
              unit^.doublereal := switches[sw_doublereal];
              unit^.shortinteger:= switches[sw_short];
              unit^.casesensitive:= switches[sw_case];
              unit^.fpcoprocessor:= switches[sw_68881];
              fppinuse := fppinuse or unit^.fpcoprocessor;
              unit^.externaladdress.addr := 
                loophole(addressrange, a) + (tablelen - externtablelen);
              unit^.next := unitlist;
              unitlist := unit;
              end;
	    end;
	  end;
        end;
      until done;
  end; {getunitlist}
