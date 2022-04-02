{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: all
  System: all

  Pascal-2 debugger expression evaluation machine for target.

 Last modified by KRIS on 26-Nov-1990 13:44:13
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{****************************************************************************

      -----  Expression Evaluation Machine

 This routine is basically a machine which will evaluate expressions passed
 down from the interpreter in the form of a stream of datatokens.  Within
 the machine there is a current context maintained which comprises an
 address, offset, storage class and, if needed, a literal local value.
 There is a separate stack used to store and restore context as needed.  
 Probes into memory, either to retrieve a value or write over a location, 
 are always made with respect to the current context.  A stack of 
 datatokens is also maintained to evaluate expressions which are arriving 
 in reversed polish form.  The datapackets may either be literal values, or a
 stored context which can be converted to a literal value, if needed.  A 
 literal local value is used as the context memory loaction when the previous 
 value of a watched variable is wanted.  This value is retrieved from the 
 watched variable record and stored in "currentlocaldata" while it needed.

******************************************************************************}


procedure xt_dataaccess {(tokenlist: datatokenptr; var packetlist:
                         datapacketptr; var errmsg: xtmessage; var errnum:
                         integer)} ;

  {Process the datatokenlist and return a datapacketlist if required.  The 
   datatokenlist is used to drive the stack machine in order to evaluate
   expressions.}

  label
    1;

  type
    err_severity = (fatal, nonfatal);

  var
    t: datatokenptr;
    d, setd, tmpd, tmpp: datapacketptr;
    off: integer;
    i: integer;
    w: watchingrecptr;

{}
procedure d_addtoset(var s: dbset; i: integer);
  begin
    s := s + [i];
  end;

  function getdatapacket: datapacketptr;

    var
      d: datapacketptr;

    begin
    d := datapacketfreelist;
    if d = NIL then 
      begin
      new(d);
      d^.next := NIL;
      end;
    getdatapacket := d;
    datapacketfreelist := d^.next;
    d^.next := NIL;
    end;

  procedure freedatapacket(packet: datapacketptr);
  
    begin  {freedatapacket}
      packet^.next := datapacketfreelist;
      datapacketfreelist := packet;
    end;  {freedatapacket}

  procedure cleanstacks;

   {Clear the stacks in preparation for a new expression.}

    var
      i: 0..maxstack;
      d, tmpd: datapacketptr;

    begin {cleanstacks}
      for i := 0 to maxstack do
        begin
        d := packetstack[i];
        while d <> NIL do
          begin
          tmpd := d;
          d := d^.next;
          freedatapacket(tmpd);
          end;
        packetstack[i] := NIL;        
        end;
      packetindex := 0;
      contextindex := 0;
      currentoffset := 0;
    end; {cleanstacks}


  procedure accesserror(err: xtmessage; severity: err_severity);

   {Process an error condition.  For command line errors, the index into
    the command line is returned for the current token. }


    begin {accesserror}
      errmsg := err;
      if not processerror then
        if (err > firstcommanderror) and (err < lastcommanderror) then
          errnum := t^.cmdloc;
      if severity = fatal then
        begin
        if processerror then errnum := errornum
        else if errnum = 0 then errnum := -1
        else errnum := - errnum;
        cleanstacks;
        processerror := false;
        goto 1;
        end;
    end; {accesserror}


  procedure savecontext;

   {Save the current context.}


    begin {savecontext}
      if contextindex >= maxstack then accesserror(stackoverflow, fatal);
      contextindex := contextindex + 1;
      context[contextindex].addr := currentaddress;
      context[contextindex].offset := currentoffset;
      context[contextindex].storeclass := currentstorageclass;
    end; {savecontext}


  procedure restorecontext;

    {Pop the context from the top of the stack and make that the current
     context.}


    begin {restorecontext}
      if contextindex = 0 then accesserror(stackunderflow, fatal);
      currentaddress := context[contextindex].addr;
      currentoffset := context[contextindex].offset;
      currentstorageclass := context[contextindex].storeclass;
      contextindex := contextindex - 1;
    end; {restorecontext}


  procedure pushpacket(thislit: datapacketptr);

   {Push the given packet onto the stack.}


    begin {pushpacket}
      if packetindex >= maxstack then accesserror(stackoverflow, fatal);
      packetindex := packetindex + 1;
      packetstack[packetindex] := thislit;
    end; {pushpacket}


  procedure poppacket(var thislit: datapacketptr);

   {Pop a packet from the top of the stack.}


    begin {poppacket}
      if packetindex = 0 then thislit := nil
      else
        begin
        thislit := packetstack[packetindex];
        packetstack[packetindex] := nil;
        packetindex := packetindex - 1;
        end;
    end; {poppacket}


  procedure setoffset(bias: integer);

    {Add the given bias to the currentoffset and evaluate the new
     address and offset.}

    var
      off: integer;


    begin {setoffset}
      off := bias + currentoffset;
      case currentstorageclass of
        watchedlocal:
          begin
          currentoffset := off mod bitspertransferunit;
          currentaddress.addr := currentaddress.addr + (off div
                               bitspertransferunit);
          end;
        gen_reg, ptr_reg, real_reg, fpp_reg:
          currentoffset := bias;
        dat_space, ins_space:
          begin
          currentoffset := off mod bitsperaddressableunit;
          if off < 0 then off := off - bitsperaddressableunit + 1;
          currentaddress.addr := currentaddress.addr + (off div
                                 bitsperaddressableunit) * 
                                 addressableunitsize;
         end;
       end;
    end; {setoffset}



{**************************************************************************

      --------  Fetch and Stash Routines

  These routines use the current context and the length provided to either
  retrieve a value or to store a given value.  The values are locally 
  stored in datapackets, which can be in a linked list for large values.

***************************************************************************}


  function lenmeasure(bitlen: integer): integer;

    {Determine the number number of transfer units needed to span the
     given number of bits.}


    begin
      lenmeasure := (bitlen + currentoffset) div bitspertransferunit +
                    ord(((bitlen + currentoffset) mod bitspertransferunit) >
                    0);
    end;


  function lastmeasure(bitlen: integer): integer;

   {Determine the number of bits that overlap into the last transfer unit.}


    begin
      lastmeasure := (bitlen + currentoffset) mod bitspertransferunit;
    end;


  procedure leftshift(var a: data_array;
                      len: integer;
                      off: integer);

   {Shift the data_array the given number of bits to the left.}

    var
      i: integer;


    begin {leftshift}
      for i := 1 to len - 1 do
        if originright then
          a[i] := (a[i] and
                  fieldmask[bitspertransferunit - off]) div bitmask[off] +
                  a[i + 1] * bitmask[bitspertransferunit - off]
        else
          a[i] := (a[i] and
                  fieldmask[bitspertransferunit - off]) * bitmask[off] +
                  a[i + 1] div bitmask[bitspertransferunit - off];
      if originright then
        a[len] := a[len] and
                  fieldmask[bitspertransferunit - off] div bitmask[off]
      else
        a[len] := a[len] and
                  fieldmask[bitspertransferunit - off] * bitmask[off];
    end; {leftshift}


  procedure rightshift(var a: data_array;
                       len: integer;
                       off: integer);

   {Shift the data_array the given number of bits to the right.}

    var
      i: integer;


    begin {rightshift}
      for i := len downto 2 do
        if originright then
          a[i] := (a[i - 1] and
                  fieldmask[off]) div bitmask[bitspertransferunit - off] +
                  (a[i] and not fieldmask[off]) * bitmask[off]
        else
          a[i] := (a[i - 1] and
                  fieldmask[off]) * bitmask[bitspertransferunit - off] +
                  (a[i] and not fieldmask[off]) div bitmask[off];
      if originright then
        a[1] := (a[1] and not fieldmask[off]) * bitmask[off]
      else a[1] := (a[1] and not fieldmask[off]) div bitmask[off];
    end; {rightshift}


  procedure fetchvalue(bitlen: integer;
                       var thislit: datapacketptr);

    {Fetch a value from the currentstorageclass and place in a datapacket
     or datapacket list.  If form memory of register, the routine "fetch"
     is called to fill the data_array "xferarray".  Some masking and shifting
     may then be required. }

    var
      addr: addressrec;
      last: 0..bitspertransferunit; { number of bits left over in the last
                                     transfer unit }
      length, currentxferlength, currentpacklength: integer;
      numpacks: integer; { number of datapackets requird to hold value }
      i, j, pack: integer;
      xfer: data_array; { array used to tranfer data form the currentstorage
                         area to the current packet }
      currentlit: datapacketptr; { current packet holding the literal value }
      done: boolean;
      signext: boolean; {will the result need to be sign extended}
      datatyp: types; {type of data fetched}
      realbuf: extendedrealarray;  {temp buffer for extended reals}


    begin { fetchvalue}

      if (thislit^.typ = storeref) and thislit^.signed and
         (thislit^.len < bitsperint) then
        signext := true
      else signext := false;
      datatyp := thislit^.storetyp;

      case currentstorageclass of

        dat_space, ins_space, watchedlocal:
          begin

          savecontext;
          length := lenmeasure(bitlen);
          last := lastmeasure(bitlen);
          numpacks := (length - 1) div datapacketlength + 1;
          currentlit := nil;

          if currentstorageclass = watchedlocal then
            begin
            if numpacks > 1 then accesserror(watchlimit, nonfatal);
            currentxferlength := length;
            currentpacklength := min(currentxferlength, datapacketlength);
            end
          else
            begin
            for pack := 1 to numpacks - 1 do
              begin
              currentxferlength := datatransferlength;
              fetch(dat_space, currentxferlength, currentaddress, xfer);
              if processerror then accesserror(cantreadtext, fatal);
              if last <> 0 then
                xfer[currentxferlength] := xfer[currentxferlength] and
                                           not fieldmask[bitspertransferunit -
                                           last];
              if currentoffset <> 0 then
                leftshift(xfer, currentxferlength, currentoffset);
              if currentlit = nil then currentlit := thislit
              else
                begin
                currentlit^.next := getdatapacket;
                currentlit := currentlit^.next;
                end;
              currentlit^.next := nil;
              if (datatyp in [scalars, bools, chars, ints, subranges]) then
                currentlit^.typ := scalar
              else
                currentlit^.typ := nonetype;
              currentlit^.len := bitlen;
              for i := 1 to datapacketlength do
                currentlit^.dnone[i] := xfer[i];
              setoffset(datapacketsize * bitsperunit);
              end;
            currentxferlength := length - (numpacks - 1) * datapacketlength;
            currentpacklength := min(currentxferlength, datapacketlength);
            end;


          if currentstorageclass = watchedlocal then
            begin
            currentxferlength := min(currentxferlength, datatransferlength);
            done := false;
            i := 1;
            while not done do
              begin
              j := currentaddress.addr + i - 1;
              if (j > datatransferlength) then
                begin
                errmsg := watchlimit;
                done := true;
                end;
              xfer[i] := currentlocaldata[j];
              i := i + 1;
              done := (i > currentxferlength);
              end;
            end
          else if (datatyp in [doubles, reals]) and thislit^.isfppreg then
            begin
            fetch(dat_space, 3, currentaddress, xfer);
            for i := 1 to 3 do realbuf[i] := xfer[i];
            if fpc_available then
              if currentxferlength = 1 then d$getsgl(realbuf, xfer)
              else d$getdbl(realbuf, xfer);
            end
          else fetch(dat_space, currentxferlength, currentaddress, xfer);
          if processerror then accesserror(cantreadtext, fatal);

          if last <> 0 then
            xfer[currentxferlength] := xfer[currentxferlength] and
                                       not fieldmask[bitspertransferunit -
                                       last];
          if currentoffset <> 0 then
            leftshift(xfer, currentxferlength, currentoffset);
          if currentlit = nil then currentlit := thislit
          else
            begin
            currentlit^.next := getdatapacket;
            currentlit := currentlit^.next;
            end;
          if (datatyp in [scalars, bools, chars, ints, subranges]) then
            currentlit^.typ := scalar
          else
            currentlit^.typ := nonetype;
          for i := 1 to datapacketlength do currentlit^.dnone[i] := 0;
          currentlit^.len := bitlen;
          for i := 1 to currentpacklength do currentlit^.dnone[i] := xfer[i];
          currentlit^.next := nil;

          restorecontext;

          end;

        gen_reg, ptr_reg:
          begin
          fetch(currentstorageclass, 1, currentaddress, xfer);
          if processerror then accesserror(cantreadtext, fatal);
          for i := 1 to datapacketlength do thislit^.dnone[i] := 0;
          thislit^.len := bitspertransferunit;
          if (datatyp in [scalars, bools, chars, ints, subranges]) then
            thislit^.typ := scalar
          else
            thislit^.typ := nonetype;
          if originright then
            thislit^.dnone[1] := xfer[1] and
                               not fieldmask[bitspertransferunit - bitlen]
          else
            thislit^.dnone[1] := xfer[1] and fieldmask[bitlen];
          thislit^.next := nil;
          end;

        real_reg:
          begin
          length := (thislit^.len - 1) div bitspertransferunit + 1;
          fetch(currentstorageclass, length, currentaddress, xfer);
          if processerror then accesserror(cantreadtext, fatal);
          for i := 1 to datapacketlength do thislit^.dnone[i] := 0;
          for i := 1 to length do thislit^.dnone[i] := xfer[i];
          thislit^.typ := nonetype;
          thislit^.next := nil;
          end;

        otherwise accesserror(stackerr, fatal);

        end;

 	{Ordinal values are all mapped into an integer sized unit}
      if not originright and (thislit^.len < bitspertransferunit) and
          (thislit^.typ = scalar) then
        thislit^.sc := thislit^.sc div 
            bitmask[bitspertransferunit - thislit^.len];

      if signext and (thislit^.sc and bitmask[bitlen - 1] <> 0) then
        if originright then
            thislit^.sc := thislit^.sc or
                       fieldmask[bitspertransferunit - bitlen]
        else
            thislit^.sc := thislit^.sc or not fieldmask[bitlen];

    end; {fetchvalue}


  procedure stashvalue(len: integer;
                       thislit: datapacketptr);

    {Take a value from a datapacket or datapacket list and place into the
     currentstorageclass location.}

    type
      realkludgerec = packed record
        case boolean of
          true: (r: realarray);
          false: (i: packed array [1..2] of transferunit);
        end;

    var
      addr, tmpaddr: addressrec;
      last: 0..bitspertransferunit;
      i: integer;
      length, currentlength: integer;
      xfer, tmp: data_array;
      currentlit: datapacketptr;
      real_rec: realkludgerec;
      realbuf: extendedrealarray;  {temp buffer for extended reals}


    begin { stashvalue }

      if not active then accesserror(inactive, fatal);

      if not originright and (len < bitspertransferunit) and
          (thislit^.typ = scalar) then
        thislit^.sc := (thislit^.sc and fieldmask[len]) *
            bitmask[bitspertransferunit - len];

      if (thislit^.typ in [singlereal, doublereal]) then
          {shift thislit^.dr to the start of the packet store area}
        begin
        if len = doublerealsize * bitsperunit then 
          begin
          real_rec.r := thislit^.dr;
          thislit^.dnone[1] := real_rec.i[1];
          thislit^.dnone[2] := real_rec.i[2];
          end;
        thislit^.len := len;
        end;

      case currentstorageclass of

        dat_space, ins_space:
          begin
          savecontext;
          length := lenmeasure(len);
          last := lastmeasure(len);
          currentlit := thislit;

          while currentlit^.next <> nil do
            begin
            for i := 1 to datapacketlength do
              xfer[i] := currentlit^.dnone[i];
            if currentoffset <> 0 then
              begin
              rightshift(xfer, datapacketlength, currentoffset);
              fetch(dat_space, 1, currentaddress, tmp);
              if processerror then accesserror(processerr, fatal);
              if originright then
                xfer[1] := xfer[1] and
                         fieldmask[bitspertransferunit - currentoffset] +
                         tmp[1] and
                         not fieldmask[bitspertransferunit - currentoffset]
                else
                xfer[1] := xfer[1] and
                         fieldmask[bitspertransferunit - currentoffset] +
                         tmp[1] and
                         not fieldmask[bitspertransferunit - currentoffset];
              end;
            if (last > 0) then
              begin
              savecontext;
              setoffset((datapacketlength - 1) * bitspertransferunit);
              fetch(dat_space, 1, currentaddress, tmp);
              if processerror then accesserror(processerr, fatal);
              if originright then
                xfer[datapacketlength] := xfer[datapacketlength] and
                                        not fieldmask[bitspertransferunit -
                                        last] + tmp[1] and
                                        fieldmask[bitspertransferunit -
                                        last]
              else
                xfer[datapacketlength] := xfer[datapacketlength] and
                                        not fieldmask[bitspertransferunit -
                                        last] + tmp[1] and
                                        fieldmask[bitspertransferunit -
                                        last];
              restorecontext;
              end;
            stash(dat_space, datapacketlength, currentaddress, xfer);
            if processerror then accesserror(cantwritetext, fatal);
            setoffset(datapacketsize * bitsperunit);
            currentlit := currentlit^.next;
            end;


          currentlength := length mod datapacketlength;
          if currentlength = 0 then currentlength := 8;

          if (thislit^.typ in [singlereal, doublereal]) and
            thislit^.fppregsav then
                {fpp registers are saved on the stack in extended format, so
                 we need to convert from single/double to extended first}
              begin
              for i := 1 to currentlength do xfer[i] := thislit^.dnone[i];
              if fpc_available then
                if currentlength = 1 then d$putsgl(realbuf, xfer)
                else d$putdbl(realbuf, xfer);
              for i := 1 to 3 do xfer[i] := realbuf[i];
              currentlength := 3;
              end
          else
            for i := 1 to currentlength do xfer[i] := currentlit^.dnone[i];

          if currentoffset <> 0 then
            begin
            rightshift(xfer, currentlength, currentoffset);
            fetch(dat_space, 1, currentaddress, tmp);
            if processerror then accesserror(processerr, fatal);
            xfer[1] := xfer[1] and
                       fieldmask[bitspertransferunit - currentoffset] +
                       tmp[1] and
                       not fieldmask[bitspertransferunit - currentoffset];
            end;
          if (last > 0) then
            begin
            savecontext;
            setoffset((currentlength - 1) * bitspertransferunit);
            fetch(dat_space, 1, currentaddress, tmp);
            if processerror then accesserror(cantreadtext, fatal);
            xfer[currentlength] := xfer[currentlength] and
                                   not fieldmask[bitspertransferunit -
                                   last] + tmp[1] and
                                   fieldmask[bitspertransferunit - last];
            restorecontext;
            end;
          stash(dat_space, currentlength, currentaddress, xfer);
          if processerror then accesserror(cantwritetext, fatal);
          restorecontext;

          end;

        gen_reg, ptr_reg:
          begin
          xfer[1] := thislit^.dnone[1];
          if not originright then
            xfer[1] := xfer[1] div bitmask[bitspertransferunit - len];
          stash(currentstorageclass, 1, currentaddress, xfer);
          if processerror then accesserror(cantwritetext, fatal);
          end;

        real_reg:
          begin
          length := (thislit^.len - 1) div bitspertransferunit + 1;
          for i := 1 to length do xfer[i] := thislit^.dnone[i];
          stash(currentstorageclass, length, currentaddress, xfer);
          if processerror then accesserror(cantwritetext, fatal);
          end;

        otherwise accesserror(stackerr, fatal);

        end;

    end; { stashvalue }



{*************************************************************************

    --------   Utility Routines

*************************************************************************}


  procedure stringtoreal(thislit: datapacketptr);

    {Convert a character string to a single precisonn real and a double
     precision real.}

    var
      idx: datapacketindex;
      err: sysint;
      realerr: realstatus;
      str: datapacketstring;
      tmpreal: real;
      realtrick: record case boolean of
        false: (d: realarray);
        true: (s: singlerealkludge);
        end;

    function readch: char;

      begin
	if idx <= datapacketsize then 
	  begin
          readch := str[idx];
          idx := idx + 1;
	  end
 	else readch := ' ';
      end;


    begin {stringtoreal}
      err := 0;
      idx := 1;
      str := thislit^.str;
      if (targetmachine = mc68000) and (targetopsys = unix) then
        begin
        readreal(readch, realtrick.d, realerr, IEEE_single);
        thislit^.sr := realtrick.s;
        if realerr <> r_noerror then accesserror(realformaterr, fatal);
        end
      else
        begin
        p_ctof(readch, thislit^.sr, err);
        if err <> librarynoerror then accesserror(realformaterr, fatal);
        end;
      idx := 1;
      if (targetmachine = mc68000) and (targetopsys = unix) then
        begin
        readreal(readch, thislit^.dr, realerr, IEEE_double);
        if realerr <> r_noerror then accesserror(realformaterr, fatal);
        end
      else
        begin
        tmpreal := loophole(real, thislit^.dr);
        p_ctod(readch, tmpreal, err);
        thislit^.dr := loophole(realarray, tmpreal);
        if err <> librarynoerror then accesserror(realformaterr, fatal);
        end;
      thislit^.typ := singlereal;
    end; {stringtoreal}


  procedure realtostring(thislit: datapacketptr;
                         left, right: integer;
                         floattype: datatypes);

    {Convert a real number to a character string representation of that
     number.}

    var
      idx: datapacketindex;


    procedure putch(ch: char);


      begin {putch}
	if idx < datapacketsize then
	  begin
          thislit^.str[idx] := ch;
          idx := idx + 1;
	  end;
      end; {putch}


    begin {realtostring}
      idx := 1;
      case floattype of
        singlestring:
          begin
          p_ftoc2(putch, thislit^.sr, left, right);
          thislit^.typ := singlereal;
          end;
        doublestring:
          begin
          p_dtoc2(putch, loophole(real, thislit^.dr), left, right);
          thislit^.typ := doublereal;
          end;
        end;
      thislit^.str[idx] := ' ';
    end; {realtostring}


  procedure setwatch(watchid, watchlen, lev: integer);

    {Create a watching variable record and insert into the watching list.
     This will save the current context for later reference.}

    var
      w: watchingrecptr;
      last: 0..bitspertransferunit;
      len: integer;


    begin {setwatch}
      last := lastmeasure(watchlen);
      len := lenmeasure(watchlen);
      new(w);
      with w^ do
        begin
        ident := watchid;
        level := lev;
        store := currentstorageclass;
        addr := currentaddress;
        watchlength := min(len,
                           ((watchlimitsize div transferunitsize) +
                           ord(last > 0)));
        toolarge := (len > watchlength);
        firstmask := fieldmask[bitspertransferunit - currentoffset];
        if last = 0 then last := bitspertransferunit;
        if toolarge then lastmask := - 1
        else lastmask := not fieldmask[bitspertransferunit - last];
        if not originright and ((currentstorageclass = gen_reg) or
            (currentstorageclass = ptr_reg)) then 
          begin
          len := targetregistersize;
          firstmask := fieldmask[len];
          lastmask := firstmask;
          offset := bitspertransferunit - watchlen;
          end
        else offset := 0;
        fetch(currentstorageclass, watchlength, currentaddress, value);
        if processerror then accesserror(cantreadtext, fatal);
        value[1] := value[1] and firstmask;
        value[watchlength] := value[watchlength] and lastmask;
        end;
      w^.next := watchinglist;
      watchinglist := w;
    end; {setwatch}

  function inbounds(i, first, last: integer; signed: boolean): boolean;
    
    begin
      if signed then
        inbounds := (i >= first) and (i <= last)
      else
        inbounds := true;
    end;


  procedure makelocal(var d: datapacketptr);
    { Convert a storage reference datapacket into a literal value by
      setting the current context, then fetch the value.  In the case
      of reals, create both a single and double precision version of
      the number. }

    var
      localtype: types;
      makereal, isreal: boolean;
      len: integer;
      i: integer;
      k:
        record
          case boolean of
            true: (s: packed array [1..doublerealsize] of char);
            false: (r: realarray);
        end;


    begin {makelocal}
      savecontext;
      len := d^.len;
      localtype := d^.storetyp;
      currentaddress := d^.addr;
      currentoffset := d^.off;
      currentstorageclass := d^.store;
      makereal := d^.makereal;
      isreal := d^.storetyp in [reals, doubles];
      fetchvalue(len, d);
      if localtype in [scalars, ints, subranges, chars, bools] then 
        d^.typ := scalar
      else if localtype in [reals, doubles] then d^.typ := singlereal
      else d^.typ := nonetype;
      if makereal then
        if not (targetopsys = msdos) then
          begin
          i := d^.sc;
          d^.sr := d$sglconv(i, fppinuse);
          d^.dr := d$dblconv(i, fppinuse);
          end;
      if isreal then
        begin
        if len = (singlerealsize * bitsperunit) then
          begin
          d^.sr := loophole(singlerealkludge, d^.dnone[1]);
          realtostring(d, 31, 25, singlestring);
          stringtoreal(d);
          end
        else
          begin
          for i := 1 to doublerealsize do k.s[i] := d^.str[i];
          d^.dr := k.r;
          realtostring(d, 31, 25, doublestring);
          stringtoreal(d);
          end;
        end;
      restorecontext;
    end; {makelocal}



{**************************************************************************

       Expression Evaluation

***************************************************************************}


  procedure evaluate;

    label 1;
    type
      realkludgerec = packed record
        case boolean of
          true: (r: realarray);
          false: (i: packed array [1..2] of transferunit);
        end;
    var
      currop: operatortypes;
      currd, left, right, tmpd, ltmpd, rtmpd: datapacketptr;
      bumpoffset, sofar: boolean;
      tmplen: integer;
      tmpreal: real;
      real_rec: realkludgerec;
      i: integer;


    begin {evaluate}
      bumpoffset := false;
      poppacket(currd);
      if currd = nil then accesserror(stackerr, fatal);
      if currd^.typ = operators then
        begin
        while (currd <> nil) and (currd^.typ = operators) do
          begin
          currop := currd^.op;
          case currop of

            equals:
              begin
              poppacket(right);
              poppacket(left);
              if left^.typ <> storeref then accesserror(stackerr, fatal);
              if right^.typ = storeref then makelocal(right);
              if (left^.storetyp = reals) or (left^.storetyp = doubles) then
                right^.fppregsav := left^.isfppreg;
              if left^.storetyp in [subranges, scalars, bools, chars] then
                if not inbounds(right^.sc, left^.lower, 
                                left^.upper, left^.signed) then
                  accesserror(subrangerr, nonfatal);
              if left^.xisstring then
                if ((right^.typ = storeref) and not right^.xisstring) or
                   (right^.typ = stringtype) or (right^.typ = scalar) then
                  begin
                  if right^.typ = scalar then tmplen := bitsperunit
                  else tmplen := right^.len;
                  if tmplen > left^.len then 
                    accesserror(incompatsize, nonfatal)
                  else left^.len := tmplen;
                  tmpd := getdatapacket;
                  tmpd^.next := nil;
                  tmpd^.str[1] := chr(tmplen div bitsperunit);
                  tmpd^.typ := stringtype;
                  stashvalue(bitsperunit, tmpd);
                  freedatapacket(tmpd);
                  bumpoffset := true;
                  end;
              currentaddress := left^.addr;
              currentoffset := left^.off;
              if bumpoffset then setoffset(bitsperunit);
              currentstorageclass := left^.store;
              stashvalue(left^.len, right);
           1:
              freedatapacket(left);
              freedatapacket(right);
              end;

            negateop:
              begin
              poppacket(right);
              if right^.typ = storeref then makelocal(right);
              if (currd^.optyp = reals) or (currd^.optyp = doubles) then
                begin
                if not (targetopsys = msdos) then
                  begin
                  right^.sr := d$sglnegate(right^.sr, currd^.fpp);
                  right^.dr := d$dblnegate(right^.dr, currd^.fpp);
                  end;
                end
              else right^.sc := - right^.sc;
              pushpacket(right);
              end;

            notop:
              begin
              poppacket(right);
              if right^.typ = storeref then makelocal(right);
              right^.sc := ord(right^.sc = 0);
              pushpacket(right);
              end;

            addop, subtractop, timesop, intoop, divop, modop, andop, orop:
              begin
              poppacket(right);
              tmplen := right^.len;
              if right^.typ = storeref then makelocal(right);
              poppacket(left);
              if left^.typ = storeref then makelocal(left);
              case currd^.optyp of
                ints:
                  begin
                  case currd^.op of
                    addop: left^.sc := left^.sc + right^.sc;
                    subtractop: left^.sc := left^.sc - right^.sc;
                    timesop: left^.sc := left^.sc * right^.sc;
                    divop: left^.sc := left^.sc div right^.sc;
                    modop: left^.sc := left^.sc mod right^.sc;
                    andop: left^.sc := left^.sc and right^.sc;
                    orop: left^.sc := left^.sc or right^.sc;
                    end;
                  end;
                reals, doubles:
                  if not (targetopsys = msdos) then
                    case currd^.op of
                    addop:
                      begin
                      left^.sr := d$sgladd(left^.sr, right^.sr, currd^.fpp);
                      left^.dr := d$dbladd(left^.dr, right^.dr, currd^.fpp);
                      end;
                    subtractop:
                      begin
                      left^.sr := d$sglsub(left^.sr, right^.sr, currd^.fpp);
                      left^.dr := d$dblsub(left^.dr, right^.dr, currd^.fpp);
                      end;
                    timesop:
                      begin
                      left^.sr := d$sglmult(left^.sr, right^.sr, currd^.fpp);
                      left^.dr := d$dblmult(left^.dr, right^.dr, currd^.fpp);
                      end;
                    intoop:
                      begin
                      left^.sr := d$sglinto(left^.sr, right^.sr, currd^.fpp);
                      left^.dr := d$dblinto(left^.dr, right^.dr, currd^.fpp);
                      end;
                    end;
                bools:
                  begin
                  case currd^.op of
                    andop: left^.sc := ord((left^.sc and right^.sc) <> 0);
                    orop: left^.sc := ord((left^.sc or right^.sc) <> 0);
                    end;
                  end;
                sets:
                  begin
                  case currd^.op of
                    addop: left^.st := left^.st + right^.st;
                    subtractop: left^.st := left^.st - right^.st;
                    timesop: left^.st := left^.st * right^.st;
                    end;
                  end;
                end {case} ;
              pushpacket(left);
              freedatapacket(right);
              end;

            equalop, notequalop:
              begin
              poppacket(right);
              tmplen := right^.len;
              if right^.typ = storeref then makelocal(right);
              poppacket(left);
              if left^.typ = storeref then makelocal(left);
              sofar := true;
              rtmpd := right;
              ltmpd := left;
              i := 1;
              while sofar and (tmplen > 0) and (ltmpd <> nil) do
                begin
                if currd^.op = equalop then
                  sofar := left^.str[i] = right^.str[i]
                else sofar := left^.str[i] <> right^.str[i];
                tmplen := tmplen - bitsperunit;
                i := i + 1;
                if i > datapacketsize then 
                  begin
                  ltmpd := ltmpd^.next;
                  rtmpd := rtmpd^.next;
                  i := 1;
                  end;
                end;
              left^.sc := ord(sofar);
              rtmpd := right^.next;
              ltmpd := left^.next;
              while ltmpd <> nil do
                begin
                tmpd := ltmpd^.next;
                freedatapacket(ltmpd);
                ltmpd := tmpd;
                tmpd := rtmpd^.next;
                freedatapacket(rtmpd);
                rtmpd := tmpd;
                end;
              freedatapacket(right);
              left^.next := nil;
              pushpacket(left);
              end;

            lessthanop, lessequalop, greaterthanop, greaterequalop, inop:
              begin
              poppacket(right);
              if right^.typ = storeref then makelocal(right);
              poppacket(left);
              if left^.typ = storeref then makelocal(left);
              if (currd^.optyp = reals) or (currd^.optyp = doubles) then
                begin
                if not (targetopsys = msdos) then
                case currd^.op of
                  lessthanop:
                    left^.sc := d$dblless(left^.dr, right^.dr, currd^.fpp);
                  lessequalop:
                    left^.sc := d$dbllesseq(left^.dr, right^.dr, currd^.fpp);
                  greaterthanop:
                    left^.sc := d$dblgret(left^.dr, right^.dr, currd^.fpp);
                  greaterequalop:
                    left^.sc := d$dblgreteq(left^.dr, right^.dr, currd^.fpp);
                  end;
                end
              else if currd^.optyp = sets then
                case currd^.op of
                  lessequalop: left^.sc := ord(left^.st <= right^.st);
                  greaterequalop: left^.sc := ord(left^.st >= right^.st);
                  inop: left^.sc := ord(left^.sc in right^.st);
                  end
              else
                case currd^.op of
                  lessthanop: left^.sc := ord(left^.sc < right^.sc);
                  lessequalop: left^.sc := ord(left^.sc <= right^.sc);
                  greaterthanop: left^.sc := ord(left^.sc > right^.sc);
                  greaterequalop: left^.sc := ord(left^.sc >= right^.sc);
                  end;
              pushpacket(left);
              freedatapacket(right);
              end;

            end {case} ;

          freedatapacket(currd);
          poppacket(currd);
          end;
        end;
      if currd <> nil then pushpacket(currd);
    end; {evaluate}




  begin {xt_dataaccess}
    processerror := false;
    errmsg := noerror;
    errnum := 0;
    t := datatokenlist;
    while (t <> nil) and (t^.typ <> theend) and (errmsg = noerror) do
      begin
      case t^.typ of

        commence: 
          begin
          cleanstacks;
          if fpc_available then
            if embeddedjump and fppinuse and
              not fppregsaved then
              {save the floating point coprocessor registers}
              begin
              if embeddedjump then d$fppsav;
              fppregsaved := true;
              end;
          end;

        addressref:
        {initialize the current address and offset}
          begin
          currentstorageclass := t^.store;
          currentoffset := 0;
          case t^.store of
            dat_space, ins_space: currentaddress := t^.addr;
            watchedlocal:
              begin
              w := watchinglist;
              while (w <> nil) and (t^.addr.addr <> w^.ident) do
                w := w^.next;
              if w = nil then accesserror(watcherr, fatal);
              currentlocaldata := w^.value;
              currentaddress.addr := 1;
              currentoffset := w^.offset;
              end;
            otherwise currentaddress.addr := t^.off;
            end;
          end;

        offset: setoffset(t^.bitoff);
        {adjust the current address and offset}

        deref:
        {use the value at the current context to set the new context}
          begin
          d := getdatapacket;
          d^.typ := storeref;
          d^.signed := false;
          d^.storetyp := ptrs;
          d^.isfppreg := false;
          d^.next := nil;
          fetchvalue(size(intptr) * bitsperunit, d);
          if segmented then
            begin
            currentaddress.offset := d^.pt.addr;
            { this works only of the data segment = the stack segment }
            currentaddress.segment := stacksegment;
            end
          else
            currentaddress := d^.pt;
          if d^.pt.addr = niladdressvalue then 
            begin
            freedatapacket(d);
            accesserror(illegaladdress, fatal);
            end
          else
            begin
            currentoffset := 0;
            currentstorageclass := dat_space;
            freedatapacket(d);
            end;
          end;

        newlevel: savecontext;
        {save the current context}

        resetlevel: restorecontext;
        {restore the previous context}

        operator:
          begin
          d := getdatapacket;
          with d^ do
            begin
            typ := operators;
            op := t^.op;
            optyp := t^.optyp;
            fpp := t^.fpcoprocessor;
            next := nil;
            end;
          pushpacket(d);
          evaluate;
          end;

        atomic:
          {Put a new datapacket on the stack.  If it is a real written as a
            string, then convert to the machine representation.}
          begin
          d := getdatapacket;
          d^ := t^.packet^;
          if (d^.typ = realstring) then stringtoreal(d);
          if t^.packet^.next <> nil then
            begin
            tmpp := t^.packet^.next;
            tmpd := d;
            while tmpp <> nil do
              begin
              tmpd^.next := getdatapacket;
              tmpd := tmpd^.next;
              tmpd^ := tmpp^;
              tmpp := tmpp^.next;
              end;
            end;
          pushpacket(d);
          end;

        returnvalue:
            {Get the datapacket from the top of the stack, make it literal if
             needed, if real then convert to its string representation, then
             send it back to the interpreter. }
          begin
          poppacket(d);
          if d <> nil then
            begin
            if d^.typ = storeref then 
              begin
              if (t^.evaltyp in [singlestring, doublestring]) then
                d^.storetyp := reals;
              makelocal(d);
              end;
            if (d^.typ = singlereal) or (d^.typ = doublereal) or
               (t^.evaltyp = singlestring) or
               (t^.evaltyp = doublestring) then
              realtostring(d, t^.left, t^.right, t^.evaltyp);
            if (t^.evaltyp = scalar) then
              if not inbounds(d^.sc, t^.lowerord, 
                              t^.upperord, (not t^.extended)) then
                accesserror(subrangerr, nonfatal);
            end;
          tmpp := packet;
          tmpd := d;
          while (tmpp <> nil) and (tmpd <> nil) do
            begin
            tmpp^.dnone := tmpd^.dnone;
            tmpp^.len := tmpd^.len;
            tmpp^.typ := tmpd^.typ;
            tmpp := tmpp^.next;
            tmpd := tmpd^.next;
            freedatapacket(d);
            d := tmpd;
            end;
          end;

        evaladdr:
        {Use the value at the top of the stack to create a new address}
          begin
          poppacket(d);
          if d^.typ = storeref then makelocal(d);
          if d^.sc mod addressableunitsize <> 0 then 
            accesserror(illegaladdress, fatal)
          else
            begin
            currentstorageclass := dat_space;
            if segmented then
              begin
              currentaddress.offset := d^.pt.addr;
              { this works only if the data segment = the stack segment }
              currentaddress.segment := stacksegment;
              end
            else
              currentaddress := d^.pt;
            currentoffset := 0;
            end;
          freedatapacket(d);
          end;

        evaloffset:
            {Use the value at the top of the stack to compute an offset, 
             restore the previous context, then adjust that context by the
             computed offset.}
          begin
          poppacket(d);
          if d^.typ = storeref then makelocal(d);
          restorecontext;
          if (d^.sc < t^.lowerbnd) then 
            begin
            {return the illegal value}
            packet^.sc := d^.sc;
            freedatapacket(d);
            accesserror(indexlow, fatal);
            end
          else if (d^.sc > t^.upperbnd) then 
            begin
            {return the illegal value}
            packet^.sc := d^.sc;
            freedatapacket(d);
            accesserror(indexhigh, fatal);
            end;
          off := (d^.sc - t^.lowerbnd) * t^.bitsperelt;
          setoffset(off);
          freedatapacket(d);
          end;

        reference:
            {Establish a reference datapacket based on the current context,
             and push onto the stack.}
          begin
          d := getdatapacket;
          with d^ do
            begin
            typ := storeref;
            addr := currentaddress;
            off := currentoffset;
            store := currentstorageclass;
            len := t^.reflen;
{
            if (storetyp = sets) then
                len := ((len div bitsperunit) + 1) * bitsperunit;
}
            signed := t^.signed;
            xisstring := t^.refisstring;
            isfppreg := t^.isfppreg;
            storetyp := t^.reftyp;
            if t^.reftyp in [subranges, scalars, bools, chars] then
              begin
              upper := t^.upperlim;
              lower := t^.lowerlim;
              end;
            makereal := false;
            next := nil;
            end;
          pushpacket(d);
          end;

        makereal:
            {Convert the integer value at the top of the stack to a real.
             If the datapacket is not literal, then set flag.}
          begin
          poppacket(d);
          if d^.typ = storeref then d^.makereal := true
          else
            if not (targetopsys = msdos) then
              begin
              i := d^.sc;
              d^.sr := d$sglconv(i, fppinuse);
              d^.dr := d$dblconv(i, fppinuse);
              end;
          pushpacket(d);
          end;

        makeset:
            {Create an empty set datapacket and push onto the stack.  A set
             will be contructed by building on this datapacket.}
          begin
          d := getdatapacket;
          d^.st := [];
          d^.typ := settype;
          d^.next := nil;
          pushpacket(d);
          end;

        addtoset:
        {Add an element to the datapacket set at the top of stack}
          begin
          poppacket(d);
          lastsetitem := d^.sc;
          if t^.range then
            begin
            freedatapacket(d);
            poppacket(d); 
            end;
          poppacket(setd);
for i := d^.sc to lastsetitem do {}d_addtoset(setd^.st, i);
{}{setd^.st := setd^.st + [i];}
          pushpacket(setd);
          freedatapacket(d);
          end;

        watchtoken:
        {Create an entry to the watched variables list}
          begin
          setwatch(t^.ident, t^.wlen, t^.level);
          with packet^ do
            begin
            typ := storeref;
            len := t^.wlen;
            addr := currentaddress;
            off := currentoffset;
            store := currentstorageclass;
            makereal := false;
            end;
          end;

        updatewatch:
            {Update the saved value of a watched variable with the current
             value.}
          begin
          w := watchinglist;
          while (w <> nil) and (t^.watchid <> w^.ident) do w := w^.next;
          if w = nil then accesserror(watcherr, fatal);
          with w^ do
            begin
            fetch(currentstorageclass, watchlength, currentaddress, value);
            if processerror then accesserror(cantreadtext, fatal);
            value[1] := value[1] and firstmask;
            value[watchlength] := value[watchlength] and lastmask;
            end;
          end;

        watchreg:
          begin
          currentstorageclass := t^.regtype;
          currentaddress.addr := t^.regoffset;
          currentoffset := 0;
          end;

        returnaddr:
          begin
          d := getdatapacket;
          with d^ do
            begin
            typ := scalar;
            pt := currentaddress;
            next := nil;
            end;
          pushpacket(d);
          end;

        end {case} ;

      t := t^.next;

      end {while} ;

  1:

  end; {xt_dataaccess}



{***************************************************************************

    -------  Watching List

***************************************************************************}


procedure xt_wtchrelease {(watchid: watchidrange; var errmsg: xtmessage; var
                       errnum: integer)} ;

 {Release the watch variable with ident # "watchid" from the watching list.}

  var
    id: watchidrange;
    w, wback: watchingrecptr;


  begin {xt_wtchrelease}
    w := watchinglist;
    while (w <> nil) and (w^.ident <> watchid) do
      begin
      wback := w;
      w := w^.next;
      end;
    if w <> nil then
      begin
      if w = watchinglist then watchinglist := watchinglist^.next
      else wback^.next := w^.next;
      dispose(w);
      end;
  end; {xt_wtchrelease}


procedure xt_wtchlstrelease {(var errmsg: xtmessage; var errnum: integer)} ;

 {Clear the watching list.}

  var
    w: watchingrecptr;


  begin {xt_wtchlstrelease}
    while watchinglist <> nil do
      begin
      w := watchinglist;
      watchinglist := watchinglist^.next;
      dispose(w);
      end;
  end; {xt_wtchlstrelease}
