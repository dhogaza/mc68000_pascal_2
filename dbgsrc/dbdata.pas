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

  Pascal-2 debugger common data emission routines.

 Last modified by KRIS on 26-Nov-1990 13:47:50
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


procedure setregresidence(var item: dataitem; storetyp: data_access_types;
                             regidx: regindex; lowframe: stackpointer;
                             highframe: stackpointer; var lost: boolean);
  forward;

procedure createreferencetoken(var item:dataitem;
                               usebaseform, usescalar: boolean);
  forward;

function d$getvalue {(var item: dataitem; gettyp: datatypes; util1, util2:
                     integer; var packet: datapacketptr; var err: imessage):
                     boolean} ;

  var
    form: debugrecord;
    success: boolean;
    location: commandlineindex;


  begin {d$getvalue}
    d$getvalue := false;
    if item.store <> local then
      createreferencetoken(item, false, false);
    adddatatoken(item, returnvalue);
    with item.dtok^ do
      begin
      evaltyp := gettyp;
      case gettyp of
        scalar:
          begin
          lowerord := util1;
          upperord := util2;
          extended := d$extended(item);
          end;
        singlestring, doublestring:
          begin
          left := util1;
          right := util2;
          end;
        settype, stringtype, nonetype: returnlen := item.bitlen;
        end;
      end;
    adddatatoken(item, theend);
    d$dataaccess(item.datatokenlist, packet, success, err, location);
    cleardatatokens(item);
    d$getvalue := success;
  end; {d$getvalue}

{************************************************************************

     ------ Scalar Type Limit Calculation

************************************************************************}


function d$bound {(mdl: mdl_pointer; f: stackpointer; var form: debugrecord;
                  boundoffset: integer): integer} ;

 { Pick up an index bound off of the stack (conformant array bound). }

  var
    rec: debugrecord;
    packet: datapacketptr;
    err: imessage;
    success: boolean;
    bounditem: dataitem;
    resultform: debugrecord;
    location: commandlineindex;


  begin {d$bound}
    d$getobject(mdl, form.lowbound + boundoffset, rec);
    bounditem.mdl := mdl;
    bounditem.index := rec.vartype;
    bounditem.datatokenlist := nil;
    bounditem.dtok := nil;
    bounditem.store := dat_space;
    d$getform(bounditem, resultform);
    if resultform.bitaddress then bounditem.bitlen := resultform.size
    else bounditem.bitlen := resultform.size * bitsperunit;
    adddatatoken(bounditem, newlevel);
    adddatatoken(bounditem, addressref);
    with bounditem.dtok^ do
      begin
      store := dat_space;
      addr.addr := f^.database + rec.offset;
      end;
    packet := getdatapacket;
    packet^.next := nil;
    if d$getvalue(bounditem, scalar, lower(resultform), upper(resultform),
                  packet, err) then
      d$bound := packet^.sc;

    freedatapacket(packet);
    { We need to reset the context level in the target. }
    adddatatoken(bounditem, resetlevel);
    adddatatoken(bounditem, theend);
    d$dataaccess(bounditem.datatokenlist, packet, success, err, location);
    cleardatatokens(bounditem);
    if not success then
      begin
      if commands = nil then d$imesg(err)
      else
        begin
        commands^.idx := location;
        d$cmderror(err);
        end;
      end;
  end; {d$bound}



{************************************************************************

     ------ Data Item Output

     This routine writes the value of the variable or constant
     described by RSLT.  The parameters L and R are formatting
     information describing how to display the information.  For
     simple variables of type INTEGER, REAL, etc., a single value
     is displayed.  For more complicated structures this routine
     becomes recursive to print all the values in an array or
     record.  For arrays, the value of each component is printed
     followed by a space.  For records, the field name is printed
     followed by the data.  This routine should be able to display
     any complicated data structure (as long as you don't care much
     about the formatting).

************************************************************************}


procedure d$wrtdata { (resultitem: dataitem; l, r: integer; var newline:
                     boolean)} ;

  label
    1;

  const
    indentstep = 2;
    maxcolumn = 72;

  var
    success: boolean;
    err: imessage;
    packet: datapacketptr;
    location: integer;



  procedure sizedata(resultindex: symbolindex;
                     var width: integer);

    { Compute an upper bound for the printed width of a data type,
      to be used in deciding how to format compound data structures. }

    var
      resultform, memberform: debugrecord;
      memberwidth: integer;
      index, last: symbolindex;
      lastoffset: integer;


    begin { SizeData }
      d$getobject(resultitem.mdl, resultindex, resultform);
      case resultform.typ of
        ints, subranges: width := 10;
        scalars:
        { HUH: width calculation is excessively conservative }
          width := msymbolname;
        bools: width := 5;
        chars: width := 1;
        reals, doubles: width := 10;
        ptrs: width := 10;
        sets:
          begin
          d$getobject(resultitem.mdl, resultform.basetype, memberform);
          sizedata(resultform.basetype, memberwidth);
          width := (upper(memberform) + 1) * memberwidth;
          end;
        arrays:
          begin
          d$getobject(resultitem.mdl, resultform.indextype, memberform);
          if d$isstring(resultitem.mdl, resultform) then
            width := upper(memberform) + 2
          else
            begin
            sizedata(resultform.elementtype, memberwidth);
            width := (upper(memberform) + 1) * (memberwidth + 2);
            end;
          end;
        strings:
          begin
          width := sizeof(resultform) + 1;
          end;
        fields:
          begin
          width := 0;
          index := resultform.firstfield;
          last := resultform.lastfield + 1;
          lastoffset := - 1;
          while index <= last do
            begin
            d$getobject(resultitem.mdl, index, memberform);
            if memberform.kind = symboldesc then
              if memberform.namekind = fieldname then
                if memberform.name = resultform.fieldid then
                  if memberform.offset <= lastoffset then index := last
                  else
                    begin
                    lastoffset := memberform.offset;
                    sizedata(memberform.vartype, memberwidth);
                    { HUH: width calculation is excessively conservative }
                    width := width + memberwidth + 16 + 2;
                    end;
            index := index + 1;
            end;
          end;
        otherwise width := 0;
        end;
    end; { SizeData }


  procedure writeindent(indent: integer);


    begin
      writeln(out);
      if indent > 0 then write(out, ' ': indent);
    end;


  procedure writedatalevel(var resultitem: dataitem;
                           indent: integer;
                           split: boolean);

    const
      ourname = 'writedatalevel';
    type
      errorseverity = (fatal, nonfatal);

    var
      resultform, form: debugrecord;
      i: symbolindex;
      j: integer;
      err: imessage;


    procedure writeerror(err: imessage;
                         packet: datapacketptr;
                         item: dataitem;
                         severity: errorseverity);


      begin {writeerror}
        if severity = fatal then
          begin
          d$cmderror(err);
          if err in [subscripttoohigh, subscripttoolow] then
            begin
            write(out, 'ord of index = ', packet^.sc);
            end;
          if (packet <> nil) then freedatapacket(packet);
          cleardatatokens(item);
          goto 1;
          end
        else d$iwarn(err);
      end; {writeerror}


    procedure writescalar(val: integer;
                          var resultform: debugrecord);

      var
        symbol: debugrecord;


      begin {writescalar}
        if (val < 0) or (val > resultform.lastord) then
          writeln(out, 'Illegal value for scalar (ord=', val: 1, ')')
        else
          begin
          d$getobject(resultitem.mdl, resultform.firstscalar + 2 * val,
                      symbol);
          d$wrtsymbolname(symbol.identchars, 1);
          end;
      end; {writescalar}


    procedure writeitem(val: integer;
                        var resultform: debugrecord);

     {  Write a simple atomic in the appropriate format. }

      var
        cons: scalarkludgerecord;


      begin {writeitem}
        cons := loophole(scalarkludgerecord, val);
        case resultform.typ of
          ints, subranges: write(out, cons.i: l);
          bools: write(out, cons.b);
          chars: write(out, cons.c: l);
          scalars: writescalar(val, resultform);
          end;
      end; {writeitem}


    procedure wrset;

      var
        memberitem: dataitem;
        memberform: debugrecord;
        last, idx, low: integer;
        first, hold: boolean;
        success: boolean;
        err: imessage;
        packet: datapacketptr;


      procedure wrrange(low, high: integer);


        begin
          if first then first := false
          else write(out, ',');
          writeitem(low, memberform);
          if low < high then
            begin
            if (low + 1) < high then write(out, '..')
            else write(out, ',');
            writeitem(high, memberform);
            end;
        end;


      begin { wrset }
        resultitem.bitlen := setsize * bitsperunit;
        packet := getdatapacket;
        packet^.next := nil;
        if not d$getvalue(resultitem, settype, 0, 0, packet, err) then
          writeerror(err, packet, resultitem, fatal)
        else
          begin
          memberitem.mdl := resultitem.mdl;
          memberitem.frame := nil;
          memberitem.index := resultform.basetype;
          d$getform(memberitem, memberform);

          write(out, '[');
          last := d$min(upper(memberform), setsize * bitsperunit - 1);
          first := true;
          hold := false;
          for idx := 0 to last do
            begin
            if idx in packet^.st then
              begin
              if not hold then
                begin
                hold := true;
                low := idx
                end;
              end
            else
              begin
              if hold then
                begin
                wrrange(low, idx - 1);
                hold := false
                end;
              end;
            end;
          if hold then wrrange(low, last);
          write(out, ']');
          freedatapacket(packet);
          if err <> notanerror then
            writeerror(err, nil, resultitem, nonfatal);
          end;
      end; { wrset }


    procedure wrarray;

      var
        elementitem: dataitem;
        indexform: debugrecord;
        idx, first, last, width: integer;
        str: boolean;
        rem, j, startindex: integer;
        numpacks, numelements: integer;
        p: datapacketptr;
        err: imessage;


      begin { wrarray }
        if split then
          begin
          sizedata(resultitem.index, width);
          if (indent + width) <= maxcolumn then split := false;
          end;
        last := d$upperbound(resultitem.mdl, resultitem.frame, resultform);
        first := d$lowerbound(resultitem.mdl, resultitem.frame, resultform);
        if errorhappened then goto 1;
        str := (resultform.typ = strings) or d$isstring(resultitem.mdl,
                                                        resultform);
        if str then
          begin
          write(out, '''');
          numelements := last - first + 1;
          resultitem.bitlen := numelements * bitsperunit;
          packet := getdatapacket;
          p := packet;
          for j := 1 to numelements div datapacketsize do
            begin
            p^.next := getdatapacket;
            p := p^.next;
            end;
          p^.next := nil;
          if not d$getvalue(resultitem, stringtype, 0, 0, packet, err) then
            writeerror(err, packet, resultitem, fatal)
          else
            begin
            if resultform.typ = strings then
              begin
              if ord(packet^.str[1]) < numelements then
                numelements := ord(packet^.str[1]) + 1
              else numelements := numelements + 1;
              startindex := 2;
              end
            else startindex := 1;
            while numelements > datapacketsize do
              begin
              for j := startindex to datapacketsize do
                write(out, packet^.str[j]);
              packet := packet^.next;
              numelements := numelements - datapacketsize;
              startindex := 1;
              end;
            for j := startindex to numelements do write(out, packet^.str[j]);
            write(out, '''');
            if err <> notanerror then
              writeerror(err, packet, resultitem, nonfatal);
            end;
          while packet <> nil do
            begin
            p := packet;
            packet := packet^.next;
            freedatapacket(p);
            end;
          end
        else
          begin
          if split then write(out, '( ')
          else write(out, '(');
          idx := first;
          while (idx <= last) and not errorhappened do
            begin
            elementitem := resultitem;
            elementitem.index := resultform.elementtype;
            if resultform.packedflag then
              elementitem.bitlen := resultform.elementsize
            else elementitem.bitlen := resultform.elementsize * bitsperunit;
            adddatatoken(elementitem, newlevel);
            writedatalevel(elementitem, indent + indentstep, split);
            if not errorhappened then
              begin
              resultitem.datatokenlist := elementitem.datatokenlist;
              resultitem.dtok := elementitem.dtok;
              adddatatoken(resultitem, resetlevel);
              if not str and (idx <> last) then
                begin
                write(out, ',');
                if split then writeindent(indent)
                else write(out, ' ');
                end;
              if idx <> last then
                begin
                adddatatoken(resultitem, offset);
                with resultitem.dtok^ do
                  begin
                  if resultform.packedflag then
                    bitoff := resultform.elementsize
                  else bitoff := resultform.elementsize * bitsperunit;
                  end;
                end;
              end;
            idx := idx + 1;
            end;
          write(out, ')');
          newline := true;
          end;
      end; { wrarray }


    procedure wrrecord;

      var
        fielditem: dataitem;
        rec: debugrecord;
        index, last: symbolindex;
        lastoffset, width: integer;
        first: boolean;
        stop: boolean;
        sym: symbolname;
        nameid: integer;


      begin { wrrecord }
        newline := true;
        if split then
          begin
          sizedata(resultitem.index, width);
          if (indent + width) <= maxcolumn then split := false;
          end;
        nameid := resultform.fieldid;
        index := resultform.firstfield;
        last := resultform.lastfield + 1;
        lastoffset := - 1;
        if split then write(out, '( ')
        else write(out, '(');
        first := true;
        stop := false;
        while not stop and not errorhappened do
          begin
          d$getobject(resultitem.mdl, index, rec);
          if index > last then stop := true
          else if rec.kind = identdesc then sym := rec.identchars
          else if rec.kind = symboldesc then
            if rec.namekind = fieldname then
              if rec.name = resultform.fieldid then
              { HUH: only print first variant? }
                if rec.offset <= lastoffset then stop := true
                else
                  begin
                  if first then first := false
                  else
                    begin
                    write(out, ',');
                    if split then writeindent(indent)
                    else write(out, ' ');
                    end;
                  d$wrtsymbolname(sym, 1);
                  write(out, ': ');
                  if (rec.offset <> 0) then
                    begin
                    adddatatoken(resultitem, offset);
                    with resultitem.dtok^ do
                      begin
                      if resultform.packedflag then
                        bitoff := rec.offset - lastoffset
                      else bitoff := (rec.offset - lastoffset) * bitsperunit;
                      end;
                    end;
                  lastoffset := rec.offset;
                  fielditem := resultitem;
                  fielditem.index := rec.vartype;
                  if resultform.packedflag then
                    fielditem.bitlen := rec.length
                  else fielditem.bitlen := rec.length * bitsperunit;
                  adddatatoken(fielditem, newlevel);
                  writedatalevel(fielditem, indent + indentstep, split);
                  if not errorhappened then
                    begin
                    resultitem.datatokenlist := fielditem.datatokenlist;
                    resultitem.dtok := fielditem.dtok;
                    adddatatoken(resultitem, resetlevel);
                    end;
                  end;
          index := index + 1;
          end;
        write(out, ')');
      end; { wrrecord }


    begin {writedatalevel}
      d$getbaseform(resultitem, resultform, i);
      if interrupted then goto 1;
      case resultform.typ of
        ints, bools, chars, scalars:
          begin
          packet := getdatapacket;
          packet^.next := nil;
          d$getform(resultitem, form);
          if not d$getvalue(resultitem, scalar, lower(form), upper(form),
                            packet, err) then
            writeerror(err, packet, resultitem, fatal)
          else
            begin
            if err <> notanerror then
              begin
              writeerror(err, packet, resultitem, nonfatal);
              write(out, 'ordinal value = ');
              write(out, packet^.sc);
              end
            else
              writeitem(packet^.sc, resultform);
            end;
          freedatapacket(packet);
          end;
        reals, doubles:
          begin
          packet := getdatapacket;
          packet^.next := nil;
          if (resultform.typ = doubles) or (resultitem.mdl = nil) or 
              (resultitem.mdl^.realsize = 8) then
            begin
            resultitem.bitlen := doublerealsize * bitsperunit;
            if not d$getvalue(resultitem, doublestring, l, r, packet,
               err) then
              writeerror(err, packet, resultitem, fatal);

            end
          else
            begin
            resultitem.bitlen := singlerealsize * bitsperunit;
            if not d$getvalue(resultitem, singlestring, l, r, packet,
               err) then
              writeerror(err, packet, resultitem, fatal);
            end;
          j := 1;
          {reals are returned as a string}
          while (packet^.str[j] = ' ') do
            begin
            write(out, packet^.str[j]);
            j := j + 1;
            end;
          while (packet^.str[j] <> ' ') do
            begin
            write(out, packet^.str[j]);
            j := j + 1;
            end;
          freedatapacket(packet);
          if err <> notanerror then
            writeerror(err, nil, resultitem, nonfatal);
          end;
        ptrs:
          begin
          packet := getdatapacket;
          packet^.next := nil;
          if not d$getvalue(resultitem, scalar, lower(form), upper(form),
                            packet, err) then
            writeerror(err, packet, resultitem, fatal)
          else d$wrtaddress(packet^.pt);
          freedatapacket(packet);
          if err <> notanerror then
            writeerror(err, nil, resultitem, nonfatal);
          end;
        sets: wrset;
        strings, arrays, conformantarrays: wrarray;
        fields: wrrecord;
        files: d$cmderror(nocanwrite);
        otherwise choke(ourname);
        end {case} ;
    end; {writedatalevel}


  begin {d$wrtdata}
    newline := false;
    writedatalevel(resultitem, 2, true);
  1:
  end; {d$wrtdata}


procedure d$wrtwtch {(w: watchpointer)} ;

  {Write out the old and new values for a watched variable.
  }

  var
    packet: datapacketptr;
    success: boolean;
    err: imessage;
    location: commandlineindex;
    dummy: boolean;


  begin {d$wrtwtch}
    writeln(out);
    write(out, 'Old value: ');
    w^.what.datatokenlist := nil;
    w^.what.dtok := nil;
    adddatatoken(w^.what, commence);
    adddatatoken(w^.what, addressref);
    with w^.what.dtok^ do
      begin
      store := watchedlocal;
      addr.addr := w^.ident;
      end;
    adddatatoken(w^.what, offset);
    w^.what.dtok^.bitoff := w^.where.off;
    adddatatoken(w^.what, theend);
    packet := nil;
    d$dataaccess(w^.what.datatokenlist, packet, success, err, location);
    if not success then d$imesg(err);
    cleardatatokens(w^.what);
    d$wrtdata(w^.what, 1, 7, dummy);
    writeln(out);
    write(out, 'New value: ');
    adddatatoken(w^.what, commence);
    adddatatoken(w^.what, addressref);
    with w^.what.dtok^ do
      begin
      store := w^.where.store;
      addr := w^.where.addr;
      end;
    adddatatoken(w^.what, offset);
    with w^.what.dtok^ do
      begin
      bitoff := w^.where.off;
      end;
    adddatatoken(w^.what, updatewatch);
    with w^.what.dtok^ do
      begin
      watchid := w^.ident;
      end;
    adddatatoken(w^.what, theend);
    packet := nil;
    d$dataaccess(w^.what.datatokenlist, packet, success, err, location);
    if not success then d$imesg(err);
    cleardatatokens(w^.what);
    d$wrtdata(w^.what, 1, 7, dummy);
    writeln(out);
    writeln(out);
  end; {d$wrtwtch}



procedure d$resetwatchlist;

  {Reinitialize the template (old) values of watched variables.  This is
   done whenever a new process has been spawned in separate process 
   models.
  }

  var
    w: watchpointer;
    packet: datapacketptr;
    success: boolean;
    err: imessage;
    location: commandlineindex;
    watchitem: dataitem;


  begin {d$resetwatchlist}
    if separateprocess then
      begin
      w := watchlist;
      watchitem.datatokenlist := nil;
      watchitem.dtok := nil;
      while w <> nil do
        begin
        adddatatoken(watchitem, commence);
        adddatatoken(watchitem, addressref);
        with watchitem.dtok^ do
          begin
          store := w^.where.store;
          addr := w^.where.addr;
          end;
        adddatatoken(watchitem, offset);
        with watchitem.dtok^ do
          begin
          bitoff := w^.where.off;
          end;
        adddatatoken(watchitem, updatewatch);
        with watchitem.dtok^ do
          begin
          watchid := w^.ident;
          end;
        w := w^.next;
        end;
      adddatatoken(watchitem, theend);
      packet := nil;
      d$dataaccess(watchitem.datatokenlist, packet, success, err, location);
      if not success then d$imesg(err);
      cleardatatokens(watchitem);
      end;
  end; {d$resetwatchlist}
