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
  Processor: ~processor~
  System: VERSADOS

  PMA utility routines: find source line, and print address

 Last modified by KRIS on 26-Nov-1990 14:04:13
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='PMA utility routines: find source line, and print address'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include opfig; {configuration file}

const
  dbg_flg = false; { Print debug info }
  wordsize = 2; {size of a machine word in addressing units}
  maxbitsperword = 16; {maximum number of bits in a word}
  bitsperwordminusone = 15; {as above less one}

type
  address = 0..16#FFFFFFFF;
  signed_word = - 32768..32767;
  frame_ptr = ^frame;
  frame =
    record
      old_frame: frame_ptr;
      return_pc: address;
    end;

  {bit addressing stuff}
  bitrange = 0..bitsperwordminusone; {bits in a word}
  bit = 0..1; {values of a bit}
  bitsperword = 0..maxbitsperword;
  wordbitsptr = ^wordbits;
  wordbits = packed array [bitrange] of bit;

  {The bits in a bit_address are addressed from low to high order, i.e:
   Bit order = high-order ... 3 2 1 0.
  }
  bit_address =
    packed record
      word_part: wordbitsptr;
      bit_part: bitrange;
    end;

  error_code = integer; {encoded error data}
  bodykind = (nobody, p2procbody, p2funcbody, p2mainbody, m2procbody,
              m2modbody, cprocbody); {types of bodies}

  diaghdrptr = ^diaghdr; {pointer to a diagnostic tables header record}
  diaghdr =
    packed record
      tablelen: signed_word; {length of the diagnostic table}
      codestart: address; {start address of the code}
      codelen: integer; {length of the code}
    end;

  { Pascal post-mortem analyzer.  Prints a stack walback based on data
    from the state at the time of an error.
  }


function nextbit(var where: bit_address): bitrange;
  { Get the next bit from the diagnostic stream addressed by
    "where". This routine assumes that the low-order bit (or
    the last bit of a word in the diagnostic stream) is being
    addressed when "bit_part" is zero.

    Note: This routine is dependent on the host system bit
    packing scheme. The VAX packs right to left, so it happens
    to conform to the above assumption.
  }


  begin
    with where do
      begin
      if bit_part < bitsperwordminusone then bit_part := bit_part + 1
      else
        begin
        bit_part := 0; {set to the high order bit}
        word_part := loophole(wordbitsptr, loophole(address,
                              word_part) + wordsize);
        end;
      nextbit := word_part^[bit_part];
      end;
  end; {nextbit}


function p_gascii(var where: bit_address): char;
{
  Get one eight bit character from the bit stream.
}
  external;


function p_gascii;

  var
    j: integer; {number being retrieved}
    count: integer; {bit count}
    k: integer;


  begin {getbits}
    j := 0;
    count := 8;
    while count > 0 do
      begin
      count := count - 1;
      j := (j * 2) + nextbit(where);
      end {while count > 0} ;
    p_gascii := chr(j);
  end; {p_gascii}


function number(var where: bit_address): integer;

  { Read a number in the compressed format used in the diagnostic tables.
    These are stored as a bit pattern in one of the following ranges.

    0..2          2 bits of n
    3..18         2 bits = 3, 4 bits of (n-3)
    19..48        2 bits = 3, 4 bits = 15, 5 bits of (n-18)
    otherwise     2 bits = 3, 4 bits = 15, 5 bits = 31, 16 bits of n

    cute, huh?
  }

  var
    i: integer; {target number}


  function getbits(count: bitsperword): integer;
    { Get "count" bits from the bit stream and return as a number.
    }

    var
      j: integer; {number being retrieved}


    begin {getbits}
      j := 0;
      while count > 0 do
        begin
        count := count - 1;
        j := (j * 2) + nextbit(where);
        end;
      getbits := j;
    end {getbits} ;


  begin {number}
    i := getbits(2);
    if i = 3 then
      begin
      i := i + getbits(4);
      if i = 18 then
        begin
        i := i + getbits(5);
        if i = 49 then i := getbits(16);
        end;
      end;
    number := i;
  end; {number}


function p_nxtchr(var where: bit_address): char;

  { Decode and return the next character from a Huffman-encoded bit stream.
    Alpha characters are returned as lowercase.

    The linearized Huffman decode tree is represented with terminal nodes
    tagged "true" and nonterminals "false". The tree is walked according
    to the diagnostic bit stream until a terminal node is reached and the
    character value is retrieved. Nonterminal nodes contain the index
    of the next right node, with the left node being the next sequential
    node of the table.
  }
  external;


function p_nxtchr;

  type
    treenode = 0..76; {nodes of the tree}
    code_tree = {compiler bug packed} array [treenode] of
    {packed}
        record
          case terminal: boolean of
            true: (value: ' '..'~');
            false: (next_node: treenode);
        end;

  const
    tree = code_tree((false, 64), (false, 25), (false, 18), (false, 11),
                     (false, 6), (true, 'o'), (false, 10), (false, 9),
                     (true, 'x'), (true, 'k'), (true, 'u'), (false, 17),
                     (false, 14), (true, 'f'), (false, 16), (true, 'y'),
                     (true, 'w'), (true, '_'), (false, 24), (false, 21),
                     (true, 'd'), (false, 23), (true, 'b'), (true, 'g'),
                     (true, 't'), (false, 61), (false, 60), (false, 29),
                     (true, 'c'), (false, 59), (false, 58), (false, 53),
                     (false, 34), (true, 'q'), (false, 36), (true, '1'),
                     (false, 46), (false, 45), (false, 44), (false, 41),
                     (true, '6'), (false, 43), (true, '8'), (true, '7'),
                     (true, '4'), (true, '3'), (false, 48), (true, '0'),
                     (false, 50), (true, '5'), (false, 52), (true, '$'),
                     (true, '9'), (false, 55), (true, 'z'), (false, 57),
                     (true, 'j'), (true, '2'), (true, 'v'), (true, 'h'),
                     (true, 'n'), (false, 63), (true, 'r'), (true, 'a'),
                     (false, 72), (false, 71), (false, 68), (true, 's'),
                     (false, 70), (true, 'p'), (true, 'm'), (true, ' '),
                     (false, 76), (false, 75), (true, 'i'), (true, 'l'),
                     (true, 'e'));

  var
    this_node: treenode; {current tree node}
    ch: char;


  begin
    this_node := 0;
    repeat
      if nextbit(where) = 0 then this_node := this_node + 1
      else this_node := tree[this_node].next_node;
    until tree[this_node].terminal;
    p_nxtchr := tree[this_node].value;
  end; {p_nxtchr}


procedure p_dgdata(pc: address; {address}
                   var found: boolean; {true if the table exists}
                   var this_table: diaghdrptr {address of diag table}
                   {var codestart: address {start of code block}
                   );
  external;


procedure p_wtname(var f: text; {file on which to write}
                   function getch(var x: bit_address): char;
                   { proc to supply characters }
                   where: bit_address; { start of text }
                   width: integer); { width of field }
{
  Write a space terminated string of characters from the diagnostic table.
}
  external;


procedure p_wtname;

  var
    ch: char; { character to write }
    i: integer; { character count }
    done: boolean;


  begin {p_wtname}
    i := 0;
    done := false;
    while not done do
      begin
      ch := getch(where);
      if ch = ' ' then
        begin
        while i < width do
          begin
          write(f, ' ');
          i := i + 1;
          end {while i < width} ;
        done := true;
        end {if ch = ' '}
      else
        begin
        write(f, ch);
        i := i + 1;
        end;
      end;
  end; {p_wtname}


procedure p_picoff(var code, data: integer);
  external;


procedure p_getlno(pc: address; {program counter value to find}
                   var line: integer; {resulting line number}
                   var kind: bodykind; {kind of code body}
                   var proc_name: bit_address; {start of proc name}
                   var gotname: boolean; {source name found in table}
                   var srcname: bit_address; {start of source file name}
                   var er_code: error_code {actual error code, if any} );
  external;


procedure p_getlno;

  { Find the source line number and procedure name (if any)
    for the PC address supplied.  The address is actually expected to be
    a return address, possibly just beyond the last code for the line.
  }

  var
    found: boolean; {set if diagnostic code found}
    done: boolean; {set if done}
    diagtbl: diaghdrptr; {address of the diagnostic table}
    where: bit_address; {the table data to examine}
    scanpc: address; {address of the search}
    k, i: integer;
    ch: char;
    code, data: integer; {offsets for position indpendant programs}


  begin {p_getlno}
    line := 0;
    er_code := 0;
    kind := nobody;
    gotname := false;
    p_picoff(code, data);
    p_dgdata(pc - code, found, diagtbl);

    if found then
      begin
      
      line := 1;
      scanpc := diagtbl^.codestart;
      {set "where" to the last bit of the heading}
      where.bit_part := bitsperwordminusone;
      where.word_part := loophole(wordbitsptr, loophole(integer, diagtbl) + 8);
      done := false;
      repeat
        i := number(where); {pc increment if nonzero}
        
        if i > 0 then
          begin
          scanpc := scanpc + i * 2;
          
          if scanpc >= pc - code then
            begin
            
            done := true
            end
          else
            begin
            line := line + 1;
            
            end
          end
        else
          begin
          i := number(where); {line increment if nonzero}
          
          if i > 32767 then i := i - 65536;
          if i <> 0 then
            begin
            line := line + i;
            
            end
          else
            begin
            i := number(where); {block name if nonzero}
            
            if i > 0 then
              begin
              kind := loophole(bodykind, i mod 256);
              
              proc_name := where;
              repeat {skip characters}
                if kind in [p2procbody, p2funcbody, p2mainbody] then
                  ch := p_nxtchr(where)
                else ch := p_gascii(where);
                
              until ch = ' ';
              
              end
            else
              begin { 000 is an error code or source }
              i := number(where);
              
              if i <> 0 then
                begin {real error data only if pc match}
                if i + scanpc = pc - code then
                  begin
                  er_code := number(where);
                  
                  end
                else
                  begin
                  i := number(where); {get code anyway}
                  
                  end;
                
                end
              else
                begin { 0000 is new source file name }
                srcname := where;
                repeat
                  ch := p_gascii(where);
                  
                until ch = ' ';
                line := number(where);
                
                gotname := true;
                end;
              end;
            end;
          end;
      until done;
      end;
  end; {p_getlno}


procedure p_addrprint(var f: text; {file on which to write}
                      pc: address {error location} );

  { Print the address "pc" as explicitly as possible.
  }
  external;


procedure p_addrprint;

  var
    this_line: integer; {line number}
    this_proc: bit_address; {procedure name data}
    have_file: boolean; { source name was found }
    this_file: bit_address; { source file name }
    kind: bodykind; {kind of body containing the location}
    dummycode: error_code; {dummy error code}
    code, data: integer; {offsets for position indpendant programs}


  begin {p_addrprint}
    p_getlno(pc, this_line, kind, this_proc, have_file, this_file, dummycode);
    if this_line = 0 then
      begin {no line data for this module}
      write(f, 'location ');
      write(f, pc: - 8);
      p_picoff(code, data);
      if code <> 0 then
        begin
        write(f, ' (', pc - code: - 8, ')');
        end {if code <> 0} ;
      end
    else write(f, 'line ', this_line: 1);

    if kind <> nobody then {write proc type if we know it}
      begin
      case kind of
        m2modbody: write(f, ' mod   ');
        p2mainbody: write(f, ' in program ');
        p2funcbody: write(f, ' in function ');
        otherwise write(f, ' in procedure ');
        end;

      case kind of
        p2procbody, p2funcbody, p2mainbody:
          p_wtname(f, p_nxtchr, this_proc, 1 {20} ); { write procedure name }
        otherwise
          p_wtname(f, p_gascii, this_proc, 1 {20} ); { write procedure name }
        end;

      if have_file then
        begin
        write(f, ' (');
        p_wtname(f, p_gascii, this_file, 1 {20} ); { Always eight bit text }
        write(f, ')');
        end;
      end;
  end; {p_addrprint}
