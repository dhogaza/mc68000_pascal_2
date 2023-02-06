{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Lexical Scanner Global Declarations

 Last modified by KRIS on 21-Nov-1990 15:17:47
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

const

  { Machine Dependent parameters for scanner }

  maxscanswitch = 40; { max number of recognized embedded switches }
  maxscanswitchlen = 14; { max length is NoPointerCheck }

  { Language specific parameters for scanner }

  tabspace = 8; { The number of spaces a tab input becomes }

  reservedcount = 42; {Number of reserved words}
  minreslen = 2; {Length of shortest reserved word}
  maxreslen = 9; {Length of longest reserved word}
  maxreslen1 = 10; {maxreslen + 1}

 { special characters - ignore nul,rubout and convert tabch, formfeed to ' ' }

  nul = 0;
  rubout = 127;
  tabch = 9;
  formfeed = 12;
  lowercase = 32; {difference between ord of upper case and lower case char}

  { real number conversion parameters -- passed into p2ReadReal }

  DECformat = 0; { format used by PDP-11 and VAX }
  IEEEformat = 256; { format used by M68000 and N16000 }
  INTELformat = 512; { same as IEEE, except result gets stored backwards }
  IBMformat = 768; { hexadecimal format used by 360/370/31xx/43xx, etc. }

  SinglePrecision = 0;
  DoublePrecision = 16;
  QuadPrecision = 32; { not currently implemented }

  inputbufsize = 20; {length of input line if using fixed arrays}

type

  {Dummy declaration for tempfiletwo, which isn't used in this pass}
  tempfiletwodata = packed array [0..diskbufsize] of hostfilebyte;
  tempfiletwotype = file of tempfiletwodata;

  {$include 'sahdr'} {scan-analys interface declarations}

type
  lineindex = 0..linelen; {index into input line}
  linetype = packed array [1..linelen] of char;
  lineptr = ^linetype;

  halfrealsize = 0..65535; {subrange which is half size of single real}

  alfa = packed array [1..10] of char; {to hold directives}

  reswordtype = packed array [1..maxreslen] of char; {reserved word spelling}
  reservedindex = 1..reservedcount; {index for reserved words}
  identifierrange = 0..1000; {some arbitrary range}

  {internal scanner switches}
  internalswitch = (codesectsw, modulesw, identsw, xshortsectsw, xversionsw,
                    xsectionsw);
  scanswitchindex = 0..maxscanswitch; {switchtable switch}
  switchvalue = - maxswitchvalue..maxswitchvalue; {bumpvalue}
  switchname = packed array [1..maxscanswitchlen] of char; {name of switch}

  hashtablerecord = record
                      pos: stringindex; {string index of id text}
                      len: columnindex; {length of id text}
                      key: hashindex {key assigned to this id}
                    end;

 { declarations for kluged type to allow writing to environment file }
  envirtype = (en_scan_var_block, en_string_block, en_hash_block,
               en_disk_block, en_switch_block);

  { file begins with scanner globals (en_scan_var_block), followed by
    compressed hash table (ehashblock, where ehashblock.pos < 0 means
    that - ehashblock.pos zero entries were compressed from the file),
    and finally the stringtable.
  }

  switchblock = packed record
                  s: switch;
                  v: switchvalue;
                end;

  scanswitchentry = record
                      n: switchname; {name of switch}
                      case internal: boolean of
                        true: (is: internalswitch; {internal switch label} );
                        false:
                          (s: switch; {switch for which this is the entry}
                           v: switchvalue;
                           {amount to increment counter} );
                    end;

  hashtableblock = packed record
                     pos: -hashtablesize .. stringtablesize;
                     len: columnindex; {length of id text}
                     key: hashindex {key assigned to this id}
                   end;

  envirrecord =
    record
      case envirtype of
        en_scan_var_block:
         (elastswitch: switchindex;
          estringfilecount: integer;
          enextstringfile: 0..diskbufsize;
          ecurstringblock: shortint;
          estringtabletop: stringindex;
          einsertions: integer;
          ecodesect_string: stringindex;
          ecodesect_strlength: columnindex;
          emodule_string: stringindex;
          emodule_strlength: columnindex;
          eident_string: stringindex;
          eident_strlength: columnindex;
          ecodesection: shortint;
          eshortsection: boolean;
          eobjversion: shortint;
          edatasection: shortint;
          eownsect_string: stringindex;
          eownsect_strlength: columnindex;
          eswitcheverplus: switcheverplusarray;
          eversion: packed array[1..40] of char);
        en_switch_block:
         (eswitches: array [0..switchesperblock] of switchblock);
        en_string_block: (estringblock: diskblock);
        en_hash_block:
         (ehashblock: array [0..hashtableentriesperblock] of hashtableblock);
        en_disk_block: (ediskblock: diskblock);
    end;

var
  nextch: char; {next character to be scanned}
  ch: char; {current character being scanned}
  unconvertedch: char; {current character in raw (not case converted) state}
  currentline: lineptr; {current input line}
  currentbuf: packed array [1..inputbufsize] of char; {input buffer}
  linepos: lineindex; {current position in input line}
  linesize: integer; {length of current input line}
  charcount: columnindex; {effective position of nextch in this line}
  chpos: columnindex; {effective position of "ch" in this line}
  oldchpos: columnindex; {effective position of last "ch"}
  new_filepos: integer; {for tracking fileposition}
  current_filepos: integer; {new_filepos, delayed by one character}
  lasttokenline: integer; {line of last token read}
  lastbaseline: integer; {baseline for last file read}
  endofline: boolean; {effective eoln with nextch}
  endofinput: boolean; {effective overall eof}
  convertingcase: boolean; {true if uppercase wanted (except in strings)}
  skippingblanks: boolean; {currently skipping blanks}
  saveinput: array [1..sourcedepth] of
      record
        savech: char; {nextch for pushed source levels}
        saveendofline: boolean;
        saveline: lineptr;
        savelen: lineindex;
        savebuf: packed array [1..inputbufsize] of char;
        savepos: lineindex;
        savefileindex: integer;
        savefilename_length: filenameindex;
      end;
  baseline: array [1..sourcedepth] of integer; {starting line for current
                                                file}

  { stringbuf[curstringbuf] is used to buffer the current quoted string
    (nexttoken), while stringbuf[not curstringbuf] is the buffer for the
    previous (token) string, if any. }

  stringbuf: array [boolean] of packed array [0..linelen] of char;

  tokenbufindex: 0..diskbufsize; {next available space in token file block}

  { hashtable, includes pointer to loc in string table }
  hashtable: array [hashindex] of hashtablerecord;

  scanswitchtable: array [scanswitchindex] of {table of embedded switch names}
                          scanswitchentry;

  {Table of reserved words, by length, and pointers into that table}

  {pointers to reswords}
  reslentable: array [minreslen..maxreslen1] of reservedindex;
  reswords: array [reservedindex] of reswordtype; {text of reswords}
  reswordtokens: array [reservedindex] of tokentype; {tokens for reswords}

  tokentable: array [')'..'^'] of tokentype; {tokens for single char tokens}

  mapchars: packed array ['A'..'Z'] of char; {lower to upper conversion table}
  incomment: boolean; {used to detect an unfinished comment}
  inliteralstring: boolean; {used to detect an unfinished literal string}
  first_real_seen: boolean; {used to detect error when $double occurs after
                             first real constant.}
  first_token_seen: boolean; {used to detect error when $case occurs after
                              the first token.}
  current_fileindex: integer; { pointer to current filename }

  {switch buffers used to delay effect of switches processed while reading
   nexttoken until it actually becomes thistoken (scanalys only).
  }

  nextswitchread: boolean; {set true when first switch within comment is found}
  nextswitchcounters: switchcounterarray; {buffer for switchcounters}
  nextswitcheverplus: switcheverplusarray; {buffer for switcheverplus}

procedure p_rdsfst(var f: text;
                   var p: lineptr;
                   var l2: integer);
  external;


procedure p_expfnm(var f: text; {open text file whose name we want}
                   var fn  : packed array of char; {file name to be updated}
                   var len : shortint {length to be updated} );
  external;
