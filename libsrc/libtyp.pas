{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Library definitions (Pascal-2 version)

 Last modified by KRIS on 26-Nov-1990 14:09:50
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{
  Pascal Support Library common and configuration definitions.
  The definition in this module must be fully compatible with assembly
  definitions in libtyp.sa.     ====    =====
}
{$nolist}

Const

{
The following constant is the amount of stack space which must be left for
trap handling in case of error. This must allow for the data pushed on the
stack by the trap, plus working storage used by the error handler.
}
  trapmin = 84;

{
The following constant is used to initialize heap storage to an "improbable"
value.  It should be odd, to generate an address trap if used as a pointer,
but should not have a small absolute value nor be a common character string.
}

  improb = 16#EFEFEFEF;
{
The following constants define the standard input and output files passed to
the task as it is initialized.
}

  stdinp = 5; {FORTRAN reigns, even though}
  stdout = 6; {this machine has no FORTRAN compiler}

{
The following constant is the value on "nil" used by the compiler
}
  nil_const = 1;

type

  longint = 0..16#FFFFFFFF;
  shortint = 0..16#FFFF;
  byte = 0..16#FF;

{
ios parameter block definitions.
the ios parameter block is a versados defined structure used to control
i/o operations.
}
  io_block =
    packed record
      i_func: shortint; {code/command for i/o operation}
      i_opt: shortint; {options which modify operation}
      i_status: byte; {status returned for last operation}
      i_lun: byte; {logical unit number}
      i_resv: shortint; {reserved, must be zero}
      i_recnum: longint; {random record number}
      i_bufst: longint; {starting address of buffer}
      i_bufend: longint; {ending address of buffer}
      i_rlen: longint; {length of last record read}
      i_compl: longint; {completion address (zero for pascal)}
    end {io_block} ;

{
fhs parameter block
the fhs parameter block is a versados defined structured used for file
accessing (open, close) operations.
}
  fhs_block =
    packed record
      p_code: byte; {fhs instruction}
      p_cmd: byte; {fhs command}
      p_opt: shortint; {options which modify operation}
      p_status: byte; {status returned for last operation}
      p_lun: byte; {logical unit number}
      case boolean of
        false: {usual control block}
          (p_voln: packed array [1..4] of char; {volume name}
           p_usern: shortint; {user number}
           p_catnm: packed array [1..8] of char; {catalog name}
           p_filenm: packed array [1..8] of char; {file name}
           p_ext: packed array [1..2] of char; {extension}
           p_resv: shortint; {reserved}
           p_prot: shortint; {read/write protection codes}
           p_rlen: shortint; {record length}
           p_sizept: longint; {size/pointer info} );
        true: {Special control block for change logical unit}
          (pc_lub: byte; {new logical unit name}
           pc_res: byte; {reserved}
           pc_tn: longint; {task name}
           pc_ts: longint; {task session} );
    end {fhs_block} ;

{
The following status bits describe the state of the file.  The compiler must
know the location of s.eof, s.eoln, and s.def because these bits represent
the eof() and eoln() standard functions and the compiler makes direct
reference to the defined bit before any reference to eoln, eof, or f^.
These bits live in f.status in the fdb.
}
  filestatusbits = (s_def, {current component defined}
                    s_eof, {end of file detected}
                    s_eoln, {logical end of record}
                    s_text, {set=text file, clear=record file}
                    s_inp, {input operations allowed ( reset() )}
                    s_out, {output operations allowed ( rewrite() )}
                    s_newl, {new input line should be read}
                    s_int, {interactive device (for lazy i/o)}
                    s1_bit0, {not used}
                    s1_cont, {file is contiguous}
                    s1_sngl, {single character mode}
                    s1_necho, {echo is off}
                    s1_noerr, {do not trap on error}
                    s1_ran, {random access operations enabled ( seek() )}
                    s1_perm, {file may not be closed (input and output)}
                    s1_asnch {asynchronous I/O} );
  filestatus = packed set of filestatusbits;
{
pascal file descriptor block (fdb)
This data structure contains all the information the pascal support library
needs to access a file.  All active files appear in a linked list whose root
is in the global area.
Note that the fdb contains an ios parameter block as well as a fhs parameter
block.  These parameter blocks are used to perform the i/o operations for
the file.
}
  fdb_block_ptr = ^fdb_block;
  fdb_block =
    record
      f_point: longint; {pointer to data (f^ for pascal)}
      f_status: filestatus; {pascal file status (eof, eoln, etc)}
      f_next: fdb_block_ptr; {pointer to next file in list (or zero)}
      f_buff: longint; {address of file's record buffer}
      f_buffsz: shortint; {length of record buffer}
      f_fvar: longint; {back pointer to file variable}
      f_err: shortint; {pascal error code}
      f_ios: io_block; {ios parameter block}
      f_fhs: fhs_block; {fhs parameter block}
    end {fdb_block} ;

const
  f_dsize = 132; {default text file buffer size}

{
support library work area definitions.
These offsets define the support library work area.  See the init module
for more information.
}

type
  err_area_ptr = ^err_area;
  lib_area =
    packed record
      l_files: fdb_block_ptr; {pointer to linked list of active files}
      l_free: longint; {pointer to linked list of free blocks on heap}
      l_zero: longint; {must be zero}
      l_heap: longint; {current top of heap}
      l_high: longint; {high water mark for heap top}
      l_hchk: longint; {if non-zero, top of heap area}
      l_schk: longint; {if non-zero, address of stack check}
      l_rworkl: longint; {requested size of work area}
      l_aworkl: longint; {actual size of work area}
      l_globl: longint; {length of global area}
      l_globa: longint; {address of global area}
      l_erra: err_area_ptr; {address of error area}
      l_input: longint; {pointer to fdb for standard file "input"}
      l_output: longint; {pointer to fdb for standard file "output"}
      l_father: packed array [1..4] of char; {name of task which spawned this
                                              task}
      l_sessio: packed array [1..4] of char; {current session number}
      l_voln: packed array [1..4] of char; {default volume id}
      l_user: shortint; {default user number}
      l_cat: packed array [1..8] of char; {default catalog name}
      l_coml: shortint; {length of command line}
      l_luns: longint; {lun's in use by this task}
      l_taskn: packed array [1..4] of char; {current task name}
      l_uterm: packed array [1..4] of char; {user's terminal id}
      l_ruser: shortint; {real user number}
      l_break: byte; {set if break has occurred}
      l_bkwant: byte; {set if the user wants breaks}
      l_dbgbuf: longint; {buffer available for debugger}
      l_pma: longint; {Address of PMA code}
      l_term: longint; {Address of termination code}
      l_abort: shortint; {abort code (if any)}
      l_fcheck: shortint; {doing file open check (not pointer chk)}
      l_lfile: longint; {contains file ptr on error only}
    end {lib_area} ;

{
pascal-2 global area
During the life of a pascal-2 program, the global pointer (a5) always points
to the global area.  The first few words of the global area are used for
communication between the support library and the compiled code.  The rest
of the global area (larger offsets from a5) represents the global data for
the user's program.  The init module is passed the size of the global data
area when it is called by the user's code.
}

const
  g_nsize = 2;

{following structure is preceeded by the :
      g_liblev: shortint; {level of nesting in library routines}
  {which is usually accessed by the offset -2 of p2_globl}

type
  paw_ptr = ^paw;
  p2_globl =
    record
      g_spovf: longint; {address of lowest usable word on stack}
      g_own: longint; {pointer to the base of "own" section}
      g_sp: longint; {original value of stack pointer (for unwinding stack)}
      g_lib: paw_ptr; {address of support library work area}
      g_output: longint; {address of standard output file}
      g_input: longint; {address of standard input file}
    end {p2_globl} ;
  p2_globl_ptr = ^p2_globl;

{
Error communication region definitions.
This record is used to communicate between the error handler and the post 
mortem analyzer.
}

  err_area =
    packed record
      er_usp: longint; {User program stack pointer}
      er_code: shortint; {Error code}
      er_aux: longint; {Auxiliary error code}
    end {err_area} ;

{
PAW$ segment layout
This segment is allocated in the intialization routine and contains all
working storage for the Pascal program.  In particular, it contains the
error communication region, the error work space, the library work space,
the global variables, the stack, and the heap.
}

  paw =
    record
      h_lib: lib_area; {library work area}
      g_liblev: shortint; {level of nesting in library routines}
      h_glob: p2_globl; {start of global area}
    end {paw} ;

{
error definitions
These bits define the data included with an error message. For bus and
address errors, the hardware provides extra information on the stack.
There is also the possibility of some auxiliary error data, which may be
in several locations. Finally, the user can provide the entire error
code in line following the trap. These bits are all in the top byte of
the two byte error code. The actual code is in the bottom byte.
}

const
  ebit_bus = 16#100;
  ebit_user = 16#200;
  ebit_file = 16#400;
  ebit_a0 = 16#800;
  ebit_stk = 16#1000;
  ebit_code = 16#2000;

{
error codes, ranges assigned are:
0            user provides error inline
1            error message in aux data
2..15        compiled code
16..31       machine exceptions
32..127      library errors
128..255     user defined
}

  e_user = 16#200; {user provides error number inline}
  e_umsg = 16#1; {aux data points to error message}
  e_ptr = 16#2; {illegal pointer reference}
  e_case = 16#3; {no case provided for value}
  e_range = 16#4; {general range error}
  e_assran = 16#5; {assignment range error}
  e_index = 16#6; {array index error}
  e_stack = 16#7; {stack overflow}
  e_zdiv = 16#8; {divide by zero}
  e_trap = 16#10; {unexpected trap}
  e_bus = 16#111; {bus error}
  e_addr = 16#112; {address error}
  e_illeg = 16#13; {illegal instruction}
  e_priv = 16#14; {privilege violation}
  e_unexin = 16#15; {unexpected interrupt}
  e_trace = 16#16; {trace mode interrupt}
  e_spurin = 16#17; {spurious interrupt}
  e_uninin = 16#18; {uninitialized interrupt}
  e_short = 16#820; {workspace is too short}
  e_notopn = 16#21; {file not open}
  e_notout = 16#422; {file is not an output file}
  e_notinp = 16#423; {file is not an input file}
  e_writf = 16#424; {error writing output file}
  e_readf = 16#425; {error reading input file}
  e_eof = 16#426; {attempt to read past end of file}
  e_neof = 16#427; {put not at end of file}
  e_rewind = 16#428; {can't rewind file}
  e_trunc = 16#429; {can't truncate file}
  e_notran = 16#42A; {seek not permitted with text file}
  e_seek = 16#42B; {unable to seek to record}
  e_toomny = 16#2C; {too many files open}
  e_alloc = 16#42D; {can't create file}
  e_assign = 16#42E; {can't open file}
  e_attr = 16#42F; {can't read file's attributes}
  e_badnam = 16#30; {bad syntax for file name}
  e_badint = 16#31; {illegal value for integer}
  e_seek0 = 16#432; {seek to record zero}
  e_disnil = 16#33; {attempt to dispose of a nil pointer}
  e_disbad = 16#34; {dispose arg never allocated witn new}
  e_double = 16#35; {pointer already disposed}
  e_nomem = 16#36; {not enough available memory}
  e_udfile = 16#37; {reset of undefined file}
  e_none = 16#38; {don't display any error message}
  e_badswt = 16#42; {illegal i/o switch}
  e_badlun = 16#43; {invalid lun defined via /lun switch}
  e_contsz = 16#444; {no size for contiguous file}
  e_eofcnt = 16#445; {seek past end of (contiguous) file}

  {floating point coprocessor specific errors}
  ef_fline = 16#39; {f-line emulator}
  ef_prot = 16#3A; {coprocessor protocol violation}
  ef_ucond = 16#3B; {branch or set on unordered condition}
  ef_inex = 16#3C; {inexact result}
  ef_divz = 16#3D; {floating-point divide by zero}
  ef_under = 16#3E; {underflow}
  ef_operr = 16#3F; {operand error}
  ef_overf = 16#40; {overflow}
  ef_nan = 16#41; {signaling "nan"}
  {$list}
