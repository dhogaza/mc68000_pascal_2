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

  Pascal-2 debugger interpreter error messages.

 Last modified by KRIS on 26-Nov-1990 13:46:21
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


procedure d$imesg { (msg: imessage) } ;


  begin
    case msg of
      notanerror: ;
      arrayexpected: write(out, 'Variable is not an array');
      badexpression: write(out, 'Invalid variable or expression');
      badindex: write(out, ''']'' or '','' must follow index expression');
      badinteger: write(out, 'Invalid integer constant');
      badreal: write(out, 'Invalid real constant');
      badsetelement: write(out, 'Set elements must be non-real scalar type');
      badsubrange: write(out, 'Variable subrange exceeded');
      booleanconstexpected: write(out, 'TRUE or FALSE expected here');
      cantassignvariable: write(out, 'Can''t assign variable');
      cantexec: write(out,  'Unable to execute command');
      cantfindreg:
        write(out, 'Can''t access stored register value from stack');
      cantreadprocess: write(out, ' Can''t read child process text');
      cantwriteprocess: write(out, ' Can''t write child process text');
      commaexpected: write(out, 'Comma expected here');
      commandnotterminated: write(out, 'Command not properly terminated');
      constassign: write(out, 'Can''t assign a value to a constant');
      elementnotcompatible:
        write(out, 'Set element is incompatible or out of range');
      expstackerr: write(out, 'Expression evaluation stack error');
      expstackoverflo: write(out, 'Expression evaluation stack underflow');
      expstackunderflo: write(out, 'Expression evaluation stack overflow');
      fieldexpected: write(out, 'Field identifier expected here');
      fpcerror: 
        begin
        writeln(out, 
          'Program compiled with floating point co-processor switch - ');
        write(out, 
          '    floating point co-processor not detected on current machine');
        end;
      illegaladdr: write(out, 'Illegal address'); 
      illegalassign: write(out, 'Invalid command or assignment');
      illegalident: write(out, 'Invalid identifier');
      illegalscalar: write(out, 'Illegal value for scalar variable');
      inactiveprocess: write(out, 'Process is inactive');
      indexnotcompatible:
        write(out, 'Index is incompatible or out of range');
      lparenexpected: write(out, 'Left paren expected here');
      linetoolong: write(out, 'Line too long');
      macronameexpected: write(out, 'Macro name expected');
      nilpointer: write(out, 'Attempted reference through a NIL pointer');
      nocanwrite: write(out, 'Can''t write variables of this type');
      noclosingquote: write(out, 'No closing quote for literal string');
      noexecfilemsg: write(out,  'Lookup failed on executable file'); 
      nomaindebug: write(out, 'Main module not compiled with debug switch');
      nomainsymbols:
        write(out, 'Can''t locate symbol table for main program module');
      nomodname: write(out, 'Module name expected here');
      noprocname: write(out, 'Procedure name expected here');
      norealexpr: write(out, 'Real expressions not allowed');
      norefonregstore: write(out, 'ref() invalid on register stored variable');
      nostackframes: write(out, 'Can''t identify stack frames');
      nosuchbreakpoint: write(out, 'No breakpoint is set at this location');
      nosuchframe: write(out, 'No such stack frame');
      notcompatible: write(out, 'Assignment operands of incompatible types');
      notacommand: write(out, 'Invalid debugger command');
      notafield: write(out, 'This identifier is not a field name');
      notavariable: write(out, 'This identifier is not a variable');
      notdebugcompiled: write(out, 'Program not compiled with debug switch');
      notoption: write(out, 'This option not available');
      numberexpected: write(out, 'Number expected here');
      ipanicmsg: write(out, 'Unexpected condition');
      pointerexpected: write(out, 'Pointer expected here');
      processterminated: write(out, 'Application process has terminated');
      processtrace: write(out, 'Error tracing process');
      procnotfound: write(out, 'Can''t find this procedure');
      rparenexpected: write(out, 'Right parenthesis expected here');
      recordexpected: write(out, 'Variable of type record expected here');
      stmtnoexpected: write(out, 'Statement number expected here');
      stmtnotfound: write(out, 'No such statement in this procedure');
      stringexpected: write(out, 'Character string expected here');
      subscripttoohigh: write(out, 'Array subscript too large');
      subscripttoolow: write(out, 'Array subscript too small');
      toomanywatches: write(out, 'Too many watched variables');
      variableexpected: write(out, 'Variable name expected here');
      watchlimitreached:
        writeln(out, 'Size limit reached for watched variable');
      watchnotfound: write(out, 'Unable to find watched variable data');
      otherwise write(out, 'Unknown message (', ord(msg): 1, ')')
      end;
  end;


procedure d$iwarn { (msg: imessage) } ;

 {Publish a warning message.}


  begin
    writeln(out);
    write(out, 'Warning: ');
    d$imesg(msg);
    writeln(out);
  end;


procedure choke { (info: PanicInfo) } ;

 {Used when a consistency error is encountered.  Execution is then halted.}

  var
    i: integer;


  begin
    d$imesg(ipanicmsg);
    write(out, ' (');
    for i := low to high do write(out, info[i]);
    writeln(out, ')');
    d$finalend;
  end;
