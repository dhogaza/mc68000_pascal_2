{[b+,l+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984
  by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1
  Processor: ~processor~
  System: ~system~
  Flavor: ~flavor~
  Pascal-2 external declarations for module "commc.pas".
 Last modified by KRIS on 21-Nov-1990 15:28:25
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

unit commonc;

interface

uses config, hdr, utils, hdrc, t_c;



procedure insertnewesd;

procedure findesdid;

function getvartableptr(i: integer {index in vartable}): vartablerecptr;


implementation

  procedure insertnewesd;

{ The global variable "newesd" is to be inserted into the esdtable.
  Additionally, we must check for table overflow (abort if so).
}
    begin
      if nextesd = lastesd then compilerabort(manyexterns);

      esdtable[nextesd] := newesd;  { that's easy enough }
      nextesd := nextesd + 1;   { may actually be beyond linker's range }
    end;  { insertnewesd }


procedure findesdid;

{ Compute the esdid for the given entry.  If not found, insert it.

  Note that the esdid is not just the index into the esdtable because
  XDEF's and "standard load sections" (for instance) are not assigned esdid's.
}
  var
    esd: esdrange;

  begin
    esd := firstesd;
    esdid := firstesd;
    found := false;

    while (esd < nextesd) and not found do begin

      with esdtable[esd] do
        if esdkind = esdsupport then

          if (newesd.esdkind = esdsupport) and
             (newesd.suppno = suppno) then
            found := true
          else esdid := esdid + 1

        else if esdkind = esdexternal then

          if (newesd.esdkind = esdexternal) and
             (newesd.exproc = exproc) then
            found := true
          else esdid := esdid + 1

        else if esdkind = esddefine then
          begin
          { esddefine is odd because there is no esdid assigned, but the check
            is here so xdefs are not added to the table more than once.
          }
          if (newesd.esdkind = esddefine) and
             (newesd.vartabindex = vartabindex) then
            found := true
          end

        else if esdkind in [esduse, esdshrvar] then

          if (newesd.esdkind = esdkind) and
             (newesd.vartabindex = vartabindex) then
            found := true
          else esdid := esdid + 1

        else if esdkind in [esdcommon, esddiag] then
          if newesd.esdkind = esdkind then found := true
          else esdid := esdid + 1;

      esd := esd + 1;
      end; {while}

    if not found then
      insertnewesd;   { nextesd bumps, but not esdid }
  end; { findesdid }
 
function getvartableptr(i: integer {index in vartable}): vartablerecptr;

{ Returns a pointer to the ith vartable entry.
}

  begin {getvartableptr}
  getvartableptr := @(vartable[i div (maxvarentries + 1) + 1]^
                                [i mod (maxvarentries + 1)]);
  end {getvartableptr} ;

end.
