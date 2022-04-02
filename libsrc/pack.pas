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

  Run-time routines for pack and unpack

 Last modified by KRIS on 26-Nov-1990 14:06:45
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

program pkunpk;

{
    This pascal procedure implements the PACK procedure.
}

 {$NoMain,nowalkback}
 {$NoPointer,NoRange,Noindex}
 {$section=8,version=0045,ident='Run-time routines for pack and unpack'}

  type
    all = 0..65535;

    kludge =
      record
        case integer of
          1: (b1: packed array [all] of 0..1);
          2: (b2: packed array [all] of 0..3);
          4: (b4: packed array [all] of 0..15);
          8: (b8: packed array [all] of 0..255);
          16: (b16: array [all] of all);
          32: (b32: array [all] of 0..16#FFFFFFFF);
          - 1: (s1: packed array [all] of - 1..0);
          - 2: (s2: packed array [all] of - 3..2);
          - 4: (s4: packed array [all] of - 8..7);
          - 8: (s8: packed array [all] of - 128..127);
          - 16: (s16: array [all] of - 32768..32767);
          - 32: (s32: array [all] of integer);
      end;


  procedure p_entlib;
    external;


  procedure p_exitlb;
    external;

{
 error message routines
}


  procedure p_pklow;
    external;


  procedure p_pkhi;
    external;


  procedure p_upklow;
    external;


  procedure p_upkhi;
    external;


  procedure p_pack(var a: kludge;
                   lowa, higha, sizea_bytes: integer;
                   offset: integer;
                   var z: kludge;
                   lowz, highz, sizez_bits: integer;
                   zpacked: boolean;
                   unsigned: boolean);
    external;


  procedure p_pack;

    var
      i, j, k, cnt: all;


    begin
      p_entlib;
      if (not zpacked) or (zpacked and (sizez_bits > 8)) then sizez_bits := 8;
      if offset < lowa then p_pklow;
      if (offset > higha) or ((offset + highz - lowz) > higha) then p_pkhi;
      k := offset - lowa;
      cnt := highz - lowz;
      case sizez_bits of
        1:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4: z.b1[i] := a.b32[k];
              2: z.b1[i] := a.b16[k];
              otherwise z.b1[i] := a.b8[k];
              end;
            k := k + 1;
            end; { for }
        2:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4: z.b2[i] := a.b32[k];
              2: z.b2[i] := a.b16[k];
              otherwise z.b2[i] := a.b8[k];
              end;
            k := k + 1;
            end; { for }
        4:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4: z.b4[i] := a.b32[k];
              2: z.b4[i] := a.b16[k];
              otherwise z.b4[i] := a.b8[k];
              end;
            k := k + 1;
            end; { for }
        8:
          begin
          if (not zpacked) or (sizea_bytes = 1) then
            begin
            k := k * sizea_bytes;
            for i := 0 to (cnt + 1) * sizea_bytes - 1 do
              begin
              z.b8[i] := a.b8[k];
              k := k + 1;
              end;
            end
          else if sizea_bytes = 2 then
            begin
            for i := 0 to cnt do
              begin
              z.b8[i] := a.b16[k];
              k := k + 1;
              end
            end
          else if sizea_bytes = 4 then
            for i := 0 to cnt do
              begin
              z.b8[i] := a.b32[k];
              k := k + 1;
              end
          end; { 8 }
        end; { case }
      p_exitlb;
    end; {p_pack}

{
    This procedure implements the UNPACK procedure in Pascal-2.
}


  procedure p_unpack(var z: kludge;
                     lowz, highz, sizez_bits: integer;
                     zpacked: boolean;
                     unsigned: boolean;
                     var a: kludge;
                     lowa, higha, sizea_bytes: integer;
                     offset: integer);
    external;


  procedure p_unpack;

    var
      i, j, k, cnt: all;


    begin
      p_entlib;
      if (not zpacked) or (zpacked and (sizez_bits > 8)) then sizez_bits := 8;
      if offset < lowa then p_upklow;
      if (offset > higha) or ((offset + highz - lowz) > higha) then p_upkhi;
      k := offset - lowa;
      cnt := highz - lowz;
      case sizez_bits of
        1:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4:
                if unsigned then a.b32[k] := z.b1[i]
                else a.s32[k] := z.s1[i];
              2:
                if unsigned then a.b16[k] := z.b1[i]
                else a.s16[k] := z.s1[i];
              otherwise
                if unsigned then a.b8[k] := z.b1[i]
                else a.s8[k] := z.s1[i];
              end;
            k := k + 1;
            end; { for }
        2:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4:
                if unsigned then a.b32[k] := z.b2[i]
                else a.s32[k] := z.s2[i];
              2:
                if unsigned then a.b16[k] := z.b2[i]
                else a.s16[k] := z.s2[i];
              otherwise
                if unsigned then a.b8[k] := z.b2[i]
                else a.s8[k] := z.s2[i];
              end;
            k := k + 1;
            end; { for }
        4:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4:
                if unsigned then a.b32[k] := z.b4[i]
                else a.s32[k] := z.s4[i];
              2:
                if unsigned then a.b16[k] := z.b4[i]
                else a.s16[k] := z.s4[i];
              otherwise
                if unsigned then a.b8[k] := z.b4[i]
                else a.s8[k] := z.s4[i];
              end;
            k := k + 1;
            end; { for }
        8:
          begin
          if (not zpacked) or (sizea_bytes = 1) then
            begin
            k := k * sizea_bytes;
            for i := 0 to (cnt + 1) * sizea_bytes - 1 do
              begin
              if unsigned then a.b8[k] := z.b8[i]
              else a.s8[k] := z.s8[i];
              k := k + 1;
              end;
            end
          else if sizea_bytes = 2 then
            begin
            for i := 0 to cnt do
              begin
              if unsigned then a.b16[k] := z.b8[i]
              else a.s16[k] := z.s8[i];
              k := k + 1;
              end;
            end
          else if sizea_bytes = 4 then
            for i := 0 to cnt do
              begin
              if unsigned then a.b32[k] := z.b8[i]
              else a.s32[k] := z.s8[i];
              k := k + 1;
              end
          end; { 8 }
        16:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4:
                if unsigned then a.b32[k] := z.b16[i]
                else a.s32[k] := z.s16[i];
              2:
                if unsigned then a.b16[k] := z.b16[i]
                else a.s16[k] := z.s16[i];
              otherwise
                if unsigned then a.b8[k] := z.b16[i]
                else a.s8[k] := z.s16[i];
              end;
            k := k + 1;
            end; { for }
        32:
          for i := 0 to cnt do
            begin
            case sizea_bytes of
              4:
                if unsigned then a.b32[k] := z.b32[i]
                else a.s32[k] := z.s32[i];
              2:
                if unsigned then a.b16[k] := z.b32[i]
                else a.s16[k] := z.s32[i];
              otherwise
                if unsigned then a.b8[k] := z.b32[i]
                else a.s8[k] := z.s32[i];
              end;
            k := k + 1;
            end; { for }
        end; { case }
      p_exitlb;
    end; {p_unpack}
