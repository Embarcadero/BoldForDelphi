
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldMD5;

(* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software. *)

interface

uses
  Classes,
  BoldDefs;

type
  TBoldMD5DigestStr = string[32];
  TBoldMD5Digest = array[0..15] of Byte;
  TBoldMD5DigestInteger = array[0..3] of Integer;

  TBoldMD5IntegerBuf = array[0..15] of Integer;
  TBoldMD5ByteBuf = array[0..63] of Byte;
  CharPtr = ^AnsiChar;

  TBoldMD5DigestData = record
    DigestStr: TBoldMD5DigestStr;
    Digest: TBoldMD5Digest;
    DigestInteger: TBoldMD5DigestInteger;
  end;


  TBoldMD5 = class
  private
    fDigest: TBoldMD5Digest;
    fAccDigest: TBoldMD5DigestInteger;
    fBitCountLo: Integer;
    fBitCountHi: Integer;
    fBuffer: TBoldMD5IntegerBuf;
    fBufferLenght: Cardinal;
    fBufferChanged: Boolean;
    procedure Transform(var lAccu: TBoldMD5DigestInteger; const lBuf: TBoldMD5IntegerBuf);
    function GetDigestData: TBoldMD5DigestData;
  protected
  public
    constructor Create;
    procedure ResetBuffer;
    procedure Update(const ChkBuf; Len: Cardinal); overload;
    procedure Update(const Data: TBoldAnsiString); overload;
    property DigestData: TBoldMD5DigestData read GetDigestData;
    class function Test(Data, CorrectDigest: TBoldAnsiString): Boolean;
    class function DigestString(const S: TBoldAnsiString): TBoldMD5DigestStr;
    class function DigestStream(const Source: TStream): TBoldMD5DigestStr;
    class procedure SelfTest;
  end;

implementation

uses
  SysUtils,
  BoldUtils;


{ TBoldMD5 }

{$IFOPT Q+}
  {$DEFINE BOLD_OLD_OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

procedure TBoldMD5.Transform(var lAccu: TBoldMD5DigestInteger; const lBuf: TBoldMD5IntegerBuf);
var
  a, b, c, d: Integer;

  function ROL(x: Integer; n: Integer): Integer;
  begin
    Result:= (x shl n) or (x shr (32-n));
  end;

  function FF(a, b, c, d, x, s, ac: Integer): Integer;
  begin
    Result:= ROL(a+x+ac+(b and c or not b and d), s)+b;
  end;

  function GG(a, b, c, d, x, s, ac: Integer): Integer;
  begin
    Result:= ROL(a+x+ac+(b and d or c and not d), s)+b;
  end;

  function HH(a, b, c, d, x, s, ac: Integer): Integer;
  begin
    Result:= ROL(a+x+ac+(b xor c xor d), s)+b;
  end;

  function II(a, b, c, d, x, s, ac: Integer): Integer;
  begin
    Result:= ROL(a+x+ac+(c xor (b or not d)), s)+b;
  end;

begin
  a:= lAccu[0];
  b:= lAccu[1];
  c:= lAccu[2];
  d:= lAccu[3];

  a:= FF(a,b,c,d, lBuf[ 0],  7, Integer($d76aa478)); { 1 }
  d:= FF(d,a,b,c, lBuf[ 1], 12, Integer($e8c7b756)); { 2 }
  c:= FF(c,d,a,b, lBuf[ 2], 17, Integer($242070db)); { 3 }
  b:= FF(b,c,d,a, lBuf[ 3], 22, Integer($c1bdceee)); { 4 }
  a:= FF(a,b,c,d, lBuf[ 4],  7, Integer($f57c0faf)); { 5 }
  d:= FF(d,a,b,c, lBuf[ 5], 12, Integer($4787c62a)); { 6 }
  c:= FF(c,d,a,b, lBuf[ 6], 17, Integer($a8304613)); { 7 }
  b:= FF(b,c,d,a, lBuf[ 7], 22, Integer($fd469501)); { 8 }
  a:= FF(a,b,c,d, lBuf[ 8],  7, Integer($698098d8)); { 9 }
  d:= FF(d,a,b,c, lBuf[ 9], 12, Integer($8b44f7af)); { 10 }
  c:= FF(c,d,a,b, lBuf[10], 17, Integer($ffff5bb1)); { 11 }
  b:= FF(b,c,d,a, lBuf[11], 22, Integer($895cd7be)); { 12 }
  a:= FF(a,b,c,d, lBuf[12],  7, Integer($6b901122)); { 13 }
  d:= FF(d,a,b,c, lBuf[13], 12, Integer($fd987193)); { 14 }
  c:= FF(c,d,a,b, lBuf[14], 17, Integer($a679438e)); { 15 }
  b:= FF(b,c,d,a, lBuf[15], 22, Integer($49b40821)); { 16 }

  a:= GG(a,b,c,d, lBuf[ 1],  5, Integer($f61e2562)); { 17 }
  d:= GG(d,a,b,c, lBuf[ 6],  9, Integer($c040b340)); { 18 }
  c:= GG(c,d,a,b, lBuf[11], 14, Integer($265e5a51)); { 19 }
  b:= GG(b,c,d,a, lBuf[ 0], 20, Integer($e9b6c7aa)); { 20 }
  a:= GG(a,b,c,d, lBuf[ 5],  5, Integer($d62f105d)); { 21 }
  d:= GG(d,a,b,c, lBuf[10],  9, Integer($02441453)); { 22 }
  c:= GG(c,d,a,b, lBuf[15], 14, Integer($d8a1e681)); { 23 }
  b:= GG(b,c,d,a, lBuf[ 4], 20, Integer($e7d3fbc8)); { 24 }
  a:= GG(a,b,c,d, lBuf[ 9],  5, Integer($21e1cde6)); { 25 }
  d:= GG(d,a,b,c, lBuf[14],  9, Integer($c33707d6)); { 26 }
  c:= GG(c,d,a,b, lBuf[ 3], 14, Integer($f4d50d87)); { 27 }
  b:= GG(b,c,d,a, lBuf[ 8], 20, Integer($455a14ed)); { 28 }
  a:= GG(a,b,c,d, lBuf[13],  5, Integer($a9e3e905)); { 29 }
  d:= GG(d,a,b,c, lBuf[ 2],  9, Integer($fcefa3f8)); { 30 }
  c:= GG(c,d,a,b, lBuf[ 7], 14, Integer($676f02d9)); { 31 }
  b:= GG(b,c,d,a, lBuf[12], 20, Integer($8d2a4c8a)); { 32 }

  a:= HH(a,b,c,d, lBuf[ 5],  4, Integer($fffa3942)); { 33 }
  d:= HH(d,a,b,c, lBuf[ 8], 11, Integer($8771f681)); { 34 }
  c:= HH(c,d,a,b, lBuf[11], 16, Integer($6d9d6122)); { 35 }
  b:= HH(b,c,d,a, lBuf[14], 23, Integer($fde5380c)); { 36 }
  a:= HH(a,b,c,d, lBuf[ 1],  4, Integer($a4beea44)); { 37 }
  d:= HH(d,a,b,c, lBuf[ 4], 11, Integer($4bdecfa9)); { 38 }
  c:= HH(c,d,a,b, lBuf[ 7], 16, Integer($f6bb4b60)); { 39 }
  b:= HH(b,c,d,a, lBuf[10], 23, Integer($bebfbc70)); { 40 }
  a:= HH(a,b,c,d, lBuf[13],  4, Integer($289b7ec6)); { 41 }
  d:= HH(d,a,b,c, lBuf[ 0], 11, Integer($eaa127fa)); { 42 }
  c:= HH(c,d,a,b, lBuf[ 3], 16, Integer($d4ef3085)); { 43 }
  b:= HH(b,c,d,a, lBuf[ 6], 23, Integer($04881d05)); { 44 }
  a:= HH(a,b,c,d, lBuf[ 9],  4, Integer($d9d4d039)); { 45 }
  d:= HH(d,a,b,c, lBuf[12], 11, Integer($e6db99e5)); { 46 }
  c:= HH(c,d,a,b, lBuf[15], 16, Integer($1fa27cf8)); { 47 }
  b:= HH(b,c,d,a, lBuf[ 2], 23, Integer($c4ac5665)); { 48 }

  a:= II(a,b,c,d, lBuf[ 0],  6, Integer($f4292244)); { 49 }
  d:= II(d,a,b,c, lBuf[ 7], 10, Integer($432aff97)); { 50 }
  c:= II(c,d,a,b, lBuf[14], 15, Integer($ab9423a7)); { 51 }
  b:= II(b,c,d,a, lBuf[ 5], 21, Integer($fc93a039)); { 52 }
  a:= II(a,b,c,d, lBuf[12],  6, Integer($655b59c3)); { 53 }
  d:= II(d,a,b,c, lBuf[ 3], 10, Integer($8f0ccc92)); { 54 }
  c:= II(c,d,a,b, lBuf[10], 15, Integer($ffeff47d)); { 55 }
  b:= II(b,c,d,a, lBuf[ 1], 21, Integer($85845dd1)); { 56 }
  a:= II(a,b,c,d, lBuf[ 8],  6, Integer($6fa87e4f)); { 57 }
  d:= II(d,a,b,c, lBuf[15], 10, Integer($fe2ce6e0)); { 58 }
  c:= II(c,d,a,b, lBuf[ 6], 15, Integer($a3014314)); { 59 }
  b:= II(b,c,d,a, lBuf[13], 21, Integer($4e0811a1)); { 60 }
  a:= II(a,b,c,d, lBuf[ 4],  6, Integer($f7537e82)); { 61 }
  d:= II(d,a,b,c, lBuf[11], 10, Integer($bd3af235)); { 62 }
  c:= II(c,d,a,b, lBuf[ 2], 15, Integer($2ad7d2bb)); { 63 }
  b:= II(b,c,d,a, lBuf[ 9], 21, Integer($eb86d391)); { 64 }

  Inc(lAccu[0], a);
  Inc(lAccu[1], b);
  Inc(lAccu[2], c);
  Inc(lAccu[3], d)
end;

constructor TBoldMD5.Create;
begin
  ResetBuffer;
end;

procedure TBoldMD5.ResetBuffer;
begin
  fBitCountLo:= 0;
  fBitCountHi:= 0;
  fBufferLenght:= 0;
  {Load magic initialization constants.}
  fAccDigest[0]:= Integer($67452301);
  fAccDigest[1]:= Integer($efcdab89);
  fAccDigest[2]:= Integer($98badcfe);
  fAccDigest[3]:= Integer($10325476);
  fBufferChanged:= True;
end;

procedure TBoldMD5.Update(const ChkBuf; Len: Cardinal);
var
  ChkBufPtr: CharPtr;
  Left: Cardinal;
begin
  fBufferChanged:= True;
  if fBitCountLo + Integer(Len) shl 3 < fBitCountLo then
    Inc(fBitCountHi);
  Inc(fBitCountLo, Integer(Len) shl 3);
  Inc(fBitCountHi, Integer(Len) shr 29);

  ChkBufPtr := @ChkBuf;
  while (fBufferLenght + Len) >= 64 do
  begin
    Left := 64 - fBufferLenght;
    Move(ChkBufPtr^, TBoldMD5ByteBuf(fBuffer)[fBufferLenght], Left);
    Inc(ChkBufPtr, Left);
    Dec(Len, Left);
    Transform(fAccDigest, fBuffer);
    fBufferLenght:= 0;
  end;
  if Len > 0 then
  begin
    Move(ChkBufPtr^, TBoldMD5ByteBuf(fBuffer)[fBufferLenght], Len);
    Inc(fBufferLenght, Len);
  end
end;

procedure TBoldMD5.Update(const Data: TBoldAnsiString);
begin
  if Length(Data)>0 then
  Update(Data[1], Length(Data));
end;

function TBoldMD5.GetDigestData: TBoldMD5DigestData;
const
  hc: array[0..$F] of AnsiChar = '0123456789ABCDEF';
var
  WorkBuf: TBoldMD5IntegerBuf;
  WorkLen: Cardinal;
  i: 0..15;
begin
  if fBufferChanged then
  begin
    fDigest:= TBoldMD5Digest(fAccDigest);
    Move(fBuffer, WorkBuf, fBufferLenght); {make copy of buffer}
    TBoldMD5ByteBuf(WorkBuf)[fBufferLenght]:= $80;
    WorkLen:= fBufferLenght+1;
    if WorkLen > 56 then
    begin
      if WorkLen < 64 then
        FillChar(TBoldMD5ByteBuf(WorkBuf)[WorkLen], 64-WorkLen, 0);
      TransForm(TBoldMD5DigestInteger(fDigest), WorkBuf);
      WorkLen:= 0;
    end;
    FillChar(TBoldMD5ByteBuf(WorkBuf)[WorkLen], 56-WorkLen, 0);
    WorkBuf[14]:= fBitCountLo;
    WorkBuf[15]:= fBitCountHi;
    Transform (TBoldMD5DigestInteger(fDigest), WorkBuf);
    fBufferChanged:= False;
  end;
  Result.Digest := fDigest;
  Result.DigestStr[0]:= #32;
  for i:= 0 to 15 do
  begin
    Result.DigestStr[1+i shl 1]:= hc[fDigest[i] shr 4];
    Result.DigestStr[2+i shl 1]:= hc[fDigest[i] and $F]
  end;
  TBoldMD5Digest(Result.DigestInteger):= fDigest
end;

class function TBoldMD5.DigestString(const S: TBoldAnsiString): TBoldMD5DigestStr;
var
  MD5: TBoldMD5;
begin
  Result:= '';
  MD5:= TBoldMD5.Create;
  if Length(S)>0 then
    MD5.Update(S[1], Length(S));
  Result:= MD5.DigestData.DigestStr;
  MD5.Free;
end;

class function TBoldMD5.DigestStream(const Source: TStream): TBoldMD5DigestStr;
const
  MaxBufSize = $F000;
var
  MD5: TBoldMD5;
  BufSize, N: Integer;
  Buffer: PAnsiChar;
  Count: Int64;
begin
  Result:= '';
  MD5:= TBoldMD5.Create;
  Source.Position := 0;
  Count := Source.Size;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      MD5.Update(Buffer^, N);
      Dec(Count, N);
    end;
    Result:= MD5.DigestData.DigestStr;
  finally
    FreeMem(Buffer, BufSize);
    MD5.Free;
  end;
end;

class function TBoldMD5.Test(Data, CorrectDigest: TBoldAnsiString): Boolean;
var
  S: TBoldMD5DigestStr;
begin
  with TBoldMD5.Create do
  try
    if Length(Data)>0 then
      Update(Data[1], Length(Data));
    S := DigestData.DigestStr;
    Result := CorrectDigest = S
  finally
    Free;
  end;
end;

class procedure TBoldMD5.SelfTest;
begin
  if not (Test('', 'D41D8CD98F00B204E9800998ECF8427E') and
          Test('a', '0CC175B9C0F1B6A831C399E269772661') and
          Test('abc', '900150983CD24FB0D6963F7D28E17F72') and
          Test('message digest', 'F96B697D7CB7938D525A2F31AAF161D0') and
          Test('abcdefghijklmnopqrstuvwxyz', 'C3FCD3D76192E4007DFB496CCA67E13B') and
          Test('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 'D174AB98D277D9F5A5611C2C9F419D9F') and
          Test('12345678901234567890123456789012345678901234567890123456789012345678901234567890', '57EDF4A22BE3C955AC49DA2E2107B67A') and
          Test('123456789012345678901234567890123456789012345678901234567890123', 'C3EB67ECE68488BB394241D4F6A54244'))
     or  (Test('1234567890', '96B697D7C697D7CB7938D31AAF161D0F')) then
    raise Exception.Create('Internal self test failed');
end;

{$IFDEF BOLD_OLD_OVERFLOWCHECKS_ON}
  {$OVERFLOWCHECKS ON}
  {$UNDEF BOLD_OLD_OVERFLOWCHECKS_ON}
{$ENDIF}

end.
