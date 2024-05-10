
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit WinStuff;

interface

{$IFDEF LINUX}
type
  THandle = integer;
const
  GMEM_FIXED = 1024;


function LockResource(h: THandle): Pointer;
function FreeResource(h: THandle): boolean;
function GlobalLock(h: THandle): pointer;
function GlobalUnlock(h: THandle): boolean;
function GlobalAlloc(Flags: Integer; Bytes: longint): THandle;
function GlobalFree(h: THandle): THandle;
function FindResource(h: THandle; p1, p2: PAnsiChar): THandle;
function LoadResource(Module: THandle; Resource: THandle): THandle;
function SizeOfResource(Module: THandle; ResInfo: THandle): longint;


{$ENDIF}

implementation
{$IFDEF LINUX}


function LockResource(h: THandle): Pointer;
begin
  Result := Pointer(h);
end;

function FreeResource(h: THandle): boolean;
begin
end;

function GlobalLock(h: THandle): pointer;
begin
  Result := Pointer(h);
end;

function GlobalUnlock(h: THandle): boolean;
begin
  Result := True;
end;

function GlobalAlloc(Flags: Integer; Bytes: longint): THandle;
begin
end;

function GlobalFree(h: THandle): THandle;
begin
end;

function FindResource(h: THandle; p1, p2: PAnsiChar): THandle;
begin
end;

function LoadResource(Module: THandle; Resource: THandle): THandle;
begin
end;


function SizeOfResource(Module: THandle; ResInfo: THandle): longint;
begin
end;

{$ENDIF}

end.