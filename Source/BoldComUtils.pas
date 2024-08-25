
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldComUtils;

interface

uses
  ActiveX,
  Classes,
  BoldDefs;

{//} const
{//}  BOLD_DEFAULT_SERVER_CONTEXT = CLSCTX_INPROC_SERVER or
{//}    CLSCTX_LOCAL_SERVER or CLSCTX_REMOTE_SERVER;

  {CoInitializeSecurity}
  {authentication levels}
  RPC_C_AUTHN_LEVEL_DEFAULT = 0;
  RPC_C_AUTHN_LEVEL_NONE = 1;
  RPC_C_AUTHN_LEVEL_CONNECT = 2;
  RPC_C_AUTHN_LEVEL_CALL = 3;
  RPC_C_AUTHN_LEVEL_PKT = 4;
  RPC_C_AUTHN_LEVEL_PKT_INTEGRITY = 5;
  RPC_C_AUTHN_LEVEL_PKT_PRIVACY = 6;

  alDefault = RPC_C_AUTHN_LEVEL_DEFAULT;
  alNone = RPC_C_AUTHN_LEVEL_NONE;
  alConnect = RPC_C_AUTHN_LEVEL_CONNECT;
  alCall = RPC_C_AUTHN_LEVEL_CALL;
  alPacket = RPC_C_AUTHN_LEVEL_PKT;
  alPacketIntegrity = RPC_C_AUTHN_LEVEL_PKT_INTEGRITY;
  alPacketPrivacy = RPC_C_AUTHN_LEVEL_PKT_PRIVACY;


  {impersonation levels}
  RPC_C_IMP_LEVEL_DEFAULT = 0;
  RPC_C_IMP_LEVEL_ANONYMOUS = 1;
  RPC_C_IMP_LEVEL_IDENTIFY = 2;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  RPC_C_IMP_LEVEL_DELEGATE = 4;

  ilDefault = RPC_C_IMP_LEVEL_DEFAULT;
  ilAnonymous = RPC_C_IMP_LEVEL_ANONYMOUS;
  ilIdentiry = RPC_C_IMP_LEVEL_IDENTIFY;
  ilImpersonate = RPC_C_IMP_LEVEL_IMPERSONATE;
  ilDelegate = RPC_C_IMP_LEVEL_DELEGATE;

  {authentication capabilities}
  EOAC_NONE = $0;
  EOAC_DEFAULT = $800;
  EOAC_MUTUAL_AUTH = $1;
  EOAC_STATIC_CLOACKING = $20;
  EOAC_DYNAMIC_CLOAKING = $40;
  EOAC_ANY_AUTHORITY = $80;

  RPC_C_AUTHN_WINNT = 10;

  RPC_C_AUTHNZ_NONE = 0;
  RPC_C_AUTHNZ_NAME = 1;
  RPC_C_AUTHNZ_DCE = 2;

type
  EBoldCom = class(EBold);

{ Variant support }
function BoldVariantIsType(V: OleVariant; TypeCode: Integer): Boolean;

function BoldMemoryToVariant(const Buffer; BufSize: Integer): OleVariant;
function BoldStreamToVariant(Stream: TStream): OleVariant;
function BoldStringsToVariant(Strings: TStrings): OleVariant;
function BoldVariantToMemory(V: OleVariant; var Buffer; BufSize: Integer): Integer;
function BoldVariantToStream(V: OleVariant; Stream: TStream): Integer;
function BoldVariantArrayOfArraysOfStringToStrings(V: OleVariant; Strings: TStrings): Integer;

{ Named values }
function BoldVariantIsNamedValues(V: OleVariant): Boolean;
function BoldCreateNamedValues(const Names: array of string;
  const Values: array of OleVariant): OleVariant;
function BoldGetNamedValue(Data: OleVariant; const Name: string): OleVariant;
procedure BoldSetNamedValue(Data: OleVariant; const Name: string; Value: OleVariant);

function BoldCreateGUID: TGUID;
function BoldCreateComObject(const ClsId, IId: TGUID; out Obj; out Res: HResult): Boolean;
function BoldCreateRemoteComObject(const HostName: string;
  const ClsId, IId: TGUID; out Obj; out Res: HResult): Boolean;

procedure BoldInitializeComSecurity(AuthenticationLevel, ImpersonationLevel: longint);
procedure BoldSetSecurityForInterface(AuthenticationLevel, ImpersonationLevel: longint; Unk: IUnknown);

implementation

uses
  SysUtils,
  Variants,
  ComObj,
  Windows;

var
  NeedToUninitialize: Boolean = false;

function BoldVariantIsType(V: OleVariant; TypeCode: Integer): Boolean;
begin
  Result := ((VarType(V) and varTypeMask) = TypeCode);
end;

function BoldMemoryToVariant(const Buffer; BufSize: Integer): OleVariant;
var
  P: Pointer;
begin
  Result := VarArrayCreate([0, BufSize], varByte);
  P := VarArrayLock(Result);
  try
    Move(Buffer, P^, BufSize);
  finally
    VarArrayUnlock(Result);
  end;
end;

function BoldStreamToVariant(Stream: TStream): OleVariant;
var
  P: Pointer;
  TempStream: TMemoryStream;
  CreatedTempStream: Boolean;
begin
  CreatedTempStream := False;
  TempStream := nil;
  try
    if Stream is TMemoryStream then
    begin
      TempStream := Stream as TMemoryStream;
      CreatedTempStream := False;
    end
    else
    begin
      TempStream := TMemoryStream.Create;
      CreatedTempStream := True;
      TempStream.CopyFrom(Stream,Stream.Size);
    end;
    Result := VarArrayCreate([0, TempStream.Size], varByte);
    P := VarArrayLock(Result);
    try
      Move(TempStream.Memory^, P^, TempStream.Size);
    finally
      VarArrayUnlock(Result);
    end;
  finally
    if CreatedTempStream then TempStream.Free;
  end;
end;

function BoldStringsToVariant(Strings: TStrings): OleVariant;
var
  I: Integer;
begin
  if Assigned(Strings) and (Strings.Count > 0) then
  begin
    Result := VarArrayCreate([0, Strings.Count - 1], varVariant);
    for I := 0 to Strings.Count - 1 do Result[I] := WideString(Strings[I]);
  end
  else
    Result := Null;
end;

function BoldVariantToMemory(V: OleVariant; var Buffer; BufSize: Integer): Integer;
var
  P: Pointer;
begin
  Result := 0;
  if VarIsArray(V) and ((VarType(V) and varTypeMask) = varByte) and
    (VarArrayDimCount(V) = 1) then
  begin
    Result := VarArrayHighBound(V, 1);
    if Result > BufSize then Exit;
    P := VarArrayLock(V);
    try
      Move(P^, Buffer, Result);
    finally
      VarArrayUnlock(V);
    end;
  end;
end;

function BoldVariantToStream(V: OleVariant; Stream: TStream): Integer;
var
  P: Pointer;
begin
  Result := 0;
  if VarIsArray(V) and ((VarType(V) and varTypeMask) = varByte) and
    (VarArrayDimCount(V) = 1) then
  begin
    Result := VarArrayHighBound(V, 1);
    P := VarArrayLock(V);
    try
      Stream.Write(P^, Result);
    finally
      VarArrayUnlock(V);
    end;
  end;
end;

function BoldVariantArrayOfArraysOfStringToStrings(V: OleVariant; Strings: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if VarIsArray(V) and (VarArrayDimCount(V) = 1) then
  begin
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    begin
      Strings.Add(V[I][0]);
      Inc(Result);
    end;
  end;
end;

function BoldVariantIsNamedValues(V: OleVariant): Boolean;
var
  Va: OleVariant;
begin
  Result := VarIsArray(V);
  if Result then
  begin
    Result := VarArrayDimCount(V) = 1;
    Result := Result and (VarArrayLowBound(V,1) = 0);
    Result := Result and (VarArrayHighBound(V,1) = 1);
  end;
  if Result then
  begin
    Va := V[0];
    Result := VarIsArray(Va) and
      ((VarType(Va) and varTypeMask) = varOleStr) and (VarArrayDimCount(Va) = 1);
  end;
  if Result then
  begin
    Va := V[1];
    Result := VarIsArray(Va) and
      ((VarType(Va) and varTypeMask) = varVariant) and (VarArrayDimCount(Va) = 1);
  end;
end;

function BoldCreateNamedValues(const Names: array of string;
  const Values: array of OleVariant): OleVariant;
var
  NameList,ValueList: OleVariant;
  I: Integer;
begin
  if (Length(Names) > 0) and (Length(Names) = Length(Values)) then
  begin
    NameList := VarArrayCreate([0,High(Names)],varOleStr);
    ValueList := VarArrayCreate([0,High(Names)],varVariant);
    for I := 0 to High(Names) do
    begin
      NameList[I] := Names[I];
      ValueList[I] := Values[I];
    end;
    Result := VarArrayCreate([0,1],varVariant);
    Result[0] := NameList;
    Result[1] := ValueList;
  end
  else
    Result := Null;
end;

function BoldGetNamedValue(Data: OleVariant; const Name: string): OleVariant;
var
  NameList,ValueList: OleVariant;
  I: Integer;
begin
  Result := Unassigned;
  NameList := Data[0];
  for I := 0 to VarArrayHighBound(NameList, 1) do
  begin
    if AnsiCompareText(Name,NameList[I]) = 0 then
    begin
      ValueList := Data[1];
      Result := ValueList[I];
      Break;
    end;
  end;
end;

procedure BoldSetNamedValue(Data: OleVariant; const Name: string; Value: OleVariant);
var
  NameList,ValueList: OleVariant;
  I: Integer;
begin
  NameList := Data[0];
  for I := 0 to VarArrayHighBound(NameList, 1) do
  begin
    if AnsiCompareText(Name,NameList[I]) = 0 then
    begin
      ValueList := Data[1];
      ValueList[I] := Value;
      Break;
    end;
  end;
end;

function BoldCreateGUID: TGUID;
var
  GUID: TGUID;
begin
  CoCreateGuid(GUID);
  Result := GUID;
end;

function BoldCreateComObject(const ClsId, IId: TGUID; out Obj; out Res: HResult): Boolean;
begin
  Res := CoCreateInstance(ClsID,nil,BOLD_DEFAULT_SERVER_CONTEXT,IId,Obj);
  Result :=  Res = S_OK;
end;

function BoldCreateRemoteComObject(const HostName: string;
  const ClsId, IId: TGUID; out Obj; out Res: HResult): Boolean;
var
  MultiQI: TMultiQI;
  ServerInfo: TCoServerInfo;
  WS: WideString;
begin
  FillChar(ServerInfo, SizeOf(ServerInfo), 0);
  ws := HostName;
  ServerInfo.pwszName := PWideChar(WS);
  MultiQI.IID := @IId;
  MultiQI.ITF := nil;
  MultiQI.HR  := 0;
  Res := CoCreateInstanceEx(ClsID, nil,
    BOLD_DEFAULT_SERVER_CONTEXT, @ServerInfo, 1, @MultiQI);
  Result :=  Res = S_OK;
  Result := Result and (MultiQI.HR = S_OK);
  if Result then IUnknown(Obj) := MultiQI.ITF;
end;

procedure BoldInitializeComSecurity(AuthenticationLevel, ImpersonationLevel: longint);
begin
  if not IsLibrary then
  begin
    NeedToUninitialize := Succeeded(CoInitialize(nil));
    OleCheck(CoInitializeSecurity(nil, -1, nil, nil, AuthenticationLevel, ImpersonationLevel, nil, EOAC_NONE, nil));
  end;
end;

procedure BoldSetSecurityForInterface(AuthenticationLevel, ImpersonationLevel: longint; Unk: IUnknown);
begin
  CoSetProxyBlanket(Unk, RPC_C_AUTHN_LEVEL_NONE, RPC_C_AUTHNZ_NONE, nil, AuthenticationLevel, ImpersonationLevel,
                      nil, EOAC_NONE);
end;

initialization

finalization
  if NeedToUninitialize then CoUninitialize;

end.