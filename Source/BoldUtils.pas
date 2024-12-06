
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUtils;

interface

uses
  Variants,
  SysUtils,
  Classes,
  TypInfo,
  Windows,
  BoldDefs,
  WideStrings;

type
  TBoldNotificationEvent = procedure(AComponent: TComponent; Operation: TOperation) of object;

  TBoldPassthroughNotifier = class(TComponent)
  private
    fNotificationEvent: TBoldNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor CreateWithEvent(NotificationEvent: TBoldNotificationEvent; Owner: TComponent = nil);
  end;

function CharCount(c: char; const s: string): integer;
function BoldNamesEqual(const name1, name2: string): Boolean;
procedure BoldAppendToStrings(strings: TStrings; const aString: string; const ForceNewLine: Boolean);
function BoldSeparateStringList(strings: TStringList; const Separator, PreString, PostString: String; AIndex: integer = -1): String;
function BoldSeparatedAppend(const S1, S2: string;const Separator: string = ','): string;
function BoldTrim(const S: string): string;
function BoldIsPrefix(const S, Prefix: string): Boolean;
function BoldStrEqual(P1, P2: PChar; Len: integer): Boolean;
function BoldStrAnsiEqual(P1, P2: PChar; Len: integer): Boolean;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
function BoldAnsiEqual(const S1, S2: string): Boolean;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
function BoldStrStringEqual(const S1: string; P2: PChar; Len: integer): Boolean;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
function BoldCaseIndependentPos(const Substr, S: string): Integer;
procedure EnumToStrings(aTypeInfo: pTypeInfo; Strings: TStrings);
function CapitalisedToSpaced(Capitalised: String): String;
function SpacedToCapitalised(Spaced: String): String;
function BooleanToString(BoolValue: Boolean): String;
function StringToBoolean(StrValue: String): Boolean;
function GetUpperLimitForMultiplicity(const Multiplicity: String): Integer;
function GetLowerLimitForMultiplicity(const Multiplicity: String): Integer;
function StringListToVarArray(List: TStringList): variant;
function IsLocalMachine(const Machinename: WideString): Boolean;
function GetComputerNameStr: string;
function TimeStampComp(const Time1, Time2: TTimeStamp): Integer;
function StrToDateFmt(const ADateString: string; const ADateFormat: string;
  const ATimeFormat: string; const ADateSeparatorChar: char = '/';
  const ADateTimeSeperatorChar: char = ' '): TDateTime;
function DateToStrFmt(const aDate: TDateTime; DateFormat: string; const DateSeparatorChar: char = '/'): String;
function BoldParseFormattedDateList(const value: String; const formats: TStrings; var Date: TDateTime): Boolean;
function BoldParseFormattedDate(const value: String; const formats: array of string; var Date: TDateTime): Boolean;
{$IFDEF MSWINDOWS}
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
{$ENDIF}
function UserTimeInTicks: Int64;
function TicksToDateTime(Ticks: Int64): TDateTime;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
procedure EnsureTrailing(var Str: String; ch: char);
{ Taken from FileCtrl to remove unit dependency }
function DirectoryExists(const Name: string): Boolean;
function ForceDirectories(Dir: string): Boolean;

function BoldRootRegistryKey: string;
function GetModuleFileNameAsString(IncludePath: Boolean): string;

{variant support}
function BoldVariantToStrings(V: OleVariant; Strings: TStrings): Integer;

{$IFNDEF BOLD_DELPHI13_OR_LATER}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$ENDIF}

var BoldRunningAsDesignTimePackage: boolean = false;

implementation

uses
  BoldCoreConsts,
  BoldRev;

{$IFDEF LINUX}
const
  MAX_COMPUTERNAME_LENGTH = 128;
{$ENDIF}

{$IFDEF MSWINDOWS}

type
  TFileTimeAligner = record
  case integer of
  0: (asFileTime: TFileTime);
  1: (asInt64: Int64);
  end;
var
  CurrentProcess: THANDLE = INVALID_HANDLE_VALUE ;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
const
  Nr100nsPerDay = 3600.0*24*10000000;
var
  FileTimeAsInt64: Int64;
begin
  Move(FileTime, FileTimeAsInt64, 8);
  Result := FileTimeAsInt64/Nr100nsPerDay;

end;
{$ENDIF}

function UserTimeInTicks: Int64;
var
  UserTime, CreationTime, ExitTime, KernelTime: TFileTimeAligner;
begin
{$IFDEF MSWINDOWS}
  if CurrentProcess <> 0 then
    CloseHandle(CurrentProcess);
  CurrentProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, GetCurrentProcessId);
  if GetProcessTimes(CurrentProcess, CreationTime.asFileTime, ExitTime.asFileTime, KernelTime.asFileTime, UserTime.asFileTime) then
    Result := UserTime.asInt64
  else
    Result := 0;  
{$ELSE}
  FIXME
{$ENDIF}
end;
function TicksToDateTime(Ticks: Int64): TDateTime;
const
  Nr100nsPerDay = 3600.0*24.0*10000000.0;
begin
{$IFDEF MSWINDOWS}
  Result := Ticks/Nr100nsPerDay;
{$ELSE}
  FIXME
{$ENDIF}
end;

{$IFNDEF BOLD_DELPHI13_OR_LATER}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

{ Taken from FileCtrl to remove unit dependency }
function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    raise Exception.Create(sCannotCreateDirectory);
  Dir := ExcludeTrailingPathDelimiter(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit;
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function BoldIsPrefix(const S, Prefix: string): Boolean;
{ril - resusing length by variable }
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(Prefix);
  Result := (Length(s) >= PrefixLen) and CompareMem(@s[1], @Prefix[1], PrefixLen * SizeOf(Char));
end;

function BoldStrEqual(P1, P2: PChar; Len: integer): Boolean;
begin
 Result := CompareMem(P1, P2, Len * SizeOf(Char));
end;

function BoldStrAnsiEqual(P1, P2: PChar; Len: integer): Boolean;
begin
  Result := CompareMem(P1, P2, Len * SizeOf(Char)) or (AnsiStrLIComp(P1, P2, Len) = 0);
end;

function BoldStrCaseIndpendentEqual(P1, P2: PChar; Len: integer): Boolean;
var
  ch1, ch2: Char;
begin
  if not CompareMem(P1, P2, Len * SizeOf(Char)) then
    while Len <> 0 do
    begin
      ch1 := P1^;
      ch2 := P2^;
      if Ch1 = ch2 then
        // match
      else
      begin
        Ch1 := char(ord(ch1) or 32);
        if (Ch1 >= 'a') and (Ch1 <= 'z') and (ch1 = char(ord(ch2) or 32)) then
          // match
        else
        begin
          Result := False;
          Exit;
        end;
      end;
      Inc(P1);
      Inc(P2);
      Dec(Len);
    end;
  Result := True;
end;

function BoldAnsiEqual(const S1, S2: string): Boolean;
var
  Len: integer;
begin
  Len := Length(S1);
  if Len <> Length(S2) then
    Result := False
  else
    Result := BoldStrAnsiEqual(PChar(S1), PChar(S2), Len);
end;

function BoldStrStringEqual(const S1: string; P2: PChar; Len: integer): Boolean;
begin
  if Len <> Length(S1) then
    Result := False
  else
    Result := CompareMem(PChar(S1), P2, Len * SizeOf(Char));
end;

function BoldCaseIndependentPos(const Substr, S: string): Integer;
var
  SubstrLen: integer;
begin
  SubStrLen := Length(Substr);
  if SubstrLen > Length(S) then
    Result := 0
  else
  begin
    Result := Pos(Substr, S);
    if (Result = 0) or (Result > SubStrLen) then
      Result := Pos(AnsiUpperCase(Substr), AnsiUpperCase(S));
  end;
end;


procedure EnsureTrailing(var Str: String; ch: char);
begin
  if (length(str) > 0) and (str[length(str)] <> ch) then
    str := str + ch;
end;

function BooleanToString(BoolValue: Boolean): String;
begin
  if BoolValue then Result := 'True' else Result := 'False';
end;

function StringToBoolean(StrValue: String): Boolean;
begin
  Result := False;
  if (UpperCase(StrValue)= 'Y') or (UpperCase(StrValue) = 'T') or (UpperCase(StrValue) = 'TRUE') then
    Result := True;
end;

function BoldRootRegistryKey: string;
begin
  Result := Format('Software\BoldSoft\%s\%s',  [BoldProductNameShort,BoldProductVersion]);
end;

function GetModuleFileNameAsString(IncludePath: Boolean): string;
var
 Buffer: array[0..261] of Char;
 ModuleName: string;
begin
  SetString(ModuleName, Buffer, Windows.GetModuleFileName(HInstance,
        Buffer, SizeOf(Buffer)));
  if IncludePath then
    Result := ModuleName
  else
    Result := ExtractFileName(ModuleName);  
end;

procedure EnumToStrings(aTypeInfo: pTypeInfo; Strings: TStrings);
var
  i: integer;
  TypeData: pTypeData;
begin
  TypeData := GetTypeData(aTypeInfo);
  for i := TypeData.MinValue to TypeData.MaxValue do
    Strings.Add(GetEnumName(aTypeInfo, i));
end;

function BoldNamesEqual(const name1, name2: string): Boolean;
begin
  Result := (AnsiCompareText(name1, name2) = 0);
end;

function BoldSeparateStringList(strings: TStringList; const Separator, PreString, PostString: String; AIndex: integer): String;
{$IFDEF RIL}
var
  i, Cnt, Size: integer;
  SB: TStringBuilder;
begin
  Cnt := strings.Count;
  case Cnt of
    0: Result := '';
    1: Result := PreString + Strings[0] + PostString;
  else
  begin
    Size := length(PreString) + length(PostString);
    for I := 0 to Cnt - 1 do
      Inc(Size, Length(Strings[I]));
    Inc(Size, Length(Separator) * Cnt);
    SB := TStringBuilder.Create(Size);
    SB.Append(PreString);
    for i := 0 to Cnt-2 do
    begin
      //result := result + Strings[i] + Separator;
      SB.Append(Strings[i]);
      if AIndex <> -1 then
        SB.Append(IntToStr(AIndex));
      SB.Append(Separator);
    end;
    // result := result + Strings[Strings.Count - 1]
    SB.Append(Strings[Cnt-1]);
    if AIndex <> -1 then
      SB.Append(IntToStr(AIndex));
    { no need to check Cnt again it IS > 0 so: }
    // result := PreString + result +PostString;
    SB.Append(PostString);
    Result := SB.ToString;
    FreeAndNil(SB);
  end;
  end;
{$ELSE}
var
  i: integer;
begin
  result := '';
  if strings.Count > 0 then
  begin
    for i := 0 to strings.Count-2 do
    begin
      result := result + Strings[i];
      if AIndex <> -1 then
        result := result + IntToStr(AIndex);
      result := result + Separator;
    end;
    result := result + Strings[Strings.Count - 1];
    if AIndex <> -1 then
      result := result + IntToStr(AIndex);
  end
  else
    result := '';
  if strings.Count > 0 then
    result := PreString + result +PostString;
{$ENDIF}
end;

procedure BoldAppendToStrings(Strings: TStrings; const aString: string; const ForceNewLine: Boolean);
{$IFDEF RIL}
var
  StrCount, SplitterPos: Integer;
  SB: TStringBuilder;
  TempStr: string;
begin

  { replace all LFs and CRs in string with space chars. This is "Superfast" : }
  if (Pos(BOLDLF, aString)>0) or (Pos(BOLDCR, aString)>0) then
  begin
    SB := TStringBuilder.Create(aString);
    SB.Replace(BOLDCR, ' ');
    SB.Replace(BOLDLF, ' ');
    TempStr := SB.ToSTring;
    FreeAndNil(SB);
  end
  else
    TempStr := aString;

  Strings.BeginUpdate;
  try
    StrCount := Strings.Count-1;
    if (StrCount = -1) or ForceNewLine then
    begin
      Strings.Add(TempStr);
      Inc(StrCount);
    end
    else
      Strings[StrCount] := Strings[StrCount] + TempStr;

    { break lines into max 80 chars per line }
    while Length(Strings[StrCount]) > 80 do
    begin
      SplitterPos := 80;

      while (Pos(Strings[StrCount][SplitterPos],' ,=')=0) and (SplitterPos > 1)  do
        Dec(SplitterPos);

      Strings.Append(Copy(Strings[StrCount], SplitterPos + 1, 65536));
      Strings[StrCount] := Copy(Strings[StrCount], 1, SplitterPos);

      Inc(StrCount);
    end;
  finally
    Strings.EndUpdate;
  end;
{$ELSE}
var
  StrCount: integer;
  i: integer;
  TempStr: String;
  SplitterPos: Integer;
begin
  Strings.BeginUpdate;
  try
    TempStr := aString;
    for i := 1 to length(TempStr) do
      if CharInSet(TempStr[i], [BOLDLF, BOLDCR]) then
        TempStr[i] := ' ';
    StrCount := Pred(Strings.Count);
    if (StrCount = -1) or ForceNewLine then
    begin
      Strings.Add('');
      Inc(StrCount);
    end;
    Strings[StrCount] := Strings[StrCount] + TempStr;
    while length(Strings[StrCount]) > 80 do
    begin
      SplitterPos := 80;
      while not (Strings[StrCount][SplitterPos] in [' ', ',', '=']) and (SplitterPos > 1) do
        Dec(SplitterPos);
      Strings.Add(Copy(Strings[StrCount], SplitterPos + 1, 65536));
      Strings[Strcount] := Copy(Strings[StrCount], 1, SplitterPos);
      Inc(StrCount);
    end;
  finally
    Strings.EndUpdate;
  end;
{$ENDIF}
end;

function BoldSeparatedAppend(const S1, S2: string; const Separator: string = ','): string;
begin
  if S1 = '' then
    Result := S2
  else if S2 = '' then
    Result := S1
  else
    Result := S1 + Separator + S2;
end;

function BoldTrim(const S: string): string;
var
  I, L, OldL: Integer;
begin
  L := Length(S);
  OldL := L;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    if (I > 1) or (L < OldL) then
      Result := Copy(S, I, L - I + 1)
    else
      Result := S;
  end;
end;

{ TBoldPassThroughNotification }

constructor TBoldPassthroughNotifier.CreateWithEvent(NotificationEvent: TBoldNotificationEvent; Owner: TComponent = nil);
begin
  inherited create(Owner);
  fNotificationEvent := NotificationEvent;
end;

procedure TBoldPassthroughNotifier.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  fNotificationEvent(AComponent, Operation);
end;

function CapitalisedToSpaced(Capitalised: String): String;
var
  I: Integer;
  Start: Integer;
begin
  Result := '';
  Start := 1;
  for I := Start to Length(Capitalised) do
    if (I>1) and (Capitalised[I] >= 'A') and (Capitalised[I] <= 'Z')
    and not ((Capitalised[I - 1] >= 'A') and (Capitalised[I - 1] <= 'Z')) then
      Result := Result + ' ' + Capitalised[I]
    else
      Result := Result + Capitalised[I];
end;

function SpacedToCapitalised(Spaced: String): String;
var
  I: Integer;
begin
  for I:= 1 to Length(Spaced) do
    if Spaced[I] <> ' ' then
      Result := Result + Spaced[I];
end;

function GetLowerLimitForMultiplicity(const Multiplicity: String): Integer;
var
  p: Integer;
begin
  p := Pos('..', Multiplicity);
  if p = 0 then
    Result := StrToIntDef(Multiplicity, 0)
  else
    Result := StrToIntDef(Copy(Multiplicity, 1, p - 1), -1);
end;

function GetUpperLimitForMultiplicity(const Multiplicity: String): Integer;
var
  p: Integer;
begin

  if (Multiplicity = '') or (BoldTrim(Multiplicity) = '') then
    result := 1
  else
  begin
    p := Pos('..', Multiplicity);
    if p = 0 then
      Result := StrToIntDef(Multiplicity, MaxInt)
    else
      Result := StrToIntDef(Copy(Multiplicity, p + 2, MaxInt), MaxInt);
  end;
  if Result < 0 then
    result :=  MaxInt;
end;

function StringListToVarArray(List: TStringList): variant;
var
  Count, i: integer;
  varList: variant;
begin
  Count := List.Count;
  if Count = 0 then
    Result := UnAssigned
  else
  begin
    varList := VarArrayCreate([0,Count - 1],varOleStr);
    for i := 0 to Count - 1 do
      varList[i] := List[i];
    Result := varList;
  end;
end;

function IsLocalMachine(const Machinename: WideString): Boolean;
var
  MachName: string;
begin
  MachName:= BoldTrim(MachineName);
  Result := (MachName = '') or (AnsiCompareText(GetComputerNameStr, MachName) = 0);
end;
{$IFDEF MSWINDOWS}
function GetComputerNameStr: string;
var
  Size: DWORD;
  LocalMachine: array[0..MAX_COMPUTERNAME_LENGTH] of char;
begin
  Size := SizeOf(LocalMachine);
  GetComputerName(LocalMachine, Size);
  Result := LocalMachine;
end;
{$ENDIF}

{$IFDEF LINUX}
function GetComputerNameStr: string;
begin
  Result := 'MyMachine';
end;
{$ENDIF}

function TimeStampComp(const Time1, Time2: TTimeStamp): Integer;
var
  cTime1, cTime2: Real;
begin
  cTime1 := TimeStampToMSecs(Time1);
  cTime2 := TimeStampToMSecs(Time2);
  if (cTime1 = cTime2) then
    Result := 0
  else if (cTime1 > cTime2) then
    Result := 1
  else
    Result := -1;
end;

function StrToDateFmt(const ADateString: string; const ADateFormat: string;
  const ATimeFormat: string; const ADateSeparatorChar: char = '/';
  const ADateTimeSeperatorChar: char = ' '): TDateTime;
var
  sPreviousShortDateFormat: string;
  sPreviousDateSeparator: char;
  {$IFDEF BOLD_DELPHI28_OR_LATER}
  sPreviousShortTimeFormat: string;
  {$ENDIF}
begin
  sPreviousShortDateFormat := {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat;
  {$IFNDEF BOLD_DELPHI28_OR_LATER}
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat := ADateFormat;
  if ATimeFormat <> '' then
  begin
    {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat :=
      {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat + ADateTimeSeperatorChar + ATimeFormat;
  end;
  {$ELSE}
    FormatSettings.ShortDateFormat := ADateFormat;

    sPreviousShortTimeFormat := FormatSettings.LongTimeFormat;
    FormatSettings.LongTimeFormat := ATimeFormat;
  {$ENDIF}
  sPreviousDateSeparator := {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator;
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator := ADateSeparatorChar;

  try
    Result := StrToDateTime(ADateString);
  finally
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat := sPreviousShortDateFormat;
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator := sPreviousDateSeparator;

  {$IFDEF BOLD_DELPHI28_OR_LATER}
  FormatSettings.LongTimeFormat := sPreviousShortTimeFormat;
  {$ENDIF}
  end;
end;

function DateToStrFmt(const aDate: TDateTime; DateFormat: string; const DateSeparatorChar: char = '/'): String;
var
  PreviousShortDateFormat: string;
  PreviousDateSeparator: char;
begin
  PreviousShortDateFormat := {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat;
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat := DateFormat;
  PreviousDateSeparator := {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator;
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator := DateSeparatorChar;
  try
    Result := DateToStr(aDate);
  finally
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}ShortDateFormat := PreviousShortDateFormat;
  {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator := PreviousDateSeparator;
  end;
end;

function BoldVariantToStrings(V: OleVariant; Strings: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if VarIsArray(V) and (VarArrayDimCount(V) = 1) then
  begin
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    begin
      Strings.Add(V[I]);
      Inc(Result);
    end;
  end;
end;

function CharCount(c: char; const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = c then Inc(Result);
end;

function BoldParseFormattedDateList(const Value: String; const formats: TStrings; var date: TDateTime): Boolean;

  function IsLeapYear(Year: Word): Boolean;
  begin
    result := ((Year mod 4 = 0) and not (year mod 100 = 0)) or (year mod 400 = 0);
  end;

  function InternalTryToParse(Value: String; format: string; var date: TDateTime): Boolean;
  var
    i: integer;
    y, m, d: string;
    year, month, day: word;
    CurrentYear: Word;
  const
    DaysPerMonth: array[1..12] of integer = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  begin
    result := false;
    DecodeDate(now, year, month, day);
    CurrentYear := year;
    if length(value) = length(format) then
    begin
      for i := 1 to length(format) do
      begin
        case format[i] of
          'y': y := y + value[i];
          'm': m := m + value[i];
          'd': d := d + value[i]
          else if not (value[i] = format[i]) then
            exit;
        end;
        if CharInSet(format[i], ['y', 'm', 'd']) and
           (not CharInSet(value[i], ['0'..'9'])) then
        begin
          exit;
        end;
      end;
      if length(y) = 0 then
        year := CurrentYear
      else
      begin
        year := StrToInt(y);
        if length(y) = 2 then
        begin
          if year < ((CurrentYear + {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow) mod 100) then
            year := year + (CurrentYear div 100)*100
          else
            year := year + ((CurrentYear div 100)+1)*100;
        end
      end;
      month := StrToInt(m);
      day := StrToInt(d);
      if (month < 1) or (month > 12) then
        exit;

      if (day < 1) or (day > DaysPerMonth[month]) then
        exit;

      if (month = 2) and not IsLeapYear(year) and (day > 28) then
        exit;
      result := true;
      Date := EncodeDate(year, month, day);
    end;
  end;

var
  i: integer;
begin
  result := false;
  for i := 0 to formats.Count-1 do
  begin
    result := InternalTryToParse(Value, Formats[i], Date);
    if result then
      exit;
  end;
end;

function BoldParseFormattedDate(const value: String; const formats: array of string; var Date: TDateTime): Boolean;
var
  formatsList: TStringList;
  i: integer;
begin
  FormatsList := TStringLIst.Create;
  for i := 0 to high(formats) do
    FormatsList.Add(Formats[i]);
  result := BoldParseFormattedDateList(value, formatsList, Date);
  FormatsList.Free;
end;

initialization

finalization
  if CurrentProcess<>INVALID_HANDLE_VALUE then
    CloseHandle(CurrentProcess);
end.
