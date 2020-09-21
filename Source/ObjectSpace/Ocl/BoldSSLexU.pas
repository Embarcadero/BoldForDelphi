unit BoldSSLexU;

interface

uses
  Classes,
  WinTypes,
  WinProcs,
  BoldBase;

procedure SSAux;

function  SSHugeInc(ThePointer: Pointer; TheInc: Longint): Pointer;
procedure SSHugeCopy(TheCopyTo: Pointer; TheCopyFrom: Pointer; TheLength: Longint);

{$IFDEF SSAUX}
{$I SSAUXDEF.PAS}
{$ENDIF}

const
  SSHugeIncVal = 8;
  SSLexMaxRead = $7fffffff; {32767}
  SSLexMaxCopy = $7fffffff; {32767}
  SSLexConsumerBof = -1;
  SSLexMaxBlock = $7fffffff-1; {32528}
  SSLexStateInvalid = -1;
  SSLexBufferMax = $7fffffff; {32767}
  SSLexThreshold = 65536;
  SSLexStructMax = 65520;
  SSLexPairTable = $53534C58;
  SSLexFinalStateFlagsContextStart = $01;
  SSLexFinalStateFlagsStartOfLine  = $02;
  SSLexFinalStateFlagsPop          = $08;
  SSLexFinalStateFlagsFinal        = $10;
  SSLexFinalStateFlagsPush         = $20;
  SSLexFinalStateFlagsIgnore       = $40;
  SSLexFinalStateFlagsContextEnd   = $80;
  SSLexFinalStateFlagsReduce       = $100;

  SSLexMsgFileOpen = 'SSLex0101e: Error opening file, %s';
  SSLexMsgFileRead = 'SSLex0102e: Invalid file length or read error, %s';
  SSLexMsgLexemeLength = 'SSLex0103e: Lexeme too long, %ld';
  SSLexMsgBadTable = 'SSLex0104e: Invalid table, %s';
  SSLexMsgError = 'SSLex0105e: Invalid token, Line %ld, Offset %ld, %s';
  SSLexMsgBadList = 'SSLex0106e: Invalid expression list index, %ld';
  SSLexMsgMissingPart = 'SSLex0107e: Table or consumer missing, required';
  SSLexMsgFindResource = 'SSLex0108e: Unable to locate resource, %s';
  SSLexMsgBadResource = 'SSLex0109e: Bad resource format, %s';

  SSLifoMsgStackTop = 'SSLifo0002e: No top, stack empty';
  SSLifoMsgStackPop = 'SSLifo0004e: Cannot pop empty stack';

  SSLexMsgOutOfMemory = 'SSLex1000e: Out of memory';

type
  SSArrayOfPointer = array[ 0..(SSLexStructMax div sizeof(Pointer))] of Pointer;
  PSSArrayOfPointer = ^SSArrayOfPointer;


type
  SSStack = class(TBoldMemoryManagedObject)
  public
  Incr        : Integer;
  Size        : Integer;
  TopOfStack  : Integer;
  PArray      : PSSArrayOfPointer;

  constructor Create;
  procedure   Pop;
{  procedure   PopAll;}
  function    Top: Pointer;
  procedure   Push(ThePointer: Pointer);
  destructor  Destroy; override;
end;



type
  SSLexTableBase = record
  Size      : Longint;
  TableType : Longint;
  Reserved  : array[ 0..6] of Longint;
end;

type
  SSLexTableHeader = record
  TableType : Longint;
  Size      : Longint;
  Reserved0 : array[ 0..7] of Longint;
  Rows      : Longint;
  Push      : Longint;
  Index     : Longint;
  Final     : Longint;
  Reserved1 : array[ 0..7] of Longint;
end;

type
  SSLexTableRow = record
  Size      : Longint;
end;

type
  SSLexTableRowEntry = record
  StartPoint: Longint;
  EndPoint  : Longint;
  State     : Longint;
end;

type
  SSLexTableIndex = record
  Row       : Longint;
end;

type
  SSLexFinalState = record
  Token   : Longint;
  Push    : Longint;
  Flags   : Longint;
  Reserved: array[ 0..3] of Longint;
end;

type
  PSSLexTableBase = ^SSLexTableBase;
  PSSLexTableHeader = ^SSLexTableHeader;
  PSSLexTableRow = ^SSLexTableRow;
  PSSLexTableRowEntry = ^SSLexTableRowEntry;
  PSSLexTableIndex = ^SSLexTableIndex;
  PSSLexFinalState = ^SSLexFinalState;
  SSLexRowArray = array[ 0..(SSLexStructMax div sizeof(PSSLexTableRow))] of PSSLexTableRow;
  SSLexFinalArray = array[ 0..((SSLexStructMax div sizeof(SSLexFinalState))-1)] of SSLexFinalState;
  SSLexRowOffsetArray = array[ 0..(SSLexStructMax div sizeof(Longint))] of Longint;
  SSLexRowEntryArray = array[ 0..((SSLexStructMax div sizeof(SSLexTableRowEntry))-1)] of SSLexTableRowEntry;
  PSSLexRowArray = ^SSLexRowArray;
  PSSLexFinalArray = ^SSLexFinalArray;
  PSSLexRowOffsetArray = ^SSLexRowOffsetArray;
  PSSLexRowEntryArray = ^SSLexRowEntryArray;

type
  SSLexExpressionList = class(TBoldMemoryManagedObject)
  public
    Buffer      : Pointer;
    Size        : Longint;
    Index       : Longint;
    Final       : Longint;
    Rows        : Longint;
    PPRows      : PSSLexRowArray;
    PFinalStates: PSSLexFinalArray;

    constructor Create;
    constructor CreateBuffer(TheHeader: PSSLexTableHeader; TheFile: PChar);
    procedure   Open;
    destructor  Destroy; override;

    function LookupState(TheToken, TheState: Longint): Longint;
    function LookupFinal(TheState: Longint): PSSLexFinalState;
end;

type
  SSLexExpressionListStack = class(SSStack)
  public
  function  Top: SSLexExpressionList;
  procedure Push(TheList: SSLexExpressionList);
end;

type
  SSLexTable = class(TBoldMemoryManagedObject)
  public
    ExpressionLists: TList;
    NumLists       : Integer;

    constructor Create(FileName: String);
    constructor CreateResource(TheInstance: THandle; TheName, TheType: PChar);
    function    GetExpressionList(TheList: Longint): SSLexExpressionList;
    destructor  Destroy; override;
end;

type
  SSLexLexeme = class(TBoldMemoryManagedObject)
  public
    Buffer: PChar;
    Use   : Integer;
    Line  : Longint;
    Offset: Longint;
    Length: Longint;
    Token : Cardinal;  // was LongInt, changed to foor Delphi-hints

    constructor Create(TheData: PChar; TheLength, TheLine, TheOffset: Longint);
    procedure   RefInc;
    function    RefDec: Boolean;
    destructor  Destroy; override;
end;

type
  SSLexConsumerMode = (SSLexBinaryMode, SSLexTextMode);
  SSLexConsumer = class(TBoldMemoryManagedObject)
  public
    Buffer           : PChar;
    Bof              : Longint;
    Line             : Longint;
    Offset           : Longint;
    BuffLen          : Longint;
    BuffInc          : Longint;
    Current          : Longint;
    MarkLine         : Longint;
    MarkOffset       : Longint;
    ScanLine         : Longint;
    ScanOffset       : Longint;
    EndOfData        : Boolean;
    Start            : Longint;
    Index            : Integer;
    DataLen          : Longint;
    Mark             : Longint;
    MarkContext      : Longint;
    MarkContextLine  : Longint;
    MarkContextOffset: Longint;
    Mode             : SSLexConsumerMode;

    constructor Create(TheIncrement: Longint;
                  TheMode: SSLexConsumerMode);
    destructor  Destroy; override;

    function    Next: Boolean; virtual;
    function    NextBuffer: Longint; virtual;

    procedure   MarkFinal;
    procedure   FlushLexeme;
    procedure   FlushLexemeAll;
    procedure   SetContextFinal;
    procedure   MarkContextFinal;
    procedure   FlushStartOfLine;
    function    Lexeme: SSLexLexeme;
    function    LexemeAll: SSLexLexeme;
{    function    ShiftBuffer(var TheOffset, TheFill: Longint): Boolean;
    function    ExpandBuffer(var TheOffset, TheFill: Longint): Boolean;
    }
end;

type
  SSLexStringConsumer = class(SSLexConsumer)
  public
  constructor Create(TheString: PChar);
end;

type
  SSLexFileConsumer = class(SSLexConsumer)
  public
    Handle: Integer;

{    constructor  Create(TheFileName: PChar; TheLength, TheIncrement: Longint;
                   TheMode: SSLexConsumerMode);
    function     NextBuffer: Longint; override;
    function     ReadData(TheBuffer: PChar; TheLength: Longint): Longint;
    procedure    Close;
    destructor   Destroy; override;
 }
end;

type
  SSLex = class(TBoldMemoryManagedObject)
  public
  State   : Longint;
  Table   : SSLexTable;
  Consumer: SSLexConsumer;
  List    : SSLexExpressionList;
  Stack   : SSLexExpressionListStack;

  constructor Create(TheConsumer: SSLexConsumer; TheTable: SSLexTable);
{  procedure   Reset;}
  procedure   PopExpressionList;
  function    Next: SSLexLexeme;
  procedure   PushExpressionList(TheList: Longint);
  procedure   GotoExpressionList(TheList: Longint);
  procedure   ProcessExpressionList(ThePFinal: PSSLexFinalState);
{  function    IsCurrentExpressionList(TheList: Longint): Boolean;}

  function    Error: SSLexLexeme; virtual;
  function    Complete(TheToken: Longint): SSLexLexeme; virtual;
  function    TokenToString(TheToken: Longint): string; virtual;
  destructor  Destroy; override;

end;

implementation

uses
  BoldSSExcept,
  BoldCoreConsts,
  SysUtils;

constructor SSLexConsumer.Create(TheIncrement: Longint;
              TheMode: SSLexConsumerMode);
begin
  inherited Create;
  BuffInc := TheIncrement;
  ScanLine := 0;
  ScanOffset := 0;
  Line := 1;
  Offset := 1;
  Bof := SSLexConsumerBof;
  Mode := TheMode;
  Buffer := nil;
end;

destructor SSLexConsumer.Destroy;
begin
  if Buffer <> nil then
    FreeMem(Buffer, BuffLen);
  inherited Destroy;
end;

procedure SSLexConsumer.MarkFinal;
begin
  Mark := Index;
  MarkLine := ScanLine;
  MarkOffset := ScanOffset;
end;

procedure SSLexConsumer.MarkContextFinal;
begin
  MarkContext := Index;
  MarkContextLine := ScanLine;
  MarkContextOffset := ScanOffset;
end;

procedure SSLexConsumer.SetContextFinal;
begin
  Mark := MarkContext;
  MarkLine := MarkContextLine;
  MarkOffset := MarkContextOffset;
end;

procedure SSLexConsumer.FlushStartOfLine;
begin
  Inc(Start);
  Inc(Line);
  Dec(MarkLine);
  Dec(MarkContextLine);
  Offset := 1;
end;

procedure SSLexConsumer.FlushLexeme;
begin
  Start := Mark;
  Index := Mark;
  Line := Line + MarkLine;
  Offset := MarkOffset;
  ScanLine := 0;
  ScanOffset := MarkOffset;
end;

procedure SSLexConsumer.FlushLexemeAll;
begin
  Start := Index;
  Line := Line + ScanLine;
  Offset := ScanOffset;
  ScanLine := 0;
end;


function SSLexConsumer.Next: Boolean;
var
  NewLen: Integer;

begin
    if EndOfData = True then
      Result := False
    else
      begin
      if Bof <> 0 then
        begin
        Current := Bof;
        Bof := 0;
        Result := True;
        end
      else
        begin
        if Index >= DataLen then
          begin
          NewLen := NextBuffer;
          if NewLen = 0 then
            begin
            EndOfData := True;
            Result := False;
            end
          else
            begin
            DataLen := DataLen + NewLen;
            Result := True;
            end
          end
        else
          Result := True;


        if Result = True then
          begin
          Current := Longint(Buffer[ Index]);
          Inc(Index);
          if Current = Ord($0A) then
            begin
            Inc(ScanLine);
            ScanOffset := 1;
            end
          else
            Inc(ScanOffset);
          end
        end;
      end;

end;

{function SSLexConsumer.ShiftBuffer(var TheOffset, TheFill: Longint): Boolean;
var
  StartOfData: PChar;

begin
  if ((Start = 0) and (DataLen = 0)) then
    begin
    TheOffset := 0;
    TheFill := BuffLen;
    Result := False;
    end
  else if Start = 0 then
    Result := True
  else
    begin
    TheOffset := DataLen - Start;
    if Offset <> 0 then
      begin
      StartOfData := Buffer + Start;
      StrMove(Buffer, StartOfData, TheOffset);
      TheFill := BuffLen - TheOffset;
      Index := Index - Start;
      Mark := Mark - Start;
      MarkContext := MarkContext - Start;
      DataLen := DataLen - Start;
      Start := 0;
      Result := False;
      end
    else
      begin
      TheOffset := 0;
      DataLen := 0;
      Index := 0;
      Start := 0;
      TheFill := BuffLen;
      Result := False;
      end
    end
end;

function SSLexConsumer.ExpandBuffer(var TheOffset, TheFill: Longint): Boolean;
var
  NewBuff: PChar;
  NewLen : Longint;

begin
  if (BuffInc = 0) or (BuffLen >= SSLexBufferMax) then
    Result := True
  else
    begin
    NewLen := BuffLen + BuffInc;
    if NewLen > SSLexBufferMax then
      NewLen := SSLexBufferMax;
    GetMem(NewBuff, NewLen);
    StrMove(NewBuff, Buffer, BuffLen);
    TheOffset := DataLen;
    FreeMem(Buffer, BuffLen);
    Buffer := NewBuff;
    TheFill := NewLen - DataLen;
    BuffLen := NewLen;
    Result := False;
    end
end;
 }
constructor SSLexStringConsumer.Create(TheString: PChar);
begin
  inherited Create(0, SSLexBinaryMode);
  DataLen := StrLen(TheString);
  BuffLen := DataLen;
  GetMem(Buffer, BuffLen);
  StrMove(Buffer, TheString, BuffLen);
end;
(*
constructor SSLexFileConsumer.Create(TheFileName: PChar;
  TheLength, TheIncrement: Longint; TheMode: SSLexConsumerMode);
var
  Size         : Longint;
  NewException : SSException;

begin
  inherited Create(TheIncrement, TheMode);
  DataLen := 0;
  Mode := TheMode;
  BuffLen := TheLength;
  BuffInc := TheIncrement;
  Handle := _lopen(TheFileName, OF_READ);
  if Handle = -1 { HFILE_ERROR } then
    begin
    NewException := SSException.CreateName(SSExceptionLexFileOpen, SSLexMsgFileOpen, TheFileName);
    raise NewException;
    end
  else
    begin
    Size := _llseek(Handle, 0, 2);
    if Size < BuffLen then
      BuffLen := Size;
    if BuffLen > SSLexBufferMax then
      BuffLen := SSLexBufferMax;
    GetMem(Buffer, BuffLen);
    _llseek(Handle, 0, 0);
    DataLen := ReadData(Buffer, BuffLen);
    if (DataLen = 0) then
      Close;
    end
end;

function SSLexFileConsumer.ReadData(TheBuffer: PChar; TheLength: Longint): Longint;
var
  TestChar  : Byte;
  IntBuff   : PChar;
  i         : Integer;
  SaveLength: Integer;

begin
  Result := 0;
  if Mode = SSLexTextMode then
    begin
    GetMem(IntBuff, TheLength);
    SaveLength := TheLength;
    TheLength := _lread(Handle, IntBuff, TheLength);
    for i := 0 to TheLength - 1 do
      begin
      TestChar := PByteArray(IntBuff)^[ i];
      if (TestChar <> $0D) and (TestChar <> $1A) then
        begin
        PByteArray(TheBuffer)^[ 0] := PByteArray(IntBuff)^[ i];
        Inc(Result);
        TheBuffer := TheBuffer + 1;
        end;
      end;
    FreeMem(IntBuff, SaveLength);
    end
  else
    Result := _lread(Handle, TheBuffer, TheLength);
end;

procedure SSLexFileConsumer.Close;
begin
  if Handle <> 0 then
    begin
    _lclose(Handle);
    Handle := 0;
    end
end;

function SSLexFileConsumer.NextBuffer: Longint;
var
  TestChar: Char;
  TestLen: Longint;
  Offset, Fill: Longint;
  NewException: SSException;

begin
  if Handle <> 0 then
    begin
    if (ShiftBuffer(Offset, Fill) and ExpandBuffer(Offset, Fill)) then
      begin
      TestLen := _lread(Handle, PChar(@TestChar), 1);
      if TestLen <> 0 then
        begin
        NewException := SSException.CreateLong(SSExceptionLexLexemeLength, SSLexMsgLexemeLength, BuffLen);
        raise NewException;
        end
      else
        begin
        Close;
        Result := 0;
        end
      end
    else
      begin
      Result := ReadData(Buffer + Offset, Fill);
      if (Result = 0) then
        Close;
      end
    end
  else
    Result := 0;

end;

destructor SSLexFileConsumer.Destroy;
begin
  Close;
  inherited Destroy;
end;
*)

function SSLexConsumer.Lexeme: SSLexLexeme;
var
  Length   : Longint;
begin
   Length := Mark - Start;
   Result := SSLexLexeme.Create(Buffer + Start, Length, Line, Offset);
   FlushLexeme;
end;

function SSLexConsumer.LexemeAll: SSLexLexeme;
begin
  Result := SSLexLexeme.Create(Buffer + Start, Index - Start, Line, Offset);
  FlushLexemeAll;
end;


constructor SSLexLexeme.Create(TheData: PChar; TheLength, TheLine, TheOffset: Longint);
begin
  inherited Create;
  GetMem(Buffer, TheLength + 1);
  StrMove(Buffer, TheData, TheLength);
  PByteArray(Buffer)^[ TheLength] := $0;
  Length := TheLength;
  Line := TheLine;
  Offset := TheOffset;
  Use := 0;
end;

procedure SSLexLexeme.RefInc;
begin
  Assert(Use>=0);
  Inc(Use);
end;

function SSLexLexeme.RefDec: Boolean;
begin
  Dec(Use);
  Result := (Use = 0);
  Assert(Use >= 0);
end;

destructor SSLexLexeme.Destroy;
begin
  Assert(Use=0);
  if Buffer <> nil then
    FreeMem(Buffer, Length);
  inherited Destroy;
end;

function SSLexConsumer.NextBuffer: Longint;
begin
  Result := 0;
end;

function SSHugeInc(ThePointer: Pointer; TheInc: Longint): Pointer;
begin
  Result := PChar(ThePointer) + TheInc;
end;

constructor SSLexTable.Create(FileName: String);
var
  i           : Integer;
  Current     : Longint;
  FileHandle  : TFileStream;
  Base        : SSLexTableBase;
  Header      : SSLexTableHeader;
  ExprList    : SSLexExpressionList;

begin
  inherited Create;
  SSAux;
  ExpressionLists := nil;
  if not FileExists(FileName) then
    raise Exception.CreateFmt(sFileDoesNotExist, [FileName]);

  FileHandle := TFileStream.Create(FileName, fmOpenRead);
  FileHandle.Read(Base, sizeOf(Base));

  if Base.TableType <> SSLexPairTable then
    raise SSException.CreateName(SSExceptionLexBadTable, SSLexMsgBadTable, PChar(FileName));

  NumLists := Base.Size;
  ExpressionLists := TList.Create;
  for i := 0 to NumLists - 1 do
  begin
    Current := FileHandle.Position;
    FileHandle.Read(Header, SizeOf(Header));
    ExprList := SSLexExpressionList.Create;
    GetMem(ExprList.Buffer, Header.Size);
    ExprList.Size := Header.Size;
    ExprList.Final := Header.Final;
    ExprList.Index := Header.Index;
    ExprList.Rows := Header.Rows;
    FileHandle.Position := Current;
    FileHandle.Read(ExprList.Buffer^, ExprList.Size);
    ExprList.Open;
    ExpressionLists.Insert(i, ExprList);
  end;
  FreeAndNil(FileHandle);
end;

constructor SSLexTable.CreateResource(TheInstance: THandle; TheName, TheType: PChar);
var
  LoadHandle  : THandle;
  FindHandle  : THandle;
  Resource    : Pointer;
  i           : Integer;
  NewException: SSException;
  Base        : SSLexTableBase;
  Header      : SSLexTableHeader;
  ExprList    : SSLexExpressionList;

begin
  inherited Create;
  LoadHandle := 0; // to prevent compiler warning
  SSAux;
  Resource := nil;
  ExpressionLists := nil;
  FindHandle := FindResource(TheInstance, TheName, TheType);
  if (FindHandle <> 0) then
  begin
    LoadHandle := LoadResource(TheInstance, FindHandle);
    if (LoadHandle <> 0) then
      Resource := LockResource(LoadHandle);
  end;
  if (Resource = nil) then
  begin
    NewException := SSException.CreateName(SSExceptionLexFindResource, SSLexMsgFindResource, TheType);
    raise NewException;
    end
  else
  begin
    SSHugeCopy(@Base, Resource, sizeof(Base));
    if Base.TableType <> SSLexPairTable then
    begin
      NewException := SSException.CreateName(SSExceptionLexBadResource, SSLexMsgBadResource, TheType);
      raise NewException;
    end;
    NumLists := Base.Size;
    ExpressionLists := TList.Create;
    Resource := SSHugeInc(Resource, sizeof(Base));
    for i := 0 to NumLists - 1 do
    begin
      SSHugeCopy(@Header, Resource, sizeof(Header));
      ExprList := SSLexExpressionList.Create;
      Getmem(ExprList.Buffer, Header.Size);
      ExprList.Size := Header.Size;
      ExprList.Final := Header.Final;
      ExprList.Index := Header.Index;
      ExprList.Rows := Header.Rows;
      SSHugeCopy(ExprList.Buffer, Resource, ExprList.Size);
      ExprList.Open;
      ExpressionLists.Insert(i, ExprList);
      Resource := SSHugeInc(Resource, ExprList.Size);
    end;
    FreeResource(LoadHandle);
  end
end;

constructor SSLexExpressionList.CreateBuffer(TheHeader: PSSLexTableHeader; TheFile: PChar);
var
//  i           : Integer;
  NewException: SSException;
  Header      : SSLexTableHeader;
begin
  inherited Create;
  SSHugeCopy(@Header, TheHeader, sizeof(SSLexTableHeader));
  if Header.TableType <> SSLexPairTable then
    begin
    NewException := SSException.CreateName(SSExceptionLexBadTable, SSLexMsgBadTable, TheFile);
    raise NewException;
    end;
  GetMem(Buffer, Header.Size);  //Buffer := SSHugeAlloc(Header.Size, Handle);
  Size := Header.Size;
  Final := Header.Final;
  Index := Header.Index;
  Rows := Header.Rows;
  SSHugeCopy(Buffer, TheHeader, Size);
  Open;
end;

function SSLexTable.GetExpressionList(TheList: Longint): SSLexExpressionList;
var
  AnException: SSException;
begin
  if (TheList < 0) or (TheList >= ExpressionLists.Count) then
    begin
    AnException := SSException.CreateLong(SSExceptionBadList, SSLexMsgBadList, TheList);
    raise AnException;
    end;
  Result := ExpressionLists.Items[ TheList];
end;

procedure SSLexExpressionList.Open;
var
  i          : Integer;
  PRowOffsets: PSSLexRowOffsetArray;
begin
  PPRows := SSHugeInc(Buffer, Index);
  PRowOffsets := Pointer(PPRows);
  PFinalStates := SSHugeInc(Buffer, Final);
  for i := 0 to Rows - 1 do
    PPRows^[ i] := SSHugeInc(Buffer, PRowOffsets^[ i]);
end;

procedure SSHugeCopy(TheCopyTo: Pointer; TheCopyFrom: Pointer; TheLength: Longint);
begin
  Move(TheCopyFrom^, TheCopyTo^, TheLength);
end;


function SSLexExpressionList.LookupState(TheToken, TheState: Longint): Longint;
var
  i       : Integer;
  PRow    : PSSLexTableRow;
  PEntry  : PSSLexTableRowEntry;
  PEntries: PSSLexRowEntryArray;

begin
  Result := SSLexStateInvalid;
  PRow := PPRows^[ TheState];
  PEntries := PSSLexRowEntryArray(PChar(PRow) + sizeof(SSLexTableRow));
  for i := 0 to PRow^.Size - 1 do
    begin
    PEntry := @PEntries^[ i];
    if TheToken = SSLexConsumerBof then
      begin
      if PEntry^.StartPoint = SSLexConsumerBof then
        begin
        Result := PEntry^.State;
        Break;
        end
      end
    else if TheToken < PEntry^.StartPoint then
      Break
    else
      begin
      if TheToken <= PEntry^.EndPoint then
        begin
        Result := PEntry^.State;
        Break;
        end
      end;
    end;
end;

function SSLexExpressionList.LookupFinal(TheState: Longint): PSSLexFinalState;
begin
  Result := @PFinalStates^[ TheState];
end;

destructor SSLexTable.Destroy;
var
  i   : Integer;
  List: SSLexExpressionList;
begin
  if ExpressionLists <> nil then
    begin
    for i := 0 to ExpressionLists.Count - 1 do
      begin
      List := ExpressionLists.Items[ i];
      List.Free;
      end;
    end;
  ExpressionLists.Free;
  inherited Destroy;
end;

constructor SSLexExpressionList.Create;
begin
  inherited Create;
  Buffer := nil;
end;

destructor SSLexExpressionList.Destroy;
begin
  if Buffer <> nil then
    FreeMem(Buffer, Size);
  inherited Destroy;
end;

constructor SSLex.Create(TheConsumer: SSLexConsumer; TheTable: SSLexTable);
var
  AnException: SSException;
begin
  inherited Create;
  Stack := nil;
  if (TheTable = nil) or (TheConsumer = nil) then
    begin
    AnException := SSException.Create(SSExceptionMissingPart, SSLexMsgMissingPart);
    raise AnException;
    end;
  Table := TheTable;
  Consumer := TheConsumer;
  Stack := SSLexExpressionListStack.Create;
  PushExpressionList(0);
end;

{procedure SSLex.Reset;
begin
  Stack.PopAll;
  Stack.Push(Table.GetExpressionList(0));
end;
 }
procedure SSLex.PopExpressionList;
begin
  Stack.Pop;
  List := Stack.Top;
end;

function SSLex.Next: SSLexLexeme;
var
  Consumed    : Boolean;
  PFinal      : PSSLexFinalState;
  PTempFinal  : PSSLexFinalState;
begin
  result := nil;
  while True do
    begin
    State := 0;
    Result := nil;
    Consumed := False;
    PFinal := List.LookupFinal(State);
    while Consumer.Next do
      begin
      Consumed := True;
      State := List.LookupState(Consumer.Current, State);
      if State = SSLexStateInvalid then
        Break
      else
        begin
        PTempFinal := List.LookupFinal(State);
        if (PTempFinal^.Flags and SSLexFinalStateFlagsFinal) <> 0 then
          begin
          Consumer.MarkFinal;
          PFinal := PTempFinal;
          end;
        if (PTempFinal^.Flags and SSLexFinalStateFlagsContextStart) <> 0 then
          Consumer.MarkContextFinal;
        end;
      end;

    if Consumed = False then
      Break;

    if (PFinal^.Flags and SSLexFinalStateFlagsContextEnd) <> 0 then
      Consumer.SetContextFinal;

    if (PFinal^.Flags and SSLexFinalStateFlagsIgnore) <> 0 then
      begin
      Consumer.FlushLexeme;
      ProcessExpressionList(PFinal);
      Continue;
      end;

    if (PFinal^.Flags and SSLexFinalStateFlagsFinal) = 0 then
      begin
      Result := Error;
      if Result <> nil then
        Break
      else
        begin
        Consumer.FlushLexemeAll;
        Continue;
        end;
      end;

    ProcessExpressionList(PFinal);
    if (PFinal^.Flags and SSLexFinalStateFlagsStartOfLine) <> 0 then
      if (Consumer.Line <> 1) and (Consumer.Offset <> 1) then
        Consumer.FlushStartOfLine;

    Result := Complete(PFinal^.Token);
    if Result <> nil then
      Break
    end;

end;

procedure SSLex.ProcessExpressionList(ThePFinal: PSSLexFinalState);
begin
  if ((ThePFinal^.Flags and SSLexFinalStateFlagsPop) <> 0) and
     ((ThePFinal^.Flags and SSLexFinalStateFlagsPush) <> 0) then
       GotoExpressionList(ThePFinal^.Push)
  else if (ThePFinal^.Flags and SSLexFinalStateFlagsPop) <> 0 then
    PopExpressionList
  else if (ThePFinal^.Flags and SSLexFinalStateFlagsPush) <> 0 then
    PushExpressionList(ThePFinal^.Push);
end;

procedure SSLex.PushExpressionList(TheList: Longint);
begin
  Stack.Push(Table.GetExpressionList(TheList));
  List := Stack.Top;
end;

procedure SSLex.GotoExpressionList(TheList: Longint);
begin
  Stack.Pop;
  PushExpressionList(TheList);
end;

{function SSLex.IsCurrentExpressionList(TheList: Longint): Boolean;
begin
  if Table.GetExpressionList(TheList) = Stack.Top then
    Result := True
  else
    Result := False;
end;
}

function SSLex.Error: SSLexLexeme;
var
  Lexeme     : SSLexLexeme;
  AnException: SSException;
begin
  Lexeme := Consumer.LexemeAll;
  AnException := SSException.CreateLongLongNameLen(SSExceptionLexError,
    SSLexMsgError, Lexeme.Line, Lexeme.Offset, Lexeme.Buffer, Lexeme.Length);
  raise AnException;
end;

function SSLex.TokenToString(TheToken: Longint): String;
begin
  result := '';
end;

function SSLex.Complete(TheToken: Longint): SSLexLexeme;
begin
  Result := Consumer.Lexeme;
  Result.Token := TheToken;
end;

destructor SSLex.Destroy;
begin
  Stack.Free;
  inherited Destroy;
end;

function SSLexExpressionListStack.Top: SSLexExpressionList;
begin
  Result := inherited Top;
end;

procedure SSLexExpressionListStack.Push(TheList: SSLexExpressionList);
begin
  inherited Push(TheList);
end;

constructor SSStack.Create;
begin
  inherited Create;
  Size := 32;
  Incr := 32;
  GetMem(PArray, Size * sizeof(Pointer));
  TopOfStack := 0;
end;

procedure SSStack.Pop;
var
  AnException: SSException;
begin
  if TopOfStack = 0 then
    begin
    AnException := SSException.Create(SSExceptionStackPop, SSLifoMsgStackPop);
    raise AnException;
    end;
  Dec(TopOfStack);
end;

{procedure SSStack.PopAll;
begin
  TopOfStack := 0;
end;
 }
function SSStack.Top: Pointer;
var
  AnException: SSException;
begin
  if TopOfStack = 0 then
    begin
    AnException := SSException.Create(SSExceptionStackTop, SSLifoMsgStackTop);
    raise AnException;
    end;
  Result := PArray^[ TopOfStack - 1];
end;

procedure SSStack.Push(ThePointer: Pointer);
var
  i        : Integer;
  NewSize  : Integer;
  PNewArray: PSSArrayOfPointer;
begin
  if TopOfStack >= Size then
    begin
    NewSize := Size + Incr;
    GetMem(PNewArray, NewSize * sizeof(Pointer));
    for i := 0 to TopOfStack - 1 do
      PNewArray^[ i] := PArray^[ i];
    FreeMem(PArray, Size * sizeof(Pointer));
    PArray := PNewArray;
    Size := NewSize;
    end;

  PArray^[ TopOfStack] := ThePointer;
  Inc(TopOfStack);
end;

destructor SSStack.Destroy;
begin
  FreeMem(PArray, Size * sizeof(Pointer));
  inherited;
end;

{$IFDEF SSAUX}
{$I SSAUXFUN.PAS}
{$ELSE}
procedure SSAux;
begin
end;
{$ENDIF}

end.



