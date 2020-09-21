unit BoldSSYaccU;

{$RANGECHECKS OFF}

interface

uses
  Classes,
  WinTypes,
  WinProcs,
  BoldSSLexU,
  BoldBase;

const
  SSYaccPairTableMaxSize = 65528;
  SSYaccPairTableId = $53535943;
  SSYaccPairTableRowFlagsSync = $0001;
  SSYaccPairTableRowFlagsError = $0002;
  SSYaccPairTableRowFlagsSyncAll = $0004;
  SSYaccPairTableRowFlagsDuplicate = $0008;
  SSYaccPairTableRowFlagsSyncPossible = $0005;
  SSYaccPairTableEntryMask = $f8000000;
  SSYaccPairTableEntrySync = $80000000;
  SSYaccPairTableEntryShift = $40000000;
  SSYaccPairTableEntryReduce = $20000000;
  SSYaccPairTableEntryAccept = $10000000;
  SSYaccPairTableEntryConflict = $08000000;
  SSYaccPairTableEntryMax = $07ffffff;
  SSYaccEofString = 'eof';
  SSYaccErrorString = 'error';
  SSYaccEofToken = $ffffffff;
  SSYaccErrorToken = $fffffffe;
  SSYaccMsgFileOpen = 'SSYacc0101e: Open file failed, %s';
  SSYaccMsgParse = 'SSYacc0102e: Parse failed, invalid table';
  SSYaccMsgEof = 'SSYacc0103e: Attempted read past eof';
  SSYaccMsgMissingTable = 'SSYacc0109e: Table missing, required';
  SSYaccMsgMissingLexer = 'SSYacc0110e: Lexer missing, required';
  SSYaccMsgRead = 'SSYacc0111e: Error reading table, %s';
  SSYaccMsgTableSize = 'SSYacc0112e: Table too big';
  SSYaccMsgElement = 'SSYacc0108e: Invalid index on ElementFromxxx';
  SSYaccMsgSyncErrToken = 'SSYacc0105e: SyncErr failed, no valid token';
  SSYaccMsgSyncErrEof = 'SSYacc0106e: SyncErr failed, eof';
  SSYaccMsgFindResource = 'SSYacc0107e: Unable to locate resource, %s';
  SSYaccMsgBadResource = 'SSYacc0108e: Invalid resource, %s';

type
  SSArrayOfLongint = array[ 0..(SSLexStructMax div sizeof(Longint))] of Longint;
  PSSArrayOfLongint = ^SSArrayOfLongint;

type
  SSSetOfLongint = class(TBoldMemoryManagedObject)
  public
  Incr   : Longint;
  Size   : Longint;
  Count  : Longint;
  PArray : PSSArrayOfLongint;

  constructor Create(TheSize, TheInc: Longint);
  function    Insert(TheItem: Longint): Boolean;
  function    Contains(TheItem: Longint): Boolean;
  destructor  Destroy; override;
end;

type
  SSYaccPairTable = record
  TableType : Longint;
  Prods     : Longint;
  States    : Longint;
  RowOffset : Longint;
  ProdOffset: Longint;
end;

type
  SSYaccPairTableRow = record
  Flags     : Longint;
  Gotos     : Longint;
  Actions   : Longint;
end;

type
  SSYaccPairTableEntry = record
  Entry     : Longint;
  Token     : Longint;
end;

type
  SSYaccPairTableProd = record
  Size      : Longint;
  Leftside  : Longint;
end;

type
  SSYaccAction = (ShiftAction, ErrorAction, ReduceAction, AcceptAction, ConflictAction);
  PSSYaccPairTable = ^SSYaccPairTable;
  PSSYaccPairTableRow = ^SSYaccPairTableRow;
  PSSYaccPairTableProd = ^SSYaccPairTableProd;
  PSSYaccPairTableEntry = ^SSYaccPairTableEntry;
  SSYaccEntryArray = array[ 0..(SSLexStructMax div sizeof(SSYaccPairTableEntry))] of SSYaccPairTableEntry;
  PSSYaccEntryArray = ^SSYaccEntryArray;
  SSYaccProdArray = array[ 0..(SSLexStructMax div sizeof(SSYaccPairTableProd))] of SSYaccPairTableProd;
  PSSYaccProdArray = ^SSYaccProdArray;
  SSYaccRowArray = array[ 0..(SSLexStructMax div sizeof(PSSYaccPairTableRow))] of PSSYaccPairTableRow;
  SSYaccRowOffsetArray = array[ 0..(SSLexStructMax div sizeof(Longint))] of Longint;
  PSSYaccRowArray = ^SSYaccRowArray;
  PSSYaccRowOffsetArray = ^SSYaccRowOffsetArray;

type
  SSYaccStackElement = class(TBoldMemoryManagedObject)
  public
  Lexeme: SSLexLexeme;
  State : Longint;
  Use   : Longint;

  constructor Create;
  procedure   RefInc;
  function    RefDec: Boolean;
  procedure   SetLexeme(TheLexeme: SSLexLexeme);
  destructor  Destroy; override;
end;

type
  SSYaccStack = class(SSStack)
  public
  function    Top: SSYaccStackElement;
  procedure   Push(TheElement: SSYaccStackElement);
  destructor  Destroy; override;
end;

type
  SSYaccTable = class(TBoldMemoryManagedObject)
  public
  LarTableList: TList;
  Size        : Longint;
  PPRowArray  : PSSYaccRowArray;
  PTable      : PSSYaccPairTable;
  PProdArray  : PSSYaccProdArray;

  constructor Create(FileName: String);
  constructor CreateResource(TheInstance: THandle; TheName, TheType: PChar);
  function    LarTables: Longint;
  function    Productions: Longint;
  function    RowSize(TheRow: PSSYaccPairTableRow): Longint;
  procedure   GetLarTables(TheBuffer: Pointer; TheNumber: Word; TheFile: PChar);
  destructor  Destroy; override;
end;

type
  SSYaccLexemeCache = class(TList)
  public

  constructor Create;
  function    Dequeue: SSLexLexeme;
  procedure   Enqueue(TheLexeme: SSLexLexeme);
  function    Get(TheIndex: Longint): SSLexLexeme;
  destructor  Destroy; override;
end;

type
  SSYacc = class(TBoldMemoryManagedObject)
  public
  Lex              : SSLex;
  ErrorInd         : Boolean;
  AbortInd         : Boolean;
  EndOfInput       : Boolean;
  Cache            : Longint;
  State            : Longint;
  ShiftedSinceError: Longint;
  Leftside         : Longint;
  Production       : Longint;
  ProductionSize   : Longint;
  Table            : SSYaccTable;
  Lookahead        : SSLexLexeme;
  LarLookahead     : SSLexLexeme;
  Stack            : SSYaccStack;
  EndLexeme        : SSLexLexeme;
  Action           : SSYaccAction;
  LexemeCache      : SSYaccLexemeCache;
  Element          : SSYaccStackElement;
  ExprList         : SSLexExpressionList;

{  constructor Create(TheTable: SSTable);}
  constructor CreateLex(TheLexer: SSLex; TheTable: SSYaccTable);
{  procedure   Reset;}
  procedure   SetEof;
  procedure   SyncErr;
  function    Parse: Boolean;
  function    DoShift: Boolean;
  function    DoReduce: Boolean;
  function    DoConflict: Boolean;
  function    DoLarError: Boolean;
{  procedure   SetLex(TheLex: SSLex);}
  procedure   Pop(TheNumber: Longint);
  procedure   DoGoto(TheGoto: Longint);
  function    GetLexemeCache: SSLexLexeme;
  procedure   LookupGoto(TheGoto: Longint);
  procedure   LookupAction(TheToken: Longint);
  function    GetLexeme(Look: Boolean): Boolean;
  function    DoGetLexeme(Look: Boolean): Boolean;
  procedure   SetLookahead(TheLexeme: SSLexLexeme);
  procedure   SetLarLookahead(TheLexeme: SSLexLexeme);
  procedure   SetElement(TheElement: SSYaccStackElement);
  function    GetAction(PEntry: PSSYaccPairTableEntry): SSYaccAction;
{  function    ElementFromStack(TheDepth: Longint): SSYaccStackElement;}
  function    ElementFromProduction(TheIndex: Longint): SSYaccStackElement;
{  function    ValidLookaheads(TheState: Longint; var TheCount: Longint): PSSArrayOfLongint;}

  function    NextLexeme: SSLexLexeme; virtual;
  function    Shift: SSYaccStackElement; virtual;
  function    StackElement: SSYaccStackElement; virtual;
  function    LarLook(TheLexeme: SSLexLexeme): Boolean; virtual;
  function    Error(TheState: Longint; TheLookahead: SSLexLexeme): Boolean; virtual;
  function    LarError(TheState: Longint; TheLookahead, TheLarLookahead: SSLexLexeme): Boolean; virtual;
  function    Reduce(TheProduction, TheProductionSize: Longint): SSYaccStackElement; virtual;

  destructor  Destroy; override;
end;

implementation

uses
  BoldSSExcept,
  SysUtils,
  BoldCoreConsts,
  BoldUtils;

constructor SSYaccTable.Create(FileName: String);
var
  i              : Word;
  FileStream: TFileStream;
  BaseSize       : Longint;
  PLarTables     : Pointer;
  PRowOffsetArray: PSSYaccRowOffsetArray;
begin
  SSAux;

  if not FileExists(FileName) then
    raise Exception.CreateFmt(sFileDoesNotExist, [FileName]);

  FileStream := TFileStream.Create(FileName, fmOpenRead);

  Size := FileStream.Size;

  Getmem(PTable, FileStream.Size);
  FileStream.Read(PTable^, Size);
  FileStream.Free;

  if PTable^.TableType <> SSYaccPairTableId then
  begin
    FreeMem(Ptable, Size);
    PTable := nil;
    raise SSException.CreateName(SSExceptionYaccRead, SSYaccMsgRead, PChar(FileName));
  end;

  BaseSize := sizeof(SSYaccPairTable) + sizeof(SSYaccPairTableProd) *
    Productions + sizeof(PSSYaccPairTableRow) * PTable^.States;
  if BaseSize > SSYaccPairTableMaxSize then
  begin
    FreeMem(PTable, Size);
    PTable := nil;
    raise SSException.Create(SSExceptionYaccTableSize, SSYaccMsgTableSize);
  end;

  PProdArray := PSSYaccProdArray(SSHugeInc(PTable, PTable^.ProdOffset));
  PPRowArray := PSSYaccRowArray(SSHugeInc(PTable, sizeof(SSYaccPairTable)));
  PRowOffsetArray := PSSYaccRowOffsetArray(PPRowArray);
  PLarTables := SSHugeInc(PTable, PTable^.RowOffset);
  for i := 0 to PTable^.States - 1 do
    begin
    PPRowArray^[ i] := PSSYaccPairTableRow(SSHugeInc(PTable, PRowOffsetArray^[ i]));
    PLarTables := SSHugeInc(PPRowArray^[ i], RowSize(PPRowArray^[ i]));
    end;

  if (LarTables > 0) then
    GetLarTables(PLarTables, LarTables, PChar(Filename));
end;

constructor SSYaccTable.CreateResource(TheInstance: THandle; TheName, TheType: PChar);
var
  i              : Word;
  FindHandle     : THandle;
  LoadHandle     : THandle;
  Resource       : Pointer;
  BaseSize       : Longint;
  PLarTables     : Pointer;
  NewException   : SSException;
  PRowOffsetArray: PSSYaccRowOffsetArray;
begin
  SSAux;
  LoadHandle := 0; // to prevent compiler warning
  Resource := nil;
  FindHandle := FindResource(TheInstance, TheName, TheType);
  if (FindHandle <> 0) then
    begin
    LoadHandle := LoadResource(TheInstance, FindHandle);
    if (LoadHandle <> 0) then
      Resource := LockResource(LoadHandle);
    end;
  if (Resource = nil) then
    begin
    NewException := SSException.CreateName(SSExceptionYaccFindResource, SSYaccMsgFindResource, TheType);
    raise NewException;
    end;

  Size := SizeOfResource(TheInstance, FindHandle);
  GetMem(PTable, Size);
  SSHugeCopy(PTable, Resource, Size);
  FreeResource(LoadHandle);

  if PTable^.TableType <> SSYaccPairTableId then
    begin
    FreeMem(PTable, Size);
    PTable := nil;
    NewException := SSException.CreateName(SSExceptionYaccBadResource, SSYaccMsgBadResource, TheType);
    raise NewException;
    end;

  BaseSize := sizeof(SSYaccPairTable) + sizeof(SSYaccPairTableProd) *
    Productions + sizeof(PSSYaccPairTableRow) * PTable^.States;
  if BaseSize > SSYaccPairTableMaxSize then
    begin
    FreeMem(PTable, Size);
    PTable := nil;
    NewException := SSException.Create(SSExceptionYaccTableSize, SSYaccMsgTableSize);
    raise NewException;
    end;

  PProdArray := PSSYaccProdArray(SSHugeInc(PTable, PTable^.ProdOffset));
  PPRowArray := PSSYaccRowArray(SSHugeInc(PTable, sizeof(SSYaccPairTable)));
  PRowOffsetArray := PSSYaccRowOffsetArray(PPRowArray);
  PLarTables := SSHugeInc(PTable, PTable^.RowOffset);
  for i := 0 to PTable^.States - 1 do
    begin
    PPRowArray^[ i] := PSSYaccPairTableRow(SSHugeInc(PTable, PRowOffsetArray^[ i]));
    PLarTables := SSHugeInc(PPRowArray^[ i], RowSize(PPRowArray^[ i]));
    end;

  if (LarTables > 0) then
    GetLarTables(PLarTables, LarTables, TheType);
end;

function SSYaccTable.RowSize(TheRow: PSSYaccPairTableRow): Longint;
var
  Entries: Longint;
begin
  Entries := TheRow^.Gotos + TheRow^.Actions;
  if (TheRow^.Flags and SSYaccPairTableRowFlagsError) <> 0 then
    Inc(Entries);
  Result := SizeOf(SSYaccPairTableEntry) * Entries + SizeOf(SSYaccPairTableRow);
end;

procedure SSYaccTable.GetLarTables(TheBuffer: Pointer; TheNumber: Word; TheFile: PChar);
var
  i       : Word;
  Header  : SSLexTableHeader;
  ExprList: SSLexExpressionList;
begin
  LarTableList := TList.Create;
  for i := 0 to TheNumber - 1 do
    begin
    SSHugeCopy(@Header, TheBuffer, sizeof(Header));
    ExprList := SSLexExpressionList.CreateBuffer(TheBuffer, TheFile);
    LarTableList.Insert(i, ExprList);
    TheBuffer := SSHugeInc(TheBuffer, Header.Size);
    end
end;

function SSYaccTable.Productions: Longint;
begin
  Result := PTable^.Prods and $0000ffff;
end;

function SSYaccTable.LarTables: Longint;
begin
  Result := PTable^.Prods shr 16;
end;

destructor SSYaccTable.Destroy;
begin
  if PTable <> nil then
    FreeMem(Ptable, Size);
  inherited;
end;

constructor SSYaccStackElement.Create;
begin
  inherited Create;
  Lexeme := nil;
  State := 0;
  Use := 0;
end;

procedure SSYaccStackElement.SetLexeme(TheLexeme: SSLexLexeme);
begin
  if TheLexeme <> nil then
    TheLexeme.RefInc;
  if (Lexeme <> nil) and (Lexeme.RefDec) then
    Lexeme.Free;
  Lexeme := TheLexeme;
end;

procedure SSYaccStackElement.RefInc;
begin
  Inc(Use);
end;

function SSYaccStackElement.RefDec: Boolean;
begin
  Dec(Use);
  Result := (Use = 0);
end;

destructor SSYaccStackElement.Destroy;
begin
  if (Lexeme <> nil) and (Lexeme.RefDec) then
    Lexeme.Free;
  inherited;
end;

function SSYaccStack.Top: SSYaccStackElement;
begin
  Result := inherited Top;
end;

procedure SSYaccStack.Push(TheElement: SSYaccStackElement);
begin
  TheElement.RefInc;
  inherited Push(TheElement);
end;

destructor SSYaccStack.Destroy;
var
  i      : Integer;
  Element: SSYaccStackElement;
begin
  for i := 0 to TopOfStack - 1 do
    begin
    Element := PArray^[ i];
    if Element.RefDec then
      Element.Free;
    end;
  inherited Destroy;
end;

{constructor SSYacc.Create(TheTable: SSYaccTable);
var
  AnException: SSException;
begin
  inherited Create;
  EndLexeme := nil;
  Stack := nil;
  Lex := nil;
  EndOfInput := False;
  ErrorInd := False;
  ShiftedSinceError := 0;
  AbortInd := False;
  if TheTable = nil then
    begin
    AnException := SSException.Create(SSExceptionYaccMissingTable, SSYaccMsgMissingTable);
    raise AnException;
    end;
  Table := TheTable;
  Stack := SSYaccStack.Create;
  Element := StackElement;
  Element.RefInc;
  Stack.Push(Element);
  LexemeCache := SSYaccLexemeCache.Create;
end;
}
constructor SSYacc.CreateLex(TheLexer: SSLex; TheTable: SSYaccTable);
var
  AnException: SSException;
begin
  inherited Create;
  EndLexeme := nil;
  Stack := nil;
  Lex := nil;
  EndOfInput := False;
  ErrorInd := False;
  ShiftedSinceError := 0;
  AbortInd := False;
  if TheTable = nil then
    begin
    AnException := SSException.Create(SSExceptionYaccMissingTable, SSYaccMsgMissingTable);
    raise AnException;
    end;
  Lex := TheLexer;
  Table := TheTable;
  Stack := SSYaccStack.Create;
  Element := StackElement;
  Element.RefInc;
  Stack.Push(Element);
  LexemeCache := SSYaccLexemeCache.Create;
end;

procedure SSYacc.SetEof;
var
  EofString: PChar;
begin
  EndOfInput := True;
  EofString := StrNew(SSYaccEofString);
  EndLexeme := SSLexLexeme.Create(EofString, StrLen(EofString), 0, 0);
  EndLexeme.Token := SSYaccEofToken;
  EndLexeme.RefInc;
  StrDispose(EofString);
end;

{procedure SSYacc.SetLex(TheLex: SSLex);
begin
  Lex := TheLex;
end;
 }
procedure SSYacc.LookupAction(TheToken: Longint);
var
  i          : Integer;
  PEntryArray: PSSYaccEntryArray;
  PCurrentRow: PSSYaccPairTableRow;
  PProd      : PSSYaccPairTableProd;
  PEntry     : PSSYaccPairTableEntry;
begin
  PEntry := nil;
  PCurrentRow := Table.PPRowArray^[ State];
  PEntryArray := PSSYaccEntryArray(PChar(PCurrentRow) + sizeof(SSYaccPairTableRow));
  for i := 0 to PCurrentRow^.Actions - 1 do
    if PEntryArray^[ i].Token = TheToken then
      begin
      PEntry := @PEntryArray^[ i];
      Break;
      end;

  if PEntry = nil then
    Action := ErrorAction
  else
    begin
    Action := GetAction(PEntry);
    case Action of
      ShiftAction:
        State := PEntry^.Entry and SSYaccPairTableEntryMax;
      ReduceAction:
        begin
        Production := PEntry^.Entry and SSYaccPairTableEntryMax;
        PProd := @Table.PProdArray^[ Production];
        Leftside := PProd^.Leftside;
        ProductionSize := PProd^.Size;
        end;
      ConflictAction:
        begin
        ExprList := Table.LarTableList.Items[ PEntry^.Entry and SSYaccPairTableEntryMax];
        end;

    end;
    end;
end;

function SSYacc.GetAction(PEntry: PSSYaccPairTableEntry): SSYaccAction;
begin
  if (PEntry^.Entry and SSYaccPairTableEntryShift) <> 0 then
    Result := ShiftAction
  else if (PEntry^.Entry and SSYaccPairTableEntryReduce) <> 0 then
    Result := ReduceAction
  else if (PEntry^.Entry and SSYaccPairTableEntryAccept) <> 0 then
    Result := AcceptAction
  else if (PEntry^.Entry and SSYaccPairTableEntryConflict) <> 0 then
    Result := ConflictAction
  else
    Result := ErrorAction;
end;

function SSYacc.LarError(TheState: Longint; TheLookahead, TheLarLookahead: SSLexLexeme): Boolean;
begin
  Result := Error(TheState, TheLookahead);
end;

function SSYacc.Reduce(TheProduction, TheProductionSize: Longint): SSYaccStackElement;
begin
  result := nil;
end;


function SSYacc.LarLook(TheLexeme: SSLexLexeme): Boolean;
begin
  Result := False;
end;

function SSYacc.DoLarError: Boolean;
begin
  ErrorInd := True;
  Result := LarError(State, Lookahead, LarLookahead);
  if (Result = False) then
    ShiftedSinceError := 0;
end;

procedure SSYacc.LookupGoto(TheGoto: Longint);
var
  i          : Integer;
  EndIndex   : Longint;
  AnException: SSException;
  PEntryArray: PSSYaccEntryArray;
  PCurrentRow: PSSYaccPairTableRow;
  PEntry     : PSSYaccPairTableEntry;
begin
  PEntry := nil;
  PCurrentRow := Table.PPRowArray^[ State];
  EndIndex := PCurrentRow^.Actions + PCurrentRow^.Gotos - 1;
  PEntryArray := PSSYaccEntryArray(PChar(PCurrentRow) + sizeof(SSYaccPairTableRow));
  for i := PCurrentRow^.Actions to EndIndex do
    if PEntryArray^[ i].Token = TheGoto then
      begin
      PEntry := @PEntryArray^[ i];
      Break;
      end;

  if PEntry = nil then
    begin
    AnException := SSException.Create(SSExceptionYaccParse, SSYaccMsgParse);
    raise AnException;
    end;

  State := PEntry^.Entry and SSYaccPairTableEntryMax;
end;

procedure SSYacc.DoGoto(TheGoto: Longint);
begin
  LookupGoto(Leftside);
  Element.State := State;
  Stack.Push(Element);
  LookupAction(Lookahead.Token);
end;

function SSYacc.NextLexeme: SSLexLexeme;
var
  AnException: SSException;
begin
  if Lex = nil then
    begin
    AnException := SSException.Create(SSExceptionYaccMissingLexer, SSYaccMsgMissingLexer);
    raise AnException;
    end;
  Result := Lex.Next;
end;

function SSYacc.StackElement: SSYaccStackElement;
begin
  Result := SSYaccStackElement.Create;
end;

function SSYacc.Parse: Boolean;
var
  AnException: SSException;
//  AnElement  : SSYaccStackElement;
begin
  Result := DoGetLexeme(True);
  if Result = True then Exit;

  while True do
    begin
    if AbortInd = True then
      Break;
    case Action of
      ShiftAction:
        begin
        Result := DoShift;
        if (Result = True) then Break;
        end;

      ReduceAction:
        begin
        Result := DoReduce;
        if (Result = True) then Break;
        end;

      ErrorAction:
        begin
        ErrorInd := True;
        if Error(State, Lookahead) then
          begin
          Result := True;
          Break;
          end;
        ShiftedSinceError := 0;
        end;

      ConflictAction:
        begin
        Result := DoConflict;
        if (Result = True) then Break;
        end;

      AcceptAction:
        begin
        Result := ErrorInd;
        Break;
        end;

      else
        begin
        AnException := SSException.Create(SSExceptionYaccParse, SSYaccMsgParse);
        raise AnException;
        end;
      end;
    end;
end;

function SSYacc.DoShift: Boolean;
begin
  SetElement(Shift);
  if Element = nil then
    Result := True
  else
    begin
    Element.SetLexeme(Lookahead);
    Element.State := State;
    Stack.Push(Element);
    Result := DoGetLexeme(True);
    if Result = False then
      Inc(ShiftedSinceError);
    end;
end;

function SSYacc.DoReduce: Boolean;
var
  AnElement: SSYaccStackElement;
begin
  Result := False;
  AnElement := Reduce(Production, ProductionSize);
  SetElement(AnElement);
  if Element = nil then
    Result := True
  else
    begin
    Pop(ProductionSize);
    DoGoto(Leftside);
    end;
end;

function SSYacc.GetLexemeCache: SSLexLexeme;
begin
  Result := nil;
  if (Cache <> MaxLongInt) then
    begin
    Result := LexemeCache.Get(Cache);
    Inc(Cache);
    end;
  if (Result = nil) then
    begin
    Cache := MaxLongInt;
    Result := NextLexeme;
    if (Result = nil) then
      begin
      SetEof;
      Result := EndLexeme;
      end;
    end;
  LexemeCache.Enqueue(Result);
end;

function SSYacc.DoConflict: Boolean;
var
  LarState  : Longint;
  PFinal    : PSSLexFinalState;
  PProd     : PSSYaccPairTableProd;
begin
  Cache := 0;
  LarState := ExprList.LookupState(Lookahead.Token, 0);
  SetLarLookahead(GetLexemeCache);
  while (LarLookahead <> nil) do
    begin
    LarState := ExprList.LookupState(LarLookahead.Token, LarState);
    if (State = SSLexStateInvalid) then Break;
    PFinal := ExprList.LookupFinal(LarState);
    if (PFinal^.Flags and SSLexFinalStateFlagsFinal) <> 0 then
      begin
      if (PFinal^.Flags and SSLexFinalStateFlagsReduce) <> 0 then
        begin
        Production := PFinal^.Token;
        PProd := @Table.PProdArray^[ Production];
        Leftside := PProd^.Leftside;
        ProductionSize := PProd^.Size;
        Result := DoReduce;
        Exit;
        end
      else
        begin
        State := PFinal^.Token;
        Result := DoShift;
        Exit;
        end;
      end;
    SetLarLookahead(GetLexemeCache);
    end;
  Result := DoLarError;
end;

function SSYacc.DoGetLexeme(Look: Boolean): Boolean;
begin
  SetLookahead(LexemeCache.Dequeue);
  if (Lookahead <> nil) then
    begin
    Result := LarLook(Lookahead);
    if ((Result = False) and (Look = True)) then
      LookupAction(Lookahead.Token);
    end
  else
    Result := GetLexeme(Look);
end;

function SSYacc.GetLexeme(Look: Boolean): Boolean;
var
  AnException: SSException;
begin
  if EndOfInput = True then
    begin
    AnException := SSException.Create(SSExceptionYaccEof, SSYaccMsgEof);
    raise AnException;
    end;

  SetLookahead(NextLexeme);
  if Lookahead = nil then
    begin
    SetEof;
    SetLookahead(EndLexeme);
    end;

  if (Look) then
    LookupAction(Lookahead.Token);
  Result := False;
end;

procedure SSYacc.SetElement(TheElement: SSYaccStackElement);
begin
  if TheElement <> nil then
    TheElement.RefInc;
  if (Element <> nil) and Element.RefDec then
    Element.Free;
  Element := TheElement;
end;

procedure SSYacc.SetLookahead(TheLexeme: SSLexLexeme);
begin
  if TheLexeme <> nil then
    TheLexeme.RefInc;
  if (Lookahead <> nil) and Lookahead.RefDec then
    Lookahead.Free;
  Lookahead := TheLexeme;
end;

procedure SSYacc.SetLarLookahead(TheLexeme: SSLexLexeme);
begin
  if TheLexeme <> nil then
    TheLexeme.RefInc;
  if (LarLookahead <> nil) and LarLookahead.RefDec then
    LarLookahead.Free;
  LarLookahead := TheLexeme;
end;

function SSYacc.Shift: SSYaccStackElement;
begin
  Result := StackElement;
end;

procedure SSYacc.Pop(TheNumber: Longint);
var
  i         : Integer;
  TopElement: SSYaccStackElement;
begin
  for i := 0 to TheNumber - 1 do
    begin
    TopElement := Stack.Top;
    Stack.Pop;
    if (TopElement <> nil) and TopElement.RefDec then
      TopElement.Free;
    end;
  TopElement := Stack.Top;
  State := TopElement.State;
end;

function SSYacc.Error(TheState: Longint; TheLookahead: SSLexLexeme): Boolean;
begin
  SyncErr;
  Result := False;
end;

procedure SSYacc.SyncErr;
var
  ErrorString: PChar;
  i, j       : Integer;
  AnException: SSException;
  ErrorLexeme: SSLexLexeme;
  SetOfToken : SSSetOfLongint;
  PEntryArray: PSSYaccEntryArray;
  AnElement  : SSYaccStackElement;
  PRow       : PSSYaccPairTableRow;
  PErrorRow  : PSSYaccPairTableRow;
  PEntry     : PSSYaccPairTableEntry;
  TempCardinal1, TempCardinal2, TempCardinal3, TempCardinal4: Cardinal; // used to avoid warning from DelphiParser
begin
  SetOfToken := SSSetOfLongint.Create(16, 16);
  for i := 0 to Stack.TopOfStack - 1 do
    begin
    AnElement := Stack.PArray^[ i];
    PRow := Table.PPRowArray^[ AnElement.State];
    PEntryArray := PSSYaccEntryArray(PChar(PRow) + sizeof(SSYaccPairTableRow));
    if (PRow^.Flags and SSYaccPairTableRowFlagsSyncPossible) <> 0 then
      for j := 0 to PRow^.Actions - 1 do
        begin
        PEntry := @PEntryArray^[ j];
        TempCardinal1 := PRow^.Flags;
        TempCardinal2 := SSYaccPairTableRowFlagsSyncAll;
        tempCardinal3 := PEntry^.Entry;
        tempCardinal4 := SSYaccPairTableEntrySync;

        if ((TempCardinal1 and TempCardinal2) or
           (TempCardinal3 and TempCardinal4)) <> 0 then
           SetOfToken.Insert(PEntry^.Token);
        end;
    if (PRow^.Flags and SSYaccPairTableRowFlagsError) <> 0 then
      begin
      PEntry := @PEntryArray^[ PRow^.Actions + PRow^.Gotos];
      PErrorRow := Table.PPRowArray^[ PEntry^.Entry and SSYaccPairTableEntryMax];
      PEntryArray := PSSYaccEntryArray(PChar(PErrorRow) + sizeof(SSYaccPairTableRow));
      for j := 0 to PRow^.Actions - 1 do
        SetOfToken.Insert(PEntryArray^[ j].Token);
      end
    end;

  if SetOfToken.Count = 0 then
    begin
    SetOfToken.Free;
    AnException := SSException.Create(SSExceptionYaccSyncErrToken, SSYaccMsgSyncErrToken);
    raise AnException;
    end;

  while True do
    begin
    if SetOfToken.Contains(Lookahead.Token) then
      Break;
//    SetLookahead(NextLexeme);
//    if Lookahead = nil then
    if (DoGetLexeme(False)) then
      begin
      SetOfToken.Free;
      AnException := SSException.Create(SSExceptionYaccSyncErrEof, SSYaccMsgSyncErrEof);
      raise AnException;
      end;
    end;

  SetOfToken.Free;
  while True do
    begin
    PRow := Table.PPRowArray^[ State];
    if (PRow^.Flags and SSYaccPairTableRowFlagsError) <> 0 then
      begin
      PEntryArray := PSSYaccEntryArray(PChar(PRow) + sizeof(SSYaccPairTableRow));
      PEntry := @PEntryArray^[ PRow^.Actions + PRow^.Gotos];
      State := PEntry^.Entry and SSYaccPairTableEntryMax;
      LookupAction(Lookahead.Token);
      if Action <> ErrorAction then
        begin
        ErrorString := StrNew(SSYaccErrorString);
        ErrorLexeme := SSLexLexeme.Create(ErrorString, StrLen(ErrorString), 0, 0);
        ErrorLexeme.Token := SSYaccErrorToken;
        StrDispose(ErrorString);
        SetElement(StackElement);
        Element.Lexeme := ErrorLexeme;
        Element.State := State;
        Stack.Push(Element);
        Break;
        end
      end;
    if (PRow^.Flags and SSYaccPairTableRowFlagsSyncPossible) <> 0 then
      begin
      LookupAction(Lookahead.Token);
      if Action <> ErrorAction then
        Break;
      end;

    Pop(1);
    end
end;

{function SSYacc.ElementFromStack(TheDepth: Longint): SSYaccStackElement;
var
  AnException: SSException;
begin
  if TheDepth > Stack.TopOfStack then
    begin
    AnException := SSException.Create(SSExceptionYaccElement, SSYaccMsgElement);
    raise AnException;
    end;

  Result := Stack.PArray^[ TheDepth];
end;
}
function SSYacc.ElementFromProduction(TheIndex: Longint): SSYaccStackElement;
var
  AnException: SSException;
begin
  TheIndex := Stack.TopOfStack - ProductionSize + TheIndex;
  if (TheIndex < 0) or (TheIndex >= Stack.TopOfStack) then
    begin
    AnException := SSException.Create(SSExceptionYaccElement, SSYaccMsgElement);
    raise AnException;
    end;

  Result := Stack.PArray^[ TheIndex];
end;

{procedure SSYacc.Reset;
begin
  State := 0;
  ShiftedSinceError := 0;
  if (EndLexeme <> nil) and EndLexeme.RefDec then
    EndLexeme.Free;
  ErrorInd := False;
  AbortInd := False;
  EndOfInput := False;
  Stack.Free;
  Stack := SSYaccStack.Create;
  SetElement(StackElement);
  Stack.Push(Element);
end;
 }
destructor SSYacc.Destroy;
begin
  if (LarLookahead <> nil) and (LarLookahead.RefDec) then
    LarLookahead.Free;
  if (EndLexeme <> nil) and (EndLexeme.RefDec) then
    EndLexeme.Free;
  Stack.Free;
  if (Lookahead <> nil) and (Lookahead.RefDec) then
     Lookahead.Free;
  if (Element <> nil) and (Element.RefDec) then
     Element.Free;
  LexemeCache.Free;
  inherited;
end;

constructor SSSetOfLongint.Create(TheSize, TheInc: Longint);
begin
  inherited Create;
  Size := TheSize;
  Incr := TheInc;
  Count := 0;
  GetMem(PArray, Size * sizeof(Longint));
end;

function SSSetOfLongint.Insert(TheItem: Longint): Boolean;
var
  i        : Integer;
  NewSize  : Longint;
  PNewArray: PSSArrayOfLongint;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PArray^[ i] = TheItem then
      begin
      Result := False;
      Break;
      end;

  if Result = True then
    begin
    if Count >= Size then
      begin
      NewSize := Size + Incr;
      GetMem(PNewArray, NewSize * sizeof(Longint));
      for i := 0 to Count - 1 do
        PNewArray^[ i] := PArray^[ i];
      FreeMem(PArray, Size * sizeof(Longint));
      Size := NewSize;
      PArray := PNewArray;
      end;
    PArray^[ Count] := TheItem;
    Inc(Count);
    end
end;

function SSSetOfLongint.Contains(TheItem: Longint): Boolean;
var
  i        : Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if PArray^[ i] = TheItem then
      begin
      Result := True;
      Break;
      end;
end;

destructor SSSetOfLongint.Destroy;
begin
  FreeMem(PArray, Size * sizeof(Longint));
  inherited Destroy;
end;

{function SSYacc.ValidLookaheads(TheState: Longint; var TheCount: Longint): PSSArrayOfLongint;
var
  i          : Integer;
  PEntryArray: PSSYaccEntryArray;
  PRow       : PSSYaccPairTableRow;
  PEntry     : PSSYaccPairTableEntry;
begin
  PRow := Table.PPRowArray^[ TheState];
  GetMem(Result, PRow^.Actions * sizeof(Longint));
  PEntryArray := PSSYaccEntryArray(PChar(PRow) + sizeof(SSYaccPairTableRow));
  TheCount := PRow^.Actions;
  for i := 0 to TheCount - 1 do
    begin
    PEntry := @PEntryArray^[ i];
    Result^[ i] := PEntry^.Token;
    end;
end;
 }
constructor SSYaccLexemeCache.Create;
begin
  inherited Create;
end;

function SSYaccLexemeCache.Dequeue: SSLexLexeme;
begin
  if (Count = 0) then
    Result := nil
  else
    begin
   // Result := List^[ 0];   // marco
    Result.RefDec;
    Delete(0);
    Pack;
    end;
end;

procedure SSYaccLexemeCache.Enqueue(TheLexeme: SSLexLexeme);
begin
  Add(TheLexeme);
  TheLexeme.RefInc;
end;

function SSYaccLexemeCache.Get(TheIndex: Longint): SSLexLexeme;
begin
  if ((Count = 0) or (TheIndex = MaxLongInt)) then
    Result := nil
  else
    Result := Items[ TheIndex];
end;

destructor  SSYaccLexemeCache.Destroy;
begin
  inherited Destroy;
end;

end.
