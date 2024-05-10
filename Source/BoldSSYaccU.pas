
unit BoldSSYaccU;

{$RANGECHECKS OFF}

interface

uses
  Classes, BoldSSLexU, BoldBase;

const
  SSYaccPairTableRowFlagsError = $0002;
  SSYaccPairTableRowFlagsSyncAll = $0004;
  SSYaccPairTableRowFlagsSyncPossible = $0005;
  SSYaccPairTableEntrySync = $80000000;
  SSYaccPairTableEntryShift = $40000000;
  SSYaccPairTableEntryReduce = $20000000;
  SSYaccPairTableEntryAccept = $10000000;
  SSYaccPairTableEntryConflict = $08000000;
  SSYaccPairTableEntryMax = $07FFFFFF;
  SSYaccEofString = 'eof';
  SSYaccErrorString = 'error';
  SSYaccEofToken = $FFFFFFFF;
  SSYaccErrorToken = $FFFFFFFE;

  SSYaccMsgParse = 'SSYacc0102e: Parse failed, invalid table';
  SSYaccMsgEof = 'SSYacc0103e: Attempted read past eof';
  SSYaccMsgMissingTable = 'SSYacc0109e: Table missing, required';
  SSYaccMsgMissingLexer = 'SSYacc0110e: Lexer missing, required';
  SSYaccMsgElement = 'SSYacc0108e: Invalid index on ElementFromxxx';
  SSYaccMsgSyncErrToken = 'SSYacc0105e: SyncErr failed, no valid token';
  SSYaccMsgSyncErrEof = 'SSYacc0106e: SyncErr failed, eof';

type
  SSArrayOfLongint = array[0..(SSLexStructMax div SizeOf(Longint))] of Longint;
  PSSArrayOfLongint = ^SSArrayOfLongint;

  SSSetOfLongint = class(TBoldMemoryManagedObject)
  public
    Incr: Longint;
    Size: Longint;
    Count: Longint;
    PArray: PSSArrayOfLongint;

    constructor Create(TheSize, TheInc: Longint);
    function Insert(TheItem: Longint): Boolean;
    function Contains(TheItem: Longint): Boolean;
    destructor Destroy; override;
  end;

  TYProd = record
    Size: Byte;
    Leftside: Word;
  end;

  TYEntry = record
    Entry: Longint;
    Token: Longint;
  end;

  TYRow = record
    Flags: Byte;
    Gotos: Byte;
    Actions: Byte;
    Entries: array of TYEntry;
  end;

  TYRows = array of TYRow;
  TYProds = array of TYProd;

  SSYaccAction = (ShiftAction, ErrorAction, ReduceAction, AcceptAction, ConflictAction);

  SSYaccStack = class(SSStack)
  public
    function Top: SSYaccStackElement;
    procedure Push(TheElement: SSYaccStackElement);
    destructor Destroy; override;
  end;

  SSYaccTable = class(TBoldMemoryManagedObject)
  public
    RowArray: TYRows;
    ProdArray: TYProds;
    constructor Create;
    destructor Destroy; override;
    procedure FullTab;
  end;

  SSYaccLexemeCache = class(TList)
  public
    constructor Create;
    function Dequeue: SSLexLexeme;
    procedure Enqueue(TheLexeme: SSLexLexeme);
    function Get(TheIndex: Longint): SSLexLexeme;
    destructor Destroy; override;
  end;

  SSYacc = class(TBoldMemoryManagedObject)
  public
    Lex: SSLex;
    ErrorInd: Boolean;
    AbortInd: Boolean;
    EndOfInput: Boolean;
    Cache: Longint;
    State: Longint;
    ShiftedSinceError: Longint;
    Leftside: Longint;
    Production: Longint;
    ProductionSize: Longint;
    Table: SSYaccTable;
    Lookahead: SSLexLexeme;
    LarLookahead: SSLexLexeme;
    Stack: SSYaccStack;
    EndLexeme: SSLexLexeme;
    Action: SSYaccAction;
    LexemeCache: SSYaccLexemeCache;
    Element: SSYaccStackElement;
    ExprList: SSLexExpressionList;
    constructor CreateLex(TheLexer: SSLex; TheTable: SSYaccTable);
    procedure SetEof;
    procedure SyncErr;
    function Parse: Boolean;
    function DoShift: Boolean;
    function DoReduce: Boolean;
    function DoConflict: Boolean;
    function DoLarError: Boolean;
    procedure Pop(TheNumber: Longint);
    procedure DoGoto(TheGoto: Longint);
    function GetLexemeCache: SSLexLexeme;
    procedure LookupGoto(TheGoto: Longint);
    procedure LookupAction(TheToken: Longint);
    function GetLexeme(Look: Boolean): Boolean;
    function DoGetLexeme(Look: Boolean): Boolean;
    procedure SetLookahead(TheLexeme: SSLexLexeme);
    procedure SetLarLookahead(TheLexeme: SSLexLexeme);
    procedure SetElement(TheElement: SSYaccStackElement);
    function GetAction(YEntry: TYEntry): SSYaccAction;
    function ElementFromProduction(TheIndex: Longint): SSYaccStackElement;
    function NextLexeme: SSLexLexeme; virtual;
    function Shift: SSYaccStackElement; virtual;
    function StackElement: SSYaccStackElement; virtual;
    function LarLook(TheLexeme: SSLexLexeme): Boolean; virtual;
    function Error(TheState: Longint; TheLookahead: SSLexLexeme): Boolean; virtual;
    function LarError(TheState: Longint; TheLookahead, TheLarLookahead: SSLexLexeme): Boolean; virtual;
    function Reduce(TheProduction, TheProductionSize: Longint): SSYaccStackElement; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  BoldSSExcept, SysUtils;

constructor SSYaccTable.Create;
begin
  FullTab;
end;

destructor SSYaccTable.Destroy;
begin
  RowArray := nil;
  ProdArray := nil;
  inherited;
end;

function SSYaccStack.Top: SSYaccStackElement;
begin
  Result := inherited Top as SSYaccStackElement;
end;

procedure SSYaccStack.Push(TheElement: SSYaccStackElement);
begin
  TheElement.RefInc;
  inherited Push(TheElement);
end;

destructor SSYaccStack.Destroy;
var
  i: Integer;
  Element: SSYaccStackElement;
begin
  for i := 0 to TopOfStack - 1 do
  begin
{$WARN UNSAFE_CODE OFF}
    Element := PArray^[i];
{$WARN UNSAFE_CODE ON}
    if Element.RefDec then
      Element.Free;
  end;
  inherited;
end;

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
  EofString: String;
begin
  EndOfInput := True;
  EofString := SSYaccEofString;
  EndLexeme := SSLexLexeme.Create(EofString, Length(EofString), 0, 0);
  EndLexeme.Token := SSYaccEofToken;
  EndLexeme.RefInc;
end;

procedure SSYacc.LookupAction(TheToken: Integer);
var
  i: Integer;
  CurrentRow: TYRow;
  YEntry: TYEntry;
  Prod: TYProd;
  EntryIndex: Integer;
begin
  EntryIndex := -1;
  CurrentRow := Table.RowArray[State];
  for i := 0 to CurrentRow.Actions - 1 do
    if CurrentRow.Entries[i].Token = TheToken then
    begin
      EntryIndex := i;
      YEntry := CurrentRow.Entries[i];
      Break;
    end;

  if EntryIndex = -1 then
    Action := ErrorAction
  else
  begin
    Action := GetAction(YEntry);
    case Action of
      ShiftAction:
        State := YEntry.Entry and SSYaccPairTableEntryMax;
      ReduceAction:
        begin
          Production := YEntry.Entry and SSYaccPairTableEntryMax;
          Prod := Table.ProdArray[Production];
          Leftside := Prod.Leftside;
          ProductionSize := Prod.Size;
        end;
      ConflictAction:
        begin

        end;
    end;
  end;
end;

function SSYacc.GetAction(YEntry: TYEntry): SSYaccAction;
begin
  if YEntry.Entry and SSYaccPairTableEntryShift <> 0 then
    Result := ShiftAction
  else if YEntry.Entry and SSYaccPairTableEntryReduce <> 0 then
    Result := ReduceAction
  else if YEntry.Entry and SSYaccPairTableEntryAccept <> 0 then
    Result := AcceptAction
  else if YEntry.Entry and SSYaccPairTableEntryConflict <> 0 then
    Result := ConflictAction
  else
    Result := ErrorAction;
end;

function SSYacc.LarError(TheState: Integer; TheLookahead, TheLarLookahead: SSLexLexeme): Boolean;
begin
  Result := Error(TheState, TheLookahead);
end;

function SSYacc.Reduce(TheProduction, TheProductionSize: Integer): SSYaccStackElement;
begin
  Result := nil;
end;

function SSYacc.LarLook(TheLexeme: SSLexLexeme): Boolean;
begin
  Result := False;
end;

function SSYacc.DoLarError: Boolean;
begin
  ErrorInd := True;
  Result := LarError(State, Lookahead, LarLookahead);
  if not Result then
    ShiftedSinceError := 0;
end;

procedure SSYacc.LookupGoto(TheGoto: Integer);
var
  i, EndIndex: Integer;
  AnException: SSException;
  CurrentRow: TYRow;
  YEntry: TYEntry;
  EntryIndex: Integer;
begin
  EntryIndex := -1;
  CurrentRow := Table.RowArray[State];
  EndIndex := CurrentRow.Actions + CurrentRow.Gotos - 1;
  for i := CurrentRow.Actions to EndIndex do
    if CurrentRow.Entries[i].Token = TheGoto then
    begin
      EntryIndex := i;
      YEntry := CurrentRow.Entries[i];
      Break;
    end;

  if EntryIndex = -1 then
  begin
    AnException := SSException.Create(SSExceptionYaccParse, SSYaccMsgParse);
    raise AnException;
  end;

  State := YEntry.Entry and SSYaccPairTableEntryMax;
end;

procedure SSYacc.DoGoto(TheGoto: Integer);
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
begin
  Result := DoGetLexeme(True);
  if Result = True then
    Exit;

  while True do
  begin
    if AbortInd = True then
      Break;
    case Action of
      ShiftAction:
        begin
          Result := DoShift;
          if Result = True then
            Break;
        end;

      ReduceAction:
        begin
          Result := DoReduce;
          if Result = True then
            Break;
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
          if Result = True then
            Break;
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
    if not Result then
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
  if Cache <> MaxLongint then
  begin
    Result := LexemeCache.Get(Cache);
    Inc(Cache);
  end;
  if Result = nil then
  begin
    Cache := MaxLongint;
    Result := NextLexeme;
    if Result = nil then
    begin
      SetEof;
      Result := EndLexeme;
    end;
  end;
  LexemeCache.Enqueue(Result);
end;

function SSYacc.DoConflict: Boolean;
var
  LarState: Longint;
  PFinal: TRow;
  Prod: TYProd;
begin
  Cache := 0;
  LarState := ExprList.LookupState(Lookahead.Token, 0);
  SetLarLookahead(GetLexemeCache);
  while LarLookahead <> nil do
  begin
    LarState := ExprList.LookupState(LarLookahead.Token, LarState);
    if State = SSLexStateInvalid then
      Break;
    PFinal := ExprList.ARows[LarState];
    if PFinal.Flags and SSLexFinalStateFlagsFinal <> 0 then
    begin
      if PFinal.Flags and SSLexFinalStateFlagsReduce <> 0 then
      begin
        Production := PFinal.Token;
        Prod := Table.ProdArray[Production];
        Leftside := Prod.Leftside;
        ProductionSize := Prod.Size;
        Result := DoReduce;
        Exit;
      end
      else
      begin
        State := PFinal.Token;
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
  if Lookahead <> nil then
  begin
    Result := LarLook(Lookahead);
    if (Result = False) and (Look = True) then
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

  if Look then
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

procedure SSYacc.Pop(TheNumber: Integer);
var
  i: Integer;
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

function SSYacc.Error(TheState: Integer; TheLookahead: SSLexLexeme): Boolean;
begin
  SyncErr;
  Result := False;
end;

procedure SSYacc.SyncErr;
var
  ErrorString: String;
  i, j: Integer;
  AnException: SSException;
  ErrorLexeme: SSLexLexeme;
  SetOfToken: SSSetOfLongint;
  AnElement: SSYaccStackElement;
  Row, ErrorRow: TYRow;
  YEntry: TYEntry;
  TempCardinal1, TempCardinal2, TempCardinal3, TempCardinal4: Cardinal;
begin
  SetOfToken := SSSetOfLongint.Create(16, 16);
  for i := 0 to Stack.TopOfStack - 1 do
  begin
    AnElement := Stack.PArray[i];
    Row := Table.RowArray[AnElement.State];
    if Row.Flags and SSYaccPairTableRowFlagsSyncPossible <> 0 then
      for j := 0 to Row.Actions - 1 do
      begin
        YEntry := Row.Entries[j];
        TempCardinal1 := Row.Flags;
        TempCardinal2 := SSYaccPairTableRowFlagsSyncAll;
        TempCardinal3 := YEntry.Entry;
        TempCardinal4 := SSYaccPairTableEntrySync;

        if (TempCardinal1 and TempCardinal2) or (TempCardinal3 and TempCardinal4) <> 0 then
          SetOfToken.Insert(YEntry.Token);
      end;
    if Row.Flags and SSYaccPairTableRowFlagsError <> 0 then
    begin
      YEntry := Row.Entries[Row.Actions + Row.Gotos];
      ErrorRow := Table.RowArray[YEntry.Entry and SSYaccPairTableEntryMax];
      for j := 0 to Row.Actions - 1 do
        SetOfToken.Insert(ErrorRow.Entries[j].Token);
    end;
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

    if DoGetLexeme(False) then
    begin
      SetOfToken.Free;
      AnException := SSException.Create(SSExceptionYaccSyncErrEof, SSYaccMsgSyncErrEof);
      raise AnException;
    end;
  end;

  SetOfToken.Free;
  while True do
  begin
    Row := Table.RowArray[State];
    if Row.Flags and SSYaccPairTableRowFlagsError <> 0 then
    begin
      YEntry := Row.Entries[Row.Actions + Row.Gotos];
      State := YEntry.Entry and SSYaccPairTableEntryMax;
      LookupAction(Lookahead.Token);
      if Action <> ErrorAction then
      begin
        ErrorString := SSYaccErrorString;
        ErrorLexeme := SSLexLexeme.Create(ErrorString, Length(ErrorString), 0, 0);
        ErrorLexeme.Token := SSYaccErrorToken;
        SetElement(StackElement);
        Element.Lexeme := ErrorLexeme;
        Element.State := State;
        Stack.Push(Element);
        Break;
      end;
    end;
    if Row.Flags and SSYaccPairTableRowFlagsSyncPossible <> 0 then
    begin
      LookupAction(Lookahead.Token);
      if Action <> ErrorAction then
        Break;
    end;

    Pop(1);
  end;
end;

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

  Result := Stack.PArray[TheIndex];
end;

destructor SSYacc.Destroy;
begin
  if (LarLookahead <> nil) and LarLookahead.RefDec then
    LarLookahead.Free;
  if (EndLexeme <> nil) and EndLexeme.RefDec then
    EndLexeme.Free;
  Stack.Free;
  if (Lookahead <> nil) and Lookahead.RefDec then
    Lookahead.Free;
  if (Element <> nil) and Element.RefDec then
    Element.Free;
  LexemeCache.Free;
  inherited;
end;

constructor SSSetOfLongint.Create(TheSize, TheInc: Integer);
begin
  inherited Create;
  Size := TheSize;
  Incr := TheInc;
  Count := 0;
{$WARN UNSAFE_CODE OFF}
  GetMem(PArray, Size * SizeOf(Longint));
{$WARN UNSAFE_CODE ON}
end;

function SSSetOfLongint.Insert(TheItem: Integer): Boolean;
var
  i: Integer;
  NewSize: Longint;
  PNewArray: PSSArrayOfLongint;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if PArray[i] = TheItem then
    begin
      Result := False;
      Break;
    end;

  if Result = True then
  begin
    if Count >= Size then
    begin
      NewSize := Size + Incr;
{$WARN UNSAFE_CODE OFF}
      GetMem(PNewArray, NewSize * SizeOf(Longint));
{$WARN UNSAFE_CODE ON}
      for i := 0 to Count - 1 do
        PNewArray[i] := PArray[i];
{$WARN UNSAFE_CODE OFF}
      FreeMem(PArray, Size * SizeOf(Longint));
{$WARN UNSAFE_CODE ON}
      Size := NewSize;
      PArray := PNewArray;
    end;
    PArray[Count] := TheItem;
    Inc(Count);
  end;
end;

function SSSetOfLongint.Contains(TheItem: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if PArray[i] = TheItem then
    begin
      Result := True;
      Break;
    end;
end;

destructor SSSetOfLongint.Destroy;
begin
{$WARN UNSAFE_CODE OFF}
  FreeMem(PArray, Size * SizeOf(Longint));
{$WARN UNSAFE_CODE ON}
  inherited Destroy;
end;

constructor SSYaccLexemeCache.Create;
begin
  inherited Create;
end;

function SSYaccLexemeCache.Dequeue: SSLexLexeme;
begin
  if Count = 0 then
    Result := nil
  else
  begin
{$WARN UNSAFE_CAST OFF}
    Result := SSLexLexeme(List[0]);
{$WARN UNSAFE_CAST ON}
    Result.RefDec;
    Delete(0);
    Pack;
  end;
end;

procedure SSYaccLexemeCache.Enqueue(TheLexeme: SSLexLexeme);
begin
{$WARN UNSAFE_CAST OFF}
  Add(Pointer(TheLexeme));
{$WARN UNSAFE_CAST ON}
  TheLexeme.RefInc;
end;

function SSYaccLexemeCache.Get(TheIndex: Integer): SSLexLexeme;
begin
  if (Count = 0) or (TheIndex = MaxLongint) then
    Result := nil
  else
{$WARN UNSAFE_CAST OFF}
    Result := SSLexLexeme(Items[TheIndex]);
{$WARN UNSAFE_CAST ON}
end;

destructor SSYaccLexemeCache.Destroy;
begin
  inherited Destroy;
end;


procedure SSYaccTable.FullTab;
var
  YaccProds: TYProds;
  YaccRows: TYRows;
  Row: TYRow;
  N: Integer;

  procedure YP(S, L: Word);
  begin
    with YaccProds[N] do
    begin
      Size := S;
      Leftside := L;
    end;
    Inc(N);
  end;

  procedure R(F, G, A: Byte);
  begin
    with Row do
    begin
      Flags := F;
      Gotos := G;
      Actions := A;
      SetLength(Entries, G + A);
      N := 0;
    end;
  end;

  procedure RE(E, T: Longint);
  begin
    with Row.Entries[N] do
    begin
      Entry := E;
      Token := T;
    end;
    Inc(N);
  end;

begin

  {$REGION 'Full YaccTable'}

  // === YaccProds ===

  SetLength(YaccProds, 73);
  N := 0;
  YP(2,     0);
  YP(1, 16386);
  YP(1, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(3, 16385);
  YP(2, 16385);
  YP(2, 16385);
  YP(7, 16388);
  YP(1, 16387);
  YP(5, 16387);
  YP(6, 16387);
  YP(5, 16387);
  YP(6, 16387);
  YP(1, 16389);
  YP(1, 16389);
  YP(3, 16389);
  YP(1, 16389);
  YP(3, 16389);
  YP(4, 16389);
  YP(3, 16393);
  YP(5, 16393);
  YP(7, 16393);
  YP(3, 16395);
  YP(2, 16395);
  YP(1, 16395);
  YP(1, 16395);
  YP(1, 16395);
  YP(1, 16395);
  YP(2, 16395);
  YP(0, 16397);
  YP(1, 16397);
  YP(1, 16397);
  YP(4, 16394);
  YP(1, 16402);
  YP(1, 16403);
  YP(3, 16403);
  YP(3, 16402);
  YP(4, 16400);
  YP(2, 16404);
  YP(4, 16404);
  YP(0, 16392);
  YP(3, 16392);
  YP(1, 16399);
  YP(1, 16405);
  YP(3, 16405);
  YP(1, 16390);
  YP(3, 16390);
  YP(1, 16398);
  YP(1, 16398);
  YP(2, 16391);
  YP(0, 16391);
  YP(0, 16396);
  YP(1, 16396);
  YP(1, 16406);
  YP(3, 16406);
  YP(1, 16401);
  YP(1, 16401);
  YP(1, 16401);
  YP(1, 16401);

  // === YaccRows ===

  SetLength(YaccRows, 134);
  //  [0]
  R(0, 10, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(25, 16385);
  RE(26, 16386);
  YaccRows[0] := Row;
  //  [1]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(133, 16385);
  YaccRows[1] := Row;
  //  [2]
  R(0, 0, 1);
  RE(536870984, 13);
  YaccRows[2] := Row;
  //  [3]
  R(0, 0, 1);
  RE(536870983, 13);
  YaccRows[3] := Row;
  //  [4]
  R(0, 0, 1);
  RE(536870981, 13);
  YaccRows[4] := Row;
  //  [5]
  R(0, 0, 1);
  RE(536870982, 13);
  YaccRows[5] := Row;
  //  [6]
  R(0, 1, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(132, 16398);
  YaccRows[6] := Row;
  //  [7]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(126, 16385);
  YaccRows[7] := Row;
  //  [8]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(125, 16385);
  YaccRows[8] := Row;
  //  [9]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(123, 16385);
  YaccRows[9] := Row;
  //  [10]
  R(0, 0, 2);
  RE(1073741944, 51);
  RE(1073741945, 50);
  YaccRows[10] := Row;
  //  [11]
  R(0, 0, 27);
  RE(536870953, 10);
  RE(536870953, 12);
  RE(536870953, 14);
  RE(536870953, 15);
  RE(536870953, 16);
  RE(536870953, 17);
  RE(536870953, 18);
  RE(536870953, 19);
  RE(536870953, 20);
  RE(536870953, 21);
  RE(536870953, 22);
  RE(536870953, 23);
  RE(536870953, 24);
  RE(536870953, 25);
  RE(536870953, 26);
  RE(536870953, 27);
  RE(536870953, 30);
  RE(536870953, 31);
  RE(536870953, 32);
  RE(536870953, 37);
  RE(536870953, 43);
  RE(536870953, 44);
  RE(536870953, 45);
  RE(536870953, 46);
  RE(536870953, 48);
  RE(536870953, 49);
  RE(536870953, -1);
  YaccRows[11] := Row;
  //  [12]
  R(0, 0, 27);
  RE(536870952, 10);
  RE(536870952, 12);
  RE(536870952, 14);
  RE(536870952, 15);
  RE(536870952, 16);
  RE(536870952, 17);
  RE(536870952, 18);
  RE(536870952, 19);
  RE(536870952, 20);
  RE(536870952, 21);
  RE(536870952, 22);
  RE(536870952, 23);
  RE(536870952, 24);
  RE(536870952, 25);
  RE(536870952, 26);
  RE(536870952, 27);
  RE(536870952, 30);
  RE(536870952, 31);
  RE(536870952, 32);
  RE(536870952, 37);
  RE(536870952, 43);
  RE(536870952, 44);
  RE(536870952, 45);
  RE(536870952, 46);
  RE(536870952, 48);
  RE(536870952, 49);
  RE(536870952, -1);
  YaccRows[12] := Row;
  //  [13]
  R(0, 0, 31);
  RE(536870973, 9);
  RE(536870973, 10);
  RE(536870973, 11);
  RE(536870973, 12);
  RE(536870973, 14);
  RE(536870973, 15);
  RE(536870973, 16);
  RE(536870973, 17);
  RE(536870973, 18);
  RE(536870973, 19);
  RE(536870973, 20);
  RE(536870973, 21);
  RE(536870973, 22);
  RE(536870973, 23);
  RE(536870973, 24);
  RE(536870973, 25);
  RE(536870973, 26);
  RE(536870973, 27);
  RE(536870973, 28);
  RE(536870973, 30);
  RE(536870973, 31);
  RE(536870973, 32);
  RE(536870973, 36);
  RE(536870973, 37);
  RE(536870973, 43);
  RE(536870973, 44);
  RE(536870973, 45);
  RE(536870973, 46);
  RE(536870973, 48);
  RE(536870973, 49);
  RE(536870973, -1);
  YaccRows[13] := Row;
  //  [14]
  R(0, 0, 31);
  RE(536870974, 9);
  RE(536870974, 10);
  RE(536870974, 11);
  RE(536870974, 12);
  RE(536870974, 14);
  RE(536870974, 15);
  RE(536870974, 16);
  RE(536870974, 17);
  RE(536870974, 18);
  RE(536870974, 19);
  RE(536870974, 20);
  RE(536870974, 21);
  RE(536870974, 22);
  RE(536870974, 23);
  RE(536870974, 24);
  RE(536870974, 25);
  RE(536870974, 26);
  RE(536870974, 27);
  RE(536870974, 28);
  RE(536870974, 30);
  RE(536870974, 31);
  RE(536870974, 32);
  RE(536870974, 36);
  RE(536870974, 37);
  RE(536870974, 43);
  RE(536870974, 44);
  RE(536870974, 45);
  RE(536870974, 46);
  RE(536870974, 48);
  RE(536870974, 49);
  RE(536870974, -1);
  YaccRows[14] := Row;
  //  [15]
  R(0, 0, 27);
  RE(536870951, 10);
  RE(536870951, 12);
  RE(536870951, 14);
  RE(536870951, 15);
  RE(536870951, 16);
  RE(536870951, 17);
  RE(536870951, 18);
  RE(536870951, 19);
  RE(536870951, 20);
  RE(536870951, 21);
  RE(536870951, 22);
  RE(536870951, 23);
  RE(536870951, 24);
  RE(536870951, 25);
  RE(536870951, 26);
  RE(536870951, 27);
  RE(536870951, 30);
  RE(536870951, 31);
  RE(536870951, 32);
  RE(536870951, 37);
  RE(536870951, 43);
  RE(536870951, 44);
  RE(536870951, 45);
  RE(536870951, 46);
  RE(536870951, 48);
  RE(536870951, 49);
  RE(536870951, -1);
  YaccRows[15] := Row;
  //  [16]
  R(0, 0, 27);
  RE(536870950, 10);
  RE(536870950, 12);
  RE(536870950, 14);
  RE(536870950, 15);
  RE(536870950, 16);
  RE(536870950, 17);
  RE(536870950, 18);
  RE(536870950, 19);
  RE(536870950, 20);
  RE(536870950, 21);
  RE(536870950, 22);
  RE(536870950, 23);
  RE(536870950, 24);
  RE(536870950, 25);
  RE(536870950, 26);
  RE(536870950, 27);
  RE(536870950, 30);
  RE(536870950, 31);
  RE(536870950, 32);
  RE(536870950, 37);
  RE(536870950, 43);
  RE(536870950, 44);
  RE(536870950, 45);
  RE(536870950, 46);
  RE(536870950, 48);
  RE(536870950, 49);
  RE(536870950, -1);
  YaccRows[16] := Row;
  //  [17]
  R(0, 0, 1);
  RE(1073741934, 13);
  YaccRows[17] := Row;
  //  [18]
  R(0, 0, 31);
  RE(536870971, 9);
  RE(536870971, 10);
  RE(536870971, 11);
  RE(536870971, 12);
  RE(536870971, 14);
  RE(536870971, 15);
  RE(536870971, 16);
  RE(536870971, 17);
  RE(536870971, 18);
  RE(536870971, 19);
  RE(536870971, 20);
  RE(536870971, 21);
  RE(536870971, 22);
  RE(536870971, 23);
  RE(536870971, 24);
  RE(536870971, 25);
  RE(536870971, 26);
  RE(536870971, 27);
  RE(536870971, 28);
  RE(536870971, 30);
  RE(536870971, 31);
  RE(536870971, 32);
  RE(536870971, 37);
  RE(536870971, 43);
  RE(536870971, 44);
  RE(536870971, 45);
  RE(536870971, 46);
  RE(536870971, 48);
  RE(536870971, 49);
  RE(536870971, -1);
  RE(1073741932, 36);
  YaccRows[18] := Row;
  //  [19]
  R(0, 0, 27);
  RE(536870940, 10);
  RE(536870940, 12);
  RE(536870940, 14);
  RE(536870940, 15);
  RE(536870940, 16);
  RE(536870940, 17);
  RE(536870940, 18);
  RE(536870940, 19);
  RE(536870940, 20);
  RE(536870940, 21);
  RE(536870940, 22);
  RE(536870940, 23);
  RE(536870940, 24);
  RE(536870940, 25);
  RE(536870940, 26);
  RE(536870940, 27);
  RE(536870940, 30);
  RE(536870940, 31);
  RE(536870940, 32);
  RE(536870940, 37);
  RE(536870940, 43);
  RE(536870940, 44);
  RE(536870940, 45);
  RE(536870940, 46);
  RE(536870940, 48);
  RE(536870940, 49);
  RE(536870940, -1);
  YaccRows[19] := Row;
  //  [20]
  R(0, 0, 27);
  RE(536870939, 10);
  RE(536870939, 12);
  RE(536870939, 14);
  RE(536870939, 15);
  RE(536870939, 16);
  RE(536870939, 17);
  RE(536870939, 18);
  RE(536870939, 19);
  RE(536870939, 20);
  RE(536870939, 21);
  RE(536870939, 22);
  RE(536870939, 23);
  RE(536870939, 24);
  RE(536870939, 25);
  RE(536870939, 26);
  RE(536870939, 27);
  RE(536870939, 30);
  RE(536870939, 31);
  RE(536870939, 32);
  RE(536870939, 37);
  RE(536870939, 43);
  RE(536870939, 44);
  RE(536870939, 45);
  RE(536870939, 46);
  RE(536870939, 48);
  RE(536870939, 49);
  RE(536870939, -1);
  YaccRows[20] := Row;
  //  [21]
  R(0, 1, 30);
  RE(536870976, 9);
  RE(536870976, 10);
  RE(536870976, 11);
  RE(536870976, 12);
  RE(536870976, 14);
  RE(536870976, 15);
  RE(536870976, 16);
  RE(536870976, 17);
  RE(536870976, 18);
  RE(536870976, 19);
  RE(536870976, 20);
  RE(536870976, 21);
  RE(536870976, 22);
  RE(536870976, 23);
  RE(536870976, 24);
  RE(536870976, 25);
  RE(536870976, 26);
  RE(536870976, 27);
  RE(536870976, 30);
  RE(536870976, 31);
  RE(536870976, 32);
  RE(536870976, 37);
  RE(536870976, 43);
  RE(536870976, 44);
  RE(536870976, 45);
  RE(536870976, 46);
  RE(536870976, 48);
  RE(536870976, 49);
  RE(536870976, -1);
  RE(1073741887, 28);
  RE(105, 16391);
  YaccRows[21] := Row;
  //  [22]
  R(0, 0, 27);
  RE(536870934, 10);
  RE(536870934, 12);
  RE(536870934, 14);
  RE(536870934, 15);
  RE(536870934, 16);
  RE(536870934, 17);
  RE(536870934, 18);
  RE(536870934, 19);
  RE(536870934, 20);
  RE(536870934, 21);
  RE(536870934, 22);
  RE(536870934, 23);
  RE(536870934, 24);
  RE(536870934, 25);
  RE(536870934, 26);
  RE(536870934, 27);
  RE(536870934, 30);
  RE(536870934, 31);
  RE(536870934, 32);
  RE(536870934, 37);
  RE(536870934, 43);
  RE(536870934, 44);
  RE(536870934, 45);
  RE(536870934, 46);
  RE(536870934, 48);
  RE(536870934, 49);
  RE(536870934, -1);
  YaccRows[22] := Row;
  //  [23]
  R(0, 0, 27);
  RE(536870942, 10);
  RE(536870942, 12);
  RE(536870942, 14);
  RE(536870942, 15);
  RE(536870942, 16);
  RE(536870942, 17);
  RE(536870942, 18);
  RE(536870942, 19);
  RE(536870942, 20);
  RE(536870942, 21);
  RE(536870942, 22);
  RE(536870942, 23);
  RE(536870942, 24);
  RE(536870942, 25);
  RE(536870942, 26);
  RE(536870942, 27);
  RE(536870942, 30);
  RE(536870942, 31);
  RE(536870942, 32);
  RE(536870942, 37);
  RE(536870942, 43);
  RE(536870942, 44);
  RE(536870942, 45);
  RE(536870942, 46);
  RE(536870942, 48);
  RE(536870942, 49);
  RE(536870942, -1);
  YaccRows[23] := Row;
  //  [24]
  R(0, 0, 27);
  RE(536870914, 10);
  RE(536870914, 12);
  RE(536870914, 14);
  RE(536870914, 15);
  RE(536870914, 16);
  RE(536870914, 17);
  RE(536870914, 18);
  RE(536870914, 19);
  RE(536870914, 20);
  RE(536870914, 21);
  RE(536870914, 22);
  RE(536870914, 23);
  RE(536870914, 24);
  RE(536870914, 27);
  RE(536870914, 30);
  RE(536870914, 31);
  RE(536870914, 32);
  RE(536870914, 37);
  RE(536870914, 43);
  RE(536870914, 44);
  RE(536870914, 45);
  RE(536870914, 46);
  RE(536870914, 48);
  RE(536870914, 49);
  RE(536870914, -1);
  RE(1073741884, 26);
  RE(1073741885, 25);
  YaccRows[24] := Row;
  //  [25]
  R(0, 0, 17);
  RE(536870913, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[25] := Row;
  //  [26]
  R(0, 0, 1);
  RE(268435456, -1);
  YaccRows[26] := Row;
  //  [27]
  R(0, 0, 0);
  YaccRows[27] := Row;
  //  [28]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(59, 16385);
  YaccRows[28] := Row;
  //  [29]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(58, 16385);
  YaccRows[29] := Row;
  //  [30]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(57, 16385);
  YaccRows[30] := Row;
  //  [31]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(56, 16385);
  YaccRows[31] := Row;
  //  [32]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(55, 16385);
  YaccRows[32] := Row;
  //  [33]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(54, 16385);
  YaccRows[33] := Row;
  //  [34]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(53, 16385);
  YaccRows[34] := Row;
  //  [35]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(52, 16385);
  YaccRows[35] := Row;
  //  [36]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(51, 16385);
  YaccRows[36] := Row;
  //  [37]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(50, 16385);
  YaccRows[37] := Row;
  //  [38]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(49, 16385);
  YaccRows[38] := Row;
  //  [39]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(48, 16385);
  YaccRows[39] := Row;
  //  [40]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(47, 16385);
  YaccRows[40] := Row;
  //  [41]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(46, 16385);
  YaccRows[41] := Row;
  //  [42]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(45, 16385);
  YaccRows[42] := Row;
  //  [43]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(44, 16385);
  YaccRows[43] := Row;
  //  [44]
  R(0, 0, 25);
  RE(536870921, 10);
  RE(536870921, 12);
  RE(536870921, 14);
  RE(536870921, 15);
  RE(536870921, 16);
  RE(536870921, 17);
  RE(536870921, 18);
  RE(536870921, 19);
  RE(536870921, 20);
  RE(536870921, 27);
  RE(536870921, 30);
  RE(536870921, 31);
  RE(536870921, 32);
  RE(536870921, 37);
  RE(536870921, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[44] := Row;
  //  [45]
  R(0, 0, 25);
  RE(536870920, 10);
  RE(536870920, 12);
  RE(536870920, 14);
  RE(536870920, 15);
  RE(536870920, 16);
  RE(536870920, 17);
  RE(536870920, 18);
  RE(536870920, 19);
  RE(536870920, 20);
  RE(536870920, 27);
  RE(536870920, 30);
  RE(536870920, 31);
  RE(536870920, 32);
  RE(536870920, 37);
  RE(536870920, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[45] := Row;
  //  [46]
  R(0, 0, 25);
  RE(536870919, 10);
  RE(536870919, 12);
  RE(536870919, 14);
  RE(536870919, 15);
  RE(536870919, 16);
  RE(536870919, 17);
  RE(536870919, 18);
  RE(536870919, 19);
  RE(536870919, 20);
  RE(536870919, 27);
  RE(536870919, 30);
  RE(536870919, 31);
  RE(536870919, 32);
  RE(536870919, 37);
  RE(536870919, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[46] := Row;
  //  [47]
  R(0, 0, 25);
  RE(536870923, 10);
  RE(536870923, 12);
  RE(536870923, 14);
  RE(536870923, 15);
  RE(536870923, 16);
  RE(536870923, 17);
  RE(536870923, 18);
  RE(536870923, 19);
  RE(536870923, 20);
  RE(536870923, 27);
  RE(536870923, 30);
  RE(536870923, 31);
  RE(536870923, 32);
  RE(536870923, 37);
  RE(536870923, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[47] := Row;
  //  [48]
  R(0, 0, 25);
  RE(536870922, 10);
  RE(536870922, 12);
  RE(536870922, 14);
  RE(536870922, 15);
  RE(536870922, 16);
  RE(536870922, 17);
  RE(536870922, 18);
  RE(536870922, 19);
  RE(536870922, 20);
  RE(536870922, 27);
  RE(536870922, 30);
  RE(536870922, 31);
  RE(536870922, 32);
  RE(536870922, 37);
  RE(536870922, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[48] := Row;
  //  [49]
  R(0, 0, 25);
  RE(536870924, 10);
  RE(536870924, 12);
  RE(536870924, 14);
  RE(536870924, 15);
  RE(536870924, 16);
  RE(536870924, 17);
  RE(536870924, 18);
  RE(536870924, 19);
  RE(536870924, 20);
  RE(536870924, 27);
  RE(536870924, 30);
  RE(536870924, 31);
  RE(536870924, 32);
  RE(536870924, 37);
  RE(536870924, -1);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[49] := Row;
  //  [50]
  R(0, 0, 25);
  RE(536870925, 10);
  RE(536870925, 12);
  RE(536870925, 14);
  RE(536870925, 15);
  RE(536870925, 16);
  RE(536870925, 17);
  RE(536870925, 18);
  RE(536870925, 19);
  RE(536870925, 20);
  RE(536870925, 21);
  RE(536870925, 22);
  RE(536870925, 27);
  RE(536870925, 30);
  RE(536870925, 31);
  RE(536870925, 32);
  RE(536870925, 37);
  RE(536870925, 45);
  RE(536870925, 46);
  RE(536870925, 48);
  RE(536870925, 49);
  RE(536870925, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  YaccRows[50] := Row;
  //  [51]
  R(0, 0, 25);
  RE(536870926, 10);
  RE(536870926, 12);
  RE(536870926, 14);
  RE(536870926, 15);
  RE(536870926, 16);
  RE(536870926, 17);
  RE(536870926, 18);
  RE(536870926, 19);
  RE(536870926, 20);
  RE(536870926, 21);
  RE(536870926, 22);
  RE(536870926, 27);
  RE(536870926, 30);
  RE(536870926, 31);
  RE(536870926, 32);
  RE(536870926, 37);
  RE(536870926, 45);
  RE(536870926, 46);
  RE(536870926, 48);
  RE(536870926, 49);
  RE(536870926, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  YaccRows[51] := Row;
  //  [52]
  R(0, 0, 25);
  RE(536870928, 10);
  RE(536870928, 12);
  RE(536870928, 14);
  RE(536870928, 15);
  RE(536870928, 16);
  RE(536870928, 17);
  RE(536870928, 18);
  RE(536870928, 19);
  RE(536870928, 20);
  RE(536870928, 21);
  RE(536870928, 22);
  RE(536870928, 23);
  RE(536870928, 24);
  RE(536870928, 27);
  RE(536870928, 30);
  RE(536870928, 31);
  RE(536870928, 32);
  RE(536870928, 37);
  RE(536870928, 43);
  RE(536870928, 44);
  RE(536870928, 45);
  RE(536870928, 46);
  RE(536870928, 48);
  RE(536870928, 49);
  RE(536870928, -1);
  YaccRows[52] := Row;
  //  [53]
  R(0, 0, 25);
  RE(536870927, 10);
  RE(536870927, 12);
  RE(536870927, 14);
  RE(536870927, 15);
  RE(536870927, 16);
  RE(536870927, 17);
  RE(536870927, 18);
  RE(536870927, 19);
  RE(536870927, 20);
  RE(536870927, 21);
  RE(536870927, 22);
  RE(536870927, 23);
  RE(536870927, 24);
  RE(536870927, 27);
  RE(536870927, 30);
  RE(536870927, 31);
  RE(536870927, 32);
  RE(536870927, 37);
  RE(536870927, 43);
  RE(536870927, 44);
  RE(536870927, 45);
  RE(536870927, 46);
  RE(536870927, 48);
  RE(536870927, 49);
  RE(536870927, -1);
  YaccRows[53] := Row;
  //  [54]
  R(0, 0, 25);
  RE(536870929, 10);
  RE(536870929, 12);
  RE(536870929, 14);
  RE(536870929, 15);
  RE(536870929, 16);
  RE(536870929, 17);
  RE(536870929, 18);
  RE(536870929, 19);
  RE(536870929, 20);
  RE(536870929, 21);
  RE(536870929, 22);
  RE(536870929, 23);
  RE(536870929, 24);
  RE(536870929, 27);
  RE(536870929, 30);
  RE(536870929, 31);
  RE(536870929, 32);
  RE(536870929, 37);
  RE(536870929, 43);
  RE(536870929, 44);
  RE(536870929, 45);
  RE(536870929, 46);
  RE(536870929, 48);
  RE(536870929, 49);
  RE(536870929, -1);
  YaccRows[54] := Row;
  //  [55]
  R(0, 0, 25);
  RE(536870930, 10);
  RE(536870930, 12);
  RE(536870930, 14);
  RE(536870930, 15);
  RE(536870930, 16);
  RE(536870930, 17);
  RE(536870930, 18);
  RE(536870930, 19);
  RE(536870930, 20);
  RE(536870930, 21);
  RE(536870930, 22);
  RE(536870930, 23);
  RE(536870930, 24);
  RE(536870930, 27);
  RE(536870930, 30);
  RE(536870930, 31);
  RE(536870930, 32);
  RE(536870930, 37);
  RE(536870930, 43);
  RE(536870930, 44);
  RE(536870930, 45);
  RE(536870930, 46);
  RE(536870930, 48);
  RE(536870930, 49);
  RE(536870930, -1);
  YaccRows[55] := Row;
  //  [56]
  R(0, 0, 25);
  RE(536870915, 10);
  RE(536870915, 12);
  RE(536870915, 14);
  RE(536870915, 15);
  RE(536870915, 16);
  RE(536870915, 17);
  RE(536870915, 18);
  RE(536870915, 19);
  RE(536870915, 20);
  RE(536870915, 27);
  RE(536870915, 30);
  RE(536870915, 31);
  RE(536870915, 32);
  RE(536870915, 37);
  RE(536870915, 45);
  RE(536870915, 46);
  RE(536870915, 48);
  RE(536870915, 49);
  RE(536870915, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[56] := Row;
  //  [57]
  R(0, 0, 25);
  RE(536870916, 10);
  RE(536870916, 12);
  RE(536870916, 14);
  RE(536870916, 15);
  RE(536870916, 16);
  RE(536870916, 17);
  RE(536870916, 18);
  RE(536870916, 19);
  RE(536870916, 20);
  RE(536870916, 27);
  RE(536870916, 30);
  RE(536870916, 31);
  RE(536870916, 32);
  RE(536870916, 37);
  RE(536870916, 45);
  RE(536870916, 46);
  RE(536870916, 48);
  RE(536870916, 49);
  RE(536870916, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[57] := Row;
  //  [58]
  R(0, 0, 25);
  RE(536870917, 10);
  RE(536870917, 12);
  RE(536870917, 14);
  RE(536870917, 15);
  RE(536870917, 16);
  RE(536870917, 17);
  RE(536870917, 18);
  RE(536870917, 19);
  RE(536870917, 20);
  RE(536870917, 27);
  RE(536870917, 30);
  RE(536870917, 31);
  RE(536870917, 32);
  RE(536870917, 37);
  RE(536870917, 45);
  RE(536870917, 46);
  RE(536870917, 48);
  RE(536870917, 49);
  RE(536870917, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[58] := Row;
  //  [59]
  R(0, 0, 25);
  RE(536870918, 10);
  RE(536870918, 12);
  RE(536870918, 14);
  RE(536870918, 15);
  RE(536870918, 16);
  RE(536870918, 17);
  RE(536870918, 18);
  RE(536870918, 19);
  RE(536870918, 20);
  RE(536870918, 27);
  RE(536870918, 30);
  RE(536870918, 31);
  RE(536870918, 32);
  RE(536870918, 37);
  RE(536870918, 49);
  RE(536870918, -1);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  YaccRows[59] := Row;
  //  [60]
  R(0, 2, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(18, 16398);
  RE(101, 16390);
  YaccRows[60] := Row;
  //  [61]
  R(0, 2, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(18, 16398);
  RE(62, 16390);
  YaccRows[61] := Row;
  //  [62]
  R(0, 1, 30);
  RE(536870976, 9);
  RE(536870976, 10);
  RE(536870976, 11);
  RE(536870976, 12);
  RE(536870976, 14);
  RE(536870976, 15);
  RE(536870976, 16);
  RE(536870976, 17);
  RE(536870976, 18);
  RE(536870976, 19);
  RE(536870976, 20);
  RE(536870976, 21);
  RE(536870976, 22);
  RE(536870976, 23);
  RE(536870976, 24);
  RE(536870976, 25);
  RE(536870976, 26);
  RE(536870976, 27);
  RE(536870976, 30);
  RE(536870976, 31);
  RE(536870976, 32);
  RE(536870976, 37);
  RE(536870976, 43);
  RE(536870976, 44);
  RE(536870976, 45);
  RE(536870976, 46);
  RE(536870976, 48);
  RE(536870976, 49);
  RE(536870976, -1);
  RE(1073741887, 28);
  RE(64, 16391);
  YaccRows[62] := Row;
  //  [63]
  R(0, 1, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(100, 16398);
  YaccRows[63] := Row;
  //  [64]
  R(0, 1, 29);
  RE(536870966, 10);
  RE(536870966, 11);
  RE(536870966, 12);
  RE(536870966, 14);
  RE(536870966, 15);
  RE(536870966, 16);
  RE(536870966, 17);
  RE(536870966, 18);
  RE(536870966, 19);
  RE(536870966, 20);
  RE(536870966, 21);
  RE(536870966, 22);
  RE(536870966, 23);
  RE(536870966, 24);
  RE(536870966, 25);
  RE(536870966, 26);
  RE(536870966, 27);
  RE(536870966, 30);
  RE(536870966, 31);
  RE(536870966, 32);
  RE(536870966, 37);
  RE(536870966, 43);
  RE(536870966, 44);
  RE(536870966, 45);
  RE(536870966, 46);
  RE(536870966, 48);
  RE(536870966, 49);
  RE(536870966, -1);
  RE(1073741889, 9);
  RE(66, 16392);
  YaccRows[64] := Row;
  //  [65]
  R(0, 11, 17);
  RE(536870977, 10);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(70, 16406);
  RE(17, 16401);
  RE(18, 16398);
  RE(98, 16396);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(72, 16385);
  YaccRows[65] := Row;
  //  [66]
  R(0, 1, 28);
  RE(536870935, 10);
  RE(536870935, 12);
  RE(536870935, 14);
  RE(536870935, 15);
  RE(536870935, 16);
  RE(536870935, 17);
  RE(536870935, 18);
  RE(536870935, 19);
  RE(536870935, 20);
  RE(536870935, 21);
  RE(536870935, 22);
  RE(536870935, 23);
  RE(536870935, 24);
  RE(536870935, 25);
  RE(536870935, 26);
  RE(536870935, 27);
  RE(536870935, 30);
  RE(536870935, 31);
  RE(536870935, 32);
  RE(536870935, 37);
  RE(536870935, 43);
  RE(536870935, 44);
  RE(536870935, 45);
  RE(536870935, 46);
  RE(536870935, 48);
  RE(536870935, 49);
  RE(536870935, -1);
  RE(1073741891, 11);
  RE(68, 16393);
  YaccRows[66] := Row;
  //  [67]
  R(0, 11, 17);
  RE(536870977, 12);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741893, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(70, 16406);
  RE(17, 16401);
  RE(18, 16398);
  RE(71, 16396);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(72, 16385);
  YaccRows[67] := Row;
  //  [68]
  R(0, 0, 27);
  RE(536870936, 10);
  RE(536870936, 12);
  RE(536870936, 14);
  RE(536870936, 15);
  RE(536870936, 16);
  RE(536870936, 17);
  RE(536870936, 18);
  RE(536870936, 19);
  RE(536870936, 20);
  RE(536870936, 21);
  RE(536870936, 22);
  RE(536870936, 23);
  RE(536870936, 24);
  RE(536870936, 25);
  RE(536870936, 26);
  RE(536870936, 27);
  RE(536870936, 30);
  RE(536870936, 31);
  RE(536870936, 32);
  RE(536870936, 37);
  RE(536870936, 43);
  RE(536870936, 44);
  RE(536870936, 45);
  RE(536870936, 46);
  RE(536870936, 48);
  RE(536870936, 49);
  RE(536870936, -1);
  YaccRows[68] := Row;
  //  [69]
  R(0, 0, 26);
  RE(536870974, 9);
  RE(536870974, 11);
  RE(536870974, 12);
  RE(536870974, 15);
  RE(536870974, 16);
  RE(536870974, 17);
  RE(536870974, 18);
  RE(536870974, 19);
  RE(536870974, 20);
  RE(536870974, 21);
  RE(536870974, 22);
  RE(536870974, 23);
  RE(536870974, 24);
  RE(536870974, 25);
  RE(536870974, 26);
  RE(536870974, 27);
  RE(536870974, 28);
  RE(536870974, 36);
  RE(536870974, 43);
  RE(536870974, 44);
  RE(536870974, 45);
  RE(536870974, 46);
  RE(536870974, 48);
  RE(536870974, 49);
  RE(1073741900, 38);
  RE(1073741901, 35);
  YaccRows[69] := Row;
  //  [70]
  R(0, 0, 2);
  RE(536870978, 10);
  RE(536870978, 12);
  YaccRows[70] := Row;
  //  [71]
  R(0, 0, 1);
  RE(1073741899, 12);
  YaccRows[71] := Row;
  //  [72]
  R(0, 0, 19);
  RE(536870979, 10);
  RE(536870979, 12);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741897, 27);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[72] := Row;
  //  [73]
  R(0, 10, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(74, 16406);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(72, 16385);
  YaccRows[73] := Row;
  //  [74]
  R(0, 0, 2);
  RE(536870980, 10);
  RE(536870980, 12);
  YaccRows[74] := Row;
  //  [75]
  R(0, 0, 27);
  RE(536870945, 10);
  RE(536870945, 12);
  RE(536870945, 14);
  RE(536870945, 15);
  RE(536870945, 16);
  RE(536870945, 17);
  RE(536870945, 18);
  RE(536870945, 19);
  RE(536870945, 20);
  RE(536870945, 21);
  RE(536870945, 22);
  RE(536870945, 23);
  RE(536870945, 24);
  RE(536870945, 25);
  RE(536870945, 26);
  RE(536870945, 27);
  RE(536870945, 30);
  RE(536870945, 31);
  RE(536870945, 32);
  RE(536870945, 37);
  RE(536870945, 43);
  RE(536870945, 44);
  RE(536870945, 45);
  RE(536870945, 46);
  RE(536870945, 48);
  RE(536870945, 49);
  RE(536870945, -1);
  YaccRows[75] := Row;
  //  [76]
  R(0, 4, 3);
  RE(536870955, 35);
  RE(1073741904, 33);
  RE(1073741905, 5);
  RE(82, 16405);
  RE(83, 16400);
  RE(84, 16399);
  RE(85, 16397);
  YaccRows[76] := Row;
  //  [77]
  R(0, 11, 17);
  RE(536870977, 12);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(70, 16406);
  RE(17, 16401);
  RE(18, 16398);
  RE(78, 16396);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(72, 16385);
  YaccRows[77] := Row;
  //  [78]
  R(0, 0, 1);
  RE(1073741903, 12);
  YaccRows[78] := Row;
  //  [79]
  R(0, 0, 27);
  RE(536870946, 10);
  RE(536870946, 12);
  RE(536870946, 14);
  RE(536870946, 15);
  RE(536870946, 16);
  RE(536870946, 17);
  RE(536870946, 18);
  RE(536870946, 19);
  RE(536870946, 20);
  RE(536870946, 21);
  RE(536870946, 22);
  RE(536870946, 23);
  RE(536870946, 24);
  RE(536870946, 25);
  RE(536870946, 26);
  RE(536870946, 27);
  RE(536870946, 30);
  RE(536870946, 31);
  RE(536870946, 32);
  RE(536870946, 37);
  RE(536870946, 43);
  RE(536870946, 44);
  RE(536870946, 45);
  RE(536870946, 46);
  RE(536870946, 48);
  RE(536870946, 49);
  RE(536870946, -1);
  YaccRows[79] := Row;
  //  [80]
  R(0, 0, 1);
  RE(1073741915, 13);
  YaccRows[80] := Row;
  //  [81]
  R(0, 0, 2);
  RE(536870969, 35);
  RE(1073741913, 36);
  YaccRows[81] := Row;
  //  [82]
  R(0, 0, 1);
  RE(536870968, 35);
  YaccRows[82] := Row;
  //  [83]
  R(0, 0, 1);
  RE(536870957, 35);
  YaccRows[83] := Row;
  //  [84]
  R(0, 0, 1);
  RE(536870956, 35);
  YaccRows[84] := Row;
  //  [85]
  R(0, 0, 1);
  RE(1073741910, 35);
  YaccRows[85] := Row;
  //  [86]
  R(0, 11, 17);
  RE(536870977, 12);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(70, 16406);
  RE(17, 16401);
  RE(18, 16398);
  RE(87, 16396);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(72, 16385);
  YaccRows[86] := Row;
  //  [87]
  R(0, 0, 1);
  RE(1073741912, 12);
  YaccRows[87] := Row;
  //  [88]
  R(0, 0, 27);
  RE(536870947, 10);
  RE(536870947, 12);
  RE(536870947, 14);
  RE(536870947, 15);
  RE(536870947, 16);
  RE(536870947, 17);
  RE(536870947, 18);
  RE(536870947, 19);
  RE(536870947, 20);
  RE(536870947, 21);
  RE(536870947, 22);
  RE(536870947, 23);
  RE(536870947, 24);
  RE(536870947, 25);
  RE(536870947, 26);
  RE(536870947, 27);
  RE(536870947, 30);
  RE(536870947, 31);
  RE(536870947, 32);
  RE(536870947, 37);
  RE(536870947, 43);
  RE(536870947, 44);
  RE(536870947, 45);
  RE(536870947, 46);
  RE(536870947, 48);
  RE(536870947, 49);
  RE(536870947, -1);
  YaccRows[88] := Row;
  //  [89]
  R(0, 1, 1);
  RE(1073741905, 5);
  RE(90, 16405);
  YaccRows[89] := Row;
  //  [90]
  R(0, 0, 1);
  RE(536870970, 35);
  YaccRows[90] := Row;
  //  [91]
  R(0, 1, 1);
  RE(1073741916, 34);
  RE(93, 16404);
  YaccRows[91] := Row;
  //  [92]
  R(0, 1, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(95, 16398);
  YaccRows[92] := Row;
  //  [93]
  R(0, 0, 1);
  RE(1073741918, 14);
  YaccRows[93] := Row;
  //  [94]
  R(0, 0, 1);
  RE(536870963, 35);
  YaccRows[94] := Row;
  //  [95]
  R(0, 0, 2);
  RE(536870964, 14);
  RE(1073741920, 27);
  YaccRows[95] := Row;
  //  [96]
  R(0, 1, 1);
  RE(1073741916, 34);
  RE(97, 16404);
  YaccRows[96] := Row;
  //  [97]
  R(0, 0, 1);
  RE(536870965, 14);
  YaccRows[97] := Row;
  //  [98]
  R(0, 0, 1);
  RE(1073741923, 10);
  YaccRows[98] := Row;
  //  [99]
  R(0, 0, 28);
  RE(536870967, 10);
  RE(536870967, 11);
  RE(536870967, 12);
  RE(536870967, 14);
  RE(536870967, 15);
  RE(536870967, 16);
  RE(536870967, 17);
  RE(536870967, 18);
  RE(536870967, 19);
  RE(536870967, 20);
  RE(536870967, 21);
  RE(536870967, 22);
  RE(536870967, 23);
  RE(536870967, 24);
  RE(536870967, 25);
  RE(536870967, 26);
  RE(536870967, 27);
  RE(536870967, 30);
  RE(536870967, 31);
  RE(536870967, 32);
  RE(536870967, 37);
  RE(536870967, 43);
  RE(536870967, 44);
  RE(536870967, 45);
  RE(536870967, 46);
  RE(536870967, 48);
  RE(536870967, 49);
  RE(536870967, -1);
  YaccRows[99] := Row;
  //  [100]
  R(0, 0, 29);
  RE(536870975, 9);
  RE(536870975, 10);
  RE(536870975, 11);
  RE(536870975, 12);
  RE(536870975, 14);
  RE(536870975, 15);
  RE(536870975, 16);
  RE(536870975, 17);
  RE(536870975, 18);
  RE(536870975, 19);
  RE(536870975, 20);
  RE(536870975, 21);
  RE(536870975, 22);
  RE(536870975, 23);
  RE(536870975, 24);
  RE(536870975, 25);
  RE(536870975, 26);
  RE(536870975, 27);
  RE(536870975, 30);
  RE(536870975, 31);
  RE(536870975, 32);
  RE(536870975, 37);
  RE(536870975, 43);
  RE(536870975, 44);
  RE(536870975, 45);
  RE(536870975, 46);
  RE(536870975, 48);
  RE(536870975, 49);
  RE(536870975, -1);
  YaccRows[100] := Row;
  //  [101]
  R(0, 1, 30);
  RE(536870976, 9);
  RE(536870976, 10);
  RE(536870976, 11);
  RE(536870976, 12);
  RE(536870976, 14);
  RE(536870976, 15);
  RE(536870976, 16);
  RE(536870976, 17);
  RE(536870976, 18);
  RE(536870976, 19);
  RE(536870976, 20);
  RE(536870976, 21);
  RE(536870976, 22);
  RE(536870976, 23);
  RE(536870976, 24);
  RE(536870976, 25);
  RE(536870976, 26);
  RE(536870976, 27);
  RE(536870976, 30);
  RE(536870976, 31);
  RE(536870976, 32);
  RE(536870976, 37);
  RE(536870976, 43);
  RE(536870976, 44);
  RE(536870976, 45);
  RE(536870976, 46);
  RE(536870976, 48);
  RE(536870976, 49);
  RE(536870976, -1);
  RE(1073741887, 28);
  RE(102, 16391);
  YaccRows[101] := Row;
  //  [102]
  R(0, 1, 29);
  RE(536870966, 10);
  RE(536870966, 11);
  RE(536870966, 12);
  RE(536870966, 14);
  RE(536870966, 15);
  RE(536870966, 16);
  RE(536870966, 17);
  RE(536870966, 18);
  RE(536870966, 19);
  RE(536870966, 20);
  RE(536870966, 21);
  RE(536870966, 22);
  RE(536870966, 23);
  RE(536870966, 24);
  RE(536870966, 25);
  RE(536870966, 26);
  RE(536870966, 27);
  RE(536870966, 30);
  RE(536870966, 31);
  RE(536870966, 32);
  RE(536870966, 37);
  RE(536870966, 43);
  RE(536870966, 44);
  RE(536870966, 45);
  RE(536870966, 46);
  RE(536870966, 48);
  RE(536870966, 49);
  RE(536870966, -1);
  RE(1073741889, 9);
  RE(103, 16392);
  YaccRows[102] := Row;
  //  [103]
  R(0, 1, 28);
  RE(536870937, 10);
  RE(536870937, 12);
  RE(536870937, 14);
  RE(536870937, 15);
  RE(536870937, 16);
  RE(536870937, 17);
  RE(536870937, 18);
  RE(536870937, 19);
  RE(536870937, 20);
  RE(536870937, 21);
  RE(536870937, 22);
  RE(536870937, 23);
  RE(536870937, 24);
  RE(536870937, 25);
  RE(536870937, 26);
  RE(536870937, 27);
  RE(536870937, 30);
  RE(536870937, 31);
  RE(536870937, 32);
  RE(536870937, 37);
  RE(536870937, 43);
  RE(536870937, 44);
  RE(536870937, 45);
  RE(536870937, 46);
  RE(536870937, 48);
  RE(536870937, 49);
  RE(536870937, -1);
  RE(1073741891, 11);
  RE(104, 16393);
  YaccRows[103] := Row;
  //  [104]
  R(0, 0, 27);
  RE(536870938, 10);
  RE(536870938, 12);
  RE(536870938, 14);
  RE(536870938, 15);
  RE(536870938, 16);
  RE(536870938, 17);
  RE(536870938, 18);
  RE(536870938, 19);
  RE(536870938, 20);
  RE(536870938, 21);
  RE(536870938, 22);
  RE(536870938, 23);
  RE(536870938, 24);
  RE(536870938, 25);
  RE(536870938, 26);
  RE(536870938, 27);
  RE(536870938, 30);
  RE(536870938, 31);
  RE(536870938, 32);
  RE(536870938, 37);
  RE(536870938, 43);
  RE(536870938, 44);
  RE(536870938, 45);
  RE(536870938, 46);
  RE(536870938, 48);
  RE(536870938, 49);
  RE(536870938, -1);
  YaccRows[104] := Row;
  //  [105]
  R(0, 1, 29);
  RE(536870966, 10);
  RE(536870966, 11);
  RE(536870966, 12);
  RE(536870966, 14);
  RE(536870966, 15);
  RE(536870966, 16);
  RE(536870966, 17);
  RE(536870966, 18);
  RE(536870966, 19);
  RE(536870966, 20);
  RE(536870966, 21);
  RE(536870966, 22);
  RE(536870966, 23);
  RE(536870966, 24);
  RE(536870966, 25);
  RE(536870966, 26);
  RE(536870966, 27);
  RE(536870966, 30);
  RE(536870966, 31);
  RE(536870966, 32);
  RE(536870966, 37);
  RE(536870966, 43);
  RE(536870966, 44);
  RE(536870966, 45);
  RE(536870966, 46);
  RE(536870966, 48);
  RE(536870966, 49);
  RE(536870966, -1);
  RE(1073741889, 9);
  RE(106, 16392);
  YaccRows[105] := Row;
  //  [106]
  R(0, 1, 28);
  RE(536870943, 10);
  RE(536870943, 12);
  RE(536870943, 14);
  RE(536870943, 15);
  RE(536870943, 16);
  RE(536870943, 17);
  RE(536870943, 18);
  RE(536870943, 19);
  RE(536870943, 20);
  RE(536870943, 21);
  RE(536870943, 22);
  RE(536870943, 23);
  RE(536870943, 24);
  RE(536870943, 25);
  RE(536870943, 26);
  RE(536870943, 27);
  RE(536870943, 30);
  RE(536870943, 31);
  RE(536870943, 32);
  RE(536870943, 37);
  RE(536870943, 43);
  RE(536870943, 44);
  RE(536870943, 45);
  RE(536870943, 46);
  RE(536870943, 48);
  RE(536870943, 49);
  RE(536870943, -1);
  RE(1073741891, 11);
  RE(107, 16393);
  YaccRows[106] := Row;
  //  [107]
  R(0, 0, 27);
  RE(536870944, 10);
  RE(536870944, 12);
  RE(536870944, 14);
  RE(536870944, 15);
  RE(536870944, 16);
  RE(536870944, 17);
  RE(536870944, 18);
  RE(536870944, 19);
  RE(536870944, 20);
  RE(536870944, 21);
  RE(536870944, 22);
  RE(536870944, 23);
  RE(536870944, 24);
  RE(536870944, 25);
  RE(536870944, 26);
  RE(536870944, 27);
  RE(536870944, 30);
  RE(536870944, 31);
  RE(536870944, 32);
  RE(536870944, 37);
  RE(536870944, 43);
  RE(536870944, 44);
  RE(536870944, 45);
  RE(536870944, 46);
  RE(536870944, 48);
  RE(536870944, 49);
  RE(536870944, -1);
  YaccRows[107] := Row;
  //  [108]
  R(0, 2, 2);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(18, 16398);
  RE(109, 16390);
  YaccRows[108] := Row;
  //  [109]
  R(0, 0, 30);
  RE(536870972, 9);
  RE(536870972, 10);
  RE(536870972, 11);
  RE(536870972, 12);
  RE(536870972, 14);
  RE(536870972, 15);
  RE(536870972, 16);
  RE(536870972, 17);
  RE(536870972, 18);
  RE(536870972, 19);
  RE(536870972, 20);
  RE(536870972, 21);
  RE(536870972, 22);
  RE(536870972, 23);
  RE(536870972, 24);
  RE(536870972, 25);
  RE(536870972, 26);
  RE(536870972, 27);
  RE(536870972, 28);
  RE(536870972, 30);
  RE(536870972, 31);
  RE(536870972, 32);
  RE(536870972, 37);
  RE(536870972, 43);
  RE(536870972, 44);
  RE(536870972, 45);
  RE(536870972, 46);
  RE(536870972, 48);
  RE(536870972, 49);
  RE(536870972, -1);
  YaccRows[109] := Row;
  //  [110]
  R(0, 11, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(111, 16403);
  RE(112, 16402);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(113, 16385);
  YaccRows[110] := Row;
  //  [111]
  R(0, 0, 1);
  RE(536870959, 14);
  YaccRows[111] := Row;
  //  [112]
  R(0, 0, 1);
  RE(1073741943, 14);
  YaccRows[112] := Row;
  //  [113]
  R(0, 0, 19);
  RE(536870960, 14);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741938, 37);
  RE(1073741939, 27);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[113] := Row;
  //  [114]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(118, 16385);
  YaccRows[114] := Row;
  //  [115]
  R(0, 10, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(116, 16403);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(117, 16385);
  YaccRows[115] := Row;
  //  [116]
  R(0, 0, 1);
  RE(536870961, 14);
  YaccRows[116] := Row;
  //  [117]
  R(0, 0, 18);
  RE(536870960, 14);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741939, 27);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[117] := Row;
  //  [118]
  R(0, 0, 17);
  RE(536870962, 14);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[118] := Row;
  //  [119]
  R(0, 0, 27);
  RE(536870958, 10);
  RE(536870958, 12);
  RE(536870958, 14);
  RE(536870958, 15);
  RE(536870958, 16);
  RE(536870958, 17);
  RE(536870958, 18);
  RE(536870958, 19);
  RE(536870958, 20);
  RE(536870958, 21);
  RE(536870958, 22);
  RE(536870958, 23);
  RE(536870958, 24);
  RE(536870958, 25);
  RE(536870958, 26);
  RE(536870958, 27);
  RE(536870958, 30);
  RE(536870958, 31);
  RE(536870958, 32);
  RE(536870958, 37);
  RE(536870958, 43);
  RE(536870958, 44);
  RE(536870958, 45);
  RE(536870958, 46);
  RE(536870958, 48);
  RE(536870958, 49);
  RE(536870958, -1);
  YaccRows[119] := Row;
  //  [120]
  R(0, 0, 27);
  RE(536870949, 10);
  RE(536870949, 12);
  RE(536870949, 14);
  RE(536870949, 15);
  RE(536870949, 16);
  RE(536870949, 17);
  RE(536870949, 18);
  RE(536870949, 19);
  RE(536870949, 20);
  RE(536870949, 21);
  RE(536870949, 22);
  RE(536870949, 23);
  RE(536870949, 24);
  RE(536870949, 25);
  RE(536870949, 26);
  RE(536870949, 27);
  RE(536870949, 30);
  RE(536870949, 31);
  RE(536870949, 32);
  RE(536870949, 37);
  RE(536870949, 43);
  RE(536870949, 44);
  RE(536870949, 45);
  RE(536870949, 46);
  RE(536870949, 48);
  RE(536870949, 49);
  RE(536870949, -1);
  YaccRows[120] := Row;
  //  [121]
  R(0, 0, 1);
  RE(1073741946, 51);
  YaccRows[121] := Row;
  //  [122]
  R(0, 0, 27);
  RE(536870948, 10);
  RE(536870948, 12);
  RE(536870948, 14);
  RE(536870948, 15);
  RE(536870948, 16);
  RE(536870948, 17);
  RE(536870948, 18);
  RE(536870948, 19);
  RE(536870948, 20);
  RE(536870948, 21);
  RE(536870948, 22);
  RE(536870948, 23);
  RE(536870948, 24);
  RE(536870948, 25);
  RE(536870948, 26);
  RE(536870948, 27);
  RE(536870948, 30);
  RE(536870948, 31);
  RE(536870948, 32);
  RE(536870948, 37);
  RE(536870948, 43);
  RE(536870948, 44);
  RE(536870948, 45);
  RE(536870948, 46);
  RE(536870948, 48);
  RE(536870948, 49);
  RE(536870948, -1);
  YaccRows[122] := Row;
  //  [123]
  R(0, 0, 17);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  RE(1073741948, 12);
  YaccRows[123] := Row;
  //  [124]
  R(0, 0, 27);
  RE(536870941, 10);
  RE(536870941, 12);
  RE(536870941, 14);
  RE(536870941, 15);
  RE(536870941, 16);
  RE(536870941, 17);
  RE(536870941, 18);
  RE(536870941, 19);
  RE(536870941, 20);
  RE(536870941, 21);
  RE(536870941, 22);
  RE(536870941, 23);
  RE(536870941, 24);
  RE(536870941, 25);
  RE(536870941, 26);
  RE(536870941, 27);
  RE(536870941, 30);
  RE(536870941, 31);
  RE(536870941, 32);
  RE(536870941, 37);
  RE(536870941, 43);
  RE(536870941, 44);
  RE(536870941, 45);
  RE(536870941, 46);
  RE(536870941, 48);
  RE(536870941, 49);
  RE(536870941, -1);
  YaccRows[124] := Row;
  //  [125]
  R(0, 0, 25);
  RE(536870931, 10);
  RE(536870931, 12);
  RE(536870931, 14);
  RE(536870931, 15);
  RE(536870931, 16);
  RE(536870931, 17);
  RE(536870931, 18);
  RE(536870931, 19);
  RE(536870931, 20);
  RE(536870931, 21);
  RE(536870931, 22);
  RE(536870931, 27);
  RE(536870931, 30);
  RE(536870931, 31);
  RE(536870931, 32);
  RE(536870931, 37);
  RE(536870931, 45);
  RE(536870931, 46);
  RE(536870931, 48);
  RE(536870931, 49);
  RE(536870931, -1);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741858, 24);
  RE(1073741859, 23);
  YaccRows[125] := Row;
  //  [126]
  R(0, 0, 17);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741951, 30);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[126] := Row;
  //  [127]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(128, 16385);
  YaccRows[127] := Row;
  //  [128]
  R(0, 0, 17);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741953, 31);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[128] := Row;
  //  [129]
  R(0, 9, 16);
  RE(1073741825, 47);
  RE(1073741826, 42);
  RE(1073741827, 41);
  RE(1073741828, 40);
  RE(1073741829, 39);
  RE(1073741830, 34);
  RE(1073741831, 29);
  RE(1073741832, 22);
  RE(1073741833, 11);
  RE(1073741834, 8);
  RE(1073741835, 7);
  RE(1073741836, 6);
  RE(1073741837, 5);
  RE(1073741838, 4);
  RE(1073741839, 3);
  RE(1073741840, 2);
  RE(17, 16401);
  RE(18, 16398);
  RE(19, 16395);
  RE(20, 16394);
  RE(21, 16390);
  RE(22, 16389);
  RE(23, 16388);
  RE(24, 16387);
  RE(130, 16385);
  YaccRows[129] := Row;
  //  [130]
  R(0, 0, 17);
  RE(1073741852, 49);
  RE(1073741853, 48);
  RE(1073741854, 46);
  RE(1073741855, 45);
  RE(1073741856, 44);
  RE(1073741857, 43);
  RE(1073741955, 32);
  RE(1073741858, 24);
  RE(1073741859, 23);
  RE(1073741860, 22);
  RE(1073741861, 21);
  RE(1073741862, 20);
  RE(1073741863, 19);
  RE(1073741864, 18);
  RE(1073741865, 17);
  RE(1073741866, 16);
  RE(1073741867, 15);
  YaccRows[130] := Row;
  //  [131]
  R(0, 0, 27);
  RE(536870933, 10);
  RE(536870933, 12);
  RE(536870933, 14);
  RE(536870933, 15);
  RE(536870933, 16);
  RE(536870933, 17);
  RE(536870933, 18);
  RE(536870933, 19);
  RE(536870933, 20);
  RE(536870933, 21);
  RE(536870933, 22);
  RE(536870933, 23);
  RE(536870933, 24);
  RE(536870933, 25);
  RE(536870933, 26);
  RE(536870933, 27);
  RE(536870933, 30);
  RE(536870933, 31);
  RE(536870933, 32);
  RE(536870933, 37);
  RE(536870933, 43);
  RE(536870933, 44);
  RE(536870933, 45);
  RE(536870933, 46);
  RE(536870933, 48);
  RE(536870933, 49);
  RE(536870933, -1);
  YaccRows[131] := Row;
  //  [132]
  R(0, 0, 27);
  RE(536870954, 10);
  RE(536870954, 12);
  RE(536870954, 14);
  RE(536870954, 15);
  RE(536870954, 16);
  RE(536870954, 17);
  RE(536870954, 18);
  RE(536870954, 19);
  RE(536870954, 20);
  RE(536870954, 21);
  RE(536870954, 22);
  RE(536870954, 23);
  RE(536870954, 24);
  RE(536870954, 25);
  RE(536870954, 26);
  RE(536870954, 27);
  RE(536870954, 30);
  RE(536870954, 31);
  RE(536870954, 32);
  RE(536870954, 37);
  RE(536870954, 43);
  RE(536870954, 44);
  RE(536870954, 45);
  RE(536870954, 46);
  RE(536870954, 48);
  RE(536870954, 49);
  RE(536870954, -1);
  YaccRows[132] := Row;
  //  [133]
  R(0, 0, 25);
  RE(536870932, 10);
  RE(536870932, 12);
  RE(536870932, 14);
  RE(536870932, 15);
  RE(536870932, 16);
  RE(536870932, 17);
  RE(536870932, 18);
  RE(536870932, 19);
  RE(536870932, 20);
  RE(536870932, 21);
  RE(536870932, 22);
  RE(536870932, 23);
  RE(536870932, 24);
  RE(536870932, 27);
  RE(536870932, 30);
  RE(536870932, 31);
  RE(536870932, 32);
  RE(536870932, 37);
  RE(536870932, 43);
  RE(536870932, 44);
  RE(536870932, 45);
  RE(536870932, 46);
  RE(536870932, 48);
  RE(536870932, 49);
  RE(536870932, -1);
  YaccRows[133] := Row;

  {$ENDREGION}

  RowArray := YaccRows;
  ProdArray := YaccProds;
end;


end.

