
{ Global compiler directives }
{$include bold.inc}

unit BoldSSLexU;

interface

uses
  Classes,
  BoldBase;

const
  SSLexConsumerBof = -1;
  SSLexStateInvalid = -1;
  SSLexStructMax = 65520;
  SSLexFinalStateFlagsContextStart = $01;
  SSLexFinalStateFlagsStartOfLine = $02;
  SSLexFinalStateFlagsPop = $08;
  SSLexFinalStateFlagsFinal = $10;
  SSLexFinalStateFlagsPush = $20;
  SSLexFinalStateFlagsIgnore = $40;
  SSLexFinalStateFlagsContextEnd = $80;
  SSLexFinalStateFlagsReduce = $100;

  SSLexMsgError = 'SSLex0105e: Invalid token, Line %ld, Offset %ld, %s';
  SSLexMsgBadList = 'SSLex0106e: Invalid expression list index, %ld';
  SSLexMsgMissingPart = 'SSLex0107e: Table or consumer missing, required';
  SSLifoMsgStackTop = 'SSLifo0002e: No top, stack empty';
  SSLifoMsgStackPop = 'SSLifo0004e: Cannot pop empty stack';

type
  SSLexLexeme = class;
  SSYaccStackElement = class;
  SSLexRoot = class;

  SSArrayOfSSYaccStackElement = array[0..(SSLexStructMax div SizeOf(Pointer))] of SSYaccStackElement;
  PSSArrayOfSSYaccStackElement = ^SSArrayOfSSYaccStackElement;

  SSLexRoot = class(TBoldMemoryManagedObject)
  end;

  SSYaccStackElement = class(SSLexRoot)
  public
    Lexeme: SSLexLexeme;
    State: Longint;
    Use: Longint;
    constructor Create;
    procedure RefInc;
    function RefDec: Boolean;
    procedure SetLexeme(TheLexeme: SSLexLexeme);
    destructor Destroy; override;
  end;

  SSStack = class(SSLexRoot)
  public
    Incr: Integer;
    Size: Integer;
    TopOfStack: Integer;
    PArray: PSSArrayOfSSYaccStackElement;
    constructor Create;
    procedure Pop;
    function Top: SSLexRoot;
    procedure Push(TheSSYaccStackElement: SSLexRoot);
    destructor Destroy; override;
  end;

  TEntry = record
    StartPoint: Longint;
    EndPoint: Longint;
    State: Longint;
  end;

  TRow = record
    Token: ShortInt;
    Push: ShortInt;
    Flags: ShortInt;
    Entries: array of TEntry;
  end;

  TRows = array of TRow;

  SSLexExpressionList = class(SSYaccStackElement)
  public
    ARows: TRows;
    function LookupState(TheToken, TheState: Longint): Longint;
  end;

  SSLexExpressionListStack = class(SSStack)
    function Top: SSLexExpressionList;
    procedure Push(TheList: SSLexExpressionList);
  end;

  SSLexTable = class(SSLexRoot)
  public
    ExpressionLists: TList;
    LexTab: array[0..1] of TRows;
    constructor Create;
    destructor Destroy; override;
    function GetExpressionList(TheList: Longint): SSLexExpressionList;
    procedure FullTab;
  end;

  SSLexLexeme = class(SSLexRoot)
  public
    Buffer: String;
    Use: Integer;
    Line: Longint;
    Offset: Longint;
    Length: Longint;
    Token: Cardinal;
    constructor Create(TheData: String; TheLength, TheLine, TheOffset: Longint);
    procedure RefInc;
    function RefDec: Boolean;
    destructor Destroy; override;
  end;

  SSLexConsumerMode = (SSLexBinaryMode, SSLexTextMode);

  SSLexConsumer = class(SSLexRoot)
  public
    Buffer: String;
    Bof: Longint;
    Line: Longint;
    Offset: Longint;
    BuffLen: Longint;
    BuffInc: Longint;
    Current: Longint;
    MarkLine: Longint;
    MarkOffset: Longint;
    ScanLine: Longint;
    ScanOffset: Longint;
    EndOfData: Boolean;
    Start: Longint;
    Index: Integer;
    DataLen: Longint;
    Mark: Longint;
    MarkContext: Longint;
    MarkContextLine: Longint;
    MarkContextOffset: Longint;
    Mode: SSLexConsumerMode;

    constructor Create(TheIncrement: Longint; TheMode: SSLexConsumerMode);
    destructor Destroy; override;
    function Next: Boolean; virtual;
    function NextBuffer: Longint; virtual;
    procedure MarkFinal;
    procedure FlushLexeme;
    procedure FlushLexemeAll;
    procedure SetContextFinal;
    procedure MarkContextFinal;
    procedure FlushStartOfLine;
    function Lexeme: SSLexLexeme;
    function LexemeAll: SSLexLexeme;
  end;

  SSLexStringConsumer = class(SSLexConsumer)
  public
    constructor Create(TheString: String);
  end;

  SSLexFileConsumer = class(SSLexConsumer)
  public
    Handle: Integer;
  end;

  SSLex = class(SSLexRoot)
  public
    State: Longint;
    Table: SSLexTable;
    Consumer: SSLexConsumer;
    List: SSLexExpressionList;
    Stack: SSLexExpressionListStack;

    constructor Create(TheConsumer: SSLexConsumer; TheTable: SSLexTable);

    procedure PopExpressionList;
    function Next: SSLexLexeme;
    procedure PushExpressionList(TheList: Longint);
    procedure GotoExpressionList(TheList: Longint);
    procedure ProcessExpressionList(ThePFinal: TRow);
    function Error: SSLexLexeme; virtual;
    function Complete(TheToken: Longint): SSLexLexeme; virtual;
    function TokenToString(TheToken: Longint): string; virtual;
    destructor Destroy; override;
  end;



implementation

uses
  BoldSSExcept,
  SysUtils;

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
  if (Lexeme <> nil) and Lexeme.RefDec then
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
  Result := Use = 0;
end;

destructor SSYaccStackElement.Destroy;
begin
  if (Lexeme <> nil) and Lexeme.RefDec then
    Lexeme.Free;
  inherited;
end;

constructor SSLexConsumer.Create(TheIncrement: Integer; TheMode: SSLexConsumerMode);
begin
  inherited Create;
  BuffInc := TheIncrement;
  ScanLine := 0;
  ScanOffset := 0;
  Line := 1;
  Offset := 1;
  Bof := SSLexConsumerBof;
  Mode := TheMode;
  Buffer := '';
end;

destructor SSLexConsumer.Destroy;
begin
  inherited;
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
  Inc(Line, MarkLine);
  Offset := MarkOffset;
  ScanLine := 0;
  ScanOffset := MarkOffset;
end;

procedure SSLexConsumer.FlushLexemeAll;
begin
  Start := Index;
  Inc(Line, ScanLine);
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
          Inc(DataLen, NewLen);
          Result := True;
        end;
      end
      else
        Result := True;

      if Result = True then
      begin
        Current := Ord(Buffer[Index+1]);
        Inc(Index);
        if Current = Ord($0A) then
        begin
          Inc(ScanLine);
          ScanOffset := 1;
        end
        else
          Inc(ScanOffset);
      end;
    end;
  end;

end;

constructor SSLexStringConsumer.Create(TheString: String);
begin
  inherited Create(0, SSLexBinaryMode);
  DataLen := Length(TheString);
  BuffLen := DataLen;
  Buffer := Copy(TheString, 1, BuffLen);
end;

function SSLexConsumer.Lexeme: SSLexLexeme;
var
  Length: Longint;
begin
  Length := Mark - Start;
  Result := SSLexLexeme.Create(Buffer, Length, Line, Offset);
  FlushLexeme;
end;

function SSLexConsumer.LexemeAll: SSLexLexeme;
var
  Length: Longint;
begin
  Length := Mark - Start;
  Result := SSLexLexeme.Create(Copy(Buffer, Start, Length), Index - Start, Line, Offset);
  FlushLexemeAll;
end;

constructor SSLexLexeme.Create(TheData: String; TheLength, TheLine, TheOffset: Integer);
begin
  inherited Create;
  Buffer := Copy(TheData, 1+TheOffset, TheLength);
  Length := TheLength;
  Line := TheLine;
  Offset := TheOffset;
  Use := 0;
end;

procedure SSLexLexeme.RefInc;
begin
  if Use < 0 then
    Assert(False, 'Assertion failure');
  Inc(Use);
end;

function SSLexLexeme.RefDec: Boolean;
begin
  Dec(Use);
  Result := Use = 0;
  if Use < 0 then
    Assert(False, 'Assertion failure');
end;

destructor SSLexLexeme.Destroy;
begin
  if Use <> 0 then
    Assert(False, 'Assertion failure');
  inherited;
end;

function SSLexConsumer.NextBuffer: Longint;
begin
  Result := 0;
end;

constructor SSLexTable.Create;
var
  i: Integer;
  ExprList: SSLexExpressionList;
begin
  inherited Create;

  FullTab;

  ExpressionLists := TList.Create;
  for i := 0 to Length(LexTab) - 1 do
  begin
    ExprList := SSLexExpressionList.Create;
    ExprList.ARows := LexTab[i];
    ExpressionLists.Insert(i, ExprList);
  end;
end;

function SSLexTable.GetExpressionList(TheList: Integer): SSLexExpressionList;
var
  AnException: SSException;
begin
  if (TheList < 0) or (ExpressionLists.Count <= TheList) then
  begin
    AnException := SSException.CreateLong(SSExceptionBadList, SSLexMsgBadList, TheList);
    raise AnException;
  end;
  Result := ExpressionLists[TheList];
end;

function SSLexExpressionList.LookupState(TheToken, TheState: Integer): Longint;
var
  i: Integer;
  Row: TRow;
  Entry: TEntry;
begin
  Result := SSLexStateInvalid;
  Row := ARows[TheState];
  for i := 0 to Length(Row.Entries) - 1 do
  begin
    Entry := Row.Entries[i];
    if TheToken = SSLexConsumerBof then
    begin
      if Entry.StartPoint = SSLexConsumerBof then
      begin
        Result := Entry.State;
        Break;
      end;
    end
    else if TheToken < Entry.StartPoint then
    begin
      Break;
    end
    else if TheToken <= Entry.EndPoint then
    begin
      Result := Entry.State;
      Break;
    end;
  end;
end;

destructor SSLexTable.Destroy;
var
  i: Integer;
  List: SSLexExpressionList;
begin
  if ExpressionLists <> nil then
  begin
    for i := 0 to ExpressionLists.Count - 1 do
    begin
      List := ExpressionLists[i];
      List.Free;

      LexTab[i] := nil;
    end;
  end;
  ExpressionLists.Free;

  inherited;
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

procedure SSLex.PopExpressionList;
begin
  Stack.Pop;
  List := Stack.Top;
end;

function SSLex.Next: SSLexLexeme;
var
  Consumed: Boolean;
  RFinal, RTempFinal: TRow;
begin
  while True do
  begin
    State := 0;
    Result := nil;
    Consumed := False;
    RFinal := List.ARows[State];
    while Consumer.Next do
    begin
      Consumed := True;
      State := List.LookupState(Consumer.Current, State);
      if State = SSLexStateInvalid then
      begin
        Break;
      end;
      RTempFinal := List.ARows[State];
      if RTempFinal.Flags and SSLexFinalStateFlagsFinal <> 0 then
      begin
        Consumer.MarkFinal;
        RFinal := RTempFinal;
      end;
      if RTempFinal.Flags and SSLexFinalStateFlagsContextStart <> 0 then
        Consumer.MarkContextFinal;
    end;

    if not Consumed then
      Break;

    if RFinal.Flags and SSLexFinalStateFlagsContextEnd <> 0 then
      Consumer.SetContextFinal;

    if RFinal.Flags and SSLexFinalStateFlagsIgnore <> 0 then
    begin
      Consumer.FlushLexeme;
      ProcessExpressionList(RFinal);
      Continue;
    end;

    if RFinal.Flags and SSLexFinalStateFlagsFinal = 0 then
    begin
      Result := Error;
      if Result <> nil then
        Break;

      Consumer.FlushLexemeAll;
      Continue;
    end;

    ProcessExpressionList(RFinal);
    if RFinal.Flags and SSLexFinalStateFlagsStartOfLine <> 0 then
      if (Consumer.Line <> 1) and (Consumer.Offset <> 1) then
        Consumer.FlushStartOfLine;

    Result := Complete(RFinal.Token);
    if Result <> nil then
      Break;
  end;
end;

procedure SSLex.ProcessExpressionList(ThePFinal: TRow);
begin
  if (ThePFinal.Flags and SSLexFinalStateFlagsPop <> 0)
    and (ThePFinal.Flags and SSLexFinalStateFlagsPush <> 0) then
    GotoExpressionList(ThePFinal.Push)
  else if (ThePFinal.Flags and SSLexFinalStateFlagsPop <> 0) then
    PopExpressionList
  else if (ThePFinal.Flags and SSLexFinalStateFlagsPush <> 0) then
    PushExpressionList(ThePFinal.Push);
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

function SSLex.Error: SSLexLexeme;
var
  Lexeme: SSLexLexeme;
  AnException: SSException;
begin
  Lexeme := Consumer.LexemeAll;
  AnException := SSException.CreateLongLongNameLen(SSExceptionLexError, SSLexMsgError,
    Lexeme.Line, Lexeme.Offset, Lexeme.Buffer, Lexeme.Length);
  raise AnException;
end;

function SSLex.TokenToString(TheToken: Longint): string;
begin
  Result := '';
end;

function SSLex.Complete(TheToken: Longint): SSLexLexeme;
begin
  Result := Consumer.Lexeme;
  Result.Token := TheToken;
end;

destructor SSLex.Destroy;
begin
  Stack.Free;
  inherited;
end;

function SSLexExpressionListStack.Top: SSLexExpressionList;
begin
  Result := inherited Top as SSLexExpressionList;
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
  GetMem(PArray, Size * SizeOf(Pointer));
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

function SSStack.Top: SSLexRoot;
var
  AnException: SSException;
begin
  if TopOfStack = 0 then
  begin
    AnException := SSException.Create(SSExceptionStackTop, SSLifoMsgStackTop);
    raise AnException;
  end;
  Result := PArray[TopOfStack - 1];
end;

procedure SSStack.Push(TheSSYaccStackElement: SSLexRoot);
var
  i: Integer;
  NewSize: Integer;
  PNewArray: PSSArrayOfSSYaccStackElement;
begin
  if TopOfStack >= Size then
  begin
    NewSize := Size + Incr;
    GetMem(PNewArray, NewSize * SizeOf(Pointer));
    for i := 0 to TopOfStack - 1 do
      PNewArray[i] := PArray[i];
    FreeMem(PArray, Size * SizeOf(Pointer));
    PArray := PNewArray;
    Size := NewSize;
  end;

  PArray[TopOfStack] := TheSSYaccStackElement as SSYaccStackElement;
  Inc(TopOfStack);
end;

destructor SSStack.Destroy;
begin
  FreeMem(PArray, Size * SizeOf(Pointer));
  inherited;
end;

procedure SSLexTable.FullTab;
var
  Row: TRow;
  N: Integer;

  procedure R(T, P, F, C: ShortInt);
  begin
    with Row do
    begin
      Token := T;
      Push := P;
      Flags := F;
      SetLength(Entries, C);
      N := 0;
    end;
  end;

  procedure RE(B, E, S: Longint);
  begin
    with Row.Entries[N] do
    begin
      StartPoint := B;
      EndPoint := E;
      State := S;
    end;
    Inc(N);
  end;

begin
  {$REGION 'Full LexTab'}

  // === LexTab[0] ===

  SetLength(LexTab[0], 111);
  //  [0, 0]
  R(0, 0, 0, 45);
  RE(9, 10, 1);
  RE(32, 32, 1);
  RE(35, 35, 12);
  RE(39, 39, 14);
  RE(40, 40, 16);
  RE(41, 41, 18);
  RE(42, 42, 20);
  RE(43, 43, 22);
  RE(44, 44, 24);
  RE(45, 45, 26);
  RE(46, 46, 28);
  RE(47, 47, 30);
  RE(48, 57, 32);
  RE(58, 58, 34);
  RE(60, 60, 36);
  RE(61, 61, 38);
  RE(62, 62, 40);
  RE(64, 64, 42);
  RE(65, 65, 44);
  RE(66, 66, 46);
  RE(67, 67, 48);
  RE(68, 82, 44);
  RE(83, 83, 50);
  RE(84, 90, 44);
  RE(91, 91, 52);
  RE(93, 93, 54);
  RE(97, 97, 56);
  RE(98, 99, 60);
  RE(100, 100, 58);
  RE(101, 101, 62);
  RE(102, 104, 60);
  RE(105, 105, 64);
  RE(106, 108, 60);
  RE(109, 109, 66);
  RE(110, 110, 102);
  RE(111, 111, 68);
  RE(112, 115, 60);
  RE(116, 116, 70);
  RE(117, 119, 60);
  RE(120, 120, 101);
  RE(121, 122, 60);
  RE(123, 123, 72);
  RE(124, 124, 74);
  RE(125, 125, 76);
  RE(-1, -1, 78);
  LexTab[0][0] := Row;
  //  [0, 1]
  R(1, 0, 80, 0);
  LexTab[0][1] := Row;
  //  [0, 2]
  R(0, 0, 0, 1);
  RE(48, 57, 11);
  LexTab[0][2] := Row;
  //  [0, 3]
  R(0, 0, 0, 1);
  RE(48, 57, 5);
  LexTab[0][3] := Row;
  //  [0, 4]
  R(0, 0, 0, 1);
  RE(48, 57, 2);
  LexTab[0][4] := Row;
  //  [0, 5]
  R(0, 0, 0, 2);
  RE(48, 57, 7);
  RE(58, 58, 109);
  LexTab[0][5] := Row;
  //  [0, 6]
  R(0, 0, 0, 1);
  RE(45, 45, 4);
  LexTab[0][6] := Row;
  //  [0, 7]
  R(0, 0, 0, 1);
  RE(48, 57, 9);
  LexTab[0][7] := Row;
  //  [0, 8]
  R(0, 0, 0, 1);
  RE(48, 57, 6);
  LexTab[0][8] := Row;
  //  [0, 9]
  R(0, 0, 0, 1);
  RE(45, 45, 10);
  LexTab[0][9] := Row;
  //  [0, 10]
  R(0, 0, 0, 1);
  RE(48, 57, 8);
  LexTab[0][10] := Row;
  //  [0, 11]
  R(6, 0, 16, 0);
  LexTab[0][11] := Row;
  //  [0, 12]
  R(34, 0, 16, 1);
  RE(48, 57, 3);
  LexTab[0][12] := Row;
  //  [0, 13]
  R(7, 0, 16, 0);
  LexTab[0][13] := Row;
  //  [0, 14]
  R(8, 1, 48, 0);
  LexTab[0][14] := Row;
  //  [0, 15]
  R(7, 0, 16, 1);
  RE(58, 58, 110);
  LexTab[0][15] := Row;
  //  [0, 16]
  R(11, 0, 16, 0);
  LexTab[0][16] := Row;
  //  [0, 17]
  R(26, 0, 16, 0);
  LexTab[0][17] := Row;
  //  [0, 18]
  R(12, 0, 16, 0);
  LexTab[0][18] := Row;
  //  [0, 19]
  R(37, 0, 16, 0);
  LexTab[0][19] := Row;
  //  [0, 20]
  R(23, 0, 16, 0);
  LexTab[0][20] := Row;
  //  [0, 21]
  R(3, 0, 16, 1);
  RE(48, 57, 21);
  LexTab[0][21] := Row;
  //  [0, 22]
  R(21, 0, 16, 0);
  LexTab[0][22] := Row;
  //  [0, 23]
  R(36, 0, 16, 0);
  LexTab[0][23] := Row;
  //  [0, 24]
  R(27, 0, 16, 0);
  LexTab[0][24] := Row;
  //  [0, 25]
  R(20, 0, 16, 0);
  LexTab[0][25] := Row;
  //  [0, 26]
  R(22, 0, 16, 1);
  RE(62, 62, 17);
  LexTab[0][26] := Row;
  //  [0, 27]
  R(19, 0, 16, 0);
  LexTab[0][27] := Row;
  //  [0, 28]
  R(25, 0, 16, 1);
  RE(46, 46, 19);
  LexTab[0][28] := Row;
  //  [0, 29]
  R(18, 0, 16, 0);
  LexTab[0][29] := Row;
  //  [0, 30]
  R(24, 0, 16, 0);
  LexTab[0][30] := Row;
  //  [0, 31]
  R(39, 0, 16, 4);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 122, 44);
  LexTab[0][31] := Row;
  //  [0, 32]
  R(2, 0, 16, 2);
  RE(46, 46, 106);
  RE(48, 57, 32);
  LexTab[0][32] := Row;
  //  [0, 33]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 102, 44);
  RE(103, 103, 31);
  RE(104, 122, 44);
  LexTab[0][33] := Row;
  //  [0, 34]
  R(38, 0, 16, 1);
  RE(58, 58, 23);
  LexTab[0][34] := Row;
  //  [0, 35]
  R(42, 0, 16, 4);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 122, 44);
  LexTab[0][35] := Row;
  //  [0, 36]
  R(16, 0, 16, 2);
  RE(61, 61, 27);
  RE(62, 62, 25);
  LexTab[0][36] := Row;
  //  [0, 37]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 109, 44);
  RE(110, 110, 35);
  RE(111, 122, 44);
  LexTab[0][37] := Row;
  //  [0, 38]
  R(17, 0, 16, 0);
  LexTab[0][38] := Row;
  //  [0, 39]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 110, 44);
  RE(111, 111, 37);
  RE(112, 122, 44);
  LexTab[0][39] := Row;
  //  [0, 40]
  R(15, 0, 16, 1);
  RE(61, 61, 29);
  LexTab[0][40] := Row;
  //  [0, 41]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 104, 44);
  RE(105, 105, 39);
  RE(106, 122, 44);
  LexTab[0][41] := Row;
  //  [0, 42]
  R(28, 0, 16, 0);
  LexTab[0][42] := Row;
  //  [0, 43]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 115, 44);
  RE(116, 116, 41);
  RE(117, 122, 44);
  LexTab[0][43] := Row;
  //  [0, 44]
  R(5, 0, 16, 4);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 122, 44);
  LexTab[0][44] := Row;
  //  [0, 45]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 98, 44);
  RE(99, 99, 43);
  RE(100, 122, 44);
  LexTab[0][45] := Row;
  //  [0, 46]
  R(5, 0, 16, 5);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 97, 33);
  RE(98, 122, 44);
  LexTab[0][46] := Row;
  //  [0, 47]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 100, 44);
  RE(101, 101, 45);
  RE(102, 122, 44);
  LexTab[0][47] := Row;
  //  [0, 48]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 110, 44);
  RE(111, 111, 51);
  RE(112, 122, 44);
  LexTab[0][48] := Row;
  //  [0, 49]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 107, 44);
  RE(108, 108, 47);
  RE(109, 122, 44);
  LexTab[0][49] := Row;
  //  [0, 50]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 100, 44);
  RE(101, 101, 67);
  RE(102, 122, 44);
  LexTab[0][50] := Row;
  //  [0, 51]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 107, 44);
  RE(108, 108, 49);
  RE(109, 122, 44);
  LexTab[0][51] := Row;
  //  [0, 52]
  R(9, 0, 16, 0);
  LexTab[0][52] := Row;
  //  [0, 53]
  R(41, 0, 16, 4);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 122, 44);
  LexTab[0][53] := Row;
  //  [0, 54]
  R(10, 0, 16, 0);
  LexTab[0][54] := Row;
  //  [0, 55]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 100, 44);
  RE(101, 101, 53);
  RE(102, 122, 44);
  LexTab[0][55] := Row;
  //  [0, 56]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 109, 60);
  RE(110, 110, 71);
  RE(111, 122, 60);
  LexTab[0][56] := Row;
  //  [0, 57]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 98, 44);
  RE(99, 99, 55);
  RE(100, 122, 44);
  LexTab[0][57] := Row;
  //  [0, 58]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 104, 60);
  RE(105, 105, 75);
  RE(106, 122, 60);
  LexTab[0][58] := Row;
  //  [0, 59]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 109, 44);
  RE(110, 110, 57);
  RE(111, 122, 44);
  LexTab[0][59] := Row;
  //  [0, 60]
  R(4, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][60] := Row;
  //  [0, 61]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 100, 44);
  RE(101, 101, 59);
  RE(102, 122, 44);
  LexTab[0][61] := Row;
  //  [0, 62]
  R(4, 0, 16, 8);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 107, 60);
  RE(108, 108, 91);
  RE(109, 109, 60);
  RE(110, 110, 89);
  RE(111, 122, 60);
  LexTab[0][62] := Row;
  //  [0, 63]
  R(40, 0, 16, 4);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 122, 44);
  LexTab[0][63] := Row;
  //  [0, 64]
  R(4, 0, 16, 8);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 101, 60);
  RE(102, 102, 96);
  RE(103, 108, 60);
  RE(109, 109, 98);
  RE(110, 122, 60);
  LexTab[0][64] := Row;
  //  [0, 65]
  R(5, 0, 16, 6);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 116, 44);
  RE(117, 117, 61);
  RE(118, 122, 44);
  LexTab[0][65] := Row;
  //  [0, 66]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 110, 60);
  RE(111, 111, 92);
  RE(112, 122, 60);
  LexTab[0][66] := Row;
  //  [0, 67]
  R(5, 0, 16, 8);
  RE(48, 57, 44);
  RE(65, 90, 44);
  RE(95, 95, 44);
  RE(97, 112, 44);
  RE(113, 113, 65);
  RE(114, 115, 44);
  RE(116, 116, 63);
  RE(117, 122, 44);
  LexTab[0][67] := Row;
  //  [0, 68]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 113, 60);
  RE(114, 114, 86);
  RE(115, 122, 60);
  LexTab[0][68] := Row;
  //  [0, 69]
  R(45, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][69] := Row;
  //  [0, 70]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 103, 60);
  RE(104, 104, 82);
  RE(105, 122, 60);
  LexTab[0][70] := Row;
  //  [0, 71]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 99, 60);
  RE(100, 100, 69);
  RE(101, 122, 60);
  LexTab[0][71] := Row;
  //  [0, 72]
  R(13, 0, 16, 0);
  LexTab[0][72] := Row;
  //  [0, 73]
  R(43, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][73] := Row;
  //  [0, 74]
  R(35, 0, 16, 0);
  LexTab[0][74] := Row;
  //  [0, 75]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 117, 60);
  RE(118, 118, 73);
  RE(119, 122, 60);
  LexTab[0][75] := Row;
  //  [0, 76]
  R(14, 0, 16, 0);
  LexTab[0][76] := Row;
  //  [0, 77]
  R(31, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][77] := Row;
  //  [0, 78]
  R(-1, 0, 80, 0);
  LexTab[0][78] := Row;
  //  [0, 79]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 100, 60);
  RE(101, 101, 77);
  RE(102, 122, 60);
  LexTab[0][79] := Row;
  //  [0, 80]
  R(48, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][80] := Row;
  //  [0, 81]
  R(32, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][81] := Row;
  //  [0, 82]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 100, 60);
  RE(101, 101, 105);
  RE(102, 122, 60);
  LexTab[0][82] := Row;
  //  [0, 83]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 101, 60);
  RE(102, 102, 81);
  RE(103, 122, 60);
  LexTab[0][83] := Row;
  //  [0, 84]
  R(30, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][84] := Row;
  //  [0, 85]
  R(33, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][85] := Row;
  //  [0, 86]
  R(46, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][86] := Row;
  //  [0, 87]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 108, 60);
  RE(109, 109, 85);
  RE(110, 122, 60);
  LexTab[0][87] := Row;
  //  [0, 88]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 115, 60);
  RE(116, 116, 90);
  RE(117, 122, 60);
  LexTab[0][88] := Row;
  //  [0, 89]
  R(4, 0, 16, 8);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 99, 60);
  RE(100, 100, 103);
  RE(101, 116, 60);
  RE(117, 117, 87);
  RE(118, 122, 60);
  LexTab[0][89] := Row;
  //  [0, 90]
  R(47, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][90] := Row;
  //  [0, 91]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 114, 60);
  RE(115, 115, 79);
  RE(116, 122, 60);
  LexTab[0][91] := Row;
  //  [0, 92]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 99, 60);
  RE(100, 100, 94);
  RE(101, 122, 60);
  LexTab[0][92] := Row;
  //  [0, 93]
  R(49, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][93] := Row;
  //  [0, 94]
  R(44, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][94] := Row;
  //  [0, 95]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 114, 60);
  RE(115, 115, 93);
  RE(116, 122, 60);
  LexTab[0][95] := Row;
  //  [0, 96]
  R(29, 0, 16, 4);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 122, 60);
  LexTab[0][96] := Row;
  //  [0, 97]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 100, 60);
  RE(101, 101, 95);
  RE(102, 122, 60);
  LexTab[0][97] := Row;
  //  [0, 98]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 111, 60);
  RE(112, 112, 99);
  RE(113, 122, 60);
  LexTab[0][98] := Row;
  //  [0, 99]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 107, 60);
  RE(108, 108, 104);
  RE(109, 122, 60);
  LexTab[0][99] := Row;
  //  [0, 100]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 113, 60);
  RE(114, 114, 80);
  RE(115, 122, 60);
  LexTab[0][100] := Row;
  //  [0, 101]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 110, 60);
  RE(111, 111, 100);
  RE(112, 122, 60);
  LexTab[0][101] := Row;
  //  [0, 102]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 110, 60);
  RE(111, 111, 88);
  RE(112, 122, 60);
  LexTab[0][102] := Row;
  //  [0, 103]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 104, 60);
  RE(105, 105, 83);
  RE(106, 122, 60);
  LexTab[0][103] := Row;
  //  [0, 104]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 104, 60);
  RE(105, 105, 97);
  RE(106, 122, 60);
  LexTab[0][104] := Row;
  //  [0, 105]
  R(4, 0, 16, 6);
  RE(48, 57, 60);
  RE(65, 90, 60);
  RE(95, 95, 60);
  RE(97, 109, 60);
  RE(110, 110, 84);
  RE(111, 122, 60);
  LexTab[0][105] := Row;
  //  [0, 106]
  R(0, 0, 0, 1);
  RE(48, 57, 21);
  LexTab[0][106] := Row;
  //  [0, 107]
  R(0, 0, 0, 1);
  RE(48, 57, 13);
  LexTab[0][107] := Row;
  //  [0, 108]
  R(0, 0, 0, 1);
  RE(48, 57, 15);
  LexTab[0][108] := Row;
  //  [0, 109]
  R(0, 0, 0, 1);
  RE(48, 57, 108);
  LexTab[0][109] := Row;
  //  [0, 110]
  R(0, 0, 0, 1);
  RE(48, 57, 107);
  LexTab[0][110] := Row;

  // === LexTab[1] ===

  SetLength(LexTab[1], 6);
  //  [1, 0]
  R(50, 0, 16, 7);
  RE(0, 9, 2);
  RE(11, 38, 2);
  RE(39, 39, 4);
  RE(40, 91, 2);
  RE(92, 92, 1);
  RE(93, 65535, 2);
  RE(-1, -1, 5);
  LexTab[1][0] := Row;
  //  [1, 1]
  R(50, 0, 16, 4);
  RE(0, 9, 2);
  RE(11, 91, 2);
  RE(92, 92, 3);
  RE(93, 65535, 2);
  LexTab[1][1] := Row;
  //  [1, 2]
  R(50, 0, 16, 5);
  RE(0, 9, 2);
  RE(11, 38, 2);
  RE(40, 91, 2);
  RE(92, 92, 1);
  RE(93, 65535, 2);
  LexTab[1][2] := Row;
  //  [1, 3]
  R(50, 0, 16, 4);
  RE(0, 9, 2);
  RE(11, 91, 2);
  RE(92, 92, 3);
  RE(93, 65535, 2);
  LexTab[1][3] := Row;
  //  [1, 4]
  R(51, 0, 24, 0);
  LexTab[1][4] := Row;
  //  [1, 5]
  R(-1, 0, 80, 0);
  LexTab[1][5] := Row;

  {$ENDREGION}
end;

end.

