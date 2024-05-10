
{ Global compiler directives }
{$include bold.inc}
unit BoldOcl;

interface

uses
  Classes,
  vcl.ExtCtrls,
  BoldBase,
  BoldSystemRT,
  BoldOclClasses,
  BoldElements,
  BoldSystem,
  BoldAttributes,
  BoldIndexableList,
  BoldLogHandler,
  BoldOclRTDebug,
  BoldSubscription;

var
  _BoldOCLPSEvaluationConditionBlockSize: Integer = -1;

type
  { forward declaration }
  TBoldOclEntry = class;
  TBoldOClDictionary = class;
  TBoldOcl = class;

  { TBoldOclEntry }
  TBoldOclEntry = class(TBoldMemoryManagedObject)
  public
    OclString: string;
    SelfVar: TBoldOclVariableBinding;
    Model: TBoldSystemTypeInfo;
    Context: TBoldElementTypeInfo;
    firstSemanticPass: Boolean;
    EvaluatedOnce: Boolean;
    OwnedByDictionary: Boolean;
    Ocl: TBoldOclNode;
    UsedByOtherEvaluation: Boolean;
    Evaluations: Integer;
    AccumulatedTicks: Int64;
    LastWarnedTicks: Int64;
    constructor Create(const Str: string; OclNode: TBoldOclNode);
    destructor Destroy; override;
  end;

  { TBoldOClDictionary }
  TBoldOClDictionary = class(TBoldIndexableList)
  private
    class var IX_OCLEntry: integer;
    function GetOcl(const Expr: string): TBoldOclEntry; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    procedure AddOcl(ENTRY: TBoldOclEntry);
    property OclEntryByExpressionString[const Expr: string]: TBoldOclEntry read GetOcl;
  end;

  { TBoldOcl }
  TBoldOcl = class(TBoldRTEvaluator)
  private
    fMaxMemberNameArray: array of Integer;
    fOclDictionary: TBoldOClDictionary;
    fSymbolTable: TBoldSymbolDictionary;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fGlobalEnv: TBoldOclEnvironment;
    fBoldSystem: TBoldSystem;
    fStringType: TBoldAttributeTypeInfo;
    fIntegerType: TBoldAttributeTypeInfo;
    fFloatType: TBoldAttributeTypeInfo;
    fBooleanType: TBoldAttributeTypeInfo;
    fDateType: TBoldAttributeTypeInfo;
    fTimeType: TBoldAttributeTypeInfo;
    fDateTimeType: TBoldAttributeTypeInfo;
    fTrueBool: TBABoolean;
    fCanEvaluate: Boolean;
    fOnLookUpOclDefinition: TBoldLookUpOclDefinition;
    fTodayVar: TBADate;
    fNowVar: TBADateTime;
    fTodayTimer, fNowTimer: TTimer;
    fParses: Integer;
    fDictionaryHits: Integer;
    fExpresionTypeCount: Integer;
    fExpressionTypeTicks: Int64;
    fEvaluationCount: Integer;
    fEvaluationTicks: Int64;
    procedure CalculateMaxMemberNameLength;
    function SyntacticParse(const Ocl: string; StoreInDictionary: Boolean; Context: TBoldElementTypeInfo): TBoldOclEntry;
    function LookupOclDefinition(const name: string):string;
    procedure PSEvaluation(const Expr: string; Root: TBoldElement; ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment);
    procedure AddVarsToEnv(Env: TBoldOCLEnvironment; const VariableList: TBoldExternalVariableList; Initializevalues: Boolean);
    function GetGlobalEnv: TBoldOclEnvironment;
    procedure TodayTimerEvent(Sender: TObject);
    procedure NowTimerEvent(Sender: TObject);
  protected
    function GetVariableCount: integer; override;
    function GetVariable(index: integer): TBoldIndirectElement; override;
    function GetVariableByName(const aName: string): TBoldIndirectElement; override;
  public
    constructor Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem);
    destructor Destroy; override;
    property GlobalEnv: TBoldOclEnvironment read GetGlobalEnv;
    property SymbolTable: TBoldSymbolDictionary read fsymbolTable;
    property BoldSystem: TBoldSystem read fBoldSystem;
    property Parses: Integer read fParses;
    property ExpresionTypeCount: Integer read fExpresionTypeCount;
    property ExpressionTypeTicks: Int64 read fExpressionTypeTicks;
    property EvaluationCount: Integer read fEvaluationCount;
    property EvaluationTicks: Int64 read fEvaluationTicks;
    property DictionaryHits: Integer read fDictionaryHits;
    function SemanticCheck(Ocl: string; Context: TBoldElementTypeInfo; const VariableList: TBoldExternalVariableList = nil; StoreInDictionary: Boolean = true; Env: TBoldOclEnvironment = nil): TBoldOclEntry;
    procedure DoneWithEntry(var oclEntry: TBoldOclEntry);
    procedure DefineVariable(const VariableName: String; VarValue: TBoldElement;
        VariableType: TBoldElementTypeInfo; OwnValue, IsConstant: Boolean); override;
    procedure DefineVariable(const VariableName: string; Variable: TBoldExternalVariable ); override;
    procedure UndefineVariable(Variable: TBoldExternalVariable); override;
    procedure Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber = nil; ResubscribeAll: Boolean = false; resultElement: TBoldIndirectElement = nil; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil); override;
    function ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo; override;
    function RTInfo(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldMemberRTInfo; override;
    procedure SetLookupOclDefinition(value: TBoldLookUpOclDefinition); override;
    property StringType: TBoldAttributeTypeInfo read fStringType;
    property IntegerType: TBoldAttributeTypeInfo read fIntegerType;
    property FloatType: TBoldAttributeTypeInfo read fFloatType;
    property BooleanType: TBoldAttributeTypeInfo read fBooleanType;
    property DateType: TBoldAttributeTypeInfo read fDateType;
    property TimeType: TBoldAttributeTypeInfo read fTimeType;
    property DateTimeType: TBoldAttributeTypeInfo read fDateTimeType;
  end;

var
  BoldOCLAllowCapitalMembers: Boolean = false;
  BoldNelCompatibility: Boolean = false;
  BoldOCLLogHandler: TBoldLogHandler = nil;
  BoldOCLRTDebugger: TBoldOCLRTDebugger = nil;
  BoldOclParserTablePath: String = '';

procedure BoldOCLLog(aRoot: TBoldElement; const s: string; aResult: TBoldIndirectElement);
procedure BoldForceNelCompatibility;

{$IFDEF OCLDummyValueBug}
type
  TDummyValueDifferentResultEvent = procedure(const Expression: string; Context: TBoldElement; ResultWithDummy, ResultWithoutDummy: TBoldIndirectElement) of object;
var
  OnDummyValueDifferentResult: TDummyValueDifferentResultEvent;
{$ENDIF}

implementation

uses
  // VCL
  SysUtils,
  DateUtils,
  Math,

  // Bold
  BoldUtils,
  BoldCondition,
  BoldDefs,
  BoldHashIndexes,
  BoldOclError,
  BoldOclEvaluator,
  BoldOclLightWeightNodeMaker,
  BoldOclLightWeightNodes,
  BoldOclSemantics,
  BoldOclSymbolImplementations,
  BoldORed,
  BoldMath,
  BoldSSExcept,
  BoldSSLexU,
  BoldSSYaccU,
  BoldGuard;

var
  G_OclScannerTable: SSLexTable = nil;
  G_OclParserTable: SSYaccTable = nil;

function OclScannerTable: SSLexTable;
begin
  if not assigned(G_OclScannerTable) then
    G_OclScannerTable := SSLexTable.Create;
  result := G_OclScannerTable;
end;

function OclParserTable: SSYaccTable;
begin
  if not assigned(G_OclParserTable) then
    G_OclParserTable := SSYaccTable.Create;
  result := G_OclParserTable;
end;

type
  { TOCLIndex }
  TOCLIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  { TOCLIndex }
function TOCLIndex.ItemAsKeyString(Item: TObject): string;
begin
  with TBoldOclEntry(Item) do
    if Assigned(Context) then
      Result := Context.ExpressionName + ':' + OclString
    else
      Result := OclString
end;

procedure TBoldOcl.CalculateMaxMemberNameLength;
var
  i,j: integer;
  ClassInfo: TBoldClassTypeInfo;
  vLength: integer;
begin
  SetLength(fMaxMemberNameArray, fSystemTypeInfo.TopSortedClasses.Count);
  for i := 0 to fSystemTypeInfo.TopSortedClasses.Count - 1 do
  begin
    ClassInfo := fSystemTypeInfo.TopSortedClasses[i];
    vLength := 0;
    for j := 0 to ClassInfo.AllMembersCount - 1 do
      vLength := Max(vLength, Length(ClassInfo.AllMembers[j].ExpressionName));
    fMaxMemberNameArray[i] := vLength;
  end;
end;

constructor TBoldOcl.Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem);
var
  TrueConst: TBABoolean;
  FalseConst: TBABoolean;
  MaxTimeStamp: TBAInteger;
begin
  fSystemTypeInfo := SystemTypeInfo;
  fBoldSystem := BoldSystem;
  fOclDictionary := TBoldOclDictionary.Create;
  fCanEvaluate := true;
  fSymbolTable := TBoldSymbolDictionary.Create(SystemTypeInfo, BoldSystem, fCanEvaluate);
  InitializeSymbolTable(fSymbolTable);
  fStringType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['String'];
  fIntegerType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['Integer'];
  fFloatType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['Float'];
  fBooleanType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['Boolean'];
  fTimeType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['Time'];
  fDateType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['Date'];
  fDateTimeType := fSystemTypeInfo.AttributeTypeInfoByExpressionName['DateTime']; // do not localize

  fTrueBool := TBoldMemberFactory.CreateMemberFromBoldType(fBooleanType) as TBABoolean;
  fTrueBool.AsBoolean := true;
  DefineVariable('true', fTrueBool, fBooleanType, True, True); // do not localize

  FalseConst := TBoldMemberFactory.CreateMemberFromBoldType(fBooleanType) as TBABoolean;;
  FalseConst.AsBoolean := False;
  DefineVariable('false', FalseConst, fBooleanType, True, True); // do not localize

  DefineVariable('nil', nil, SystemTypeInfo.NilTypeInfo, False, True); // do not localize

  MaxTimeStamp := TBoldMemberFactory.CreateMemberFromBoldType(fIntegerType) as TBAInteger;
  MaxTimeStamp.AsInteger := BOLDMAXTIMESTAMP;
  DefineVariable('timeStampNow', MaxTimeStamp, fIntegerType, True, False); // do not localize

  fTodayVar := TBoldMemberFactory.CreateMemberFromBoldType(fDateType) as TBADate;
  fTodayTimer := TTimer.Create(nil);
  fTodayTimer.OnTimer := TodayTimerEvent;
  fTodayTimer.Enabled := True;
  TodayTimerEvent(nil);
  DefineVariable('today', fTodayVar, fDateType, True, False); // do not localize

  // Now is rounded to full minutes and is only refreshed once per minute
  fNowVar := TBoldMemberFactory.CreateMemberFromBoldType(fDateTimeType) as TBADateTime;
  fNowTimer := TTimer.Create(nil);
  fNowTimer.OnTimer := NowTimerEvent;
  fNowTimer.Enabled := True;
  NowTimerEvent(nil);
  DefineVariable('now', fNowVar, fDateTimeType, True, False); // do not localize
  CalculateMaxMemberNameLength;
end;

destructor TBoldOcl.Destroy;
begin
  FreeAndNil(fOclDictionary);
  FreeAndNil(fSymbolTable);
  FreeAndNil(fGlobalEnv);
  FreeAndNil(fTodayTimer);
  FreeAndNil(fNowTimer);
  inherited;
end;

{ TBoldOCL }

procedure TBoldOcl.DefineVariable(const VariableName: String; VarValue:
    TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue, IsConstant:
    Boolean);
var
  NewVar: TBoldOCLVariableBinding;
begin
  NewVar := GlobalEnv.Find(VariableName);
  if NewVar is TBoldOclVariableBindingExternal then
  begin
    GlobalEnv.RemoveBinding(NewVar);
    FreeAndNil(NewVar);
  end;
  if not assigned(NewVar) then
  begin
    NewVar := TBoldOclVariableBinding.Create;
    NewVar.VariableName := LowerCase(Copy(VariableName,1,1)) + Copy(VariableName,2,MaxInt);
    GlobalEnv.pushBinding(NewVar);
  end;

  if assigned(VariableType) then
    NewVar.BoldType := VariableType
  else if assigned(VarValue) then
    NewVar.BoldType := VarValue.BoldType;

  if OwnValue then
    NewVar.SetOwnedValue(VarValue)
  else
    NewVar.SetReferenceValue(VarValue);

  // Variables like true, false, nil are always the same
  if IsConstant then begin
    NewVar.IsConstant := True;
    if Assigned(NewVar.Value) then begin
      NewVar.Value.MakeImmutable;
    end;
  end;
end;

procedure TBoldOcl.DefineVariable(const VariableName: string;
  Variable: TBoldExternalVariable);
var
  LookupVar: TBoldOCLVariableBinding;
  NewVar: TBoldOclVariableBindingExternal;
  vName: string;
begin
  if self = fSystemTypeInfo.Evaluator then
    raise EBold.CreateFmt('%s: Variable can not be registered with meta evaluator.', [VariableName]);
  if Assigned(Variable.Evaluator) and (Variable.Evaluator <> self) then
    raise EBold.CreateFmt('%s: Variable already registered with another evaluator.', [VariableName]);
  vName := LowerCase(Copy(VariableName,1,1)) + Copy(VariableName,2,MaxInt);
  LookupVar := GlobalEnv.Find(vName);
  if not assigned(LookupVar) then
  begin
    NewVar := TBoldOclVariableBindingExternal.Create;
    NewVar.VariableName := vName;
    GlobalEnv.pushBinding(NewVar);
  end
  else
  if not (LookupVar is TBoldOclVariableBindingExternal) then
  begin
    NewVar := TBoldOclVariableBindingExternal.Create;
    NewVar.VariableName := vName;
    GlobalEnv.ReplaceBinding(vName, NewVar);
  end
  else
  begin
    NewVar := LookupVar as TBoldOclVariableBindingExternal;
  end;
  NewVar.ExternalVariable := Variable;
  Variable.Evaluator := self;
end;

procedure TBoldOcl.UndefineVariable(Variable: TBoldExternalVariable);
begin
  GlobalEnv.RemoveVariable(Variable);
end;

function TBoldOcl.SyntacticParse(const Ocl: string; StoreInDictionary: Boolean; Context: TBoldElementTypeInfo): TBoldOclEntry;
var
  Lexer: SSLex;
  Parser: AYaccClass;
  Consumer: SSLexConsumer;
  ParenPos, QuotePos, Paren, i: Integer;
  InQuote: Boolean;
  FixedExpr: string;
begin
  FixedExpr := ocl;
  for i := 1 to length(FixedExpr) do
    if CharInSet(FixedExpr[i], [#9, BOLDLF, #12, BOLDCR]) then
      FixedExpr[i] := ' ';
  if (pos('«', FixedExpr) <> 0) or (pos('»', FixedExpr) <> 0) then
  begin
    InQuote := false;
    for i := 1 to Length(FixedExpr) do
    begin
      case Ocl[i] of
        '''': begin
          // QuotePos := i;
          InQuote := not InQuote;
        end;
        '«', '»': if not InQuote then
          raise EBoldOclAbort.CreateFmt('%d: Expression not complete', [i]);
      end;
    end;
  end;

  if Assigned(Context) then
    Result := fOclDictionary.OclEntryByExpressionString[Context.ExpressionName + ':' + Ocl]
  else
    Result := fOclDictionary.OclEntryByExpressionString[Ocl];

  if assigned(Result) then
  begin
    if Result.Ocl.IsConstant or (not Result.UsedByOtherEvaluation) then
    begin
      Result.UsedByOtherEvaluation := true;
      inc(fDictionaryHits);
      exit;
    end;
    StoreInDictionary := false;
  end;
  inc(fParses);
  Result := nil;

  {$IFDEF BOLD_UNICODE}
  Consumer := SSLexStringConsumer.Create(PAnsiChar(AnsiString(FixedExpr)));
  {$ELSE}
  Consumer := SSLexStringConsumer.Create(PChar(FixedExpr));
  {$ENDIF}

  Lexer := ALexClass.Create(Consumer, OclScannerTable);
  Parser := AYaccClass.CreateLex(Lexer, OclParserTable);

  try
    try
      Parser.Parse;
    except
      on e: ssException do
      begin
          for i := 0 to Parser.Stack.TopOfStack-1 do
            if TObject(Parser.Stack.PArray[i]) is AYaccStackElement then
            begin
              AYaccStackElement(Parser.Stack.PArray[i]).FreeAllOwnedElements;
            end;
          InQuote := False;
          QuotePos := 0;
          ParenPos := 0;

          if Pos('SSLex0105e: Invalid token,', e.message) <> 0 then
            for i := 1 to Length(Ocl) do
            begin
              if Ocl[i] = '''' then
                InQuote := not InQuote;
              if not InQuote and not CharInSet(Ocl[i], [' ', #9, BOLDCR, '0'..'9', 'a'..'z', 'A'..'Z', '_', '[', ']', '{', '}',
                '(', ')', '+', '-', '*', '/', '=', '>', '<', ',', '.', '@', '|', '''', ':', '#', '«', '»']) then
                raise EBoldOclAbort.CreateFmt(boeInvalidcharacter,[i - 1]);
            end;
          if Pos('SSYacc0105e: SyncErr failed, no valid token', e.message) <> 0 then
          begin
            Paren := 0;
            for i := 1 to Length(Ocl) do
            begin
              case Ocl[i] of
                '(': if not InQuote then
                begin
                  Inc(Paren);
                  ParenPos := i;
                end;
                ')': if not InQuote then
                  Dec(Paren);
                '''': begin
                  QuotePos := i;
                  InQuote := not InQuote;
                end;
              end;
            end;
            if Paren <> 0 then
              raise EBoldOclAbort.CreateFmt(boeUnMatchedParentesis, [ParenPos - 1]);
            if InQuote then
              raise EBoldOclAbort.CreateFmt(boeunterminatedQoute, [QuotePos - 1]);
          end;
          raise EBoldOclAbort.CreateFmt('%d:' + e.message, [e.Position]);
        end;
      on e: EBoldOclAbort do
      begin
          e.Ocl := Ocl;
          e.FixError;
          raise;
        end;
      on e: EBoldOclError do
      begin
          e.Ocl := Ocl;
          e.FixError;
          raise;
        end;
      on e: Exception do
      begin
        raise;
      end;
    end;
  finally
    Consumer.Free;
    Lexer.Free;
    if assigned(Parser.finalvalue) then
    begin
      Result := TBoldOclEntry.Create(Ocl, Parser.finalvalue.Node);
      Result.UsedByOtherEvaluation := true;
      Result.Context := Context;
    end;
    Parser.Free;
  end;
  if StoreInDictionary then
  begin
    fOclDictionary.AddOcl(Result);
    Result.OwnedByDictionary := True;
  end;
end;

function TBoldOcl.SemanticCheck(Ocl: string; Context: TBoldElementTypeInfo; const VariableList: TBoldExternalVariableList = nil; StoreInDictionary: Boolean = true; Env: TBoldOclEnvironment = nil): TBoldOclEntry;
var
  Visitor: TBoldOclSemanticsVisitor;
  EnvSize: Integer;
  HasVariables: boolean;
  Guard: IBoldGuard;
begin
  Result := nil;
  HasVariables := Assigned(VariableList) and VariableList.RefersToVariable(Ocl);
  if not Assigned(Env) then
  begin
    if HasVariables then
    begin
      Guard := TBoldGuard.Create(Env);
      Env := TBoldOclEnvironment.Create(GlobalEnv)
    end
    else
      Env := GlobalEnv;
  end;
  AddVarsToEnv(Env, VariableList, true);
  StoreInDictionary := StoreInDictionary and not HasVariables; // do not store in dict if there are variables (until subscriptions are implemented)

  try
    if ocl[1] = '%' then
      ocl := LookupOclDefinition(copy(ocl, 2, maxint));

    Result := SyntacticParse(Ocl, StoreInDictionary, Context);




    if (not Result.firstSemanticPass and
        assigned(Context) and
        context.ConformsTo(Result.Context)) or
       (assigned(Result.Ocl) and Result.ocl.IsConstant) then
    begin
      exit;
    end;

    Result.Context := Context;
    Result.Model := fSystemTYpeInfo;

    EnvSize := Env.Count;

    Result.SelfVar.Free;
    Result.SelfVar := TBoldOclVariableBinding.Create;
    Result.SelfVar.VariableName := 'Self';

    Result.SelfVar.BoldType := Result.Context;

    Env.pushBinding(Result.SelfVar);


    Visitor := TBoldOclSemanticsVisitor.Create(Result.Model, self, SymbolTable, Env);
    if BoldNelCompatibility and (pos('(', ocl) <> 0) then
      Visitor.IgnoreNelCompatibility := true;
    try
      try
        Visitor.Traverse(Result.Ocl);
      except
        on e: EBoldOclAbort do
        begin
          e.Ocl := Result.OclString;
          Result.Context := nil;
          raise;
        end;
        on e: EBoldOclError do
        begin
          e.Ocl := Result.OclString;
          Result.Context := nil;
          raise;
        end;
        on e: Exception do
        begin
          Result.Context := nil;
          raise EBoldOclAbort.CreateFmt('%d: %s', [-1, e.message]);
        end;
      end;
    finally
      Visitor.Free;
      Env.PopBinding;
      if EnvSize <> Env.Count then
        raise EBoldOclInternalError.CreateFmt(boeEnvSizeError, [0, EnvSize, Env.Count]);
    end;

    Result.firstSemanticPass := False;

  except
    on e: EBoldOclAbort do
    begin
      e.Ocl := Ocl;
      e.FixError;
      if Assigned(Result) and not result.OwnedByDictionary then
        FreeAndNil(result);
      raise;
    end;
    on e: EBoldOclError do
    begin
      e.Ocl := Ocl;
      e.FixError;
      if Assigned(Result) and not result.OwnedByDictionary then
        FreeAndNil(result);
      raise;
    end
    else
    begin
      if Assigned(Result) and not result.OwnedByDictionary then
        FreeAndNil(result);
      raise;
    end;
  end;
end;

function TBoldOcl.RTInfo(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList): TBoldMemberRTInfo;
var
  ResultEntry: TBoldOclEntry;
begin
  Result := nil;
  ResultEntry := nil;
  try
    try
      if ocl <> '' then
        ResultEntry := SemanticCheck(Ocl, Context, VariableList, false);
      if assigned(ResultEntry) and
        (ResultEntry.Ocl is TBoldOclMember) then
          Result := (ResultEntry.Ocl as TBoldOclMember).RTInfo;
    except
      on e: EBoldOclAbort do
      begin
          Result := nil;
          if ReRaise then
            raise;
        end;
      on e: EBoldOclError do
      begin
          Result := nil;
          if ReRaise then
            raise;
        end;
    end;
  finally
    DoneWithEntry(ResultEntry);
  end;
end;

function TBoldOcl.ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo;
var
  ResultEntry: TBoldOclEntry;
  StartTicks, EndTicks: Int64;
begin
  StartTicks := UserTimeInTicks;
  Result := nil;
  ResultEntry := nil;
  if trim(ocl) = '' then
  begin
    result := context;
  end
  else
  begin
    try
      try
        ResultEntry := SemanticCheck(Ocl, Context, VariableList);
        if assigned(ResultEntry) then
          Result := ResultEntry.Ocl.BoldType;
      except
        on e: EBoldOclAbort do
        begin
            Result := nil;
            if ReRaise then
              raise;
          end;
        on e: EBoldOclError do
        begin
          Result := nil;
          if ReRaise then
            raise;
        end;
      end;
    finally
      DoneWithEntry(ResultEntry);
      EndTicks := userTimeInTicks;
      INC(fExpresionTypeCount);
      fExpressionTypeTicks := fExpressionTypeTicks  + EndTicks - StartTicks;
    end;
  end;
end;

function TBoldOcl.GetGlobalEnv: TBoldOclEnvironment;
begin
  if not assigned(fGlobalEnv) then
    fGlobalEnv := TBoldOclEnvironment.Create(nil);
  result := fGlobalEnv;
end;

function TBoldOcl.GetVariable(index: integer): TBoldIndirectElement;
begin
  result := GlobalEnv.Bindings[Index];
end;

function TBoldOcl.GetVariableByName(const aName: string): TBoldIndirectElement;
begin
  result := GlobalEnv.Lookup(aName);
end;

function TBoldOcl.GetVariableCount: integer;
begin
  result := GlobalEnv.count;
end;

function MapResubscribe(Resubscribe: Boolean): TBoldRequestedEvent;
begin
  if Resubscribe then
    Result := breResubscribe
  else
    Result := breReEvaluate;
end;

var
  OclCounter: integer;


procedure TBoldOcl.Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber = nil; ResubscribeAll: Boolean = false; resultElement: TBoldIndirectElement = nil; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);

procedure _Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber = nil; ResubscribeAll: Boolean = false; resultElement: TBoldIndirectElement = nil; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
var
  LocalContext   : TBoldElementTypeInfo;
  EvaluatorVisitor: TBoldOclEvaluatorVisitor;
  ResultEntry: TBoldOclEntry;
  Env: TBoldOclEnvironment;
  CurrentComponentPath: String;
  StartTicks, EndTicks, Ticks, Freq: Int64;
begin
  StartTicks := UserTimeInTicks;
  if not fCanEvaluate then
    raise EBoldOclError.Create('This evaluator can not be used for evaluation, since some types are missing');
  ResultEntry := nil;
  Env := nil;

   if BoldNelCompatibility and (length(ocl) > 0) and (OCL[Length(ocl)] = '.') then
     delete(ocl, length(ocl), 1);


  if assigned(BoldOCLRTDebugger) and assigned(Subscriber) then
    CurrentComponentPath := Subscriber.ContextString
  else
    CurrentComponentPath := '';


  try
    if assigned(ROOT) then
      LocalContext := ROOT.BoldType
    else
      LocalContext := nil;

    if ROOT is TBoldObjectReference then
    begin
      if assigned(Subscriber) then
        ROOT.DefaultSubscribe(Subscriber, MapResubscribe(ResubscribeAll or (Ocl <> '')));
      ROOT := (ROOT as TBoldObjectReference).BoldObject;
    end;

    if Ocl = '' then
    begin
      if assigned(ResultElement) then
        ResultElement.SetReferenceValue(ROOT);
      if assigned(Subscriber) and assigned(ROOT) then
        ROOT.DefaultSubscribe(Subscriber, MapResubscribe(ResubscribeAll));
      exit;
    end;

    if assigned(ResultElement) and (Root is TBoldObject)
       and (Length(Ocl) <= fMaxMemberNameArray[TBoldObject(Root).BoldClassTypeInfo.TopSortedIndex]) then
    begin
      ResultElement.SetReferenceValue(TBoldObject(Root).FindBoldMemberByExpressionName(ocl));
      if Assigned(ResultElement.Value) then
      begin
        if assigned(Subscriber) then
          ResultElement.Value.DefaultSubscribe(Subscriber, MapResubscribe(ResubscribeAll));
        exit;
      end;
    end;

    if assigned(BoldOclRTDebugger) and
      BoldOclRTDebugger.HasFixFor(ocl, LocalContext) then
      ocl := BoldOclRTDebugger.GetFixFor(Ocl, LocalContext);

    try
      try
        Env := TBoldOclEnvironment.Create(GlobalEnv);
        resultEntry := SemanticCheck(Ocl, LocalContext, VariableList, true, Env);

      except
        on e: EBoldOclAbort do
        begin
          if not assigned(ROOT) and assigned(ResultEntry) then
          begin
            if assigned(ResultElement) then
              resultElement.SetReferenceValue(nil);
            exit;
          end
          else
            raise;
        end;
        on e: EBoldOclError do
        begin
          if not assigned(ROOT) and assigned(ResultEntry) then
          begin
            if assigned(ResultElement) then
              resultElement.SetReferenceValue(nil);
            exit;
          end
          else
            raise;
        end;
      end;

      if ResultEntry.Ocl.IsConstant and ResultEntry.EvaluatedOnce then
      begin
        if assigned(ResultElement) then
          resultElement.SetReferenceValue(ResultEntry.Ocl.Value);
        exit;
      end;
      ResultEntry.SelfVar.SetReferenceValue(ROOT);
      if EvaluateInPS then
      begin
        PSEvaluation(Ocl, Root, ResultEntry, Env);
      end
      else
      begin
        EvaluatorVisitor := TBoldOclEvaluatorVisitor.Create(Subscriber, ResubscribeAll, fSystemtypeInfo, BoldSystem, fTrueBool,fBooleanType,fStringType, fIntegerType, fFloatType, fDateType, fTimeType);
        try
          ResultEntry.Ocl.AcceptVisitor(EvaluatorVisitor);
        finally
          EvaluatorVisitor.Free;
        end;
      end;
      ResultEntry.EvaluatedOnce := True;
      if assigned(ResultElement) then begin
        if ResultEntry.Ocl.IsConstant and
           // see finally below: when ResultEntry is freed, the ResultElement
           // becomes invalid. In this case Value must be transferred in this place.
           ResultEntry.OwnedByDictionary then
        begin
          resultElement.SetReferenceValue(ResultEntry.Ocl.Value);
        end else begin
          ResultEntry.Ocl.TransferValue(ResultElement);
        end;

        if BoldNelCompatibility and (ResultElement.Value is TBoldClassTypeInfo) and
          assigned(fBoldSystem) then
        begin
          resultElement.SetReferenceValue(fBoldSystem.ClassByExpressionName[(ResultElement.Value as TBoldClassTypeInfo).ExpressionName]);
          // since the value has been transfered out of there, we need to redo the semantic check to get a new value
          ResultEntry.firstSemanticPass := true;
        end;

       if resultElement.OwnsValue and assigned(resultelement.value) then
       begin
         if not EvaluateInPS then
          resultelement.Value.MakeImmutable;
         if resultElement.value is TBoldObjectList then
         begin
           if not Assigned(Subscriber) then
             TBoldObjectList(resultElement.value).SubscribeToObjectsInList := true
           else
             TBoldObjectList(resultElement.value).SubscribeToLocatorsInList := true;
         end;
       end;
      end;
    except
      on e: EBoldOclAbort do
      begin
        if assigned(BoldOclRTDebugger) and
           BoldOclRTDebugger.AddFixFor(Ocl, LocalContext, CurrentComponentPath, e.Message) then
          Evaluate(ocl, root, subscriber, ResubscribeAll, resultElement)
        else
        begin
          e.Ocl := Ocl;
          e.FixError;
          raise;
        end;
      end;
      on e: EBoldOclError do
      begin
        if assigned(BoldOclRTDebugger) and
           BoldOclRTDebugger.AddFixFor(Ocl, LocalContext, CurrentComponentPath, E.MEssage) then
          Evaluate(ocl, root, subscriber, ResubscribeAll, resultElement)
        else
        begin
          e.Ocl := Ocl;
          e.FixError;
          raise;
        end;
      end;
      on e: Exception do
      begin
        if assigned(BoldOclRTDebugger) and
           BoldOclRTDebugger.AddFixFor(Ocl, LocalContext, CurrentComponentPath, e.Message) then
          Evaluate(ocl, root, subscriber, ResubscribeAll, resultElement)
        else
          raise;
      end;

    end;
  finally
    BoldOCLLog(ROOT, Ocl, resultElement);
    Inc(OclCounter);
    Env.Free;
    if Assigned(ResultEntry) then
    begin
      INC(ResultEntry.Evaluations);
      INC(fEvaluationCount);

      EndTicks := UserTimeInTicks;
      Ticks := EndTicks - StartTicks;
      if Ticks > 0 then
      begin
        fEvaluationTicks := fEvaluationTicks + Ticks;
        ResultEntry.AccumulatedTicks:=  ResultEntry.AccumulatedTicks + Ticks;
      end;
    end;
    DoneWithEntry(ResultEntry);
  end;
end;
{$IFDEF OCLDummyValueBug}
var
  IE: TBoldIndirectElement;
{$ENDIF}  
begin
  OclUseTemporaryDummyValue := true;
  _Evaluate(Ocl, ROOT, Subscriber, ResubscribeAll, resultElement, EvaluateInPS, VariableList);
{$IFDEF OCLDummyValueBug}
  if Assigned(OnDummyValueDifferentResult) and Assigned(resultElement) then
  try
    IE := TBoldIndirectElement.Create;
    OclUseTemporaryDummyValue := false;
    _Evaluate(Ocl, ROOT, Subscriber, ResubscribeAll, IE, EvaluateInPS, VariableList);
    if not (((Assigned(resultElement.Value) and Assigned(Ie.Value)) or (not Assigned(resultElement.Value) and not Assigned(Ie.Value)))) then
      OnDummyValueDifferentResult(Ocl, Root, resultElement, Ie)
    else
    if not resultElement.Value.IsEqual(IE.Value) then
      OnDummyValueDifferentResult(Ocl, Root, resultElement, Ie);
  finally
    ie.free;
  end;
{$ENDIF}  
end;

{ TBoldOClDictionary }
constructor TBoldOClDictionary.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_OCLEntry, AddIndex(TOCLIndex.Create));
end;

function TBoldOClDictionary.GetOcl(const Expr: string): TBoldOclEntry;
begin
  Result := TBoldOclEntry(TBoldCaseSensitiveStringHashIndex(indexes[IX_OCLEntry]).FindByString(Expr));
end;

procedure TBoldOClDictionary.AddOcl(ENTRY: TBoldOclEntry);
begin
  Add(Entry);
end;

{ TBoldOclEntry }
constructor TBoldOclEntry.Create(const Str: string; OclNode: TBoldOclNode);
begin
  inherited Create;
  firstSemanticPass := True;
  OclString := Str;
  Ocl := OclNode;
end;

destructor TBoldOclEntry.Destroy;
begin
  FreeAndNil(SelfVar);
  FreeAndNil(Ocl);
  inherited;
end;

procedure BoldOCLLog(aRoot: TBoldElement; const s: string; aResult: TBoldIndirectElement);
var
  vRoot: string;
  vResult: string;
begin
  if assigned(BoldOCLLogHandler) then
  begin
    if Assigned(aRoot) then
      vRoot := aRoot.BoldType.AsString
    else
      vRoot := 'nil';
    if Assigned(aResult) and Assigned(aResult.Value) then
      vResult := aResult.value.AsString
    else
      vResult := 'nil';
    BoldOclLogHandler.Log(formatDateTime('c: ', now)+
      format('OCL %4d - %s:%s:%s', [OclCounter, vRoot, trim(s), vResult]));
  end;
end;

function TBoldOcl.LookupOclDefinition(const name: string): string;
begin
  if assigned(fOnLookUpOclDefinition) then
    result := fOnLookUpOclDefinition(Name)
  else
    raise EBoldOCLError.CreateFmt('0: Can not find OCL definition for %s, no repository installed', ['%'+Name]);
end;

procedure TBoldOcl.SetLookupOclDefinition(value: TBoldLookUpOclDefinition);
begin
  fOnLookUpOclDefinition := value;
end;

procedure TBoldOcl.PSEvaluation(const Expr: string; Root: TBoldElement; ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment);
var
  LocalBoldSystem: TBoldSystem;
  ClassTypeInfo: TBoldClassTypeInfo;
  OLWNodeMaker: TBoldOLWNodeMaker;
  resList: TBoldObjectList;
  aResultType: TBoldElementTypeInfo;
  bNoBlockPSEvaluation: Boolean;

  function ExecuteOclCondition(CurrentOLCondition: TBoldOclCondition): Boolean;
  var
    aResultConList: TBoldObjectList;
  begin
    Result := False;
    if Assigned(CurrentOLCondition) then begin
      CurrentOLCondition.RootNode := OLWNodeMaker.RootNode;
      ClassTypeInfo := (aResultType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;
      CurrentOLCondition.TopSortedIndex := ClassTypeInfo.TopSortedIndex;
      if bNoBlockPSEvaluation then begin
        CurrentOLCondition.RootNode := OLWNodeMaker.RootNode;
        ResList := TBoldMemberFactory.CreateMemberFromBoldType(aResultType) as TBoldObjectList;
        LocalBoldSystem.GetAllWithCondition(resList, CurrentOLCondition);
        resultEntry.Ocl.SetOwnedValue(ResList);
      end else begin
        aResultConList := TBoldMemberFactory.CreateMemberFromBoldType(aResultType) as TBoldObjectList;
        try
          LocalBoldSystem.GetAllWithCondition(aResultConList, CurrentOLCondition);
          ResList.AddList(aResultConList);
        finally
          aResultConList.Free;
        end;
      end;
    end;
  end;

var
  OclCondition: TBoldOclCondition;
  i: integer;
  RootAsList: TBoldObjectList;
  ObjectCount: Integer;
  Block: Integer;
  Start, Stop: Integer;
const
  sInvalidForSQLEvaluation = 'Root %s: %s is not allowed for SQL-evaluation';
begin
  LocalBoldSystem := BoldSystem;
  if not assigned(LocalBoldSystem) then
  begin
    if (Root is TBoldObjectList) then
    begin
      ResList := Root as TBoldObjectList;
      if ResList.Count > 0 then
        LocalBoldSystem := ResList[0].BoldSystem
    end;
  end;

  OLWNodeMaker := TBoldOLWNodeMaker.Create(ResultEntry.Ocl, fSystemTypeInfo, LocalBoldSystem, Env);
  OclCondition := nil;
  try
    ResultEntry.Ocl.AcceptVisitor(OLWNodeMaker);
    if not OLWNodeMaker.Failed then
    begin
      OclCondition := TBoldOclCondition.Create;
      OclCondition.OclExpr := Expr;

      for i := 0 to OLWNodeMaker.ExternalVarBindings.Count - 1 do
        OclCondition.Env.Add(TBoldOLWVariableBinding(OLWNodeMaker.ExternalVarBindings[i]));
      OLWNodeMaker.ExternalVarBindings.Clear;

      aResultType := ResultEntry.Ocl.BoldType;
      if aResultType is TBoldClassTypeInfo then begin
        aResultType := TBoldClassTypeInfo(aResultType).ListTypeInfo;
      end;

      bNoBlockPSEvaluation :=
        (_BoldOCLPSEvaluationConditionBlockSize = -1)  OR
          ((Root is TBoldObjectList) and
           ((Pos(UpperCase('->orderBy'), UpperCase(Expr)) > 0) or
            (Pos(UpperCase('->orderDescending'), UpperCase(Expr)) > 0) or
            (Pos(UpperCase('->difference'), UpperCase(Expr)) > 0) or
            (Pos(UpperCase('->symmetricDifference'), UpperCase(Expr)) > 0)
           )
          );

      if bNoBlockPSEvaluation then begin
        if Root is TBoldObject then
          OclCondition.Context.Add((Root as TBoldObject).BoldObjectLocator.BoldObjectID)
        else if Root is TBoldObjectList then
        begin
          RootAsList := Root as TBoldObjectList;
          RootAsList.EnsureObjects;
          for i := 0 to RootAsList.Count - 1 do
            OclCondition.Context.Add(RootAsList[i].BoldObjectLocator.BoldObjectID)
        end
        else if assigned(root) and not (Root is TBoldSystem) then
          raise EBoldOclError.CreateFmt(sInvalidForSQLEvaluation, [Root.AsString, Root.ClassName]);

        ExecuteOclCondition(OclCondition);
      end else begin
        ResList := TBoldMemberFactory.CreateMemberFromBoldType(aResultType) as TBoldObjectList;
        if Root is TBoldObject then begin
          OclCondition.Context.Add((Root as TBoldObject).BoldObjectLocator.BoldObjectID);
          ExecuteOclCondition(OclCondition);
        end else if Root is TBoldObjectList then begin
          RootAsList := Root as TBoldObjectList;
          RootAsList.EnsureObjects;
          ObjectCount := RootAsList.Count - 1;
          for Block := 0 to (ObjectCount div _BoldOCLPSEvaluationConditionBlockSize) do
          begin
            Start := Block * _BoldOCLPSEvaluationConditionBlockSize;
            Stop := MinIntValue([Pred(Succ(Block) * _BoldOCLPSEvaluationConditionBlockSize), ObjectCount]);
            OclCondition.Context.Clear;
            for i := Start to Stop do begin              
              OclCondition.Context.Add(RootAsList[i].BoldObjectLocator.BoldObjectID);
            end;
            ExecuteOclCondition(OclCondition);
          end;
        end else if assigned(root) and not (Root is TBoldSystem) then begin
          raise EBoldOclError.CreateFmt(sInvalidForSQLEvaluation, [Root.AsString, Root.ClassName]);
        end else begin
          ExecuteOclCondition(OclCondition);
        end;
        resultEntry.Ocl.SetOwnedValue(ResList);
      end;

    end else begin
      raise EBoldOclError.CreateFmt('%d:%s', [OLWNodeMaker.FailurePosition, OLWNodeMaker.FailureReason]); // do not localize
    end;

  finally
    OclCondition.Free;
    OLWNodeMaker.Free;
  end;
end;

procedure TBoldOcl.AddVarsToEnv(Env: TBoldOCLEnvironment;
  const VariableList: TBoldExternalVariableList; Initializevalues: Boolean);
var
  i: Integer;
  VarBinding: TBoldOclVariableBinding;
begin
  if assigned(VariableList) then
  begin
    for i := 0 to VariableList.Count - 1 do
    begin
      VarBinding := TBoldOclVariableBinding.Create;
      VarBinding.VariableName := VariableList[i].Name;
      if Initializevalues then
        VarBinding.SetReferenceValue(VariableList[i].Value);
      VarBinding.BoldType := VariableList[i].ValueType;
      Env.pushBinding(VarBinding);
    end;
  end;
end;

procedure TBoldOcl.DoneWithEntry(var oclEntry: TBoldOclEntry);
begin
  if Assigned(oclEntry) then
  begin
    if oclEntry.OwnedByDictionary then
      oclEntry.UsedByOtherEvaluation := false
    else
    begin
      oclEntry.Free;
      oclEntry := nil;
    end;
  end;
end;

procedure TBoldOcl.TodayTimerEvent(Sender: TObject);
begin
  fTodayTimer.Interval := MilliSecondsBetween( Now, Today+1 );
  fTodayVar.AsDate := Today;
end;

procedure TBoldOcl.NowTimerEvent(Sender: TObject);
var
  vTime: TDateTime;
begin
  vTime := RecodeMilliSecond(now, 0);
  vTime := RecodeSecond(vTime, 0);
  fNowVar.AsDateTime := vTime;
  vTime := IncMinute(vTime);
  fNowTimer.Interval := MilliSecondsBetween( Now, vTime );
  if (fNowTimer.Interval = 0) or (fNowTimer.Interval > MSecsPerSec * SecsPerMin) then
    fNowTimer.Interval := MSecsPerSec * SecsPerMin
end;

procedure BoldForceNelCompatibility;
begin
  BoldNelCompatibility := true;
  BoldOCLAllowCapitalMembers := true;
end;

initialization
  TBoldOClDictionary.IX_OCLEntry := -1;

finalization
  FreeAndNil(G_OclParserTable);
  FreeAndNil(G_OclScannerTable);

end.
