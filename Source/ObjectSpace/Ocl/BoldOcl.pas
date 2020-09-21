unit BoldOcl;

interface

uses
  Classes,
  BoldBase,
  BoldSystemRT,
  BoldOclClasses,
  BoldElements,
  BoldSystem,
  BoldIndexableList,
  BoldLogHandler,
  BoldOclRTDebug,
  BoldSubscription;

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
    constructor Create(const Str: string; OclNode: TBoldOclNode);
    destructor Destroy; override;
  end;

  { TBoldOClDictionary }
  TBoldOClDictionary = class(TBoldIndexableList)
  private
    function GetOcl(const Expr: string): TBoldOclEntry;
  public
    constructor Create;
    procedure AddOcl(ENTRY: TBoldOclEntry);
    property OclEntryByExpressionString[const Expr: string]: TBoldOclEntry read GetOcl;
  end;

  { TBoldOcl }
  TBoldOcl = class(TBoldRTEvaluator)
  private
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
    fCanEvaluate: Boolean;
    fOnLookUpOclDefinition: TBoldLookUpOclDefinition;
    function SyntacticParse(const Ocl: string; StoreInDictionary: Boolean; var ResultEntry: TBoldOclEntry; Context: TBoldElementTypeInfo): TBoldIndirectElement;
    function SemanticCheck(Ocl: string; Context: TBoldElementTypeInfo; StoreInDictionary: Boolean; var ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment): TBoldIndirectElement;
    function LookupOclDefinition(const name: string):string;
    procedure PSEvaluation(const Expr: string; Root: TBoldElement; ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment);
    procedure AddVarsToEnv(Env: TBoldOCLEnvironment; const VariableList: TBoldExternalVariableList; Initializevalues: Boolean);
  public
    constructor Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem);
    destructor Destroy; override;
    property SymbolTable: TBoldSymbolDictionary read fsymbolTable;
    property BoldSystem: TBoldSystem read fBoldSystem;
    procedure DefineVariable(const VariableName: String; VarValue: TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue: Boolean); override;
    procedure Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil); override;
    function ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo; override;
    function RTInfo(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean): TBoldMemberRTInfo; override;
    procedure SetLookupOclDefinition(value: TBoldLookUpOclDefinition); override;
  end;

var
  BoldOCLAllowCapitalMembers: Boolean = false;
  BoldNelCompatibility: Boolean = false;
  BoldOCLLogHandler: TBoldLogHandler = nil;
  BoldOCLRTDebugger: TBoldOCLRTDebugger = nil;
  BoldOclParserTablePath: String = '';

procedure BoldOCLLog(const s: string);
procedure BoldForceNelCompatibility;

implementation

uses
  SysUtils,
  BoldAttributes,
  BoldCondition,
  BoldDefs,
  BoldOclSymbolImplementations,
  BoldOclError,
  BoldOclLightWeightNodeMaker,
  BoldOclLightWeightNodes,
  BoldOclEvaluator,
  BoldOclSemantics,
  BoldSSLexU,
  BoldSSYaccU,
  BoldSSExcept,
  BoldORed,
  BoldHashIndexes,
  BoldCoreConsts;

{.$R *.res}

var
  IX_OCLEntry: integer = -1;
  G_OclScannerTable: SSLexTable = nil;
  G_OclParserTable: SSYaccTable = nil;

function OclScannerTable: SSLexTable;
begin
  if not assigned(G_OclScannerTable) then
  begin
    G_OclScannerTable := SSLexTable.CreateResource(HInstance, 'OCLSCANNERTABLE', 'SCANNERTABLE'); // do not localize
  end;
  result := G_OclScannerTable;
end;

function OclParserTable: SSYaccTable;
begin
  if not assigned(G_OclParserTable) then
  begin
    G_OclParserTable := SSYaccTable.CreateResource(HInstance, 'OCLPARSERTABLE', 'PARSERTABLE'); // do not localize
  end;
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
  Result := TBoldOclEntry(Item).OclString;
end;

constructor TBoldOcl.Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem);
var
  TrueConst: TBABoolean;
  FalseConst: TBABoolean;
  MaxTimeStamp: TBAInteger; // this type must mirror the type of TBoldTimeStampType
begin
  fSystemTypeInfo := SystemTypeInfo;
  fBoldSystem := BoldSystem;
  fOclDictionary := TBoldOclDictionary.Create;
  fCanEvaluate := true;
  fSymbolTable := TBoldSymbolDictionary.Create(SystemTypeInfo, BoldSystem, fCanEvaluate);
  InitializeSymbolTable(fSymbolTable);
  fStringType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['String']; // do not localize
  fIntegerType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['Integer']; // do not localize
  fFloatType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['Float']; // do not localize
  fBooleanType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['Boolean']; // do not localize
  fTimeType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['Time']; // do not localize
  fDateType := fSystemTYpeInfo.AttributeTypeInfoByExpressionName['Date']; // do not localize

  TrueConst := TBoldMemberFactory.CreateMemberFromBoldType(fBooleanType) as TBABoolean;
  trueConst.AsBoolean := true;
  DefineVariable('true', TrueConst, fBooleanType, true); // do not localize

  FalseConst := TBoldMemberFactory.CreateMemberFromBoldType(fBooleanType) as TBABoolean;;
  FalseConst.AsBoolean := False;
  DefineVariable('false', FalseConst, fBooleanType, true); // do not localize

  MaxTimeStamp := TBoldMemberFactory.CreateMemberFromBoldType(fIntegerType) as TBAInteger;
  MaxTimeStamp.AsInteger := BOLDMAXTIMESTAMP;
  DefineVariable('timeStampNow', MaxTimeStamp, fIntegerType, true); // do not localize

  DefineVariable('nil', nil, SystemTypeInfo.NilTypeInfo, false); // do not localize
end;

destructor TBoldOcl.Destroy;
begin
  FreeAndNil(fOclDictionary);
  FreeAndNil(fSymbolTable);
  FreeAndNil(fGlobalEnv);
  inherited;
end;

{ TBoldOCL }

procedure TBoldOcl.DefineVariable(const VariableName: String; VarValue: TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue: Boolean);
var
  NewVar: TBoldOCLVariableBinding;
begin
  if not assigned(fGlobalEnv) then
    fGlobalEnv := TBoldOclEnvironment.Create(nil);

  NewVar := fGlobalEnv.Lookup(Uppercase(VariableName));
  if not assigned(NewVar) then
  begin
    NewVar := TBoldOclVariableBinding.Create;
    NewVar.VariableName := VariableName;
    fGlobalEnv.pushBinding(NewVar);
  end;

  if assigned(VariableType) then
    NewVar.BoldType := VariableType
  else if assigned(VarValue) then
    NewVar.BoldType := VarValue.BoldType;

  if OwnValue then
    NewVar.SetOwnedValue(VarValue)
  else
    NewVar.SetReferenceValue(VarValue);
end;

function TBoldOcl.SyntacticParse(const Ocl: string; StoreInDictionary: Boolean; var ResultEntry: TBoldOclEntry; Context: TBoldElementTypeInfo): TBoldIndirectElement;
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
    if FixedExpr[i] in [#9, BOLDLF, #12, BOLDCR] then
      FixedExpr[i] := ' ';
  if (pos('«', FixedExpr) <> 0) or (pos('»', FixedExpr) <> 0) then
  begin
    InQuote := false;
    for i := 1 to Length(FixedExpr) do
    begin
      case Ocl[i] of
        '''': begin
          //QuotePos := i; // assignment never read, reset further down.
          InQuote := not InQuote;
        end;
        '«', '»': if not InQuote then
          raise EBoldOclAbort.CreateFmt(sExpressionNotComplete, [i]);
      end;
    end;
  end;


  //  ----------------------            BEGIN MUTEX to make OCLEvaluator threadsafe

  ResultEntry := fOclDictionary.OclEntryByExpressionString[Ocl];

  if assigned(ResultEntry) then
  begin
    if resultEntry.Ocl.IsConstant or (not ResultEntry.UsedByOtherEvaluation and (ResultEntry.Context = Context)) then
    begin
      ResultEntry.UsedByOtherEvaluation := true;
      Result := ResultEntry.Ocl;
      exit;
    end
    else
    begin
      ResultEntry := nil;
      StoreInDictionary := false;
    end
  end;
  // -------------------------          END MUTEX

  Result := nil;

  Consumer := SSLexStringConsumer.Create(PChar(FixedExpr));

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

          if Pos('SSLex0105e: Invalid token,', e.message) <> 0 then // do not localize
            for i := 1 to Length(Ocl) do
            begin
              if Ocl[i] = '''' then
                InQuote := not InQuote;
              if not InQuote and not (Ocl[i] in [' ', #9, BOLDCR, '0'..'9', 'a'..'z', 'A'..'Z', '_', '[', ']', '{', '}',
                '(', ')', '+', '-', '*', '/', '=', '>', '<', ',', '.', '@', '|', '''', ':', '#', '«', '»']) then
                raise EBoldOclAbort.CreateFmt(boeInvalidcharacter,[i - 1]);
            end;
          if Pos('SSYacc0105e: SyncErr failed, no valid token', e.message) <> 0 then // do not localize
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
          raise EBoldOclAbort.CreateFmt('%d:' + e.message, [e.Position]); // do not localize
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
        raise; // EBoldOclAbort.CreateFmt('%d:' + E.message, [-1]);
      end;
    end;
  finally
    Consumer.Free;
    Lexer.Free;
    if assigned(Parser.finalvalue) then
    begin
      ResultEntry := TBoldOclEntry.Create(Ocl, Parser.finalvalue.Node);
      ResultEntry.UsedByOtherEvaluation := true;
      Result := ResultEntry.Ocl;
    end;
    Parser.Free;
  end;
  if StoreInDictionary then
  begin
    fOclDictionary.AddOcl(ResultEntry);
    ResultEntry.OwnedByDictionary := True;
  end;
end;

function TBoldOcl.SemanticCheck(Ocl: string; Context: TBoldElementTypeInfo; StoreInDictionary: Boolean; var ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment): TBoldIndirectElement;
var
  Visitor: TBoldOclSemanticsVisitor;
  EnvSize: Integer;
begin
  try
    if ocl[1] = '%' then
      ocl := LookupOclDefinition(copy(ocl, 2, maxint));

    SyntacticParse(Ocl, StoreInDictionary, ResultEntry, Context);

    // The reason for the last part in this if-statement is that the evaluation of a nil-root to a nil-value is
    // dependant on the raising of an exception in the semantic check, so therefor we can not skip it through the
    // shortcut below. If the previous evaluation was made with a nil-context we still need to get the exception
    // to be caught in the evaluator.

    if (not ResultEntry.firstSemanticPass and
        assigned(Context) and
        context.ConformsTo(ResultEntry.Context)) or
       (assigned(ResultEntry.Ocl) and ResultEntry.ocl.IsConstant) then
    begin
      Result := ResultEntry.Ocl;
      exit;
    end;

    ResultEntry.Context := Context;
    ResultEntry.Model := fSystemTYpeInfo;

    EnvSize := Env.Count;

    ResultEntry.SelfVar.Free;
    ResultEntry.SelfVar := TBoldOclVariableBinding.Create;
    ResultEntry.SelfVar.VariableName := 'Self'; // do not localize

    ResultEntry.SelfVar.BoldType := ResultEntry.Context;

    Env.pushBinding(ResultEntry.SelfVar);


    Visitor := TBoldOclSemanticsVisitor.Create(ResultEntry.Model, self, SymbolTable, Env);
    if BoldNelCompatibility and (pos('(', ocl) <> 0) then
      Visitor.IgnoreNelCompatibility := true;

    try
      try
        Visitor.Traverse(ResultEntry.Ocl);
      except
        on e: EBoldOclAbort do
        begin
          e.Ocl := ResultEntry.OclString;
          ResultEntry.Context := nil;
          raise;
        end;
        on e: EBoldOclError do
        begin
          e.Ocl := ResultEntry.OclString;
          ResultEntry.Context := nil;
          raise;
        end;
        on e: Exception do
        begin
          // Parsträdet behöver inte raderas, det är ju syntaktiskt korrekt.
          ResultEntry.Context := nil;
          raise EBoldOclAbort.CreateFmt('%d: %s', [-1, e.message]); // do not localize
        end;
      end;
    finally
      Visitor.Free;
      Env.PopBinding;
      if EnvSize <> Env.Count then
        raise EBoldOclInternalError.CreateFmt(boeEnvSizeError, [0, EnvSize, Env.Count]);
    end;

    ResultEntry.firstSemanticPass := False;

    Result := ResultEntry.Ocl;
  except
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
  end;
end;

function TBoldOcl.RTInfo(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean): TBoldMemberRTInfo;
var
  ResultEntry: TBoldOclEntry;
  Env: TBoldOclEnvironment;
begin
  Result := nil;
  ResultEntry := nil;
  Env := TBoldOclEnvironment.Create(fGlobalEnv);
  try
    try
      if ocl <> '' then
        SemanticCheck(Ocl, Context, false, ResultEntry, Env);
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
    if assigned(ResultEntry) then
    begin
      if ResultEntry.OwnedByDictionary then
        ResultEntry.UsedByOtherEvaluation := false
      else
      begin
        ResultEntry.Free;
        ResultEntry := nil;
      end;
    end;
    Env.Free;
  end;
end;

function TBoldOcl.ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo;
var
  ResultEntry: TBoldOclEntry;
  Env: TBoldOclEnvironment;
begin
  Result := nil;
  ResultEntry := nil;
  if trim(ocl) = '' then
  begin
    result := context;
  end
  else
  begin
    Env := TBoldOclEnvironment.Create(fGlobalEnv);
    AddVarsToEnv(Env, VariableList, false);
    try
      try
        SemanticCheck(Ocl, Context, false, ResultEntry, Env);
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
      if assigned(ResultEntry) then
      begin
        if ResultEntry.OwnedByDictionary then
          ResultEntry.UsedByOtherEvaluation := false
        else
        begin
          ResultEntry.Free;
          ResultEntry := nil;
        end;
      end;
      Env.Free;
    end;
  end;
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

procedure TBoldOcl.Evaluate(Ocl: string; ROOT: TBoldElement; Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
var
  LocalContext   : TBoldElementTypeInfo;
  EvaluatorVisitor: TBoldOclEvaluatorVisitor;
  ResultEntry: TBoldOclEntry;
  Env: TBoldOclEnvironment;
  CurrentComponentPath: String;
begin
  if not fCanEvaluate then
    raise EBoldOclError.Create(sTypesMissingFromEvaluator);
  ResultEntry := nil;
  Env := nil;

   if BoldNelCompatibility and (length(ocl) > 0) and (OCL[Length(ocl)] = '.') then
     delete(ocl, length(ocl), 1);


  if assigned(BoldOCLRTDebugger) and assigned(Subscriber) then
    CurrentComponentPath := Subscriber.ContextString
  else
    CurrentComponentPath := '';

  BoldOCLLog(Ocl);
  try
    Inc(OclCounter);

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

    if assigned(BoldOclRTDebugger) and
      BoldOclRTDebugger.HasFixFor(ocl, LocalContext) then
      ocl := BoldOclRTDebugger.GetFixFor(Ocl, LocalContext);

    try
      try
        Env := TBoldOclEnvironment.Create(fGlobalEnv);
        AddVarsToEnv(Env, VariableList, true);
//        try
          SemanticCheck(Ocl, LocalContext, not assigned(VariableList), resultEntry, Env);
//        finally
//          Env.Free;
//        end;
      except
        on e: EBoldOclAbort do
        begin
          if not assigned(ROOT) and assigned(ResultEntry) then
          begin
            //     ^no root             ^but syntactically correct, semantically erroneous
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
            //     ^no root             ^but syntactically correct, semantically erroneous
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
        EvaluatorVisitor := TBoldOclEvaluatorVisitor.Create(Subscriber, ResubscribeAll, fSystemtypeInfo, BoldSystem, fStringType, fIntegerType, fFloatType, fDateType, fTimeType);
        try
          ResultEntry.Ocl.AcceptVisitor(EvaluatorVisitor);
        finally
          EvaluatorVisitor.Free;
        end;
      end;
      ResultEntry.EvaluatedOnce := True;
      if assigned(ResultElement) then
      begin
        if ResultEntry.Ocl.IsConstant then
          resultElement.SetReferenceValue(ResultEntry.Ocl.Value)
        else
          ResultEntry.Ocl.TransferValue(ResultElement);

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
    Env.Free;
    if assigned(ResultEntry) then
    begin
      if ResultEntry.OwnedByDictionary then
        ResultEntry.UsedByOtherEvaluation := false
      else
      begin
        ResultEntry.Free;
        ResultEntry := nil;
      end;
    end;
  end;
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
  Result := TBoldOclEntry(TOCLIndex(indexes[IX_OCLEntry]).FindByString(Expr));
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

procedure BoldOCLLog(const s: string);
begin
  if assigned(BoldOCLLogHandler) then
    BoldOclLogHandler.Log(formatDateTime('c: ', now)+ // do not localize
      format('OCL %4d - %s', [OclCounter, trim(s)])); // do not localize
end;

function TBoldOcl.LookupOclDefinition(const name: string): string;
begin
  if assigned(fOnLookUpOclDefinition) then
    result := fOnLookUpOclDefinition(Name)
  else
    raise EBoldOCLError.CreateFmt(sCannotFindOCLDefinitionWithoutRepository, ['%'+Name]);
end;

procedure TBoldOcl.SetLookupOclDefinition(value: TBoldLookUpOclDefinition);
begin
  fOnLookUpOclDefinition := value;
end;

procedure TBoldOcl.PSEvaluation(const Expr: string; Root: TBoldElement; ResultEntry: TBoldOclEntry; Env: TBoldOclEnvironment);
var
  OclCondition: TBoldOclCondition;
  i: integer;
  ClassTypeInfo: TBoldClassTypeInfo;
  LocalBoldSystem: TBoldSystem;
  OLWNodeMaker: TBoldOLWNodeMaker;
  resList: TBoldObjectList;
  RootAsList: TBoldObjectList;
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

      OclCondition.RootNode := OLWNodeMaker.RootNode;
      ResList := TBoldMemberFactory.CreateMemberFromBoldType(ResultEntry.Ocl.BoldType) as TBoldObjectList;
      ClassTypeInfo := (ResultEntry.Ocl.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;
      OclCondition.TopSortedIndex := ClassTypeInfo.TopSortedIndex;
      LocalBoldSystem.GetAllWithCondition(resList, OclCondition);
      resultEntry.Ocl.SetOwnedValue(ResList);

    end
    else
      raise EBoldOclError.CreateFmt('%d:%s', [OLWNodeMaker.FailurePosition, OLWNodeMaker.FailureReason]); // do not localize

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

procedure BoldForceNelCompatibility;
begin
  BoldNelCompatibility := true;
  BoldOCLAllowCapitalMembers := true;
end;

initialization

finalization
  FreeAndNil(G_OclParserTable);
  FreeAndNil(G_OclScannerTable);

end.
