unit BoldExpressionSubscriber;

{$include bold.inc}

interface

uses
  Classes,
  SysUtils,

  BoldElements,
  BoldSubscription,
  BoldSystem,
  BoldSystemRt,
  BoldHandles,
  BoldDefs,
  BoldBase,
  BoldOcl,
  BoldOclClasses,
  BoldOclVariables,
  BoldId,
  BoldTypeList,
  BoldExternalObjectSpaceEventHandler,
  BoldComponentValidator;

type
  TEvaluationMode = (emAuto, emInMemory, emInPs);
const
  cDefaultSeverity = emAuto;

type
  TExpressionSubscriber = class;
  TExpressionCollection = class;
  TExpressionDefinition = class;
  TExpressionDefinitionEnumerator = class;

  TExpressionChangedEvent = procedure(AExpressionDefinition: TExpressionDefinition) of object;

  TExpressionSubscriber = class(TBoldNonSystemHandle, IBoldValidateableComponent)
  strict private
    fExpressions: TExpressionCollection;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fExpressionChanged: TExpressionChangedEvent;
    fBoldExternalObjectSpaceEventHandler: TBoldExternalObjectSpaceEventHandler;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function GetSubscriber: TBoldSubscriber;
    procedure SetBoldExternalObjectSpaceEventHandler(const Value: TBoldExternalObjectSpaceEventHandler);
    { IBoldValidateableComponent }
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean; override;
  protected
    procedure StaticBoldTypeChanged; override;
    procedure Changed(AExpressionDefinition: TExpressionDefinition);
    function GetBoldSystem: TBoldSystem;
    property Subscriber: TBoldSubscriber read GetSubscriber;
    property BoldSystem: TBoldSystem read GetBoldSystem;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute;
  published
    property Expressions: TExpressionCollection read fExpressions write fExpressions;
    property OnExpressionChanged: TExpressionChangedEvent read fExpressionChanged write fExpressionChanged;
    property BoldExternalObjectSpaceEventHandler: TBoldExternalObjectSpaceEventHandler read fBoldExternalObjectSpaceEventHandler write SetBoldExternalObjectSpaceEventHandler;
  end;

  TExpressionDefinitionEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TExpressionDefinition;
    property Current: TExpressionDefinition read GetCurrent;
  end;

  TExpressionCollection = class(TCollection)
  strict private
    fExpressionSubscriber: TExpressionSubscriber;
  protected
    function GetItems(Index: integer): TExpressionDefinition;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    property ExpressionSubscriber: TExpressionSubscriber read fExpressionSubscriber;
  public
    constructor Create(AExpressionSubscriber: TExpressionSubscriber);
    function GetEnumerator: TExpressionDefinitionEnumerator;
    function AddDefinition(const ClassExpressionName: string; Condition: TBoldExpression; EvaluationMode: TEvaluationMode): TExpressionDefinition;
    property Items[Index: integer]: TExpressionDefinition read GetItems; default;
  end;

  TExpressionDefinition = class(TCollectionItem, IBoldOCLComponent)
  strict private
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fClassExpressionName: String;
    fCondition: TBoldExpression;
    fEvaluationMode: TEvaluationMode;
    fResultList: TBoldIndirectElement;
    fRelevantClasses: TBoldTypeList;
    fVariables: TBoldOclVariables; //TBoldExternalVariableList;
    fVariablesSubscriber: TBoldPassThroughSubscriber;
    fSpan: TObject; // actually a TBoldObjectSpan
  private
    procedure SetClassExpressionName(const Value: String);
    procedure SetEvaluationMode(const Value: TEvaluationMode);
    procedure SetVariables(Value: TBoldOclVariables);
    function GetVariables: TBoldOclVariables;
    function GetSubscriber: TBoldSubscriber;
    function GetBoldSystem: TBoldSystem;
    function GetResultList: TBoldList;
    procedure Changed;
    procedure ReceiveFromVariables(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure ObjectCreated(const AClassName: string; AObjectId: TBoldObjectId);
    procedure ObjectDeleted(const AClassName: string; AObjectId: TBoldObjectId);
    procedure MemberChanged(const AClassName, AMemberName: string; AObjectId: TBoldObjectId; ABoldObject: TBoldObject);
    function IsClassRelevant(AUmlClass: TBoldClassTypeInfo): boolean;
    function GetSelectExpression: TBoldExpression;
    property Subscriber: TBoldSubscriber read GetSubscriber;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    { IInterface }
    function QueryInterface(const IId: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure Evaluate;
    procedure EvaluateCondition(AContext: TBoldElement);
    function GetDisplayName: string; override;
    { IBoldOCLComponent }
    function GetContextType: TBoldElementTypeInfo;
    function GetExpression: TBoldExpression;
    procedure SetExpression(const Value: TBoldExpression);
    function GetVariableList: TBoldExternalVariableList; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property SelectExpression: TBoldExpression read GetSelectExpression;
    property Expression: TBoldExpression read GetExpression;
    property VariableList: TBoldExternalVariableList read GetVariableList;
    property ResultList: TBoldList read GetResultList;
  published
    property ClassExpressionName: String read fClassExpressionName write SetClassExpressionName;
    property Condition: TBoldExpression read fCondition write SetExpression;
    property Variables: TBoldOclVariables read GetVariables write SetVariables;
    property EvaluationMode: TEvaluationMode read fEvaluationMode write SetEvaluationMode default cDefaultSeverity;
  end;

implementation

uses
  Contnrs,
  BoldUtils;

type
  TBoldSpanNode = class;
  TBoldSpan = class;
  TBoldObjectSpan = class;
  TBoldAttributeNode = class;
  TBoldRoleNode = class;
  TSpanArrayNamedCollection = class;
  TSpanArray = array of TBoldObjectSpan;

  SpanException = class(Exception);

  // A node in the span tree
  TBoldSpanNode = class(TBoldMemoryManagedObject)
  strict private
    fParentNode: TBoldObjectSpan;
  private
    procedure SetParentNode(NewOwner: TBoldObjectSpan);
  strict protected
    function GetIsDerived: Boolean; virtual; abstract;
    function GetHasDerivedPart: Boolean; virtual; abstract;
    function GetAlwaysCall: Boolean; virtual;
    function GetUmlClass: TBoldClassTypeInfo; virtual; abstract;
  public
    function GetAsString: string; virtual; abstract;
    function GetPath: string; virtual; abstract;
    function ShallowClone: TBoldSpanNode; virtual; abstract;
    function DeepClone: TBoldSpanNode; virtual;
    procedure CollectRelevantClasses(ATypeList: TBoldTypeList); virtual;
    function FindMatchByObject(AUmlClass: TBoldClassTypeInfo; AObjectId: TBoldObjectId): TBoldElement; virtual;
    function FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement; virtual;
    property ParentNode: TBoldObjectSpan read fParentNode;
    property IsDerived: Boolean read GetIsDerived;
    property HasDerivedPart: Boolean read GetHasDerivedPart;
    property AlwaysCall: Boolean read GetAlwaysCall;
    property UmlClass: TBoldClassTypeInfo read GetUmlClass;
    property AsString: string read GetAsString;
  end;

  TBoldObjectSpan = class(TBoldSpanNode)
  strict private
    fUmlClass: TBoldClassTypeInfo;
//    fFetchDefaultMembers: Boolean;
    fSubnodes: TObjectList;
    function GetSubnodeCount: Integer;
    function GetSubNode(Index: Integer): TBoldSpanNode;
    function GetRootNode: TBoldSpanNode;
  protected
    function GetHasDerivedPart: Boolean; override;
    function SubNodesAsString: string;
    function GetAlwaysCall: Boolean; override;
    function GetUmlClass: TBoldClassTypeInfo; override;
  public
    constructor Create(UmlClass: TBoldClassTypeInfo);
    destructor Destroy; override;
    procedure AddSubnode(Node: TBoldSpanNode);
    procedure RemoveAt(index: Integer);
    function DeepClone: TBoldSpanNode; override;
    function ExtractAt(index: Integer): TBoldSpanNode;
    procedure CollectRelevantClasses(ATypeList: TBoldTypeList); override;
    function FindMatchByObject(AUmlClass: TBoldClassTypeInfo; AObjectId: TBoldObjectId): TBoldElement; override;
    function FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement; override;
//    property UmlClass: TBoldClassTypeInfo read fUmlClass;
//    property FetchDefaultMembers: Boolean read fFetchDefaultMembers write fFetchDefaultMembers;
    property SubnodeCount: Integer read GetSubnodeCount;
    property SubNode[Index: Integer]: TBoldSpanNode read GetSubNode;
    property RootNode: TBoldSpanNode read GetRootNode;
  end;

  TBoldSpan = class(TBoldObjectSpan)
  strict private
    class var fNextSpanNo: integer;
  strict private
    fSpanNo: Integer;
  strict protected
    function GetIsDerived: Boolean; override;
    private fRefCount: integer;
  public
    constructor Create(UmlClass: TBoldClassTypeInfo);
    destructor Destroy; override;
    function GetPath: string;  override;
    function GetAsString: string; override;
    function ShallowClone: TBoldSpanNode; override;
    procedure AddRef;
    procedure RemoveRef;
  end;

  TBoldAttributeNode = class(TBoldSpanNode)
  private
    fAttribute: TBoldAttributeRTInfo;
  strict protected
    function GetIsDerived: Boolean; override;
    function GetHasDerivedPart: Boolean; override;
    function GetUmlClass: TBoldClassTypeInfo; override;
  public
    constructor Create(UmlAttribute: TBoldAttributeRTInfo);
    function GetPath: string; override;
    function GetAsString: string; override;
    function ShallowClone: TBoldSpanNode; override;
    function FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement; override;
    property Attribute: TBoldAttributeRTInfo read fAttribute write fAttribute;
  end;

  TBoldRoleNode = class(TBoldObjectSpan)
  private
    fRole: TBoldRoleRTInfo;
  strict protected
    function GetIsDerived: Boolean; override;
  public
    constructor Create(Role: TBoldRoleRTInfo);
    function GetAsString: string; override;
    function GetPath: string; override;
    function ShallowClone: TBoldSpanNode; override;
    function FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement; override;
    property Role: TBoldRoleRTInfo read fRole;
 end;

  TBoldClassFilterNode = class(TBoldObjectSpan)
  strict protected
    function GetIsDerived: Boolean; override;
  public
    constructor Create(FilterClass: TBoldClassTypeInfo);
    function GetAsString: string; override;
    function GetPath: string; override;
    function ShallowClone: TBoldSpanNode; override;
 end;

  TBoldAttributeFilterNode = class(TBoldObjectSpan)
  strict private
    fAttribute: TBoldAttributeRTInfo;
  strict protected
    function GetIsDerived: Boolean; override;
  public
    constructor Create(UmlClass: TBoldClassTypeInfo; Attribute: TBoldAttributeRTInfo);
    function GetAsString: string; override;
    function GetPath: string; override;
    function ShallowClone: TBoldSpanNode; override;
    function FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement; override;
    property Attribute:TBoldAttributeRTInfo read fAttribute;
 end;

  TBoldAsStringNode = class(TBoldObjectSpan)
  strict protected
    function GetIsDerived: Boolean; override;
    function GetAlwaysCall: Boolean; override;
  public
    constructor Create(UmlClass: TBoldClassTypeInfo);
    function GetAsString: string; override;
    function GetPath: string; override;
    function ShallowClone: TBoldSpanNode; override;
 end;

  TBoldAllInstancesNode = class(TBoldObjectSpan)
  strict private
    fLoadedOnly: Boolean;
  strict protected
    function GetIsDerived: Boolean; override;
     function GetAlwaysCall: Boolean; override;
  public
    constructor Create(AClass: TBoldClassTypeInfo; LoadedOnly: Boolean);
    function GetAsString: string; override;
    function GetPath: string; override;
    function ShallowClone: TBoldSpanNode; override;
    property LoadedOnly: Boolean read fLoadedOnly;
  end;

  TSpanArrayNamedCollectionEntry = class(TBoldMemoryManagedObject)
  private
    fName: string;
    fSpanArray:TSpanArray;
  public
    constructor Create(const Name: string; Span: TSpanArray);
    destructor Destroy; override;
    property Name: string read fName;
    property SpanArray: TSpanArray read fSpanArray;
  end;

  TSpanArrayNamedCollection = class(TBoldMemoryManagedObject)
    fStack: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushDefinition(const name: string; Spans:TSpanArray);
    function PopDefinition : TSpanArray;
    function Lookup(const name: string): TSpanArray;
  end;

  TExpressionsDefinitionParser = class(TBoldMemoryManagedObject)
  private
    fEvaluator: TBoldOcl;
    fContextualVariableStack: TSpanArrayNamedCollection;
    fGlobalVariableStack : TSpanArrayNamedCollection;
    fCurrentContextSpans: TSpanArray;

    function DoParse(Expression: String; Evaluator: TBoldOcl; ContextType: TBoldClassTypeinfo; VariableList: TBoldExternalVariableList = nil): TBoldSpan;
    function TranslateOclOperation(OclOperation: TBoldOclOperation): TSpanArray;
    function TranslateOclListCoercion(OclListCoercion: TBoldOclListCoercion): TSpanArray;
    function TranslateOclIteration(OclIteration: TBoldOclIteration): TSpanArray;
    function TranslateOclMember(OclMember: TBoldOclMember): TSpanArray;
    function TranslateOclVariableReference(OclVariableReference: TBoldOclVariableReference): TSpanArray;
    function TranslateEntry(oclNode: TBoldOclNode): TSpanArray;
    constructor Create;
  protected
    function GetTypeSystem: TBoldSystemTypeInfo;
    property TypeSystem: TBoldSystemTypeInfo read GetTypeSystem;
  public
    destructor Destroy; override;
    class function Parse(const Expression: String; ContextType: TBoldClassTypeinfo;
      VariableList: TBoldExternalVariableList = nil; Evaluator: TBoldOcl = nil): TBoldSpan;
  end;

{ TExpressionDefinitionEnumerator }

function TExpressionDefinitionEnumerator.GetCurrent: TExpressionDefinition;
begin
  result := inherited GetCurrent as TExpressionDefinition;
end;

{ TExpressionDefinition }

function TExpressionDefinition.QueryInterface(const IId: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TExpressionDefinition.Evaluate;
var
  ClassTypeInfo: TBoldClassTypeInfo;
  InPs: boolean;
  Span: TBoldObjectSpan;
begin
  if not Assigned(BoldSystem) then
    exit;
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassExpressionName];
  fSpan := TExpressionsDefinitionParser.Parse(Expression, ClassTypeInfo);
  Assert(fSpan is TBoldObjectSpan, fSpan.ClassName);
  Span := fSpan as TBoldObjectSpan;
  fRelevantClasses.clear;
  Span.CollectRelevantClasses(fRelevantClasses);
  Assert(not fRelevantClasses.Empty);
  InPs := BoldSystem.CanEvaluateInPS(Expression, ClassTypeInfo);
  BoldSystem.EvaluateExpression(Expression, fResultList, InPs, VariableList);
  Assert(fResultList.Value is TBoldList, 'Result is not a list.');
  Assert(ClassTypeInfo.ConformsTo(ResultList.BoldType), 'Result is not a ' + ClassTypeInfo.AsString + ' it is: ' + ResultList.BoldType.AsString);
  Assert(ResultList.Mutable, 'Result is not mutable.');
  Assert(ResultList.DuplicateMode = bldmMerge, 'DuplicateMode is not bldmMerge.');
  ResultList.SubscribeToExpression(Condition, Subscriber, false, false, VariableList);
end;

procedure TExpressionDefinition.EvaluateCondition(AContext: TBoldElement);
var
  IE: TBoldIndirectElement;
  TempList: TBoldList;
begin
  if Assigned(AContext) then
  begin
    if AContext is TBoldObject then
    begin
      if AContext.EvaluateExpressionAsBoolean(Condition, false, VariableList) then
        ResultList.Add(AContext)
      else
        ResultList.Remove(AContext);
      AContext.SubscribeToExpression(Condition, Subscriber, false, false, VariableList);
    end
    else
    if AContext is TBoldList then
    begin
      TempList := TBoldList(AContext).Clone as TBoldList;
      ie := TBoldIndirectElement.Create;
      try
        AContext.EvaluateAndSubscribeToExpression(SelectExpression, Subscriber, ie, false, false, VariableList);
        TempList.RemoveList(ie.value as TBoldList); // remove matching objects to have only non matching objects in TempList
        ResultList.RemoveList(TempList); // remove non matching objects from result
        ResultList.Add(ie.value); // add matching objects to result
      finally
        ie.free;
        TempList.free;
      end;
    end;
  end;
end;

procedure TExpressionDefinition.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  case OriginalEvent of
    boeObjectCreated:  // [ClassName, ObjectId]
      ObjectCreated(String(Args[0].VString), Args[1].VObject as TBoldObjectId);

    boeObjectDeleted:  // [ClassName, ObjectId]
      ObjectDeleted(String(Args[0].VString), Args[1].VObject as TBoldObjectId);

    boeMemberChanged:  // [className, memberName, ObjectId]);
      MemberChanged(String(Args[0].VString), String(Args[1].VString), Args[2].VObject as TBoldObjectId, Args[3].VObject as TBoldObject);
  end;
end;

procedure TExpressionDefinition.ReceiveFromVariables(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = FVariables) and (OriginalEvent = beDestroying) then
    Variables := nil;
end;

procedure TExpressionDefinition.ObjectCreated(const AClassName: string;
  AObjectId: TBoldObjectId);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  Span: TBoldObjectSpan;
  bo: TBoldObject;
begin
  Span := fSpan as TBoldObjectSpan;
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  if ClassTypeInfo.BoldIsA(Span.UmlClass) then // root/result class match
  begin
    bo := BoldSystem.Locators.LocatorByID[AObjectId].EnsuredBoldObject;
    EvaluateCondition(BO);
  end;
  if IsClassRelevant(ClassTypeInfo) then
  begin
    bo := Span.FindMatchByObject(ClassTypeInfo, AObjectId) as TBoldObject;
    if Assigned(bo) then
      EvaluateCondition(bo);
  end;
end;

procedure TExpressionDefinition.ObjectDeleted(const AClassName: string;
  AObjectId: TBoldObjectId);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  Span: TBoldObjectSpan;
  bo: TBoldObject;
begin
  Span := fSpan as TBoldObjectSpan;
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  if ClassTypeInfo.BoldIsA(Span.UmlClass) then // root/result class match
  begin
    bo := BoldSystem.Locators.LocatorByID[AObjectId].EnsuredBoldObject;
    EvaluateCondition(BO);
  end;
  if IsClassRelevant(ClassTypeInfo) then
  begin
    bo := Span.FindMatchByObject(ClassTypeInfo, AObjectId) as TBoldObject;
    if Assigned(bo) then
      EvaluateCondition(bo);
  end;
end;

procedure TExpressionDefinition.MemberChanged(const AClassName,
  AMemberName: string; AObjectId: TBoldObjectId; ABoldObject: TBoldObject);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  Span: TBoldObjectSpan;
  bo: TBoldObject;
  Element: TBoldElement;
  sl: TStringList;
  i,j: integer;
begin
  Span := fSpan as TBoldObjectSpan;
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  sl := TStringList.Create;
  try
    sl.CommaText := AMemberName;
    for i := 0 to sl.Count - 1 do
    begin
      j := ClassTypeInfo.MemberIndexByExpressionName[sl[i]];
//      j := ABoldObject.BoldMemberIndexByExpressionName[sl[i]];
      if j = -1 then
        raise EOSS.CreateFmt('Class %s does not have a member "%s", check OSS settings of other clients.', [AClassName, AMemberName]);
      MemberRTInfo := ClassTypeInfo.MemberRTInfoByExpressionName[AMemberName];
      Element := Span.FindMatchByMember(MemberRTInfo, bo);
      if Assigned(Element) then
        EvaluateCondition(Element);
      end;
  finally
    sl.free;
  end;
  bo := BoldSystem.Locators.LocatorByID[AObjectId].EnsuredBoldObject;
end;

function TExpressionDefinition._AddRef: Integer;
begin
  result := -1;
end;

function TExpressionDefinition._Release: Integer;
begin
  result := -1;
end;

procedure TExpressionDefinition.Assign(Source: TPersistent);
var
  SourceDefinition: TExpressionDefinition;
begin
  if Source is TExpressionDefinition then
  begin
    SourceDefinition := Source as TExpressionDefinition;
    ClassExpressionName := SourceDefinition.ClassExpressionName;
    Condition := SourceDefinition.Condition;
    EvaluationMode := SourceDefinition.EvaluationMode;
  end
  else
    inherited;
end;

procedure TExpressionDefinition.Changed;
begin
  if Assigned(fSubscriber) then
    fSubscriber.CancelAllSubscriptions;
  fResultList.SetOwnedValue(nil);
  fRelevantClasses.Clear;
  FreeAndNil(fSpan);
end;

constructor TExpressionDefinition.Create(Collection: TCollection);
begin
  inherited;
  fResultList := TBoldIndirectElement.Create;
  fVariablesSubscriber := TBoldPassthroughSubscriber.create(ReceiveFromVariables);
  fRelevantClasses := TBoldTypeList.Create;
  fRelevantClasses.DuplicateMode := bldmMerge;
end;

destructor TExpressionDefinition.Destroy;
begin
  FreeAndNil(fSubscriber);
  FreeAndNil(fVariablesSubscriber);
  FreeAndNil(fSpan);
  FreeAndNil(fResultList);
  FreeAndNil(fRelevantClasses);
  inherited;
end;

function TExpressionDefinition.GetBoldSystem: TBoldSystem;
begin
  result := TExpressionCollection(Collection).ExpressionSubscriber.BoldSystem;
end;

function TExpressionDefinition.GetContextType: TBoldElementTypeInfo;
var
  BoldSystemTypeInfo: TBoldSystemTypeInfo;
begin
  result := nil;
  BoldSystemTypeInfo := (Collection as TExpressionCollection).ExpressionSubscriber.StaticSystemTypeInfo;
  if Assigned(BoldSystemTypeInfo) then
    result := BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassExpressionName];
end;

function TExpressionDefinition.GetDisplayName: string;
begin
  result := Format('[%s]:%s', [ClassExpressionName, Condition])
end;

function TExpressionDefinition.GetExpression: TBoldExpression;
begin
  result := Format('%s.allInstances->select(%s)', [ClassExpressionName, Condition]);
end;

function TExpressionDefinition.GetResultList: TBoldList;
begin
  result := fResultList.Value as TBoldList;
end;

function TExpressionDefinition.GetSelectExpression: TBoldExpression;
begin
  result := Format('select(%s)', [Condition]);
end;

function TExpressionDefinition.GetSubscriber: TBoldSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  result := fSubscriber;
end;

function TExpressionDefinition.GetVariableList: TBoldExternalVariableList;
begin
  if assigned(fVariables) then
    result := fVariables.VariableList
  else
    result := nil;
end;

function TExpressionDefinition.GetVariables: TBoldOclVariables;
begin
  result := fVariables;
end;

function TExpressionDefinition.IsClassRelevant(
  AUmlClass: TBoldClassTypeInfo): boolean;
var
  Element: TBoldElement;
begin
  result := true;
  for Element in fRelevantClasses do
    if AUmlClass.BoldIsA(Element as TBoldElementTypeInfo) then
      exit;
  result := false;
end;

procedure TExpressionDefinition.SetClassExpressionName(const Value: String);
begin
  fClassExpressionName := Value;
  Changed;
end;

procedure TExpressionDefinition.SetEvaluationMode(const Value: TEvaluationMode);
begin
  fEvaluationMode := Value;
  Changed;
end;

procedure TExpressionDefinition.SetExpression(const Value: TBoldExpression);
begin
  fCondition := Value;
  Changed;
end;

procedure TExpressionDefinition.SetVariables(Value: TBoldOclVariables);
begin
  if Value <> Variables then
  begin
    FVariables := Value;
    Changed;
    fVariablesSubscriber.CancelAllSubscriptions;
    if assigned(Value) then
      Value.AddSmallSubscription(fVariablesSubscriber, [beDestroying]);
  end;
end;

{ TExpressionCollection }

function TExpressionCollection.AddDefinition(const ClassExpressionName: string;
  Condition: TBoldExpression;
  EvaluationMode: TEvaluationMode): TExpressionDefinition;
begin
  result := Add as TExpressionDefinition;
  result.ClassExpressionName := ClassExpressionName;
  result.Condition := Condition;
  result.EvaluationMode := EvaluationMode;
end;

constructor TExpressionCollection.Create(
  AExpressionSubscriber: TExpressionSubscriber);
begin
  inherited Create(TExpressionDefinition);
  fExpressionSubscriber := AExpressionSubscriber;
end;

function TExpressionCollection.GetEnumerator: TExpressionDefinitionEnumerator;
begin
  result := TExpressionDefinitionEnumerator.Create(self);
end;

function TExpressionCollection.GetItems(Index: integer): TExpressionDefinition;
begin
  result := TExpressionDefinition(inherited items[index]);
end;

function TExpressionCollection.GetOwner: TPersistent;
begin
  result := fExpressionSubscriber;
end;

procedure TExpressionCollection.Update(Item: TCollectionItem);
begin
  fExpressionSubscriber.Changed(Item as TExpressionDefinition);
end;

{ TExpressionSubscriber }

procedure TExpressionSubscriber.AfterConstruction;
begin
  inherited;
  fExpressions := TExpressionCollection.Create(self);
end;

procedure TExpressionSubscriber.BeforeDestruction;
begin
  FreeAndNil(fExpressions);
  FreeAndNil(fSubscriber);
  inherited;
end;

function TExpressionSubscriber.GetBoldSystem: TBoldSystem;
begin
  result := nil;
  if Assigned(StaticSystemHandle) then
    result := StaticSystemHandle.System;
end;

procedure TExpressionSubscriber.Changed(AExpressionDefinition: TExpressionDefinition);
begin
  if Assigned(fExpressionChanged) then
    fExpressionChanged(AExpressionDefinition);
end;

function TExpressionSubscriber.GetSubscriber: TBoldSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  result := fSubscriber;
end;

procedure TExpressionSubscriber.Execute;
var
  ExpressionDefinition: TExpressionDefinition;
begin
  if Assigned(BoldSystem) then
    for ExpressionDefinition in Expressions do
      ExpressionDefinition.Evaluate;
end;

procedure TExpressionSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
var
  ExpressionDefinition: TExpressionDefinition;
begin
  if OriginalEvent in beOssEvents then
    for ExpressionDefinition in Expressions do
      ExpressionDefinition.Receive(Originator, OriginalEvent, RequestedEvent, Args);
//  Execute;
end;

procedure TExpressionSubscriber.SetBoldExternalObjectSpaceEventHandler(
  const Value: TBoldExternalObjectSpaceEventHandler);
begin
  if fBoldExternalObjectSpaceEventHandler <> Value then
  begin
    fBoldExternalObjectSpaceEventHandler := Value;
    if Assigned(fBoldExternalObjectSpaceEventHandler) then
    begin
      fBoldExternalObjectSpaceEventHandler.AddSubscription(Subscriber, boeObjectCreated);
      fBoldExternalObjectSpaceEventHandler.AddSubscription(Subscriber, boeObjectDeleted);
      fBoldExternalObjectSpaceEventHandler.AddSubscription(Subscriber, boeMemberChanged);
    end;
  end;
end;

procedure TExpressionSubscriber.StaticBoldTypeChanged;
begin
  inherited;
  Execute; // TODO: do not execute right away, but add to queue instead
end;

function TExpressionSubscriber.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  Context: TBoldElementTypeInfo;
  ExpressionDefinition: TExpressionDefinition;
begin
  result := inherited ValidateComponent(ComponentValidator, NamePrefix);
  for ExpressionDefinition in Expressions do
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      ExpressionDefinition.Condition,
      ExpressionDefinition.GetContextType,
      format('%s%s.ExpressionDefinition[%d]', [NamePrefix, ExpressionDefinition.DisplayName, ExpressionDefinition.Index])) and result;
  end;
end;

{ TBoldSpanNode }

procedure TBoldSpanNode.CollectRelevantClasses(ATypeList: TBoldTypeList);
begin
// nothing here
end;

function TBoldSpanNode.DeepClone: TBoldSpanNode;
begin
  result := ShallowClone;
end;

function TBoldSpanNode.GetAlwaysCall: Boolean;
begin
  Result := false;
end;

function TBoldSpanNode.FindMatchByObject(AUmlClass: TBoldClassTypeInfo; AObjectId: TBoldObjectId): TBoldElement;
begin
  result := nil; //AUmlClass.BoldIsA(UmlClass)
end;

function TBoldSpanNode.FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement;
begin
  result := nil;
end;

procedure TBoldSpanNode.SetParentNode(NewOwner: TBoldObjectSpan);
begin
  fParentNode := NewOwner;
end;

{ TBoldObjectSpan }

function TBoldObjectSpan.GetSubNode(Index: Integer): TBoldSpanNode;
begin
  Result := fSubnodes[Index] as TBoldSpanNode;
end;

function TBoldObjectSpan.GetSubnodeCount: Integer;
begin
  if Assigned(fSubnodes) then
    Result := fSubnodes.Count
  else
    Result := 0;
end;

procedure TBoldObjectSpan.AddSubnode(Node: TBoldSpanNode);
begin
  if not Assigned(fSubnodes) then
    fSubnodes := TObjectList.Create;
  fSubnodes.Add(Node);
  Node.SetParentNode(Self);
end;

procedure TBoldObjectSpan.CollectRelevantClasses(ATypeList: TBoldTypeList);
var
  i: Integer;
begin
  ATypeList.Add(UmlClass);
  for I := 0 to SubNodeCount - 1 do
    SubNode[i].CollectRelevantClasses(ATypeList);
end;

constructor TBoldObjectSpan.Create(UmlClass: TBoldClassTypeInfo);
begin
  inherited Create;
  fUmlClass := UmlClass;
end;

function TBoldObjectSpan.DeepClone: TBoldSpanNode;
var
  i: Integer;
  NewSpan: TBoldObjectSpan;
begin
  NewSpan := inherited DeepClone as TBoldObjectSpan;
  for I := 0 to NewSpan.SubNodeCount - 1 do
    NewSpan.AddSubnode(SubNode[i].DeepClone);
  Result := NewSpan;
end;

destructor TBoldObjectSpan.Destroy;
begin
  FreeAndNil(fSubnodes);
  inherited;
end;

function TBoldObjectSpan.ExtractAt(index: Integer): TBoldSpanNode;
begin
  result := fSubNodes[index] as TBoldSpanNode;
  fSubNodes.OwnsObjects := false;
  fSubnodes.Delete(index);
  fSubNodes.OwnsObjects := true;
  result.SetParentNode(nil);
end;

function TBoldObjectSpan.GetAlwaysCall: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to SubnodeCount - 1 do
    if SubNode[i].AlwaysCall then
    begin
      Result := True;
      Exit;
    end;
end;

function TBoldObjectSpan.GetHasDerivedPart: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to SubnodeCount - 1 do
    if SubNode[i].IsDerived or SubNode[i].HasDerivedPart then
    begin
      Result := True;
      Exit;
    end;
end;

function TBoldObjectSpan.GetRootNode: TBoldSpanNode;
begin
  result := nil;
  if Assigned(ParentNode) then
    result := ParentNode.RootNode;
end;

function TBoldObjectSpan.GetUmlClass: TBoldClassTypeInfo;
begin
  result := fUmlClass;
end;

function TBoldObjectSpan.FindMatchByObject(AUmlClass: TBoldClassTypeInfo; AObjectId: TBoldObjectId): TBoldElement;
var
  i: integer;
begin
  result := inherited FindMatchByObject(AUmlClass, AObjectId);
  if Assigned(result) then
    exit;
  for I := 0 to SubnodeCount - 1 do
  begin
    result := SubNode[i].FindMatchByObject(AUmlClass, AObjectId);
    if Assigned(result) then
      exit;
  end;
end;

function TBoldObjectSpan.FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement;
var
  i: integer;
begin
  result := nil;
  for I := 0 to SubnodeCount - 1 do
  begin
    result := SubNode[i].FindMatchByMember(AMember, AObject);
    if Assigned(result) then
      exit;
  end;
end;

procedure TBoldObjectSpan.RemoveAt(index: Integer);
begin
  fSubnodes.Delete(index);
end;

function TBoldObjectSpan.SubNodesAsString: string;
var
  I: Integer;
  First: Boolean;
begin
  Result := '';
  if SubnodeCount = 0 then
   Exit;
  if SubnodeCount > 1 then
    Result := Result + '(';
  First := true;
  for I := 0 to SubnodeCount - 1 do
  begin
    if (not First) then
      Result := Result + ', ';
    Result := Result + SubNode[i].AsString;
    First := False;
  end;

  if SubnodeCount > 1 then
    Result := Result + ')';
end;

{ TBoldSpan }

procedure TBoldSpan.AddRef;
begin
  INC(fRefCount);
end;

constructor TBoldSpan.Create(UmlClass: TBoldClassTypeInfo);
begin
  Inherited Create(UmlClass);
  fRefCount := 1;
  fSPanNo := fNextSpanNo;
  INC(fNextSpanNo);
end;

destructor TBoldSpan.Destroy;
begin

  inherited;
end;

function TBoldSpan.GetAsString: string;
begin
  Result := SubNodesAsString;
end;

function TBoldSpan.GetIsDerived: Boolean;
begin
  Result := false;
end;

function TBoldSpan.GetPath: string;
begin
  Result := '<root>';
end;

procedure TBoldSpan.RemoveRef;
begin
  if fRefCount <= 1 then
    self.Free
  else
    DEC(fRefCount);
end;

function TBoldSpan.ShallowClone: TBoldSpanNode;
var
  NewSpan: TBoldSpan;
begin
  NewSpan := TBoldSpan.Create(UmlClass);
//  NewSpan.FetchDefaultMembers := FetchDefaultMembers;
  Result := NewSpan;
end;

{ TBoldAttributeNode }

constructor TBoldAttributeNode.Create(UmlAttribute: TBoldAttributeRTInfo);
begin
  fAttribute := UmlAttribute;
end;

function TBoldAttributeNode.GetAsString: string;
begin
  Result := Attribute.ExpressionName;
end;

function TBoldAttributeNode.GetHasDerivedPart: Boolean;
begin
  Result := false;
end;

function TBoldAttributeNode.GetIsDerived: Boolean;
begin
  Result := fAttribute.IsDerived;
end;

function TBoldAttributeNode.GetPath: string;
begin
  Result := ParentNode.GetPath + '.' + AsString;
end;

function TBoldAttributeNode.GetUmlClass: TBoldClassTypeInfo;
begin
  result := fAttribute.ClassTypeInfo;
end;

function TBoldAttributeNode.FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement;
begin
  result := nil;
  if fAttribute = AMember then
  begin
    result := AObject;
  end;
end;

function TBoldAttributeNode.ShallowClone: TBoldSpanNode;
begin
  Result := TBoldAttributeNode.Create(Attribute);
end;

{ TBoldRoleNode }

constructor TBoldRoleNode.Create(Role: TBoldRoleRTInfo);
begin
  Inherited Create(Role.ClassTypeInfoOfOtherEnd);
  fRole := Role;
end;

function TBoldRoleNode.GetAsString: string;
var
  SnString: string;
begin
  SnString := SubNodesAsString;
  Result := Role.ExpressionName;
//  if not FetchDefaultMembers then
//    Result := Result + '!';
  if SnString <> '' then
  begin
    if (SnString[1] <> '?') then
      Result := Result + ', ';
    Result := '(' +  Result + SnString + ')';
  end;
end;

function TBoldRoleNode.GetIsDerived: Boolean;
begin
  Result := fRole.IsDerived;
end;

function TBoldRoleNode.GetPath: string;
begin
  Result := ParentNode.GetPath + '.' + Role.ExpressionName;
end;

function TBoldRoleNode.FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement;
var
  i: integer;
  Member: TBoldMember;
begin
  result := nil;
  if fRole = AMember then
  begin
    result := AObject.BoldMembers[fRole.IndexOfOtherEnd];
  end;
  for I := 0 to SubnodeCount - 1 do
  begin
    result := SubNode[i].FindMatchByMember(AMember, AObject);
    if Assigned(result) then
    begin
      Assert(result is TBoldObject, result.classname);
      Member := TBoldObject(result).BoldMembers[fRole.IndexOfOtherEnd];
      if Member is TBoldObjectReference then
        result := TBoldObjectReference(Member).BoldObject
      else
        result := Member;
      exit;
    end;
  end;
end;

function TBoldRoleNode.ShallowClone: TBoldSpanNode;
var
  NewRole: TBoldRoleNode;
begin
  NewRole := TBoldRoleNode.Create(Role);
//  NewRole.FetchDefaultMembers := FetchDefaultMembers;
  Result := NewRole;
end;

{ TSpanArrayNamedCollectionEntry }

constructor TSpanArrayNamedCollectionEntry.Create(const Name: string;
  Span: TSpanArray);
begin
  fName := Name;
  fSpanArray := Span;
end;

destructor TSpanArrayNamedCollectionEntry.Destroy;
begin
  inherited;
end;

{ TExpressionsDefinitionParser }

constructor TExpressionsDefinitionParser.Create;
begin
  fContextualVariableStack := TSpanArrayNamedCollection.Create;
  fGlobalVariableStack := TSpanArrayNamedCollection.Create;
end;

destructor TExpressionsDefinitionParser.Destroy;
begin
  FreeAndNil(fContextualVariableStack);
  FreeAndNil(fGlobalVariableStack);
  inherited;
end;

function TExpressionsDefinitionParser.DoParse(Expression: String;
  Evaluator: TBoldOcl; ContextType: TBoldClassTypeinfo;
  VariableList: TBoldExternalVariableList): TBoldSpan;
var
  ResultEntry: TBoldOclEntry;
begin
  ResultEntry := Evaluator.SemanticCheck(Expression, ContextType, VariableList);
  Result := TBoldSpan.Create(ContextType);
  SetLength(fCurrentContextSpans, 1);
  fCurrentContextSpans[0] := result;
  try
    fContextualVariableStack.PushDefinition('self',  fCurrentContextSpans);
    if Assigned(ResultEntry) then
      TranslateEntry(ResultEntry.Ocl);
  finally
    Evaluator.DoneWithEntry(ResultEntry);
  end;
end;

function TExpressionsDefinitionParser.GetTypeSystem: TBoldSystemTypeInfo;
begin
  result := fEvaluator.BoldSystem.BoldSystemTypeInfo;
end;

class function TExpressionsDefinitionParser.Parse(const Expression: String;
  ContextType: TBoldClassTypeinfo; VariableList: TBoldExternalVariableList;
  Evaluator: TBoldOcl): TBoldSpan;
var
  Parser: TExpressionsDefinitionParser;
begin
  Parser := TExpressionsDefinitionParser.Create;
  try
    if not Assigned(Evaluator) then
      Evaluator := TBoldSystem.DefaultSystem.Evaluator as TBoldOcl;
    Parser.fEvaluator := Evaluator;
    Result := Parser.DoParse(Expression, Evaluator, ContextType, VariableList);
  finally
    Parser.Free;
  end;
end;

function TExpressionsDefinitionParser.TranslateEntry(
  oclNode: TBoldOclNode): TSpanArray;
begin
  if oclNode is TBoldOclVariableReference then
    Result := TranslateOclVariableReference(OclNode as TBoldOclVariableReference)
  else if oclNode is TBoldOclMember then
    Result := TranslateOclMember(OclNode as TBoldOclMember)
  else if oclNode is TBoldOclIteration then
    Result := TranslateOclIteration(OclNode as TBoldOclIteration)
  else if oclNode is TBoldOclMethod then
    raise SpanException.Create('Can''t autotranslate Method calls:' + (oclNode as TBoldOclMethod).OperationName)
  else if oclNode is TBoldOclOperation then
    Result := TranslateOclOperation(OclNode as TBoldOclOperation)
  else if oclNode is TBoldOclListCoercion then
    Result := TranslateOclListCoercion (OclNode as TBoldOclListCoercion)
  else if oclNode is TBoldOclStrLiteral then
    Result := nil
  else if oclNode is TBoldOclLiteral then
    Result := nil
  else if oclNode is TBoldOclTypeNode then
    Result := nil
  else
    raise SpanException.Create('Can''t autotranslate nodetype: ' + oclNode.ClassName);
end;

function TExpressionsDefinitionParser.TranslateOclIteration(
  OclIteration: TBoldOclIteration): TSpanArray;
var
  OldContext: TSpanArray;
  Arg1result: TSpanArray;
begin
  OldContext := fCurrentContextSpans;
  fCurrentContextSpans := TranslateEntry(OclIteration.Args[0]);
  fContextualVariableStack.PushDefinition(OclIteration.LoopVar.VariableName, fCurrentContextSpans);
  Arg1result := TranslateEntry(OclIteration.Args[1]);
  fContextualVariableStack.PopDefinition;
  if BoldNamesEqual(OclIteration.OperationName, 'collect') then
    Result :=  Arg1result
  else
    Result := fCurrentContextSpans;
  fCurrentContextSpans := OldContext;
end;

function TExpressionsDefinitionParser.TranslateOclListCoercion(
  OclListCoercion: TBoldOclListCoercion): TSpanArray;
begin
   Result := TranslateEntry(OclListCoercion.Child);
end;

function TExpressionsDefinitionParser.TranslateOclMember(
  OclMember: TBoldOclMember): TSpanArray;
var
  MemberOfSpans: TSpanArray;
  attributeNode: TBoldAttributeNode;
  roleNode: TBoldRoleNode;
  i: integer;
begin
  MemberOfSpans := TranslateEntry(OclMember.MemberOf);
  if not Assigned(MemberOfSpans) then
     raise SpanException.Create(Format('%s seems not to be a member of anything', [OclMember.RTInfo.ExpressionName]));

  if OclMember.RtInfo.IsAttribute then
  begin
    for i := 0 to Length(MemberOfSpans)-1  do
      begin
        attributeNode := TBoldAttributeNode.Create(Oclmember.RtInfo as TBoldAttributeRTInfo);
        MemberOfSpans[i].AddSubnode(attributeNode);
      end;
  Result := nil;
  end
  else //OclMember is TBoldRole
  begin
    SetLength(Result, Length(MemberOfSpans));
    for i := 0 to Length(MemberOfSpans)-1  do
    begin
      roleNode := TBoldRoleNode.Create(Oclmember.RtInfo as TBoldRoleRTInfo);
//      roleNode.FetchDefaultMembers := true;
      MemberOfSpans[i].AddSubnode(roleNode);
      Result[i] := RoleNode;
    end;
  end
end;

function Union(Part1: TSpanArray; Part2: TSpanArray): TSpanArray;
var
  Length1, Length2, i: integer;
begin
  Length1 := Length(Part1);
  Length2 := Length(Part2);
  SetLength(Result, Length1 + Length2);
  for I := 0 to Length1 - 1 do
    Result[i] := Part1[i];
  for I := 0 to Length2 - 1 do
    Result[I + Length1] := Part2[i];
end;

function TExpressionsDefinitionParser.TranslateOclOperation(
  OclOperation: TBoldOclOperation): TSpanArray;
var
  Arg0Value: TSpanArray;
  OperationName: string;
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassFilterNode: TBoldClassFilterNode;
  AllInstacesNode: TBoldAllInstancesNode;
  asStringNode: TBoldAsStringNode;
  TypeName: string;
  i: integer;
begin
  OperationName := OclOperation.OperationName;
  if BoldNamesEqual(OperationName, 'if') then
  begin
    TranslateEntry(OclOperation.Args[0]);
    Result := Union(TranslateEntry(OclOperation.Args[1]), TranslateEntry(OclOperation.Args[2]));
   end
  else if BoldNamesEqual(OperationName, 'first') or
    BoldNamesEqual(OperationName, 'last')
  then
  begin
    Result := TranslateEntry(OclOperation.Args[0]);
  end
  else if BoldNamesEqual(OperationName, 'at')
  then
  begin
    Result := TranslateEntry(OclOperation.Args[0]);
    TranslateEntry(OclOperation.Args[1]);
  end // no parameters, non result
  else if
    BoldNamesEqual(OperationName, 'emptyList') or
    BoldNamesEqual(OperationName, 'nullValue')
    then
    begin
      Result := nil;
    end
  else if // one parameter, no result
    BoldNamesEqual(OperationName, 'not') or
    BoldNamesEqual(OperationName, 'oclIsTypeOf') or
    BoldNamesEqual(OperationName, 'oclType') or
    BoldNamesEqual(OperationName, 'asDateTime') or
    BoldNamesEqual(OperationName, 'count') or
    BoldNamesEqual(OperationName, 'size') or
    BoldNamesEqual(OperationName, 'sum') or
    BoldNamesEqual(OperationName, 'isEmpty') or
    BoldNamesEqual(OperationName, 'notEmpty') or
    BoldNamesEqual(OperationName, 'isNull') or
    BoldNamesEqual(OperationName, 'formatDateTime') or
    BoldNamesEqual(OperationName, 'length') or
    BoldNamesEqual(OperationName, 'unary-') or
    BoldNamesEqual(OperationName, 'abs') or
    BoldNamesEqual(OperationName, 'floor') or
    BoldNamesEqual(OperationName, 'round') or
    BoldNamesEqual(OperationName, 'strToInt') or
    BoldNamesEqual(OperationName, 'strToFloat') or
    BoldNamesEqual(OperationName, 'toUpper') or
    BoldNamesEqual(OperationName, 'toLower') or
    BoldNamesEqual(OperationName, 'strToDate') or
    BoldNamesEqual(OperationName, 'strToTime') or
    BoldNamesEqual(OperationName, 'strToDateTime') or
    BoldNamesEqual(OperationName, 'asFloat') or
    BoldNamesEqual(OperationName, 'datePart') or
    BoldNamesEqual(OperationName, 'oclIsKindOf') or
    BoldNamesEqual(OperationName, 'formatDateTime') or
    BoldNamesEqual(OperationName, 'asCommaText') or
    BoldNamesEqual(OperationName, 'separate') or
    BoldNamesEqual(OperationName, 'day') or
    BoldNamesEqual(OperationName, 'month') or
    BoldNamesEqual(OperationName, 'year') or
    BoldNamesEqual(OperationName, 'dayOfWeek') or
    BoldNamesEqual(OperationName, 'asISODateTime') or
    BoldNamesEqual(OperationName, 'asISODate') or
    BoldNamesEqual(OperationName, 'week') or
    BoldNamesEqual(OperationName, 'formatNumeric') or
    BoldNamesEqual(OperationName, 'maxValue') or
    BoldNamesEqual(OperationName, 'minValue') or
    BoldNamesEqual(OperationName, 'constraints') or
    BoldNamesEqual(OperationName, 'floatAsDateTime') or
    BoldNamesEqual(OperationName, 'trim') or
    BoldNamesEqual(OperationName, 'hasDuplicates') or
    BoldNamesEqual(OperationName, 'allSubClasses') or
    BoldNamesEqual(OperationName, 'toStringCollection') or
    BoldNamesEqual(OperationName, 'toIntegerCollection') or
    BoldNamesEqual(OperationName, 'boldId')
    then
  begin
    TranslateEntry(OclOperation.Args[0]);
    Result := nil;
  end
  else if BoldNamesEqual(OperationName, 'asString')  then
  begin
    Arg0Value := TranslateEntry(OclOperation.Args[0]);
    for I := 0 to Length(Arg0Value) - 1 do
    begin
       asStringNode := TBoldAsStringNode.Create(Arg0Value[i].UmlClass);
       Arg0Value[i].AddSubNode(asStringNode);
    end;
  Result := nil;
  end
  else if // two parameters, no result
    (OperationName = '<>') or
    (OperationName = '=') or
    (OperationName = '<') or
    (OperationName = '>') or
    (OperationName = '<=') or
    (OperationName = '>=') or
    (OperationName = '+') or
    (OperationName = '-') or
    (OperationName = '*') or
    (OperationName = '/') or
    BoldNamesEqual(OperationName, 'concat') or
    BoldNamesEqual(OperationName, 'and') or
    BoldNamesEqual(OperationName, 'or') or
    BoldNamesEqual(OperationName, 'min') or
    BoldNamesEqual(OperationName, 'max') or
    BoldNamesEqual(OperationName, 'mod') or
    BoldNamesEqual(OperationName, 'like') or
    BoldNamesEqual(OperationName, 'sqlLike') or
    BoldNamesEqual(OperationName, 'sqlLikeCaseInsensitive') or
    BoldNamesEqual(OperationName, 'regExpMatch') or
    BoldNamesEqual(OperationName, 'includes') or
    BoldNamesEqual(OperationName, 'contains') or
    BoldNamesEqual(OperationName, 'indexOf') or
    BoldNamesEqual(OperationName, 'safeDiv') or
    BoldNamesEqual(OperationName, 'hoursBetween') or
    BoldNamesEqual(OperationName, 'minutesBetween') or
    BoldNamesEqual(OperationName, 'secondsBetween') then
  begin
    TranslateEntry(OclOperation.Args[0]);
    TranslateEntry(OclOperation.Args[1]);
    Result := nil;
  end
  else if // three parameters, no result
    BoldNamesEqual(OperationName, 'subString') or
    BoldNamesEqual(OperationName,'inDateRange') or
    BoldNamesEqual(OperationName, 'inTimeRange')
  then
  begin
    TranslateEntry(OclOperation.Args[0]);
    TranslateEntry(OclOperation.Args[1]);
    TranslateEntry(OclOperation.Args[2]);
    Result := nil;
  end
  else if // one parameter, which is result
    BoldNamesEqual(OperationName, 'reverseCollection') or
    BoldNamesEqual(OperationName, 'asSet') or
    BoldNamesEqual(OperationName, 'orderby') or
    BoldNamesEqual(OperationName, 'subSequence') or
    BoldNamesEqual(OperationName, 'orderdescending')
  then
  begin
    Result := TranslateEntry(OclOperation.Args[0]);
  end
  else if
    BoldNamesEqual(OperationName, 'safecast') or
    BoldNamesEqual(OperationName, 'oclAsType') or
    BoldNamesEqual(OperationName, 'filterOnType')
  then
  begin
    typeName := (OclOperation.Args[1] as TBoldOclTypeNode).typeName;
    ClassTypeInfo := TypeSystem.ClassTypeInfoByExpressionName[typeName];
    Arg0Value := TranslateEntry(OclOperation.Args[0]);
    SetLength(Result, Length(Arg0Value));

    for I := 0 to Length(Arg0Value) - 1 do
    begin
       ClassFilterNode := TBoldClassFilterNode.Create(ClassTypeInfo);
       Arg0Value[i].AddSubNode(ClassFilterNode);
       Result[i] := ClassFilterNode;
    end;
  end
  else if
    BoldNamesEqual(OperationName, 'allinstances') or
    BoldNamesEqual(OperationName, 'allLoadedObjects')
  then
  begin
    typeName := (OclOperation.Args[0] as TBoldOclTypeNode).typeName;
    ClassTypeInfo := TypeSystem.ClassTypeInfoByExpressionName[typeName];
    if Assigned(ClassTypeInfo) then // ClassTypeInfo is nil with Enumerated types (like Weekday.allInstances)
    begin
      SetLength(Result, Length(fCurrentContextSpans));
      for I := 0 to Length(fCurrentContextSpans) - 1 do
      begin
        AllInstacesNode := TBoldAllInstancesNode.Create(ClassTypeInfo, BoldNamesEqual(OperationName, 'allLoadedObjects'));
        fCurrentContextSpans[i].AddSubNode(AllInstacesNode);
        Result[i] := AllInstacesNode;
      end;
    end
    else
      Result := nil;
  end
  else if BoldNamesEqual(OperationName, 'union') or
    BoldNamesEqual(OperationName, 'symmetricDifference') or
    BoldNamesEqual(OperationName, 'includesAll') or
    BoldNamesEqual(OperationName, 'intersection') or
    BoldNamesEqual(OperationName, 'including') or
    BoldNamesEqual(OperationName, 'excluding') or
    BoldNamesEqual(OperationName, 'difference')
  then
  begin
    Result := Union(TranslateEntry(OclOperation.Args[0]), TranslateEntry(OclOperation.Args[1]));
  end
  else
    raise SpanException.Create('Can''t autotranslate operation:' + OclOperation.OperationName);
end;

function TExpressionsDefinitionParser.TranslateOclVariableReference(
  OclVariableReference: TBoldOclVariableReference): TSpanArray;
var
  VariableName: string;
begin
  VariableName := OclVariableReference.VariableName;
  if BoldNamesEqual(VariableName, 'true')
    or BoldNamesEqual(VariableName, 'false')
    or BoldNamesEqual(VariableName, 'nil')
    or not (OclVariableReference.BoldType is TBoldClassTypeInfo)
  then
    Result := nil
  else
  begin
    Result := fContextualVariableStack.Lookup(VariableName);
    if Result = nil then
    begin
      Result := fGlobalVariableStack.Lookup(VariableName);
      if (Result=nil) and (OclVariableReference.BoldType is TBoldClassTypeInfo) then
      begin
          SetLength(Result, 1);
          Result[0] := TBoldSpan.Create(OclVariableReference.BoldType as TBoldClassTypeInfo);
          fGlobalVariableStack.PushDefinition(VariableName,Result);
      end;
    end
  end;
end;

{ TSpanArrayNamedCollection }

constructor TSpanArrayNamedCollection.Create;
begin
  fStack := TObjectList.Create;
end;

destructor TSpanArrayNamedCollection.Destroy;
begin
  FreeAndNil(fStack);
  inherited;
end;

function TSpanArrayNamedCollection.Lookup(const name: string): TSpanArray;
var
  i: Integer;
  Entry: TSpanArrayNamedCollectionEntry;
begin
  Result := nil;
  for I := fStack.Count - 1 downto 0 do
  begin
    Entry := fStack[i] as TSpanArrayNamedCollectionEntry;
    if BoldNamesEqual(Entry.Name, Name)  then
    begin
      Result := Entry.SpanArray;
      Exit;
    end;
  end;
end;

function TSpanArrayNamedCollection.PopDefinition: TSpanArray;
begin
  if fStack.Count = 0 then
    Result := nil
  else begin
    Result := (fStack[fStack.Count-1]as TSpanArrayNamedCollectionEntry).SpanArray;
    fStack.Delete(fStack.Count-1);
  end
end;

procedure TSpanArrayNamedCollection.PushDefinition(const name: string;
  Spans: TSpanArray);
begin
  fStack.Add(TSpanArrayNamedCollectionEntry.Create(Name, Spans));
end;

{ TBoldClassFilterNode }

constructor TBoldClassFilterNode.Create(FilterClass: TBoldClassTypeInfo);
begin
  inherited Create(FilterClass);
end;

function TBoldClassFilterNode.GetAsString: string;
var
  SnString: string;
begin
  SnString := SubNodesAsString;
  Result := '?' + UmlCLass.ExpressionName;
  if SnString <> '' then
    Result := Result + ', ' + SnString;
end;

function TBoldClassFilterNode.GetIsDerived: Boolean;
begin
  Result := false;
end;

function TBoldClassFilterNode.GetPath: string;
begin
  Result := ParentNode.GetPath + '?' + UmlCLass.ExpressionName;
end;

function TBoldClassFilterNode.ShallowClone: TBoldSpanNode;
var
  NewFilter: TBoldClassFilterNode;
begin
  NewFilter := TBoldClassFilterNode.Create(UmlClass);
//  NewFilter.FetchDefaultMembers := FetchDefaultMembers;
  Result := NewFilter;
end;

{ TBoldAttributeFilterNode }

constructor TBoldAttributeFilterNode.Create(UmlClass: TBoldClassTypeInfo;
  Attribute: TBoldAttributeRTInfo);
begin
  inherited Create(UmlClass);
  fAttribute := Attribute;
end;

function TBoldAttributeFilterNode.GetAsString: string;
var
  SnString: string;
begin
  SnString := SubNodesAsString;
  Result := fAttribute.ExpressionName + '=true';
  if SnString <> '' then
    Result := Result + ', ' + SnString;
end;

function TBoldAttributeFilterNode.GetIsDerived: Boolean;
begin
  Result := false;
end;

function TBoldAttributeFilterNode.GetPath: string;
begin
  Result := ParentNode.GetPath + fAttribute.ExpressionName + '=true';
end;

function TBoldAttributeFilterNode.FindMatchByMember(AMember: TBoldMemberRTInfo; AObject: TBoldObject): TBoldElement;
begin
  result := nil;
  if fAttribute = AMember then
  begin
    result := nil;
  end;
end;

function TBoldAttributeFilterNode.ShallowClone: TBoldSpanNode;
var
  NewFilter: TBoldAttributeFilterNode;
begin
  NewFilter := TBoldAttributeFilterNode.Create(UmlClass, fAttribute);
//  NewFilter.FetchDefaultMembers := FetchDefaultMembers;
  Result := NewFilter;
end;

{ TBoldAsStringNode }

constructor TBoldAsStringNode.Create(UmlClass: TBoldClassTypeInfo);
begin
  inherited Create(UmlClass);
end;

function TBoldAsStringNode.GetAlwaysCall: Boolean;
begin
  Result := false;
end;

function TBoldAsStringNode.GetAsString: string;
begin
  Result := 'asString';
end;

function TBoldAsStringNode.GetIsDerived: Boolean;
begin
  Result := true;
end;

function TBoldAsStringNode.GetPath: string;
begin
  Result := ParentNode.GetPath + '.asString';
end;

function TBoldAsStringNode.ShallowClone: TBoldSpanNode;
begin
  Result := TBoldAsStringNode.Create(UmlClass);
end;

{ TBoldAllInstancesNode }

constructor TBoldAllInstancesNode.Create(AClass: TBoldClassTypeInfo;
  LoadedOnly: Boolean);
begin
  inherited Create(aClass);
  fLoadedOnly := LoadedOnly
end;

function TBoldAllInstancesNode.GetAlwaysCall: Boolean;
begin
  Result := true;
end;

function TBoldAllInstancesNode.GetAsString: string;
begin
  if fLoadedOnly then
    Result := UmlClass.ExpressionName + '.AllLoadedObjects'
  else
    Result := UmlClass.ExpressionName + '.AllInstances';
end;

function TBoldAllInstancesNode.GetIsDerived: Boolean;
begin
  Result := false;
end;

function TBoldAllInstancesNode.GetPath: string;
begin
  result := ParentNode.GetPath + '|' + AsString;
end;

function TBoldAllInstancesNode.ShallowClone: TBoldSpanNode;
var
  NewNode: TBoldAllInstancesNode;
begin
  NewNode := TBoldAllInstancesNode.Create(UmlClass, fLoadedOnly);
//  NewNode.FetchDefaultMembers := FetchDefaultMembers;
  Result := NewNode;
end;

end.
