{ Global compiler directives }
{$include bold.inc}
unit BoldElements;

interface

uses
  BoldContainers,
  BoldBase,
  BoldDefs,
  BoldSubscription;

type
  TBoldValueTypeSet = (bvtList, bvtClass, bvtAttr, bvtSystem, bvtType);
  TBoldValueTypes = set of TBoldValueTypeSet;


const
  BoldPSShift = 24;
  BoldPersistenceStateMask = 3 shl BoldPSShift;  // 00000011 - used in TBoldObject and TBoldMember
  BoldESShift = 26;
  BoldExistenceStateMask   = 3 shl BoldESShift;  // 00001100 - used in TBoldObject
  BoldDMShift = 26;
  BoldDuplicateModeMask    = 3 shl BoldDMShift;  // 00001100 - used in TBoldObejctList
  BoldDSShift = 28;
  BoldDerivationStateMask  = 7 shl BoldDSShift;  // 01110000 - used in TBoldMember

  BoldDefaultRegionModeShift = 24;
  BoldDefaultRegionModeMask  = 7 shl BoldDefaultRegionModeShift;  // 00000111 - used in TBoldRoleRTInfo

  {flags for BoldElement}
  befImmutable                      = BoldElementFlag0;
  befHasModifiedValueHolder         = BoldElementFlag1;

  {flags for DomainElement}
  befPersistent                     = BoldElementFlag2;
  befTouched                        = BoldElementFlag3;

  {flags for BoldSystem}
  befRollbackAreaAssigned           = BoldElementFlag4;
  befIsDestroying                   = BoldElementFlag5;
  befIsCommitting                   = BoldElementFlag6;
  befIsRollingBack                  = BoldElementFlag7;
  befIsUpdatingDatabase             = BoldElementFlag8;
  befDirtyObjectsInvalid            = BoldElementFlag9;

  {flags for BoldObject}
  befInDirtyList                    = BoldElementFlag4;
  befMemberModified                 = BoldElementFlag5;
  befMemberModifiedKnown            = BoldElementFlag6;
  befObjectReadOnly                 = BoldElementFlag7;
  befObjectWasCreatedNew            = BoldElementFlag8;
  befIsHistoricVersion              = BoldElementFlag10;
  befIsEffectiveInvalid             = BoldElementFlag11;
  befIsEffectiveInvalidKnown        = BoldElementFlag12;
  befInDelayDestructionList         = BoldElementFlag13;
  befDiscarding                     = BoldElementFlag14;
  befDeleting                       = BoldElementFlag15;
  befCreating                       = BoldElementFlag16;

  {flags for BoldMember}
  befIsNull                         = BoldElementFlag4;
  befDerived                        = BoldElementFlag5;
  befHasDeriver                     = BoldElementFlag6;
  befMemberReadOnly                 = BoldElementFlag7;
  befHasRtInfo                      = BoldElementFlag8;
  befEnsuringCurrent                = BoldElementFlag9;
  befOwnedByObject                  = BoldElementFlag10;
  befPreFetched                     = BoldElementFlag11;
  befIsInitializing                 = BoldElementFlag12;

  {flags for BoldObjectList}
  befAdjusted                       = BoldElementFlag13;
  befSubscribeToObjectsInList       = BoldElementFlag14;
  befSubscribeToLocatorsInList      = BoldElementFlag15;

  {flags for BoldObjectReference}
  befHasOldValues                   = BoldElementFlag13;

  {flags for BoldElementTypeInfo}
  BoldValueTypeShift = 24;
  BoldValueTypeMask = 7 shl BoldPSShift; // 00000111

  {flags for IndirectElement}
  befOwnsValue = BoldElementFlag0;

  {flags for TBoldOCLNode}
  befNeedsListCoercion = BoldElementFlag1;
  befIsConstant = BoldElementFlag2;
  befResubscribe = BoldElementFlag3;
  befHastemporaryDummyValue = BoldElementFlag4;

  {Flags for BoldSystemTypeInfo}
  befSystemPersistent = BoldElementFlag2;
  befSystemIsRunnable = BoldElementFlag3;
  befEnforceOptimisticLocking = BoldElementFlag4;
  befUpdateWholeObjects = BoldElementFlag5;
  befGenerateDefaultRegions = BoldElementFlag6;

  {Flags for BoldClassTypeInfo}
  befClassPersistent = BoldElementFlag2;
  befHasSubclasses = BoldElementFlag3;
  befIsAbstract = BoldElementFlag4;
  beFIsImported = BoldElementFlag5;
  befIsLinkClass = BoldElementFlag6;
  befClassToBeRemoved = BoldElementFlag7;
  befVersioned = BoldElementFlag8;
  befGenerateDefaultRegion = BoldElementFlag9;

  {Flags for BoldMemberRTInfo}
  befMemberPersistent = BoldElementFlag2;
  befDelayedFetch = BoldElementFlag3;
  befIsStoredInObject = BoldElementFlag4;
  befIsAttribute = BoldElementFlag5;
  befIsMultiRole = BoldElementFlag6;
  befIsSingleRole = BoldElementFlag7;
  befIsDerived = BoldElementFlag8;
  befIsReverseDerived = BoldElementFlag9;
  befIsNonVersionedInVersionedClass = BoldElementFlag10;
  befMemberToBeRemoved = BoldElementFlag11;

  {Flags for BoldRoleRTInfo}
  befIsIndirect = BoldElementFlag12;
  befIsNavigable = BoldElementFlag13;
  befIsOrdered = BoldElementFlag14;
  befOtherEndOrdered = BoldElementFlag15;
  befMandatory = BoldElementFlag16;
  befForceOtherEnd = BoldElementFlag17;
  befQualifiedMulti = BoldElementFlag18;

  {Flags for BoldAttributeRTInfo}
  befHasInitalvalue = BoldElementFlag12;
  befAllowNull = BoldElementFlag13;

type
  {forward declarations of all classes}
  TBoldEvaluator = class;
  TBoldElement = class;
  TBoldMetaElement = class;
  TBoldElementTypeInfo = class;
  TBoldListTypeInfo = class;
  TBoldIndirectElement = class;
  TBoldDirectElement = TBoldElement;
  TBoldExternalVariable = class;
  TBoldExternalVariableList = class;

  TBoldElementClass = class of TBoldElement;
  TBoldElementSubscribe = procedure (Element: TBoldElement; Subscriber: TBoldSubscriber) of object;
  TBoldLookUpOclDefinition = function(Name: string): string of object;

  {---IBoldOCLComponent---}
  IBoldOCLComponent = interface
  ['{60D40422-8710-11D3-A2C8-EA14D4000000}']
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetVariableList: TBoldExternalVariableList;
    function GetExpression: TBoldExpression;
    property ContextType: TBoldElementTypeInfo read GetContextType;
    property Expression: TBoldExpression read GetExpression write SetExpression;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  end;

  { TBoldExternalVariable }
  TBoldExternalVariable = class(TBoldMemoryManagedObject)
  private
    fName: String;
    fEvaluator: TBoldEvaluator;
    fSubscriber: TBoldPassthroughSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetSubscriber: TBoldPassthroughSubscriber;
    procedure SetEvaluator(const Value: TBoldEvaluator);
    property Subscriber: TBoldPassthroughSubscriber read GetSubscriber;
  protected
    function GetValue: TBoldElement; virtual; abstract;
    function GetValueType: TBoldElementTypeInfo; virtual; abstract;
  public
    constructor Create(AEvaluator: TBoldEvaluator; const AName: String);
    destructor Destroy; override;
    property Value: TBoldElement read GetValue;
    property Name: String read fName;
    property ValueType: TBoldElementTypeInfo read GetValueType;
    property Evaluator: TBoldEvaluator read fEvaluator write SetEvaluator;
  end;

  TBoldExternalVariableListTraverser = class(TBoldArrayTraverser)
  public
    function GetCurrent: TBoldExternalVariable;
    property Current: TBoldExternalVariable read GetCurrent;
  end;

  { TBoldExternalVariableList }
  TBoldExternalVariableList = class(TBoldObjectArray)
  private
    function GetVariables(index: integer): TBoldExternalVariable;
    function GetVariableByName(const aName: string): TBoldExternalVariable;
    function GetAsCommaText: string;
  public
    constructor Create; overload;
    constructor Create(aOwnsVariables: boolean); overload;
    class function CreateWithStringVariable(AName: string; AValue: string; AEvaluator: TBoldEvaluator = nil): TBoldExternalVariableList;
    class function CreateWithElementVariable(AName: string; AValue: TBoldElement): TBoldExternalVariableList;
    function GetEnumerator: TBoldExternalVariableListTraverser;
    procedure Add(Variable: TBoldExternalVariable); overload;
    procedure Add(AName: string; AValue: TBoldElement); overload;
    procedure Add(AName: string; AValue: string; AEvaluator: TBoldEvaluator = nil); overload;
    procedure AddInteger(AName: string; AValue: integer; AEvaluator: TBoldEvaluator = nil);
    procedure AddFloat(AName: string; AValue: Double; AEvaluator: TBoldEvaluator = nil);
    procedure AddDateTime(AName: string; AValue: TDateTime; AEvaluator: TBoldEvaluator = nil);
    procedure AddDate(AName: string; AValue: TDate; AEvaluator: TBoldEvaluator = nil);
    procedure AddTime(AName: string; AValue: TTime; AEvaluator: TBoldEvaluator = nil);
    property Variables[index: integer]: TBoldExternalVariable read GetVariables; default;
    property VariableByName[const aName: string]: TBoldExternalVariable read GetVariableByName;
    property AsCommaText: string read GetAsCommaText;
  end;

  {---TBoldEvaluator---}
  TBoldEvaluator = class(TBoldSubscribableObject)
  protected
    function GetVariableCount: integer; virtual; abstract;
    function GetVariable(index: integer): TBoldIndirectElement; virtual; abstract;
    function GetVariableByName(const aName: string): TBoldIndirectElement; virtual; abstract;
  public
    procedure DefineVariable(const VariableName: string; VarValue: TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue, IsConstant: Boolean); overload; virtual; abstract;
    procedure DefineVariable(const VariableName: string; Variable: TBoldExternalVariable ); overload; virtual; abstract;
    procedure UndefineVariable(Variable: TBoldExternalVariable); virtual; abstract;
    procedure Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber = nil; ResubscribeAll: Boolean = false; resultElement: TBoldIndirectElement = nil; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil; MaxAnswers: integer = -1; Offset: integer = -1); virtual; abstract;
    function ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo; virtual; abstract;
    procedure SetLookupOclDefinition(value: TBoldLookUpOclDefinition); virtual; abstract;
    property Variables[index: integer]: TBoldIndirectElement read GetVariable;
    property VariableByName[const aName: string]: TBoldIndirectElement read GetVariableByName;
    property VariableCount: integer read GetVariableCount;
  end;

  {---TBoldElement---}
  TBoldElement = class(TBoldSubscribableObject)
  strict private
    function GetModifiedValueHolder: TObject;
    procedure SetModifiedValueHolder(Value: TObject);
    function GetMutable: Boolean;
  protected
    function GetDisplayName: String; virtual;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; virtual;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); virtual;
    procedure CompareError(BoldElement: TBoldElement);
    procedure AssignError(BoldElement: TBoldElement);
    procedure MutableError(const NewValue: string);
    procedure CompareTypeError(CompType: TBoldCompareType; BoldElement: TBoldElement);
    function GetEvaluator: TBoldEvaluator; virtual;
    function GetBoldType: TBoldElementTypeInfo; virtual; abstract;
    function CloneIfPossible: TBoldElement; virtual;
    function GetContextString: string; override;
    function GetIsPartOfSystem: Boolean; virtual;
  public
    constructor CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo); virtual;
    destructor Destroy; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); virtual; abstract;
    procedure PrepareToDestroy;
    function GetAsVariant: Variant; virtual;
    procedure SetAsVariant(const Value: Variant); virtual;
    function IsEqual(BoldElement: TBoldElement): Boolean;
    function CompareTo(BoldElement: TBoldElement): Integer;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; virtual;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; virtual;
    procedure Assign(Source: TBoldElement); virtual;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; virtual;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; virtual;
    function ValidateVariant(const Value: Variant; Representation: TBoldRepresentation = brDefault): Boolean; virtual;
    procedure EnsureValidString(const Value: string; Representation: TBoldRepresentation);
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); virtual;
    function ObserverMayModify(Observer: TObject): Boolean; virtual;
    function ObserverMayModifyAsString(Representation: TBoldRepresentation; observer: TBoldSubscriber): Boolean; virtual;
    procedure RegisterModifiedValueHolder(observer: TObject);
    procedure UnRegisterModifiedValueHolder(observer: TObject);
    procedure MakeImmutable;
    procedure GetAsList(ResultList: TBoldIndirectElement); virtual; abstract;
    procedure GetAsValue(resultElement: TBoldIndirectElement); virtual;
    procedure SubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
    procedure EvaluateExpression(const Expression: TBoldExpression; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil; MaxAnswers: integer = -1; Offset: integer = -1);
    function EvaluateExpressionAsDirectElement(const Expression: TBoldExpression; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
    procedure EvaluateAndSubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; resultElement: TBoldIndirectElement; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil; MaxAnswers: integer = -1; Offset: integer = -1);
    function EvaluateExpressionAsString(const Expression: TBoldExpression; Representation: TBoldRepresentation = brDefault; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): string;
    function EvaluateExpressionAsBoolean(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): Boolean;
    function EvaluateExpressionAsInteger(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): Integer;
    function EvaluateExpressionAsFloat(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): Double;
    function EvaluateExpressionAsCurrency(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): Currency;
    function EvaluateExpressionAsDateTime(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): TDateTime;
    function EvaluateExpressionAsNewElement(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
    property Evaluator: TBoldEvaluator read GetEvaluator;
    property BoldType: TBoldElementTypeInfo read GetBoldType;
    property StringRepresentation[Representation: TBoldRepresentation]: string read GetStringRepresentation write SetStringRepresentation;
    property AsString: string index brDefault read GetStringRepresentation write SetStringRepresentation;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property ModifiedValueHolder: TObject read GetModifiedValueHolder;
    property Mutable: Boolean read GetMutable;
    property DisplayName: String read GetDisplayName;
    property IsPartOfSystem: Boolean read GetIsPartOfSystem;
  end;

  {---TBoldMetaElement---}
  TBoldMetaElement = class(TBoldElement)
  private
    fModelName: string;
    fDelphiName: string;
    fExpressionName: string;
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetDisplayName: String; override;
  public
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    constructor Create(const ModelName: string; const ExpressionName: string; const DelphiName: string);
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    property ExpressionName: string read fExpressionName;
    property DelphiName: string read fDelphiName;
    property ModelName: string read fModelName;
  end;

  {---TBoldElementTypeInfo---}
  TBoldElementTypeInfo = class(TBoldMetaElement)
  private
    fSystemTypeInfo: TBoldElementTypeInfo;
    function GetValueType: TBoldValueTypeSet;
  protected
    function GetEvaluator: TBoldEvaluator; override;
    procedure SetValueType(NewValue: TBoldValueTypeSet);
    function GetListTypeInfo: TBoldListTypeInfo; virtual;
  public
    constructor Create(const ModelName: string; const ExpressionName: string; const DelphiName: string; SystemTypeInfo: TBoldElementTypeInfo);
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function ConformsTo(Element: TBoldElementTypeInfo): Boolean; virtual; abstract;
    function ElementClass: TBoldElementClass; virtual;
    function CreateElement: TBoldElement; virtual;
    property SystemTypeInfo: TBoldElementTypeInfo read fSystemTypeInfo;
    property BoldValueType: TBoldValueTypeSet read GetValueType;
    property ListTypeInfo: TBoldListTypeInfo read GetListTypeInfo;
  end;

  {---TBoldListTypeInfo---}
  TBoldListTypeInfo = class(TBoldElementTypeInfo)
  private
    fListClass: TClass;
    fListElementTypeInfo: TBoldElementTypeInfo;
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  public
    constructor Create(ListElementTypeInfo: TBoldElementTypeInfo; SystemTypeInfo: TBoldElementTypeInfo; ListClass: TClass);
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
    function CreateElement: TBoldElement; override;
    property ListClass: TClass read fListClass;
    property ListElementTypeInfo: TBoldElementTypeInfo read fListElementTypeInfo;
  end;

  {---TBoldIndirectElement---}
  TBoldIndirectElement = class(TBoldFlaggedObject)
  private
    fValue: TBoldElement;
    property WriteableOwnsValue: Boolean index befOwnsValue read GetElementFlag write SetElementFlag;
  protected
    function ContextObject: TObject; override;
  public
    destructor Destroy; override;
    procedure SetReferenceValue(NewValue: TBoldElement);
    procedure SetOwnedValue(NewValue: TBoldElement);
    procedure TransferValue(Target: TBoldIndirectElement);
    function RelinquishValue: TBoldElement;
    property OwnsValue: Boolean index befOwnsValue read GetElementFlag;
    property Value: TBoldElement read fValue;
  end;

  {---TBoldSubscribableComponentViaBoldElem---}
  TBoldSubscribableComponentViaBoldElem = class(TBoldSubscribableComponent)
  end;

implementation

uses
  BoldCoreConsts,
  BoldExternalizedReferences,
  BoldOclError,
  BoldTypeList, // circular reference BoldElements->BoldTypelist->BoldSystem->BoldElements
  BoldAttributes,
  BoldOclVariables,
  BoldUtils,
  SysUtils,
  Variants,
  Windows,
  Typinfo,
  Classes;

const
  sOCLResultError = 'Expression: ''%s '' returned incorrect result. Expected: %s. Currently: %s.';
  beModifiedValueHolderDestroying = 303;
var
  G_ExternalModifiedValueHolders: TBoldExternalizedReferenceList;

function TBoldElement.EvaluateExpressionAsString(const Expression: TBoldExpression; Representation: TBoldRepresentation = brDefault; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): string;
var
  E: TBoldIndirectElement;
begin
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      Result := E.Value.StringRepresentation[Representation]
    else
      Result := '';
  finally
    E.Free;
  end;
end;

function TBoldElement.GetMutable: Boolean;
begin
  result := not GetElementFlag(befImmutable);
end;

procedure TBoldElement.SubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
begin
  EvaluateAndSubscribeToExpression(Expression, Subscriber, nil, Resubscribe, EvaluateInPS, VariableList);
end;

procedure TBoldElement.EvaluateExpression(const Expression: TBoldExpression; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil; MaxAnswers: integer = -1; Offset: integer = -1);
begin
  EvaluateAndSubscribeToExpression(Expression, nil, resultElement, False, EvaluateInPS, VariableList, MaxAnswers, Offset);
end;

function TBoldElement.EvaluateExpressionAsBoolean(
  const Expression: TBoldExpression; EvaluateInPS: Boolean;
  const VariableList: TBoldExternalVariableList): Boolean;
var
  E: TBoldIndirectElement;
begin
  Result := False;
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      if E.Value is TBABoolean then
        Result := TBABoolean(E.Value).AsBoolean
      else
        raise EBold.CreateFmt(sOCLResultError, [Expression,
            TBABoolean.ClassName, E.Value.ClassName])
  finally
    E.Free;
  end;
end;

function TBoldElement.EvaluateExpressionAsCurrency(
  const Expression: TBoldExpression; EvaluateInPS: Boolean;
  const VariableList: TBoldExternalVariableList): Currency;
var
  E: TBoldIndirectElement;
begin
  Result := 0;
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      if E.Value is TBACurrency then
        Result := TBACurrency(E.Value).AsCurrency
      else
        raise EBold.CreateFmt(sOCLResultError, [Expression,
            TBACurrency.ClassName, E.Value.ClassName])
  finally
    E.Free;
  end;
end;

function TBoldElement.EvaluateExpressionAsDateTime(
  const Expression: TBoldExpression; EvaluateInPS: Boolean;
  const VariableList: TBoldExternalVariableList): TDateTime;
var
  E: TBoldIndirectElement;
begin
  Result := 0;
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      if E.Value is TBADateTime then
        Result := TBADateTime(E.Value).AsDateTime
      else
        raise EBold.CreateFmt(sOCLResultError, [Expression,
            TBADateTime.ClassName, E.Value.ClassName])
  finally
    E.Free;
  end;
end;

function TBoldElement.EvaluateExpressionAsDirectElement(const Expression: TBoldExpression; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
var
  E: TBoldIndirectElement;
begin
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, false, VariableList);
    if E.OwnsValue then
      Result := nil
    else
      Result := E.Value;
  finally
    E.Free;
  end;
end;

function TBoldElement.EvaluateExpressionAsFloat(
  const Expression: TBoldExpression;
  EvaluateInPS: Boolean; const VariableList: TBoldExternalVariableList): Double;
var
  E: TBoldIndirectElement;
begin
  Result := 0;
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      if E.Value is TBAFloat then
        Result := TBAFloat(E.Value).AsFloat
      else
        raise EBold.CreateFmt(sOCLResultError, [Expression,
            TBAFloat.ClassName, E.Value.ClassName])
  finally
    E.Free;
  end;
end;

function TBoldElement.EvaluateExpressionAsInteger(
  const Expression: TBoldExpression;
  EvaluateInPS: Boolean;
  const VariableList: TBoldExternalVariableList): Integer;
var
  E: TBoldIndirectElement;
begin
  Result := 0;
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
      if E.Value is TBAInteger then
        Result := TBAInteger(E.Value).AsInteger
      else
        raise EBold.CreateFmt(sOCLResultError, [Expression,
            TBAInteger.ClassName, E.Value.ClassName])
  finally
    E.Free;
  end;
end;

{---TBoldElement---}

function TBoldElement.GetEvaluator: TBoldEvaluator;
begin
  if assigned(BoldType) then
    Result := BoldType.Evaluator
  else
    raise EBold.CreateFmt(sCannotGetEvaluatorWithoutType, [classname]);
end;

destructor TBoldElement.Destroy;
begin
  PrepareToDestroy;
  inherited Destroy;
end;

procedure TBoldElement.PrepareToDestroy;
begin
  FreePublisher;
  SetElementFlag(befImmutable, false);
end;

function TBoldElement.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  raise EBold.CreateFmt('%s.GetStringRepresentation: Representation %d not supported',
    [ClassName, Representation]);
end;

procedure TBoldElement.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  raise EBold.CreateFmt('%s.SetStringRepresentation: Representation %d not supported',
    [ClassName, Representation]);
end;

procedure TBoldElement.SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
end;

function TBoldElement.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := True;
end;

function TBoldElement.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(Value) do
    if not ValidateCharacter(Value[I], Representation) then
    begin
      Result := False;
      Break;
    end;
end;

function TBoldElement.ValidateVariant(const Value: Variant;
  Representation: TBoldRepresentation): Boolean;
begin
  result := ValidateString(VarToStr(Value), Representation);
end;

function TBoldElement.ObserverMayModify(Observer: TObject): Boolean;
begin
  Result := Mutable and
            ((ModifiedValueHolder = nil) or
             (ModifiedValueHolder = Observer));
end;

function TBoldElement.ObserverMayModifyAsString(Representation: TBoldRepresentation; Observer: TBoldSubscriber): Boolean;
begin
  Result := ObserverMayModify(Observer);
end;

procedure TBoldElement.RegisterModifiedValueHolder(observer: TObject);

  procedure InternalRaise(Self: TBoldElement; Value: TObject);
  var
    LockedBy: string;
  begin
    if Value is TBoldMemoryManagedObject then
      LockedBy := TBoldMemoryManagedObject(Value).DebugInfo
    else
    if Value is TComponent then
    begin
      LockedBy := TComponent(Value).Name;
      if LockedBy = '' then
        LockedBy := Value.ClassName;
    end
    else
      LockedBy := Value.ClassName;

    raise EBold.CreateFmt(sMemberAlreadyModified, [self.DisplayName, LockedBy])
  end;

var
  Value: TObject;
begin
  if GetElementFlag(befHasModifiedValueHolder) then
  begin
    Value := ModifiedValueHolder;
    if Assigned(Value) and (Value <> observer) then
      InternalRaise(Self, Observer);
  end
  else
    SetModifiedValueHolder(observer);
end;

procedure TBoldElement.UnRegisterModifiedValueHolder(observer: TObject);
begin
  SetModifiedValueHolder(nil);
end;

function TBoldElement.IsEqual(BoldElement: TBoldElement): Boolean;
begin
  Result := (self = BoldElement) or IsEqualAs(ctDefault, BoldElement);
end;

function TBoldElement.IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean;
begin
  Result := (self = BoldElement) or (CompareToAs(CompareType, BoldElement) = 0);
end;

function TBoldElement.CompareTo(BoldElement: TBoldElement): Integer;
begin
  if (self = BoldElement) then
    result := 0
  else
    result := CompareToAs(ctDefault, BoldElement);
end;

function TBoldElement.CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [ClassName, 'CompareToAs']); // do not localize
end;

procedure TBoldElement.Assign(Source: TBoldElement);
begin
  if assigned(Source) then
    raise EBold.CreateFmt(sAssignNotSupported, [ClassName, Source.ClassName])
  else
    raise EBold.CreateFmt(sAssignNilNotSupported, [ClassName])
end;

procedure TBoldElement.CompareError(BoldElement: TBoldElement);
begin
  if Assigned(BoldElement) then
    raise EBold.CreateFmt(sCompareNotSupported, [ClassName, BoldElement.ClassName])
  else
    raise EBold.CreateFmt(sCompareNilNotSupported, [ClassName]);
end;

procedure TBoldElement.AssignError(BoldElement: TBoldElement);
begin
  if Assigned(BoldElement) then
    raise EBold.CreateFmt(sAssignNotSupported, [ClassName, BoldElement.ClassName])
  else
    raise EBold.CreateFmt(sAssignNilNotSupported, [ClassName]);
end;

procedure TBoldElement.CompareTypeError(CompType: TBoldCompareType; BoldElement: TBoldElement);
var
  ElementName: string;
begin
  if Assigned(BoldElement)
  then
    ElementName := BoldElement.ClassName
  else
    ElementName := 'nil';

  raise EBold.CreateFmt(sInvalidCompareType,  [ClassName,
                        GetEnumName(TypeInfo(TBoldCompareType), Ord(CompType)), ElementName]);
end;

constructor TBoldElement.CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
begin

end;

procedure TBoldElement.MakeImmutable;
begin
  SetElementFlag(befImmutable, true);
end;

procedure TBoldElement.MutableError(const NewValue: string);
begin
  raise EBold.CreateFmt(sTriedToChangeImmutableElement, [ClassName, AsString, NewValue]);
end;

procedure TBoldElement.GetAsValue(resultElement: TBoldIndirectElement);
begin
  resultElement.SetReferenceValue(Self);
end;

procedure TBoldElement.EnsureValidString(const Value: string;
  Representation: TBoldRepresentation);
begin
  if not ValidateString(Value, Representation) then
    raise EBoldAssertionFailed.CreateFmt(ssStringValidationFailedWithClass, [Classname]);
end;

function TBoldElement.GetAsVariant: Variant;
begin
  Result := AsString;
end;

function TBoldElement.GetContextString: string;
begin
  result := DisplayName;
end;

function TBoldElement.GetDisplayName: String;
begin
  if Assigned(BoldType) then
    result := BoldType.AsString
  else
    result := ClassName;
end;

procedure TBoldElement.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

function TBoldElement.GetIsPartOfSystem: Boolean;
begin
  result := true; // always true for System and Objects, and overriden for Member
end;

{---TBoldMetaElement---}
procedure TBoldMetaElement.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  if Mutable then
    raise EBold.CreateFmt(sCannotSubscribeToMutableMetaElements, [ClassName]);
end;

constructor TBoldMetaElement.Create(const ModelName: string; const ExpressionName: string; const DelphiName: string);
begin
  inherited Create;
  fModelName := ModelName;
  fExpressionName := ExpressionName;
  fDelphiName := DelphiName;
  MakeImmutable;
end;

function TBoldMetaElement.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  Result := ExpressionName;
end;

procedure TBoldMetaElement.GetAsList(ResultList: TBoldIndirectElement);
begin
  raise EBoldFeatureNotImplementedYet.Create(sMetaElementGetAsListNotImplemented);
end;

function TBoldMetaElement.GetDisplayName: String;
begin
  result := ExpressionName;
end;

{---TBoldElementTypeInfo---}
constructor TBoldElementTypeInfo.Create(const ModelName: string; const ExpressionName: string; const DelphiName: string; SystemTypeInfo: TBoldElementTypeInfo);
begin
  inherited Create(ModelName, ExpressionName, DelphiName);
  fSystemTypeInfo := SystemTypeInfo;
end;

function TBoldElementTypeInfo.ElementClass: TBoldElementClass;
begin
  result := nil;
end;

function TBoldElementTypeInfo.CreateElement: TBoldElement;
const
  sInvalidBoldType = '%s.CreateElement: Invalid BoldType (%s)';
begin
  raise EBold.CreateFmt(sInvalidBoldType, [ClassName, asString])
end;

procedure TBoldElementTypeInfo.GetAsList(ResultList: TBoldIndirectElement);
var
  list: TBoldTypeList;
begin
  list := TBoldTypeListFactory.CreateList(self);
  list.Add(self);
  ResultList.SetOwnedValue(list);
end;

function TBoldElementTypeInfo.GetEvaluator: TBoldEvaluator;
begin
  Result := SystemTypeInfo.GetEvaluator;
end;

function TBoldElementTypeInfo.GetListTypeInfo: TBoldListTypeInfo;
begin
  raise EBold.CreateFmt('%s.GetListTypeInfo is not overriden, implement it.', [classname]);
end;

{---TBoldIndirectElement---}

destructor TBoldIndirectElement.Destroy;
begin
  if OwnsValue and not fValue.IsPartOfSystem then
    FreeAndNil(fValue)
  else
    fValue := nil;
  inherited Destroy;
end;

procedure TBoldIndirectElement.SetReferenceValue(NewValue: TBoldElement);
var
  toFree: TBoldElement;
begin
  if NewValue <> fValue then
  begin
    if OwnsValue then
      toFree := fValue
    else
      toFree := nil;
    fValue := NewValue;
    toFree.Free;
  end
  else if OwnsValue then
    raise EBold.CreateFmt(sNewValueAlreadyOwned, [ClassName]);

  WriteableOwnsValue := False;
end;

procedure TBoldIndirectElement.SetOwnedValue(NewValue: TBoldElement);
var
  toFree: TBoldElement;
begin
  if NewValue <> fValue then
  begin
    if OwnsValue then
      toFree := fValue
    else
      toFree := nil;
    fValue := NewValue;
    toFree.Free;
  end;
  WriteableOwnsValue := Assigned(newValue);
end;

procedure TBoldIndirectElement.TransferValue(Target: TBoldIndirectElement);
begin
  if Assigned(Target) then
  begin
    if OwnsValue then
      Target.SetOwnedValue(Value)
    else
      Target.SetReferenceValue(Value);
  end;
  fValue := nil;
  WriteableOwnsValue := False;
end;

function TBoldIndirectElement.RelinquishValue: TBoldElement;
begin
  Result := Value;
  fValue := nil;
  WriteableOwnsValue := False;
end;

function TBoldIndirectElement.ContextObject: TObject;
begin
  result := fValue;
end;

function TBoldElement.GetModifiedValueHolder: TObject;
begin
  if GetElementFlag(befHasModifiedValueHolder) then
    result := G_ExternalModifiedValueHolders.ReferencedObjects[self]
  else
    result := nil;
end;

procedure TBoldElement.SetModifiedValueHolder(Value: TObject);
begin
  if GetElementFlag(befHasModifiedValueHolder) then
    G_ExternalModifiedValueHolders.ReferencedObjects[self] := nil;
  if assigned(value) then
    G_ExternalModifiedValueHolders.ReferencedObjects[self] := value ;

  SetElementFlag(befHasModifiedValueHolder, assigned(Value));
end;

procedure TBoldElement.EvaluateAndSubscribeToExpression(
  const Expression: TBoldExpression; Subscriber: TBoldSubscriber;
  resultElement: TBoldIndirectElement; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil;
  MaxAnswers: integer = -1; Offset: integer = -1);
begin
  try
    Evaluator.Evaluate(Expression, Self, Subscriber, Resubscribe, resultElement, EvaluateInPS, VariableList, MaxAnswers, Offset);
    if not evaluateInPS and Assigned(resultElement) and Assigned(resultElement.Value) and resultElement.OwnsValue then
      resultElement.Value.MakeImmutable;
    if Assigned(Subscriber) then
      AddSmallSubscription(Subscriber, [beDestroying], breReSubscribe);
  except
    on E: EBoldOCLAbort do
      raise EBold.CreateFmt(sOCLExpressionError, [E.Ocl, BOLDCRLF, E.Message]);
    on E: EBoldOCLError do
      raise EBold.CreateFmt(sOCLExpressionError, [E.Ocl, BOLDCRLF, E.Message]);
  end;
end;

function TBoldElementTypeInfo.GetValueType: TBoldValueTypeSet;
begin
  result := TBoldValueTypeSet(GetInternalState(BoldValueTypeMask, BoldValueTypeShift));
end;

procedure TBoldElementTypeInfo.SetValueType(NewValue: TBoldValueTypeSet);
begin
  SetInternalState(BoldValueTypeMask, BoldValueTypeShift, Integer(NewValue));
end;

{ TBoldExternalVariable }

constructor TBoldExternalVariable.Create(AEvaluator: TBoldEvaluator; const AName: String);
begin
  inherited Create;
  Evaluator := AEvaluator;
  fName := AName;
end;

destructor TBoldExternalVariable.Destroy;
var
  vEvaluator: TBoldEvaluator;
begin
  fSubscriber.Free;
  vEvaluator := fEvaluator;
  if Assigned(vEvaluator) then
  begin
    fEvaluator := nil;
    vEvaluator.UndefineVariable(self);
  end;
  inherited;
end;

function TBoldExternalVariable.GetSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  result := fSubscriber;
end;

procedure TBoldExternalVariable.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  fEvaluator := nil;
end;

procedure TBoldExternalVariable.SetEvaluator(const Value: TBoldEvaluator);
begin
  if fEvaluator <> Value then
  begin
    fEvaluator := Value;
    Subscriber.CancelAllSubscriptions;
    if Assigned(fEvaluator) then
      fEvaluator.AddSmallSubscription(Subscriber, [beDestroying]);
  end;
end;

{ TBoldExternalOclVariableList }

procedure TBoldExternalVariableList.Add(Variable: TBoldExternalVariable);
begin
  inherited Add(Variable);
end;

procedure TBoldExternalVariableList.Add(AName, AValue: string; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateStringVariable(AName, AValue, AEvaluator));
end;

procedure TBoldExternalVariableList.Add(AName: string; AValue: TBoldElement);
begin
  Add(TBoldOclVariable.Create(AName, AValue));
end;

procedure TBoldExternalVariableList.AddDate(AName: string; AValue: TDate; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateDateVariable(AName, AValue, AEvaluator));
end;

procedure TBoldExternalVariableList.AddDateTime(AName: string; AValue: TDateTime; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateDateTimeVariable(AName, AValue, AEvaluator));
end;

procedure TBoldExternalVariableList.AddFloat(AName: string; AValue: Double; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateFloatVariable(AName, AValue, AEvaluator));
end;

procedure TBoldExternalVariableList.AddInteger(AName: string; AValue: integer; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateIntegerVariable(AName, AValue, AEvaluator));
end;

procedure TBoldExternalVariableList.AddTime(AName: string; AValue: TTime; AEvaluator: TBoldEvaluator);
begin
  Add(TBoldOclVariable.CreateTimeVariable(AName, AValue, AEvaluator));
end;

constructor TBoldExternalVariableList.Create(aOwnsVariables: boolean);
begin
  if aOwnsVariables then
    inherited create(4, [bcoDataOwner])
  else
    inherited create(4, []);
end;

constructor TBoldExternalVariableList.Create;
begin
  inherited create(4, [bcoDataOwner]);
end;

class function TBoldExternalVariableList.CreateWithElementVariable(
  AName: string; AValue: TBoldElement): TBoldExternalVariableList;
begin
  result := TBoldExternalVariableList.Create;
  result.Add(TBoldOclVariable.Create(AName, AValue));
end;

class function TBoldExternalVariableList.CreateWithStringVariable(AName,
  AValue: string; AEvaluator: TBoldEvaluator): TBoldExternalVariableList;
begin
  result := TBoldExternalVariableList.Create;
  result.Add(TBoldOclVariable.CreateStringVariable(AName, AValue, AEvaluator));
end;

function TBoldExternalVariableList.GetAsCommaText: string;
var
  i: integer;
  sl: TStringList;
begin
  result := '';
  sl:= TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(Variables[i].Name);
    result := sl.CommaText;
  finally
    sl.free;
  end;
end;

function TBoldExternalVariableList.GetEnumerator: TBoldExternalVariableListTraverser;
begin
  result := TBoldExternalVariableListTraverser.Create(self);
end;

function TBoldExternalVariableList.GetVariableByName(
  const aName: string): TBoldExternalVariable;
var
  i: integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if CompareText(Variables[i].Name, aName) = 0 then
    begin
      result := Variables[i];
      break;
    end;
end;

function TBoldExternalVariableList.GetVariables(index: integer): TBoldExternalVariable;
begin
  result := Items[index] as TBoldExternalVariable;
end;

function TBoldMetaElement.IsEqualAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Boolean;
begin
  result := BoldElement = self;
end;

function TBoldElement.EvaluateExpressionAsNewElement(
  const Expression: TBoldExpression;
  EvaluateInPS: Boolean;
  const VariableList: TBoldExternalVariableList): TBoldElement;
var
  E: TBoldIndirectElement;
begin
  E := TBoldIndirectElement.Create;
  try
    EvaluateExpression(Expression, E, EvaluateInPS, VariableList);
    if Assigned(E.Value) then
    begin
      if E.OwnsValue then
        result := E.RelinquishValue
      else
        Result := E.Value.CloneIfPossible
    end
    else
      Result := nil;
  finally
    E.Free;
  end;
end;

function TBoldElement.CloneIfPossible: TBoldElement;
begin
  Result := nil;
end;

{---TBoldListTypeInfo---}
constructor TBoldListTypeInfo.Create(ListElementTypeInfo: TBoldElementTypeInfo; SystemTypeInfo: TBoldElementTypeInfo; ListClass: TClass);
begin
  if assigned(ListElementTypeInfo) then
    inherited Create(ListElementTypeInfo.ModelName + 'List',
      'Collection(' + ListElementTypeInfo.ExpressionName + ')',
      ListElementTypeInfo.Delphiname + 'List', SystemTypeInfo)
  else
    inherited Create('Collection()', 'Collection()', 'Collection()', SystemTypeInfo);
  fListElementTypeInfo := ListElementTypeInfo;
  SetValueType(bvtList);
  fListClass := ListClass;
end;

function TBoldListTypeInfo.CreateElement: TBoldElement;
begin
  result := TBoldElementClass(ListClass).CreateWithTypeInfo(self);
end;

function TBoldListTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
var
  CompareListTypeInfo: TBoldListTypeInfo;
begin
  if CompareElement is TBoldListTypeInfo then
  begin
    CompareListTypeInfo := TBoldListTypeInfo(CompareElement);
    Result := not assigned(CompareListTypeInfo.ListElementTypeInfo) or
      (assigned(ListElementTypeInfo) and
       ListElementTypeInfo.ConformsTo(CompareListTypeInfo.ListElementTypeInfo));
  end
  else
    Result := False;
end;

function TBoldListTypeInfo.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if assigned(ListElementTypeInfo) then
    Result := 'Collection(' + ListElementTypeInfo.AsString + ')'
  else
    Result := 'Collection()';
end;

function TBoldListTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := SystemTypeInfo.BoldType;
end;

function TBoldListTypeInfo.GetListTypeInfo: TBoldListTypeInfo;
begin
  result := self;
end;

procedure InitDebugMethods;
var
  List: TBoldExternalVariableList;
begin
  // Used to force compiler to link AsCommaText, so it can be used in debugging
  exit;
  List := nil;
  List.AsCommaText;
end;

{ TBoldExternalVariableListTraverser }

function TBoldExternalVariableListTraverser.GetCurrent: TBoldExternalVariable;
begin
  result := ObjectArray[index] as TBoldExternalVariable;
end;

initialization
  G_ExternalModifiedValueHolders := TBoldExternalizedReferenceList.Create;
  InitDebugMethods;

finalization
  G_ExternalModifiedValueHolders.free;

end.
