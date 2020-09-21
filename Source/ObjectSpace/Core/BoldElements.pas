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

  {flags for BusinessElement}
  befPersistent                     = BoldElementFlag2;
  befTouched                        = BoldElementFlag3;

  {flags for BoldObject}
  befInDirtyList                    = BoldElementFlag4;
  befMemberModified                 = BoldElementFlag5;
  befMemberModifiedKnown            = BoldElementFlag6;
  befObjectReadOnly                 = BoldElementFlag7;
  befObjectWasCreatedNew            = BoldElementFlag8;
  befStoresTimeStamp                = BoldElementFlag9;

  {flags for BoldMember}
  befIsNull                         = BoldElementFlag4;
  befDerived                        = BoldElementFlag5;
  befHasDeriver                     = BoldElementFlag6;
  befMemberReadOnly                 = BoldElementFlag7;
  befHasRtInfo                      = BoldElementFlag8;
  befEnsuringCurrent                = BoldElementFlag9;
  befOwnedByObject                  = BoldElementFlag10;


  {flags for BoldObjectList}
  befAdjusted                       = BoldElementFlag11;
  befSubscribeToObjectsInList       = BoldElementFlag12;
  befSubscribeToLocatorsInList      = BoldElementFlag13;

  {flags for BoldObjectReference}
  befHasOldValues                   = BoldElementFlag11;

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
  befIsMultiRole = BoldElementFlag5;
  befIsSingleRole = BoldElementFlag6;
  befIsDerived = BoldElementFlag7;  // always false for roles...
  befIsReverseDerived = BoldElementFlag8;
  befIsNonVersionedInVersionedClass = BoldElementFlag9;
  befMemberToBeRemoved = BoldElementFlag10;

  {Flags for BoldRoleRTInfo}
  befIsIndirect = BoldElementFlag11;
  befIsNavigable = BoldElementFlag12;
  befIsOrdered = BoldElementFlag13;
  befOtherEndOrdered = BoldElementFlag14;
  befMandatory = BoldElementFlag15;
  befForceOtherEnd = BoldElementFlag16;
  befQualifiedMulti = BoldElementFlag17;

  {Flags for BoldAttributeRTInfo}
  befHasInitalvalue = BoldElementFlag11;
  befAllowNull = BoldElementFlag12;

type
  {forward declarations of all classes}
  TBoldEvaluator = class;
  TBoldElement = class;
  TBoldMetaElement = class;
  TBoldElementTypeInfo = class;
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
    procedure SetExpression(Expression: String);
    function GetVariableList: TBoldExternalVariableList;
    function GetExpression: String;
    property ContextType: TBoldElementTypeInfo read GetContextType;
    property Expression: String read GetExpression write SetExpression;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  end;

  { TBoldExternalVariable }
  TBoldExternalVariable = class(TBoldMemoryManagedObject)
  private
    fName: String;
  protected
    function GetValue: TBoldElement; virtual; abstract;
    function GetValueType: TBoldElementTypeInfo; virtual; abstract;
  public
    constructor Create(const Name: String);
    property Value: TBoldElement read GetValue;
    property Name: String read fName;
    property ValueType: TBoldElementTypeInfo read GetValueType;
  end;

  { TBoldExternalVariableList }
  TBoldExternalVariableList = class(TBoldObjectArray)
  private
    function GetVariables(index: integer): TBoldExternalVariable;
  public
    constructor create;
    procedure Add(Variable: TBoldExternalVariable);
    property Variables[index: integer]: TBoldExternalVariable read GetVariables; default;
  end;

  {---TBoldEvaluator---}
  TBoldEvaluator = class(TBoldMemoryManagedObject)
  public
    procedure DefineVariable(const VariableName: string; VarValue: TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue: Boolean); virtual; abstract;
    procedure Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil); virtual; abstract;
    function ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo; virtual; abstract;
    procedure SetLookupOclDefinition(value: TBoldLookUpOclDefinition); virtual; abstract;
  end;

  {---TBoldElement---}
  TBoldElement = class(TBoldSubscribableObject)
  private
    function GetModifiedValueHolder: TObject;
    procedure SetModifiedValueHolder(Value: TObject);
    function GetMutable: Boolean;
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; virtual;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); virtual;
    procedure CompareError(BoldElement: TBoldElement);
    procedure AssignError(BoldElement: TBoldElement);
    procedure MutableError(const NewValue: string);
    procedure CompareTypeError(CompType: TBoldCompareType; BoldElement: TBoldElement);
    function GetEvaluator: TBoldEvaluator; virtual;
    function GetBoldType: TBoldElementTypeInfo; virtual; abstract;
    function CloneIfPossible: TBoldElement; virtual;
  public
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
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; virtual;
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
    procedure EvaluateExpression(const Expression: TBoldExpression; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
    function EvaluateExpressionAsDirectElement(const Expression: TBoldExpression; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
    procedure EvaluateAndSubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; resultElement: TBoldIndirectElement; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
    function EvaluateExpressionAsString(const Expression: TBoldExpression; Representation: TBoldRepresentation; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): string;
    function EvaluateExpressionAsNewElement(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
    property Evaluator: TBoldEvaluator read GetEvaluator;
    property BoldType: TBoldElementTypeInfo read GetBoldType;
    property StringRepresentation[Representation: TBoldRepresentation]: string read GetStringRepresentation write SetStringRepresentation;
    property AsString: string index brDefault read GetStringRepresentation write SetStringRepresentation;
    property ModifiedValueHolder: TObject read GetModifiedValueHolder;
    property Mutable: Boolean read GetMutable;
  end;

  {---TBoldMetaElement---}
  TBoldMetaElement = class(TBoldElement)
  private
    fModelName: string;
    fDelphiName: string;
    fExpressionName: string;
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
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
    fSystemTypeInfo: TBoldElementTypeInfo; // FixMe this shouldn't be stored in every ValueTypeINfo
    function GetValueType: TBoldValueTypeSet;
  protected
    function GetEvaluator: TBoldEvaluator; override;
    procedure SetValueType(NewValue: TBoldValueTypeSet);
  public
    constructor Create(const ModelName: string; const ExpressionName: string; const DelphiName: string; SystemTypeInfo: TBoldElementTypeInfo);
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function ConformsTo(Element: TBoldElementTypeInfo): Boolean; virtual; abstract;
    property SystemTypeInfo: TBoldElementTypeInfo read fSystemTypeInfo;
    property BoldValueType: TBoldValueTypeSet read GetValueType;
  end;

  {---TBoldIndirectElement---}
  TBoldIndirectElement = class(TBoldFlaggedObject)
  private
    fValue: TBoldElement;
    function GetValue: TBoldElement;
    property WriteableOwnsValue: Boolean index befOwnsValue read GetElementFlag write SetElementFlag;
  public
    destructor Destroy; override;
    procedure SetReferenceValue(NewValue: TBoldElement);
    procedure SetOwnedValue(NewValue: TBoldElement);
    procedure TransferValue(Target: TBoldIndirectElement);
    function RelinquishValue: TBoldElement;
    property OwnsValue: Boolean index befOwnsValue read GetElementFlag;
    property Value: TBoldElement read GetValue;
  end;

  {---TBoldSubscribableComponentViaBoldElem---}
  TBoldSubscribableComponentViaBoldElem = class(TBoldSubscribableComponent)
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldOclError,
  Typinfo,
  BoldTypeList, // circular reference BoldElements->BoldTypelist->BoldSystem->BoldElements
  BoldExternalizedReferences,
  BoldCoreConsts;

var
  G_ExternalModifiedValueHolders: TBoldExternalizedReferenceList;

function TBoldElement.EvaluateExpressionAsString(const Expression: TBoldExpression; Representation: TBoldRepresentation; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): string;
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

procedure TBoldElement.EvaluateExpression(const Expression: TBoldExpression; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
begin
  EvaluateAndSubscribeToExpression(Expression, nil, resultElement, False, EvaluateInPS, VariableList);
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
  raise EBold.CreateFmt(sRepresentationNotSupported, [ClassName, Representation]);
end;

procedure TBoldElement.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  raise EBold.CreateFmt(sRepresentationNotSupported, [ClassName, Representation]);
end;

procedure TBoldElement.SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
end;

function TBoldElement.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := True;
end;

function TBoldElement.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
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

function TBoldElement.ObserverMayModify(Observer: TObject): Boolean;
begin
  Result := False;
end;

function TBoldElement.ObserverMayModifyAsString(Representation: TBoldRepresentation; Observer: TBoldSubscriber): Boolean;
begin
  Result := ObserverMayModify(Observer);
end;

procedure TBoldElement.RegisterModifiedValueHolder(observer: TObject);
begin
  if Assigned(ModifiedValueHolder) and (ModifiedValueHolder <> observer) then
    raise EBold.CreateFmt(sMemberAlreadyModified, [ClassName])
  else
    SetModifiedValueHolder(observer);
end;

procedure TBoldElement.UnRegisterModifiedValueHolder(observer: TObject);
begin
  // FIXME grid if  (ModifiedValueHolder <> observer) then
  //   raise EBoldInternal.Create('Internal Error')
  // else
  SetModifiedValueHolder(nil);
end;

function TBoldElement.IsEqual(BoldElement: TBoldElement): Boolean;
begin
  Result := IsEqualAs(ctDefault, BoldElement);
end;

function TBoldElement.IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean;
begin
  Result := CompareToAs(CompareType, BoldElement) = 0;
end;

function TBoldElement.CompareTo(BoldElement: TBoldElement): Integer;
begin
  Result := CompareToAs(ctDefault, BoldElement);
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
begin
  raise EBold.CreateFmt(sInvalidCompareType,
    [ClassName,
     GetEnumName(TypeInfo(TBoldCompareType), Ord(CompType)),
     BoldElement.ClassName]);
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
    raise EBoldAssertionFailed.Create(sStringValidationFailed);
end;

function TBoldElement.GetAsVariant: Variant;
begin
  Result := AsString;
end;

procedure TBoldElement.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

{---TBoldMetaElement---}
procedure TBoldMetaElement.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
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

{---TBoldElementTypeInfo---}
constructor TBoldElementTypeInfo.Create(const ModelName: string; const ExpressionName: string; const DelphiName: string; SystemTypeInfo: TBoldElementTypeInfo);
begin
  inherited Create(ModelName, ExpressionName, DelphiName);
  fSystemTypeInfo := SystemTypeInfo;
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

{---TBoldIndirectElement---}
destructor TBoldIndirectElement.Destroy;
begin
  if OwnsValue then
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
  else
    if OwnsValue then
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

function TBoldIndirectElement.GetValue: TBoldElement;
begin
  Result := fValue;
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
  resultElement: TBoldIndirectElement; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
begin
  try
    Evaluator.Evaluate(Expression, Self, Subscriber, Resubscribe, resultElement, EvaluateInPS, VariableList);
    if not evaluateInPS and Assigned(resultElement) and Assigned(resultElement.Value) and resultElement.OwnsValue then
      resultElement.Value.MakeImmutable;
    if Assigned(Subscriber) then
      AddSmallSubscription(Subscriber, [beDestroying], breReSubscribe);
  except
    on E: EBoldOCLAbort do
      raise EBold.CreateFmt(sOCLExpressionError, [E.Ocl, E.Message]);
    on E: EBoldOCLError do
      raise EBold.CreateFmt(sOCLExpressionError, [E.Ocl, E.Message]);
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

constructor TBoldExternalVariable.Create(const Name: String);
begin
  inherited Create;
  fName := Name;
end;

{ TBoldExternalOclVariableList }

procedure TBoldExternalVariableList.Add(Variable: TBoldExternalVariable);
begin
  inherited Add(Variable);
end;

constructor TBoldExternalVariableList.create;
begin
  inherited create(4, [bcoDataOwner]);
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

initialization
  G_ExternalModifiedValueHolders := TBoldExternalizedReferenceList.Create;

finalization
  G_ExternalModifiedValueHolders.free;

end.



