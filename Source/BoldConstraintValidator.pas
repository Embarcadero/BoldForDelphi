{ Global compiler directives }
{$include bold.inc}
unit BoldConstraintValidator;

interface

uses
  Classes,
  SysUtils,
  Contnrs,

  BoldElements,
  BoldSubscription,
  BoldSystem,
  BoldHandles,
  BoldDefs,
  BoldBase,
  BoldComponentValidator;

type
  TBoldConstraintSeverity = (csError, csWarning);
  TValidationMode = (vmManual, vmOnModify, vmPreUpdate);

const
  cDefaultSeverity = csError;

type
  TBoldConstraintValidator = class;
  TBoldConstraintItem = class;
  TBoldConstraintCollection = class;
  TBoldConstraintFailureList = class;
  EBoldConstraintFailure = class;

  TBoldConstraintsFailureEvent = procedure(var AHandled: boolean; AFailedConstraints: TBoldConstraintFailureList) of object;
  TBoldConstraintFailureEvent = procedure(ABoldObject: TBoldObject; AConstraint: TBoldConstraintItem; const AFailureMessage: string) of object;
  TBoldConstraintCheckEvent = procedure(ABoldObject: TBoldObject; AConstraint: TBoldConstraintItem; var AFailureMessage: string) of object;

  TBoldConstraintFailure = class(TBoldMemoryManagedObject)
  private
    fBoldObject: TBoldObject;
    fConstraint: TBoldConstraintItem;
    fFailureMessage: string;
  public
    constructor Create(ABoldObject: TBoldObject; AConstraint: TBoldConstraintItem; const AFailureMessage: string);
    property BoldObject: TBoldObject read fBoldObject;
    property Constraint: TBoldConstraintItem read fConstraint;
    property FailureMessage: string read fFailureMessage;
  end;

  TBoldConstraintFailureList = class(TObjectList)
  private
    function GetBoldObject(const index: integer): TBoldObject;
    function GeTBoldConstraint(const index: integer): TBoldConstraintItem;
    function GeTBoldConstraintFailure(const index: integer): TBoldConstraintFailure;
    function GetFailureMessage(const index: integer): string;
    function GetHasErrors: boolean;
    function GetAsString: string;
  public
    property ConstraintFailure[const index: integer]: TBoldConstraintFailure read GeTBoldConstraintFailure; default;
    property BoldObjects[const index: integer]: TBoldObject read GetBoldObject;
    property Constraints[const index: integer]: TBoldConstraintItem read GeTBoldConstraint;
    property FailureMessages[const index: integer]: string read GetFailureMessage;
    property HasErrors: boolean read GetHasErrors;
    property AsString: string read GetAsString;
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldConstraintValidator = class(TBoldNonSystemHandle, IBoldValidateableComponent)
  strict private
    fValidatioNesting: integer;
    fConstraints: TBoldConstraintCollection;
    fValidationMode: TValidationMode;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fFailedConstraints: TBoldConstraintFailureList;
    procedure PreUpdate(Sender: TObject);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
  private
//    fCheckModelConstraints: boolean;
    fOnConstraintsFailed: TBoldConstraintsFailureEvent;
    fOnConstraintFailed: TBoldConstraintFailureEvent;
    fEnabled: Boolean;
    procedure SetValidationMode(const Value: TValidationMode);
    function GetSubscriber: TBoldSubscriber;
    procedure ProcessFailedConstraints;
    procedure SetEnabled(const Value: Boolean);
  protected
    { IBoldValidateableComponent }
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean; override;
    procedure StaticBoldTypeChanged; override;
    procedure Changed;
    function BoldSystem: TBoldSystem;
    procedure BeginValidate;
    procedure EndValidate;
    procedure AddFailure(ABoldObject: TBoldObject; AConstraint: TBoldConstraintItem; const AFailureMessage: string);
    property Subscriber: TBoldSubscriber read GetSubscriber;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ValidateObject(ABoldObject: TBoldObject);
    procedure ValidateDirtyObjects;
    procedure ValidateList(AList: TBoldObjectList);
    procedure ValidateObjects(const aObjects: array of TBoldObject);
  published
    property Constraints: TBoldConstraintCollection read fConstraints write fConstraints;
    property ValidationMode: TValidationMode read fValidationMode write SetValidationMode default vmManual;
// TODO:
//    property CheckModelConstraints: boolean read fCheckModelConstraints write fCheckModelConstraints;
    property OnConstraintFailed: TBoldConstraintFailureEvent read fOnConstraintFailed write fOnConstraintFailed;
    property OnConstraintsFailed: TBoldConstraintsFailureEvent read fOnConstraintsFailed write fOnConstraintsFailed;
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;

  TBoldConstraintCollectionEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBoldConstraintItem;
    property Current: TBoldConstraintItem read GetCurrent;
  end;

  TBoldConstraintCollection = class(TCollection)
  strict private
    fValidator: TBoldConstraintValidator;
  protected
    function GetItems(Index: integer): TBoldConstraintItem;
    function GetOwner: TPersistent; override;
    property Validator: TBoldConstraintValidator read fValidator;
  public
    constructor Create(Validator: TBoldConstraintValidator);
    function GetEnumerator: TBoldConstraintCollectionEnumerator;
    property Items[Index: integer]: TBoldConstraintItem read GetItems; default;
  end;

  TBoldConstraintItem = class(TCollectionItem, IBoldOCLComponent)
  strict private
    fContext: String;
    fExpression: TBoldExpression;
    fErrorMessage: TBoldExpression;
    fSeverity: TBoldConstraintSeverity;
    fConstraintCheckEvent: TBoldConstraintCheckEvent;
  private
    function QueryInterface(const IId: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
    { IBoldOCLComponent }
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetVariableList: TBoldExternalVariableList;
    function GetExpression: TBoldExpression;
  public
    procedure Assign(Source: TPersistent); override;
    function Check(ABoldObject: TBoldObject): string;
  published
    property Context: String read fContext write fContext;
    property Expression: TBoldExpression read GetExpression write SetExpression;
    property Severity: TBoldConstraintSeverity read fSeverity write fSeverity default cDefaultSeverity;
    property ErrorMessage: TBoldExpression read fErrorMessage write fErrorMessage; // default cDefaultErrorMessage;
    property OnCheckConstraint: TBoldConstraintCheckEvent read fConstraintCheckEvent write fConstraintCheckEvent;
  end;

  EBoldConstraintFailure = class(Exception)
  private
    fFailedConstraints: TBoldConstraintFailureList;
  public
    constructor Create(AFailedConstraints: TBoldConstraintFailureList);
    property FailedConstraints: TBoldConstraintFailureList read fFailedConstraints;
  end;

implementation

uses
  BoldCoreConsts,
  BoldSystemRT;

{ TBoldConstraintCollection }

constructor TBoldConstraintCollection.Create(Validator: TBoldConstraintValidator);
begin
  inherited Create(TBoldConstraintItem);
  fValidator := Validator;
end;

function TBoldConstraintCollection.GetEnumerator: TBoldConstraintCollectionEnumerator;
begin
  result := TBoldConstraintCollectionEnumerator.Create(self);
end;

function TBoldConstraintCollection.GetItems(Index: integer): TBoldConstraintItem;
begin
  result := TBoldConstraintItem(inherited items[index]);
end;

function TBoldConstraintCollection.GetOwner: TPersistent;
begin
  result := Validator;
end;

{ TBoldConstraintItem }

procedure TBoldConstraintItem.Assign(Source: TPersistent);
var
  SourceItem: TBoldConstraintItem;
begin
  if Source is TBoldConstraintItem then
  begin
    SourceItem := Source as TBoldConstraintItem;
    Context := SourceItem.Context;
    Expression := SourceItem.Expression;
    ErrorMessage := SourceItem.ErrorMessage;
    Severity := SourceItem.Severity;
    OnCheckConstraint := SourceItem.OnCheckConstraint;
  end
  else
    inherited;
end;

function TBoldConstraintItem.Check(ABoldObject: TBoldObject): string;
begin
  result := '';
  if ABoldObject.BoldClassTypeInfo.BoldIsA(GetContextType) then
  begin
    if Assigned(fConstraintCheckEvent) then
    try
      fConstraintCheckEvent(ABoldObject, self, result);
    except
      on e:exception do
      begin
        result := '';
        raise;
//        TraceLog.SystemMessage(E.ClassName+':'+E.Message, ekError);
      end;
    end
    else
    if not ABoldObject.EvaluateExpressionAsBoolean(Expression) then
      result := ABoldObject.EvaluateExpressionAsString(ErrorMessage);
  end;
end;

function TBoldConstraintItem.GetContextType: TBoldElementTypeInfo;
var
  BoldSystemTypeInfo: TBoldSystemTypeInfo;
begin
  result := nil;
  BoldSystemTypeInfo := (Collection as TBoldConstraintCollection).Validator.StaticSystemTypeInfo;
  if Assigned(BoldSystemTypeInfo) then
    result := BoldSystemTypeInfo.ClassTypeInfoByExpressionName[Context];
end;

function TBoldConstraintItem.GetDisplayName: string;
begin
  if ErrorMessage = '' then
    result := Format('[%s]:%s', [Context, Expression])
  else
    result := Format('[%s]:%s', [Context, ErrorMessage]);
end;

function TBoldConstraintItem.GetExpression: TBoldExpression;
begin
  result := fExpression;
end;

function TBoldConstraintItem.GetVariableList: TBoldExternalVariableList;
begin
  result := nil;
end;

function TBoldConstraintItem.QueryInterface(const IId: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TBoldConstraintItem.SetExpression(const Value: TBoldExpression);
begin
  fExpression := Value;
end;

function TBoldConstraintItem._AddRef: Integer;
begin
  result := -1;
end;

function TBoldConstraintItem._Release: Integer;
begin
  result := -1;
end;

{ TBoldConstraintCollectionEnumerator }

function TBoldConstraintCollectionEnumerator.GetCurrent: TBoldConstraintItem;
begin
  result := inherited GetCurrent as TBoldConstraintItem;
end;

{ TBoldConstraintValidator }

procedure TBoldConstraintValidator.AddFailure(ABoldObject: TBoldObject;
  AConstraint: TBoldConstraintItem; const AFailureMessage: string);
begin
  fFailedConstraints.Add(TBoldConstraintFailure.Create(ABoldObject, AConstraint, AFailureMessage));
  if Assigned(fOnConstraintFailed) then
    OnConstraintFailed(ABoldObject, AConstraint, AFailureMessage);
end;

procedure TBoldConstraintValidator.AfterConstruction;
begin
  inherited;
  Enabled := true;
  fConstraints := TBoldConstraintCollection.Create(self);
  fFailedConstraints := TBoldConstraintFailureList.Create;
end;

procedure TBoldConstraintValidator.BeforeDestruction;
begin
  FreeAndNil(fFailedConstraints);
  FreeAndNil(fConstraints);
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldConstraintValidator.BeginValidate;
begin
  inc(fValidatioNesting);
end;

procedure TBoldConstraintValidator.EndValidate;
begin
  dec(fValidatioNesting);
  Assert(fValidatioNesting >= 0);
  if (fValidatioNesting = 0) and (fFailedConstraints.count > 0) then
  begin
    ProcessFailedConstraints;
  end;
end;

function TBoldConstraintValidator.BoldSystem: TBoldSystem;
begin
  result := nil;
  if Assigned(StaticSystemHandle) then
    result := StaticSystemHandle.System;
end;

procedure TBoldConstraintValidator.Changed;
begin
  FreeAndNil(fSubscriber);
  if (BoldSystem <> nil) and Enabled then
  begin
    BoldSystem.OnPreUpdate := nil;
    case ValidationMode of
      vmManual:;
      vmOnModify:
        BoldSystem.AddSubscription(Subscriber, beCompleteModify, beCompleteModify);
      vmPreUpdate:
        BoldSystem.OnPreUpdate := PreUpdate;
    end;
  end;
end;

function TBoldConstraintValidator.GetSubscriber: TBoldSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  result := fSubscriber;
end;

procedure TBoldConstraintValidator.PreUpdate(Sender: TObject);
begin
  if Enabled and (ValidationMode = vmPreUpdate) then
    ValidateList(Sender as TBoldObjectList);
end;

procedure TBoldConstraintValidator.ProcessFailedConstraints;
var
  Handled: boolean;
begin
  Handled := false;
  try
    if Assigned(fOnConstraintsFailed) then
      OnConstraintsFailed(Handled, fFailedConstraints);
    if not Handled and fFailedConstraints.HasErrors then
      raise EBoldConstraintFailure.Create(fFailedConstraints);
  finally
    fFailedConstraints.clear;
  end;
end;

procedure TBoldConstraintValidator.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
var
  BoldObject: TBoldObject;
begin
  if Enabled then
  case RequestedEvent of
    beCompleteModify:
    if (Originator is TBoldMember) then
    begin
      Assert(Args[0].VObject is TBoldMember);
      BoldObject := TBoldMember(Args[0].VObject).OwningObject;
      ValidateObject(BoldObject);
    end;
  end;
end;

procedure TBoldConstraintValidator.SetEnabled(const Value: Boolean);
begin
  if Value <> fEnabled then
  begin
    fEnabled := Value;
    Changed;
  end;
end;

procedure TBoldConstraintValidator.SetValidationMode(const Value: TValidationMode);
begin
  if fValidationMode <> Value then
  begin
    fValidationMode := Value;
    Changed;
  end;
end;

procedure TBoldConstraintValidator.StaticBoldTypeChanged;
begin
  inherited;
  Changed;
end;

procedure TBoldConstraintValidator.ValidateList(AList: TBoldObjectList);
var
  BoldObject: TBoldObject;
begin
  BeginValidate;
  try
    for BoldObject in AList do
      ValidateObject(BoldObject);
  finally
    EndValidate;
  end;
end;

procedure TBoldConstraintValidator.ValidateObject(ABoldObject: TBoldObject);
var
  Constraint: TBoldConstraintItem;
  FailureMessage: string;
begin
  if ABoldObject.BoldObjectIsDeleted then // do not check contraints for deleted objects
    exit;
  BeginValidate;
  try
    for Constraint in Constraints do
    begin
      FailureMessage := Constraint.Check(ABoldObject);
      if FailureMessage <> '' then
        AddFailure(ABoldObject, Constraint, FailureMessage);
    end;
  finally
    EndValidate;
  end;
end;

procedure TBoldConstraintValidator.ValidateObjects(const aObjects: array of TBoldObject);
var
  i: integer;
begin
  BeginValidate;
  try
    for i := 0 to high(AObjects) do
      ValidateObject(AObjects[i]);
  finally
    EndValidate;
  end;
end;

function TBoldConstraintValidator.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  Constraint: TBoldConstraintItem;
begin
  result := inherited ValidateComponent(ComponentValidator, NamePrefix);
  for Constraint in Constraints do
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      Constraint.Expression,
      Constraint.GetContextType,
      format('%s%s.Constraint[%d]', [NamePrefix, Constraint.DisplayName, Constraint.Index])) and result;
    result := ComponentValidator.ValidateExpressionInContext(
      Constraint.ErrorMessage,
      Constraint.GetContextType,
      format('%s%s.Constraint[%d]', [NamePrefix, Constraint.DisplayName, Constraint.Index])) and result;
  end;
end;

procedure TBoldConstraintValidator.ValidateDirtyObjects;
var
  List: TBoldObjectList;
begin
  if Assigned(StaticSystemHandle) and Assigned(StaticSystemHandle.System) and StaticSystemHandle.System.BoldDirty then
  begin
    List := StaticSystemHandle.System.DirtyObjectsAsBoldList;
    try
      ValidateList(List)
    finally
      List.free;
    end;
  end;
end;

{ EBoldConstraintFailure }

constructor EBoldConstraintFailure.Create(
  AFailedConstraints: TBoldConstraintFailureList);
var
  i: integer;
  sl: TStringList;
begin
  fFailedConstraints := AFailedConstraints;
  sl := TStringList.Create;
  try
    for I := 0 to fFailedConstraints.Count - 1 do
    begin
      if fFailedConstraints[i].Constraint.Severity = csError then
        sl.Add(fFailedConstraints.FailureMessages[i]);
    end;
    inherited Create(sl.text);
  finally
    sl.free;
  end;
end;

{ TBoldConstraintFailure }

constructor TBoldConstraintFailure.Create(ABoldObject: TBoldObject;
  AConstraint: TBoldConstraintItem; const AFailureMessage: string);
begin
  inherited Create;
  fBoldObject := ABoldObject;
  fConstraint := AConstraint;
  fFailureMessage := AFailureMessage;
end;

{ TBoldConstraintFailureList }

function TBoldConstraintFailureList.GeTBoldConstraintFailure(
  const index: integer): TBoldConstraintFailure;
begin
  result := self.Items[index] as TBoldConstraintFailure;
end;

function TBoldConstraintFailureList.GetAsString: string;
var
  i: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(FailureMessages[i]);
    result := sl.text;
  finally
    sl.free;
  end;
end;

function TBoldConstraintFailureList.GetBoldObject(
  const index: integer): TBoldObject;
begin
  result := ConstraintFailure[index].BoldObject
end;

function TBoldConstraintFailureList.GeTBoldConstraint(
  const index: integer): TBoldConstraintItem;
begin
  result := ConstraintFailure[index].Constraint
end;

function TBoldConstraintFailureList.GetFailureMessage(const index: integer): string;
begin
  result := ConstraintFailure[index].FailureMessage;
end;

function TBoldConstraintFailureList.GetHasErrors: boolean;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    if Constraints[i].severity = csError then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

end.

