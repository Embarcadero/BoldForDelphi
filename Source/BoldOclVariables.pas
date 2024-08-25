
{ Global compiler directives }
{$include bold.inc}

unit BoldOclVariables;

interface

uses
  Classes,
{$IFNDEF BOLD_DELPHI16_OR_LATER}
  Controls, //   TDate = type TDateTime;
{$ENDIF}
  BoldSubscription,
  BoldHandles,
  BoldElements;

type
  { forward declarations }
  TBoldHandleBasedExternalVariable = class;
  TBoldOclVariables = class;
  TBoldVariableTupleList = class;
  TBoldVariableTuple = class;

  { TBoldHandleBasedExternalVariable }
  TBoldHandleBasedExternalVariable = class(TBoldExternalVariable)
  private
    fHandle: TBoldElementHandle;
    fUseListElement: Boolean;
    fHandleSubscriber: TBoldPassThroughSubscriber;
    procedure _ReceiveHandleEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetHandle(const Value: TBoldElementHandle);
  protected
    function GetValue: TBoldElement; override;
    function GetValueType: TBoldElementTypeInfo; override;
    property Handle: TBoldElementHandle read fHandle write SetHandle;
  public
    constructor Create(Name: String; Handle: TBoldElementHandle; UseListElement: Boolean);
    destructor Destroy; override;
  end;

  { TBoldOclVariables }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldOclVariables = class(TBoldSubscribableComponent)
  private
    fVariableTupleList: TBoldVariableTupleList;
    fVariableList: TBoldExternalVariableList;
    fGlobalSystemHandle: TBoldAbstractSystemhandle;
    fSubscriber: TBoldPassthroughSubscriber;
    procedure SetVariableTupleList(const Value: TBoldVariableTupleList);
    procedure VariablesChanged;
    function GetVariableList: TBoldExternalVariableList;
    procedure SetGlobalSystemHandle(aSystemHandle: TBoldAbstractSystemhandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure PlaceSubscriptions;
    procedure SubscribeToHandle(AHandle: TBoldElementHandle);
    function GetGlobalSystemHandle: TBoldAbstractSystemhandle;
    function GetVariableFromHandle(AHandle: TBoldElementHandle): TBoldExternalVariable;
  protected
    procedure Loaded; override;
    procedure RegisterVariables;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure SubscribeToHandles(Subscriber: TBoldSubscriber); overload;
    procedure SubscribeToHandles(Subscriber: TBoldSubscriber; Expression: string); overload;
    function AddVariable(aVariableName: string; aBoldHandle: TBoldElementHandle; aUseListElement: boolean = false): TBoldVariableTuple;
    procedure AddVariables(aBoldOclVariables: TBoldOclVariables);
    function FindVariableByName(const aVariableName: string): TBoldExternalVariable;
    function GetVariableValue(const aVariableName: string): TBoldElement;
    function LinksToHandle(Handle: TBoldElementHandle): Boolean;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  published
    property GlobalSystemHandle: TBoldAbstractSystemhandle read GetGlobalSystemHandle write SetGlobalSystemHandle;
    property Variables: TBoldVariableTupleList read fVariableTupleList write SetVariableTupleList;
  end;

  TBoldVariableTupleListEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TBoldVariableTuple;
    property Current: TBoldVariableTuple read GetCurrent;
  end;

  { TBoldVariableTupleList }
  TBoldVariableTupleList = class(TCollection)
  private
    fOwningDefinition: TBoldOclVariables;
    function GetItems(Index: integer): TBoldVariableTuple;
    function GetVariableByName(const aName: string): TBoldVariableTuple;
  protected
    function GetOwner: TPersistent; override;
    property OwningDefinition: TBoldOclVariables read fOwningDefinition;
  public
    constructor Create(aOwningDefinition: TBoldOclVariables);
    function GetEnumerator: TBoldVariableTupleListEnumerator;
    function NameIsUnique(Name: String): Boolean;
    function NameIsValid(Name: String): Boolean;
    function GetUniqueName: String;
    property Items[Index: integer]: TBoldVariableTuple read GetItems; default;
    property VariableByName[const aName: string]: TBoldVariableTuple read GetVariableByName;
  end;

  { TBoldVariableTuple }
  TBoldVariableTuple = class(TCollectionItem)
  private
    FVariableName: String;
    FBoldHandle: TBoldElementHandle;
    fBoldHandleSubscriber: TBoldPassThroughSubscriber;
    FUseListElement: Boolean;
    procedure _ReceiveHandleEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetBoldHandle(const Value: TBoldElementHandle);
    procedure SetVariableName(const Value: String);
    procedure SetUseListElement(const Value: Boolean);
    procedure Changed;
    function GetTupleList: TBoldVariableTupleList;
    function GetEffectiveUseListElement: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function LinksToHandle(Handle: TBoldElementHandle): Boolean;
    property TupleList: TBoldVariableTupleList read GetTupleList;
    property EffectiveUseListElement: Boolean read GetEffectiveUseListElement;
  published
    property BoldHandle: TBoldElementHandle read FBoldHandle write SetBoldHandle;
    property VariableName: String read FVariableName write SetVariableName;
    property UseListElement: Boolean read fUseListElement write SetUseListElement default false;
  end;


  TBoldOclVariable = class(TBoldExternalVariable)
  private
    fBoldIndirectElement: TBoldIndirectElement;
    fBoldElementTypeInfo: TBoldElementTypeInfo;
    function GetBoldIndirectElement: TBoldIndirectElement;
    function EnsureEvaluator(AEvaluator: TBoldEvaluator): TBoldEvaluator;
  protected
    function GetValue: TBoldElement; override;
    function GetValueType: TBoldElementTypeInfo; override;
  public
    constructor Create(const aName: string; aValue: TBoldElement{; aOwnsValue: boolean = false}); overload;
    constructor CreateWithTypeInfo(const aName: string; AValue: TBoldElement; ABoldElementTypeInfo: TBoldElementTypeInfo);
    constructor CreateFromIndirectElement(const aName: string; ABoldIndirectElement: TBoldIndirectElement);
    constructor CreateStringVariable(const aName: string; const aValue: string; AEvaluator: TBoldEvaluator = nil);
    constructor CreateIntegerVariable(const aName: string; const aValue: integer; AEvaluator: TBoldEvaluator = nil);
    constructor CreateFloatVariable(const aName: string; const aValue: Double; AEvaluator: TBoldEvaluator = nil);
    constructor CreateDateVariable(const aName: string; const aValue: TDate; AEvaluator: TBoldEvaluator = nil);
    constructor CreateDateTimeVariable(const aName: string; const aValue: TDateTime; AEvaluator: TBoldEvaluator = nil);
    constructor CreateTimeVariable(const aName: string; const aValue: TTime; AEvaluator: TBoldEvaluator = nil);
    destructor Destroy; override;
    property BoldIndirectElement: TBoldIndirectElement read GetBoldIndirectElement;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldSystem,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldAttributes,
  BoldUtils,
  BoldOcl;

{ TBoldOclVariables }

function TBoldOclVariables.AddVariable(aVariableName: string;
  aBoldHandle: TBoldElementHandle; aUseListElement: boolean): TBoldVariableTuple;
begin
  result := Variables.VariableByName[aVariableName];
  if not Assigned(result) then
  begin
    result := Variables.Add as TBoldVariableTuple;
    result.VariableName := aVariableName;    
  end;
  result.BoldHandle := aBoldHandle;
  result.UseListElement := aUseListElement;
end;

procedure TBoldOclVariables.AddVariables(aBoldOclVariables: TBoldOclVariables);
var
  i: integer;
  lBoldVariableTuple: TBoldVariableTuple;
begin
  for I := 0 to aBoldOclVariables.Variables.Count - 1 do
  begin
    lBoldVariableTuple := Variables.VariableByName[aBoldOclVariables.Variables[i].VariableName];
    if Assigned(lBoldVariableTuple) then
      lBoldVariableTuple.Assign(aBoldOclVariables.Variables[i])
    else
      Variables.Add.Assign(aBoldOclVariables.Variables[i]);
  end;
end;

constructor TBoldOclVariables.create(Owner: TComponent);
begin
  inherited;
  fVariableList := nil;
  fVariableTupleList := TBoldVariableTupleList.Create(self);
  fSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  PlaceSubscriptions;
end;

destructor TBoldOclVariables.destroy;
begin
  FreePublisher;
  FreeAndNil(fVariableList);
  FreeAndNil(fVariableTupleList);
  FreeAndNil(fSubscriber);
  inherited;
end;

function TBoldOclVariables.FindVariableByName(
  const aVariableName: string): TBoldExternalVariable;
var
  i: integer;
begin
  for I := 0 to VariableList.Count - 1 do
  begin
    if CompareText(VariableList.Variables[i].Name, aVariableName) = 0 then
    begin
      result := VariableList.Variables[i];
      exit;
    end;
  end;
  result := nil;
end;

function TBoldOclVariables.GetGlobalSystemHandle: TBoldAbstractSystemhandle;
begin
  result := fGlobalSystemHandle
end;

function TBoldOclVariables.GetVariableFromHandle(
  AHandle: TBoldElementHandle): TBoldExternalVariable;
var
  i: integer;
  vTuple: TBoldVariableTuple;
begin
  for i := 0 to Variables.count - 1 do
  begin
    vTuple := Variables[i];
    if vTuple.BoldHandle = AHandle then
    begin
      result := VariableList[i];
      exit;
    end;
  end;
  result := nil;
end;

function TBoldOclVariables.GetVariableList: TBoldExternalVariableList;
var
  i: integer;
  NewList: Boolean;
begin
  if not assigned(fVariableList) then
  begin
    fVariableList := TBoldExternalVariableList.Create;
    NewList := true;
  end
  else
    NewList := false;
  for i := 0 to Variables.Count -1 do
  begin
    if Newlist then
    begin
      fVariableList.Add(TBoldHandleBasedExternalVariable.Create(Variables[i].VariableName, Variables[i].BoldHandle, Variables[i].EffectiveUseListElement));
    end
    else
    begin
      TBoldHandleBasedExternalVariable(fVariableList[i]).fHandle := Variables[i].BoldHandle;
    end;
  end;
  result := fVariableList;
end;

function TBoldOclVariables.GetVariableValue(
  const aVariableName: string): TBoldElement;
var
  lVariable: TBoldExternalVariable;
begin
  lVariable := FindVariableByName(aVariableName);
  if Assigned(lVariable) then
    result := lVariable.Value
  else
    result := nil;
end;

function TBoldOclVariables.LinksToHandle(Handle: TBoldElementHandle): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Variables.Count-1 do
    result := result or Variables[i].linksToHandle(Handle);
end;

procedure TBoldOclVariables.Loaded;
begin
  inherited Loaded;
  PlaceSubscriptions;
end;

procedure TBoldOclVariables.PlaceSubscriptions;
begin
  fSubscriber.CancelAllSubscriptions;
  RegisterVariables;
  SubscribeToHandles(fSubscriber);
end;

procedure TBoldOclVariables.RegisterVariables;
var
  vEvaluator: TBoldEvaluator;
  vVariable: TBoldExternalVariable;
  vTuple: TBoldVariableTuple;
  vTypeInfo: TBoldElementTypeInfo;  
begin
  if (VariableList.Count > 0) and Assigned(GlobalSystemHandle) and Assigned(GlobalSystemHandle.SystemTypeInfoHandle) and GlobalSystemHandle.SystemTypeInfoHandle.IsSystemTypeInfoAvailable then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if GlobalSystemHandle.Active then
      begin
        vEvaluator := GlobalSystemHandle.System.Evaluator;
        if Assigned(vEvaluator) then
        for vVariable in VariableList do
        begin
          Assert(not Assigned(vVariable.Evaluator) or (vVariable.Evaluator = vEvaluator));
          vEvaluator.DefineVariable(vVariable.Name, vVariable);
        end;
      end
      else
      begin
        for vVariable in VariableList do
        begin
          if Assigned(vVariable.Evaluator) then
            vVariable.Evaluator := nil;
        end;
      end;
    end;
    // register into type/meta evaluator
    vEvaluator := GlobalSystemHandle.BoldType.Evaluator;
    for vTuple in Variables do
    if Assigned(vTuple.BoldHandle) then
    begin
      if vTuple.EffectiveUseListElement then
        vTypeInfo := TBoldAbstractListHandle(vTuple.BoldHandle).StaticListType
      else
        vTypeInfo := vTuple.BoldHandle.StaticBoldType;
      vEvaluator.DefineVariable(vTuple.VariableName, nil, vTypeInfo, false, false);
    end;
  end;
end;

procedure TBoldOclVariables.SetGlobalSystemHandle(
  aSystemHandle: TBoldAbstractSystemhandle);
begin
  if aSystemHandle <> fGlobalSystemHandle then
  begin
    fGlobalSystemHandle := aSystemHandle;
    PlaceSubscriptions;
  end;
end;

procedure TBoldOclVariables.SetVariableTupleList(
  const Value: TBoldVariableTupleList);
begin
  fVariableTupleList := Value;
end;

procedure TBoldOclVariables.SubscribeToHandles(
  Subscriber: TBoldSubscriber);
var
  vTuple: TBoldVariableTuple;
begin
  if self.fSubscriber <> Subscriber then
    self.AddSubscription(Subscriber, beDestroying, breReSubscribe);
  if Assigned(GlobalSystemHandle) then
  begin
    GlobalSystemHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged, beDestroying], beValueIdentityChanged);
    if GlobalSystemHandle.Active then
      GlobalSystemHandle.System.AddSmallSubscription(Subscriber, [beDestroying], beDestroying);
  end;
  for vTuple in Variables do
    if assigned(vTuple.boldHandle) then
      vTuple.BoldHandle.AddSubscription(Subscriber, beValueIdentityChanged, breResubscribe);
end;

procedure TBoldOclVariables.SubscribeToHandle(AHandle: TBoldElementHandle);
var
  vVariable: TBoldExternalVariable;
begin
  if (not (csDestroying in ComponentState) and ((Owner = nil) or not (csDestroying in TComponent(Owner).ComponentState)))
     and (VariableList.Count > 0) and Assigned(GlobalSystemHandle) and GlobalSystemHandle.Active then // Assigned(GlobalSystemHandle.SystemTypeInfoHandle) and GlobalSystemHandle.SystemTypeInfoHandle.IsSystemTypeInfoAvailable then
  begin
    GlobalSystemHandle.System.AddSmallSubscription(fSubscriber, [beDestroying], beDestroying);
    vVariable := GetVariableFromHandle(AHandle);
    if Assigned(vVariable) then
    begin
      AHandle.AddSubscription(fSubscriber, beDestroying, breReSubscribe);
      AHandle.AddSubscription(fSubscriber, beValueIdentityChanged, breResubscribe);
      GlobalSystemHandle.System.Evaluator.DefineVariable(vVariable.Name, vVariable);
    end;
  end;
end;

procedure TBoldOclVariables.SubscribeToHandles(Subscriber: TBoldSubscriber;
    Expression: string);
var
  vVariable: TBoldVariableTuple;
begin
  if self.fSubscriber <> Subscriber then
    self.AddSubscription(Subscriber, beDestroying, breReSubscribe);
  if Assigned(GlobalSystemHandle) then
  begin
    GlobalSystemHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged, beDestroying], beValueIdentityChanged);
    if GlobalSystemHandle.Active then
      GlobalSystemHandle.System.AddSmallSubscription(Subscriber, [beDestroying], beDestroying);
  end;
  if Assigned(Subscriber) then
    for vVariable in Variables do
      if Assigned(vVariable.BoldHandle) then
        if BoldCaseIndependentPos(vVariable.VariableName, Expression) > 0 then
          vVariable.BoldHandle.AddSubscription(Subscriber, beValueIdentityChanged, breResubscribe);
end;

procedure TBoldOclVariables.VariablesChanged;
begin
  FreeAndNil(fVariableList);
  SendEvent(Self, beValueChanged);
end;

procedure TBoldOclVariables._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fGlobalSystemHandle) then
  begin
    case OriginalEvent of
      beDestroying:
        begin
          GlobalSystemHandle := nil;
          fSubscriber.CancelAllSubscriptions;
        end;
      beValueIdentityChanged:
        begin
          RegisterVariables;
        end;
    end;
  end
  else
  if (Originator is TBoldSystem)then
  begin
    if OriginalEvent = beDestroying  then
    begin
      FreeAndNil(fVariableList);
      SendEvent(Self, beValueChanged);
    end
    else
      RegisterVariables;
  end
  else
  if Originator is TBoldElementHandle then
  begin
    SubscribeToHandle(TBoldElementHandle(Originator));
  end;
end;

{ TBoldVariableTupleList }

constructor TBoldVariableTupleList.create(aOwningDefinition: TBoldOclVariables);
begin
  inherited Create(TBoldVariableTuple);
  fOwningDefinition := aOwningDefinition;
end;

function TBoldVariableTupleList.GetEnumerator: TBoldVariableTupleListEnumerator;
begin
  result := TBoldVariableTupleListEnumerator.Create(self);
end;

function TBoldVariableTupleList.GetItems(
  Index: integer): TBoldVariableTuple;
begin
  result := TBoldVariableTuple(inherited items[index]);
end;

function TBoldVariableTupleList.GetOwner: TPersistent;
begin
  result := fOwningDefinition;
end;

function TBoldVariableTupleList.GetUniqueName: String;
var
  i: integer;
begin
  i := 1;
  repeat
    result := 'Variable'+IntToStr(i);
    Inc(i);
  until NameIsUnique(result);
end;

function TBoldVariableTupleList.GetVariableByName(
  const aName: string): TBoldVariableTuple;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
  if Items[i].VariableName = aName then
  begin
    result := Items[i];
    exit;
  end;
  result := nil;
end;

function TBoldVariableTupleList.NameIsUnique(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to Count-1 do
    if CompareText(Name, Items[i].VariableName) = 0 then
    begin
      result := false;
      exit;
    end;
end;

function TBoldVariableTupleList.NameIsValid(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(name) do
    if not CharInSet(name[i], ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
    begin
      result := false;
      exit;
    end;
end;

{ TBoldVariableTuple }

procedure TBoldVariableTuple.Assign(Source: TPersistent);
var
  SourceTuple: TBoldVariableTuple;
begin
  if Source is TBoldVariableTuple then
  begin
    SourceTuple := Source as TBoldVariableTuple;
    VariableName := SourceTuple.VariableName;
    UseListElement := SourceTuple.UseListElement;
    BoldHandle := SourceTuple.BoldHandle;
  end
  else
    inherited;
end;

procedure TBoldVariableTuple.Changed;
begin
  (Collection as TBoldVariableTupleList).OwningDefinition.VariablesChanged;
end;

constructor TBoldVariableTuple.Create(Collection: TCollection);
begin
  inherited;
  Changed;
  VariableName := (Collection as TBoldVariableTupleList).GetUniqueName;
  fBoldHandleSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveHandleEvent);
end;

destructor TBoldVariableTuple.Destroy;
begin
  FreeAndNil(fBoldHandleSubscriber);
  inherited;
end;

function TBoldVariableTuple.GetDisplayName: string;
begin
  result := VariableName;
  if assigned(BoldHandle) then
    Result := result + ': ' + BoldHandle.Name
  else
    result := result + ': Not Connected';
  if EffectiveUseListElement then
    result := result + ' (list)';
end;

function TBoldVariableTuple.GetEffectiveUseListElement: Boolean;
begin
  result := UseListElement and (BoldHandle is TBoldAbstractListHandle);
end;

function TBoldVariableTuple.GetTupleList: TBoldVariableTupleList;
begin
  result := Collection as TBoldVariableTupleList;
end;

function TBoldVariableTuple.LinksToHandle(Handle: TBoldElementHandle): Boolean;
begin
  result := assigned(BoldHandle) and (Handle = BoldHandle);
  if not result and (BoldHandle is TBoldRootedHandle) then
    result := (BoldHandle as TBoldRootedHandle).IsRootLinkedTo(Handle);
end;

procedure TBoldVariableTuple.SetBoldHandle(const Value: TBoldElementHandle);
begin
  if value <> fBoldHandle then
  begin
    if assigned(value) and Value.RefersToComponent(TupleList.OwningDefinition) then
      raise EBold.CreateFmt(sCircularReference, [classname, 'SetBoldHandle', TupleList.OwningDefinition.name, value.name]);
    FBoldHandle := Value;
    fBoldHandleSubscriber.CancelAllSubscriptions;
    if assigned(value) then
      Value.AddSmallSubscription(fBoldHandleSubscriber, [beDestroying], beDestroying);
    Changed;
  end;
end;

procedure TBoldVariableTuple.SetUseListElement(const Value: Boolean);
begin
  if Value <> fUseListElement then
  begin
    FUseListElement := Value;
    Changed;
  end;
end;

procedure TBoldVariableTuple.SetVariableName(const Value: String);
var
  vNewName: string;
begin
  if CompareText(FVariableName, Value) <> 0 then
  begin
    vNewName := LowerCase(Copy(Value,1,1)) + Copy(Value,2,MaxInt);
    if not (Collection as TBoldVariableTupleList).NameIsUnique(vNewName) then
      raise EBold.CreateFmt(sNameNotUnique, [vNewName]);
    if not (Collection as TBoldVariableTupleList).NameIsValid(vNewName) then
      raise EBold.Create(sIllegalCharsInName);
    FVariableName := vNewName;
    Changed;
  end;
end;

procedure TBoldVariableTuple._ReceiveHandleEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (originator = BoldHandle) and (requestedEvent = beDestroying) then
    BoldHandle := nil;
end;

{ TBoldHandleBasedExternalVariable }

procedure TBoldHandleBasedExternalVariable._ReceiveHandleEvent(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if (RequestedEvent = beDestroying) and (originator = fHandle) then
    fHandle := nil;
end;

constructor TBoldHandleBasedExternalVariable.Create(Name: String;
  Handle: TBoldElementHandle; UseListElement: Boolean);
begin
  if Assigned(Handle) and Assigned(Handle.BoldSystem) then
    inherited Create(Handle.BoldSystem.Evaluator , Name)
  else
    inherited Create(nil , Name);
  fHandle := Handle;
  fUseListElement := UseListElement;
  fHandleSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveHandleEvent);
  if assigned(fHandle) then
    fHandle.AddSmallSubscription(fHandleSubscriber, [beDestroying], beDestroying);
end;

destructor TBoldHandleBasedExternalVariable.Destroy;
begin
  FreeAndNil(fHandleSubscriber);
  inherited;                    
end;

function TBoldHandleBasedExternalVariable.GetValue: TBoldElement;
begin
  if assigned(fHandle) then
  begin
    if fUseListElement and (fHandle is TBoldAbstractListHandle) then
      result := (fHandle as TBoldAbstractListHandle).List
    else
      result := fHandle.value
  end
  else
    result := nil;
end;

function TBoldHandleBasedExternalVariable.GetValueType: TBoldElementTypeInfo;
begin
  if assigned(fHandle) then
  begin
    if fUseListElement and (fHandle is TBoldAbstractListHandle) then
      result := (fHandle as TBoldAbstractListHandle).StaticListType
    else
      result := fHandle.StaticBoldType
  end
  else
    result := nil;
end;

procedure TBoldHandleBasedExternalVariable.SetHandle(
  const Value: TBoldElementHandle);
begin
  if fHandle = Value then
    exit;
  fHandleSubscriber.CancelAllSubscriptions;
  fHandle := Value;
  if assigned(fHandle) then
    fHandle.AddSmallSubscription(fHandleSubscriber, [beDestroying], beDestroying);
end;

{ TBoldOclVariable }

constructor TBoldOclVariable.Create(const aName: string;
  aValue: TBoldElement{; aOwnsValue: boolean = false});
begin
  inherited Create(aValue.Evaluator, aName);
{  if aOwnsValue then
    BoldIndirectElement.SetOwnedValue(aValue)
  else
}
    BoldIndirectElement.SetReferenceValue(aValue);
end;

constructor TBoldOclVariable.CreateFromIndirectElement(const aName: string;
  ABoldIndirectElement: TBoldIndirectElement);
begin
  inherited Create(ABoldIndirectElement.Value.Evaluator, aName);
  aBoldIndirectElement.TransferValue(BoldIndirectElement);
end;

constructor TBoldOclVariable.CreateWithTypeInfo(const aName: string; AValue: TBoldElement;
  ABoldElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited Create(nil, aName);
  BoldIndirectElement.SetOwnedValue(AValue);
  fBoldElementTypeInfo := aBoldElementTypeInfo;
end;

function TBoldOclVariable.EnsureEvaluator(AEvaluator: TBoldEvaluator): TBoldEvaluator;
begin
  result := AEvaluator;
  if not Assigned(AEvaluator) and (TBoldAbstractSystemHandle.DefaultBoldSystemHandle <> nil) and TBoldAbstractSystemHandle.DefaultBoldSystemHandle.Active then
    result := TBoldAbstractSystemHandle.DefaultBoldSystemHandle.System.Evaluator;
end;

constructor TBoldOclVariable.CreateStringVariable(const aName, aValue: string; AEvaluator: TBoldEvaluator);
var
  vString: TBAString;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vString := TBAString.CreateWithTypeInfo(TBoldOcl(AEvaluator).StringType);
  vString.AsString := aValue;
  BoldIndirectElement.SetOwnedValue(vString);
end;

constructor TBoldOclVariable.CreateFloatVariable(const aName: string; const aValue: Double; AEvaluator: TBoldEvaluator);
var
  vFloat: TBAFloat;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vFloat := TBAFloat.Create;
  vFloat.AsFloat := aValue;
  BoldIndirectElement.SetOwnedValue(vFloat);
end;

constructor TBoldOclVariable.CreateTimeVariable(const aName: string; const aValue: TTime; AEvaluator: TBoldEvaluator);
var
  vTime: TBATime;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vTime := TBATime.CreateWithTypeInfo(TBoldOcl(AEvaluator).TimeType);
  vTime.AsTime := aValue;
  BoldIndirectElement.SetOwnedValue(vTime);
end;

constructor TBoldOclVariable.CreateIntegerVariable(const aName: string; const aValue: integer; AEvaluator: TBoldEvaluator);
var
  vInteger: TBAinteger;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vInteger := TBAInteger.CreateWithTypeInfo(TBoldOcl(AEvaluator).IntegerType);
  vInteger.Asinteger := aValue;
  BoldIndirectElement.SetOwnedValue(vInteger);
end;

constructor TBoldOclVariable.CreateDateTimeVariable(const aName: string; const aValue: TDateTime; AEvaluator: TBoldEvaluator);
var
  vDateTime: TBADateTime;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vDateTime := TBADateTime.CreateWithTypeInfo(TBoldOcl(AEvaluator).DateTimeType);
  vDateTime.AsDateTime := aValue;
  BoldIndirectElement.SetOwnedValue(vDateTime);
end;

constructor TBoldOclVariable.CreateDateVariable(const aName: string; const aValue: TDate; AEvaluator: TBoldEvaluator);
var
  vDate: TBADate;
begin
  AEvaluator := EnsureEvaluator(AEvaluator);
  inherited Create(AEvaluator, aName);
  vDate := TBADate.CreateWithTypeInfo(TBoldOcl(AEvaluator).DateType);
  vDate.AsDate := aValue;
  BoldIndirectElement.SetOwnedValue(vDate);
end;

destructor TBoldOclVariable.Destroy;
begin
  FreeAndNil(fBoldIndirectElement);
  inherited;
end;

function TBoldOclVariable.GetBoldIndirectElement: TBoldIndirectElement;
begin
  if not Assigned(fBoldIndirectElement) then
  begin
    fBoldIndirectElement := TBoldIndirectElement.Create;
  end;
  result := fBoldIndirectElement;
end;

function TBoldOclVariable.GetValue: TBoldElement;
begin
  result:= BoldIndirectElement.Value;
end;

function TBoldOclVariable.GetValueType: TBoldElementTypeInfo;
begin
  if GetValue <> nil then
    result:= GetValue.BoldType
  else
    result := fBoldElementTypeInfo;
end;

{ TBoldVariableTupleListEnumerator }

function TBoldVariableTupleListEnumerator.GetCurrent: TBoldVariableTuple;
begin
  result := inherited GetCurrent as TBoldVariableTuple; 
end;

end.
