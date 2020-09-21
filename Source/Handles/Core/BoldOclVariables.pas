unit BoldOclVariables;

interface

uses
  Classes,
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
    fhandleSubscriber: TBoldPassThroughSubscriber;
    procedure _ReceiveHandleEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetValue: TBoldElement; override;
    function GetValueType: TBoldElementTypeInfo; override;
  public
    constructor Create(Name: String; Handle: TBoldElementHandle; UseListElement: Boolean);
    destructor Destroy; override;
  end;

  { TBoldOclVariables }
  TBoldOclVariables = class(TBoldSubscribableComponent)
  private
    fVariableTupleList: TBoldVariableTupleList;
    fVariableList: TBoldExternalVariableList;
    procedure SetVariableTupleList(const Value: TBoldVariableTupleList);
    procedure VariablesChanged;
    function GetVariableList: TBoldExternalVariableList;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure SubscribeToHandles(Subscriber: TBoldSubscriber);
    function LinksToHandle(Handle: TBoldElementHandle): Boolean;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  published
    property Variables: TBoldVariableTupleList read fVariableTupleList write SetVariableTupleList;
  end;

  { TBoldVariableTupleList }
  TBoldVariableTupleList = class(TCollection)
  private
    fOwningDefinition: TBoldOclVariables;
    function GetItems(Index: integer): TBoldVariableTuple;
  protected
    function GetOwner: TPersistent; override;
    property OwningDefinition: TBoldOclVariables read fOwningDefinition;
  public
    constructor Create(aOwningDefinition: TBoldOclVariables);
    function NameIsUnique(Name: String): Boolean;
    function NameIsValid(Name: String): Boolean;
    function GetUniqueName: String;
    property Items[Index: integer]: TBoldVariableTuple read GetItems; default;
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
    property UseListElement: Boolean read fUseListElement write SetUseListElement;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldRootedHandles,
  HandlesConst,
  BoldAbstractListHandle;

{ TBoldOclVariables }

constructor TBoldOclVariables.create(Owner: TComponent);
begin
  inherited;
  fVariableList := nil;
  fVariableTupleList := TBoldVariableTupleList.Create(self);
end;

destructor TBoldOclVariables.Destroy;
begin
  FreePublisher;
  FreeAndNil(fVariableList);
  FreeAndNil(fVariableTupleList);
  inherited;
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

function TBoldOclVariables.LinksToHandle(Handle: TBoldElementHandle): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Variables.Count-1 do
    result := result or Variables[i].linksToHandle(Handle);
end;

procedure TBoldOclVariables.SetVariableTupleList(
  const Value: TBoldVariableTupleList);
begin
  fVariableTupleList := Value;
end;

procedure TBoldOclVariables.SubscribeToHandles(
  Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  if assigned(Subscriber) then
    for i := 0 to Variables.Count-1 do
      if assigned(Variables[i].boldHandle) then
        Variables[i].BoldHandle.AddSubscription(Subscriber, beValueIdentityChanged, breResubscribe);
end;

procedure TBoldOclVariables.VariablesChanged;
begin
  FreeAndNil(fVariableList);
  SendEvent(Self, beValueChanged);
end;

{ TBoldVariableTupleList }

constructor TBoldVariableTupleList.create(aOwningDefinition: TBoldOclVariables);
begin
  inherited Create(TBoldVariableTuple);
  fOwningDefinition := aOwningDefinition;
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
    result := 'Variable' + IntToStr(i); // do not localize
    Inc(i);
  until NameIsUnique(result);
end;

function TBoldVariableTupleList.NameIsUnique(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to Count-1 do
    if AnsiCompareStr(Name, Items[i].VariableName) = 0 then
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
    if not (name[i] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
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
    result := result + ': Not Connected';  // do not localize
  if EffectiveUseListElement then
    result := result + ' (list)'; // do not localize
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
      raise EBold.CreateFmt(sCircularReference, [classname, TupleList.OwningDefinition.name, value.name]);
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
begin
  if FVariableName <> Value then
  begin
    if not (Collection as TBoldVariableTupleList).NameIsUnique(Value) then
      raise EBold.CreateFmt(sNameNotUnique, [Value]);
    if not (Collection as TBoldVariableTupleList).NameIsValid(Value) then
      raise EBold.Create(sIllegalCharsInName);
    FVariableName := Value;
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
  inherited Create(Name);
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

end.
