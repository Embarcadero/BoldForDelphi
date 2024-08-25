{ Global compiler directives }
{$include bold.inc}
unit BoldListHandle;

interface

uses
  Classes,
  BoldSystem,
  BoldDefs,
  BoldElements,
  BoldHandles,
  BoldExpressionHandle,
  BoldSubscription,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldCursorHandle,
  BoldOclVariables;

type
  {---Forward declarations---}
  TBoldListHandle = class;       

  {---TBoldListHandle---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldListHandle = class(TBoldCursorHandle, IBoldOCLComponent)
  private
    fExpressionHandle: TBoldExpressionHandle;
    fFilterHandle: TBoldFilteredHandle;
    fSorterHandle: TBoldSortedHandle;
    FInternalRootHandle: TBoldSortedHandle;
    fMutableListExpression: TBoldExpression;
    fUsePrefetch: boolean;
    function GetContextType: TBoldElementTypeInfo;
    function GetComparer: TBoldComparer;
    function GetExpression: TBoldExpression;
    function GetFilter: TBoldFilter;
    procedure SetComparer(Value: TBoldComparer);
    procedure SetExpression(const Value: TBoldExpression);
    procedure SetFilter(Value: TBoldFilter);
    procedure SetVariables(Value: TBoldOclVariables);
    function GetEvaluateInPS: Boolean;
    procedure SetEvaluateInPS(const Value: Boolean);
    procedure SetMutableListExpression(const Value: TBoldExpression);
    procedure FixupRoots;
    function GetUsePrefetch: Boolean;
    procedure SetUsePrefetch(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetSubscribe(Value: Boolean); override;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); override;
    procedure Loaded; override;
    function GetRootHandle: TBoldElementHandle; override;
    procedure SetRootHandle(const Value: TBoldElementHandle); override;
    procedure SetRootTypeName(Value: string); override;
    function GetMutableList: TBoldList; override;
    procedure DoAssign(Source: TPersistent); override;
    function GetVariables: TBoldOclVariables; virtual;
    function GetVariableList: TBoldExternalVariableList; virtual;
    function GetExpressionHandleClass: TBoldExpressionHandleClass; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure MarkOutOfDate; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  published
    property BoldComparer: TBoldComparer read GetComparer write SetComparer;
    property BoldFilter: TBoldFilter read GetFilter write SetFilter;
    property Expression: TBoldExpression read GetExpression write SetExpression;
    property Variables: TBoldOclVariables read GetVariables write SetVariables;
    property EvaluateInPS: Boolean read GetEvaluateInPS write SetEvaluateInPS default false;
    property MutableListExpression: TBoldExpression read fMutableListExpression write SetMutableListExpression;
    property UsePrefetch: Boolean read GetUsePrefetch write SetUsePrefetch default true;
   end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldRootedHandles;

type
  THackSortedHandle = class(TBoldSortedHandle);

{---TBoldListHandle---}
constructor TBoldListHandle.Create(Owner: TComponent);
begin
  inherited;
  fExpressionHandle := GetExpressionHandleClass.Create(self);
  FInternalRootHandle := TBoldSortedHandle.Create(nil);
  FixupRoots;
  UsePrefetch := true;
end;

destructor TBoldListHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fSorterHandle);
  FreeAndNil(fFilterHandle);
  FreeAndNil(fExpressionHandle);
  FreeAndNil(FInternalRootHandle);
  inherited;
end;

procedure TBoldListHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldListHandle then with TBoldListHandle(Source) do
  begin
    self.BoldComparer := BoldComparer;
    self.BoldFilter := BoldFilter;
    self.Expression := Expression;
    self.Variables := Variables;
    self.EvaluateInPS := EvaluateInPS;
    self.MutableListExpression := MutableListExpression;
  end;
end;

procedure TBoldListHandle.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = BoldFilter) then
    BoldFilter := nil;
  if (Operation = opRemove) and (AComponent = BoldComparer) then
    BoldComparer := nil;
end;

procedure TBoldListHandle.SetSubscribe(Value: Boolean);
begin
  fExpressionHandle.Subscribe:= Value;
  if Assigned(fFilterHandle) then
    fFilterHandle.Subscribe := Value;
  if Assigned(fSorterHandle) then
    fSorterHandle.Subscribe := Value;
  FInternalRootHandle.Subscribe := Value;
  inherited;
end;

procedure TBoldListHandle.MarkOutOfDate;
begin
  fExpressionHandle.MarkOutOfDate;
  if Assigned(fFilterHandle) then
    fFilterHandle.MarkOutOfDate;
  if Assigned(fSorterHandle) then
    fSorterHandle.MarkOutOfDate;
  FInternalRootHandle.MarkOutOfDate;
  inherited;
end;

procedure TBoldListHandle.FixupRoots;
var
  NextHandle: TBoldElementHandle;
begin
  NextHandle := fExpressionHandle;
  fExpressionHandle.Name := Name + '_expr';

  if Assigned(fFilterHandle) then
  begin
    fFilterHandle.RootHandle := NextHandle;
    fFilterHandle.Name := Name + '_filt';
    NextHandle := fFilterHandle;
  end;
  if Assigned(fSorterHandle) then
  begin
    FSorterHandle.RootHandle := NextHandle;
    fSorterHandle.Name := Name + '_sort';
    NextHandle := FSorterHandle;
  end;

  FInternalRootHandle.RootHandle := NextHandle;
  FInternalRootHandle.Name := Name + '_root'; // do not localize
  NextHandle := FInternalRootHandle;

  InternalRootHandle := NextHandle;
end;

procedure TBoldListHandle.SetExpression(const Value: TBoldExpression);
begin
  if value <> fExpressionHandle.Expression then
  begin
    fExpressionHandle.Expression := Value;
    StaticBoldTypeChanged;
  end;
end;

function TBoldListHandle.GetExpression: TBoldExpression;
begin
  Result := fExpressionHandle.Expression;
end;

function TBoldListHandle.GetExpressionHandleClass: TBoldExpressionHandleClass;
begin
  result := TBoldExpressionHandle;
end;

procedure TBoldListHandle.SetFilter(Value: TBoldFilter);
begin
  if Value = BoldFilter then
    Exit;
  if Value = nil then
  begin
    if Assigned(fFilterHandle) then
    begin
      if Assigned(FSorterHandle) then begin
        FSorterHandle.RootHandle := FExpressionHandle;
      end else begin
        FInternalRootHandle.RootHandle := FExpressionHandle;
      end;
      FreeAndNil(fFilterHandle);
    end
  end
  else
  begin
    if not Assigned(fFilterHandle) then
      fFilterHandle := TBoldFilteredHandle.Create(nil);
    fFilterHandle.BoldFilter := Value;
    Value.FreeNotification(Self);
  end;
  FixupRoots;
end;

function TBoldListHandle.GetFilter: TBoldFilter;
begin
  if Assigned(fFilterHandle) then
    Result := fFilterHandle.BoldFilter
  else
    Result := nil;
end;

procedure TBoldListHandle.SetComparer(Value: TBoldComparer);
begin
  if Value = BoldComparer then begin
    if Assigned(FSorterHandle) then begin
      THackSortedHandle(FSorterHandle).MarkSubscriptionOutOfdate;
    end;
    Exit;
  end;
  if Value = nil then
  begin
    if Assigned(FSorterHandle) then
    begin
      if Assigned(FFilterHandle) then begin
        FInternalRootHandle.RootHandle := FFilterHandle;
      end else begin
        FInternalRootHandle.RootHandle := FExpressionHandle;
      end;
      FreeAndNil(fSorterHandle);
    end
  end
  else
  begin
    if not Assigned(fSorterHandle) then
      fSorterHandle := TBoldSortedHandle.Create(nil);
    fSorterHandle.BoldComparer := Value;
    Value.FreeNotification(Self);
  end;
  FixupRoots;
end;

function TBoldListHandle.GetComparer: TBoldComparer;
begin
  if Assigned(fSorterHandle) then
    Result := fSorterHandle.BoldComparer
  else
    Result := nil;
end;

{procedure TBoldListHandle.ValidateComponent(
  Helper: TBoldComponentValidationHelper);
begin
  helper.ValidateOclWithContext(self, StaticRootType, expression);
end;
}

function TBoldListHandle.GetRootHandle: TBoldElementHandle;
begin
  Result := fExpressionHandle.RootHandle;
end;

procedure TBoldListHandle.SetUsePrefetch(const Value: Boolean);
begin
  fUsePrefetch := Value;
  fExpressionHandle.UsePrefetch := Value;
end;

function TBoldListHandle.GetUsePrefetch: Boolean;
begin
  result := fUsePrefetch;
end;

procedure TBoldListHandle.SetRootHandle(const Value: TBoldElementHandle);
begin
  fExpressionHandle.RootHandle := Value;
end;

procedure TBoldListHandle.SetRootTypeName(Value: string);
begin
  inherited;
  fExpressionHandle.RootTypeName := Value;
  fInternalROotHandle.RootTypeName := Value;
  StaticBoldTypeChanged;
end;

procedure TBoldListHandle.SetStaticSystemHandle(
  Value: TBoldAbstractSystemHandle);
begin
  inherited;
  fExpressionHandle.StaticSystemHandle := Value;
end;

function TBoldListHandle.GetContextType: TBoldElementTypeInfo;
begin
  result := fExpressionHandle.StaticRootType;
end;

procedure TBoldListHandle.Loaded;
begin
  inherited;
  FixupRoots;
end;

function TBoldListHandle.GetVariables: TBoldOclVariables;
begin
  Result := fExpressionHandle.Variables;
end;

procedure TBoldListHandle.SetVariables(Value: TBoldOclVariables);
begin
  if assigned(value) and value.LinksToHandle(self) then
    raise EBold.CreateFmt(sCircularReference, [classname, 'SetVariables', name, value.name]);
  fExpressionHandle.Variables := Value;
end;

function TBoldListHandle.GetEvaluateInPS: Boolean;
begin
  result := fExpressionHandle.EvaluateInPS;
end;

procedure TBoldListHandle.SetEvaluateInPS(const Value: Boolean);
begin
  if value <> fExpressionHandle.EvaluateInPS then
  begin
    fExpressionHandle.EvaluateInPS := Value;
    MarkOutOfDate;
  end;
end;

function TBoldListHandle.GetMutableList: TBoldList;
var
  ie: TBoldIndirectElement;
begin
  ie := TBoldIndirectElement.create;
  // if we have a sorterhandle or a filterhandle, we are sure that the inherited
  // result is not mutable... no need to ask.
  if assigned(fSorterHandle) or assigned(fFilterHandle) then
    result := nil
  else
    result := inherited GetMutableList;

  try
    // try to get the value before the comparer
    if not assigned(result) and
       assigned(fSorterHandle)
       and assigned(fSorterHandle.RootHandle)
       and assigned(fSorterHandle.RootHandle.Value) then
    begin
      fSorterHandle.RootHandle.Value.GetAsList(ie);
      if not ie.OwnsValue and assigned(ie.value) and ie.value.Mutable then
        result := ie.value as TBoldList;
    end;
    // try to get the value given by MutableListExpression
    if not assigned(result) and
      (MutableListExpression <> '') and
      assigned(rootHandle) and
      assigned(rootHandle.value) then
    begin
      ie.SetReferenceValue(rootHandle.Value.EvaluateExpressionAsDirectElement(MutableListExpression));
      if assigned(ie.value) then
        ie.value.GetAsList(ie);
      if not ie.OwnsValue and assigned(ie.value) and ie.value.Mutable then
        result := ie.value as TBoldList;
    end;
  finally
    ie.Free;
  end;
end;

procedure TBoldListHandle.SetMutableListExpression(
  const Value: TBoldExpression);
begin
  fMutableListExpression := Value;
end;

function TBoldListHandle.GetVariableList: TBoldExternalVariableList;
begin
  if assigned(variables) then
    result := variables.VariableList
  else
    result := nil;
end;

function TBoldListHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = variables;
end;

end.
