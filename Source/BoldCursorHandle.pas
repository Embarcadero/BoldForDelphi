{ Global compiler directives }
{$include bold.inc}
unit BoldCursorHandle;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldAbstractListHandle;

type
  { forward declaration of classes }
  TBoldCursorHandle = class;

  { TBoldCursorHandle }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldCursorHandle = class(TBoldAbstractListHandle)
  private
    fAutoFirst: Boolean;
    fListElement: TBoldIndirectElement;    
    fCurrentIndex: Integer;
    fListSubscriber: TBoldPassThroughSubscriber;
    procedure SetAutoFirst(Value: Boolean);
    procedure _ReceiveFromList(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    function GetCurrentElement: TBoldElement; override;
    function GetCurrentIndex: Integer; override;
    function GetList: TBoldList; override;
    procedure SetCurrentIndex(NewIndex: Integer); override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    property ListElement: TBoldIndirectElement read fListElement;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoFirst: Boolean read fAutoFirst write SetAutoFirst default true;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  {$IFDEF ATTRACS}
  AttracsPerformance,
  AttracsDefs,
  AttracsTraceLog,
  {$ENDIF}
  BoldSystemRT;

{ TBoldCursorhandle }

constructor TBoldCursorhandle.Create(Owner: TComponent);
begin
  inherited;
  fListElement := TBoldIndirectElement.Create;
  fListSubscriber := TBoldPassThroughSubscriber.Create(_ReceiveFromList);
  fCurrentIndex := -1;
  AutoFirst := True;
end;

procedure TBoldCursorhandle.DeriveAndSubscribe(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);
var
  ListCount,
  IndexOfOldCurrent,
  NewCurrentIndex: integer;
  TheList: TBoldList;
  NewValue: TBoldElement;
  {$IFDEF ATTRACS}
  PerformanceMeasurement : TPerformanceMeasurement;
  HandleLongName, ListTypeName : String;
  {$ENDIF}
begin
  if csDestroying in ComponentState then
    raise EBold.CreateFmt('%s.DeriveAndSubscribe: %s Handle is in csDestroying state, can not DeriveAndSubscribe.', [classname, name]);
  {$IFDEF ATTRACS}
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
  {$ENDIF}
  fListSubscriber.CancelAllSubscriptions;

  if EffectiveRootValue = nil then
    fListElement.SetOwnedValue(nil)
  else
    EffectiveRootValue.GetAsList(fListElement);

  if assigned(fListElement.Value) and not fListElement.OwnsValue then
    fListElement.Value.AddSmallSubscription(fListSubscriber, [beDestroying], beDestroying);

  TheList := TBoldList(fListElement.Value);
  if Assigned(TheList) then
  begin
    ListCount := TheList.Count;
    IndexOfOldCurrent := TheList.IndexOf(ResultElement.Value)
  end
  else
  begin
    ListCount := 0;
    IndexOfOldCurrent := -1;
  end;

  if IndexOfOldCurrent <> -1 then
    NewCurrentIndex := IndexOfOldCurrent
  else if fCurrentIndex >= ListCount then
     NewCurrentIndex := ListCount - 1
  else if (fCurrentIndex < 0) and AutoFirst and (ListCount > 0) then
    NewCurrentIndex := 0
  else
    NewCurrentIndex := fCurrentIndex;

  fCurrentIndex := NewCurrentIndex;

  if (fCurrentIndex = -1) then
    NewValue := nil
  else
    NewValue := TheList[fCurrentIndex];
  if (NewValue <> resultElement.value) then
  begin
    ResultElement.SetReferenceValue(NewValue);
    SubscribeToValue;
  end;

  if Assigned(subscriber) and (EffectiveRootValue <> nil) then
    EffectiveRootValue.DefaultSubscribe(Subscriber, breReEvaluate);
  {$IFDEF ATTRACS}
  if not PerformanceMeasurement.AcceptableTimeForSmallComputation then
  begin
    if Assigned(Self.Owner) then
      HandleLongName := Owner.Name + '.' + Self.Name
    else
      HandleLongName := Self.Name;

    if Assigned(ListType) then
      ListTypeName :=  ListType.ModelName
    else
      ListTypeName := 'Unknown';

     PerformanceMeasurement.WhatMeasured := 'Deriving TBoldExpressionHandle ' + HandleLongName;
     PerformanceMeasurement.WhatMeasuredParameter := Format('a list of type %s(%d)',[ListTypeName, ListCount]);
     PerformanceMeasurement.Trace;
  end; // if not AcceptableTimeForSmallComputation
  {$ENDIF}
end;

destructor TBoldCursorhandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fListSubscriber);
  FreeAndNil(fListElement);
  inherited;
end;

function TBoldCursorhandle.GetCurrentElement: TBoldElement;
begin
  if Enabled then
  begin
    EnsureCurrent;
    Result := ResultElement.Value;
  end
  else
    result := nil;
end;

function TBoldCursorhandle.GetCurrentIndex: Integer;
begin
  if Enabled then
  begin
    EnsureCurrent;
    Result := FCurrentIndex;
  end
  else
    result := -1;
end;

function TBoldCursorhandle.GetList: TBoldList;
begin
  if Enabled then
  begin
    EnsureCurrent;
    Assert((fListElement.Value is TBoldList) or not Assigned(fListElement.Value));
    Result := TBoldList(fListElement.Value);
  end
  else
    result := nil;
end;

function TBoldCursorhandle.GetStaticBoldType: TBoldElementTypeInfo;
var
  SRType: TBoldElementTypeInfo;
begin
  SRType := StaticRootType;
  if SRType is TBoldListTypeInfo then
    Result := TBoldListTypeInfo(SRType).ListElementTypeInfo
  else
    Result := SRType;
end;

procedure TBoldCursorhandle._ReceiveFromList(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  Assert(OriginalEvent = beDestroying);
  if not (isDeriving) then
  begin
    fListElement.SetReferenceValue(nil);
    fListSubscriber.CancelAllSubscriptions;
    ValueIdentityChanged;
  end;
end;

procedure TBoldCursorhandle.SetAutoFirst(Value: Boolean);
begin
  FAutoFirst := Value;
  if Value and (CurrentIndex < 0) and (Count > 0) then
    CurrentIndex := 0;
end;

procedure TBoldCursorhandle.SetCurrentIndex(NewIndex: Integer);
var
  NewValue: TBoldElement;
begin
  if (NewIndex < -1) or (NewIndex >= Count) then // -1 accepted as "no current element"
    raise EBold.CreateFmt(sIndexOutOfBounds, [ClassName, Count-1, NewIndex]);
  if (NewIndex = -1) then
    NewValue := nil
  else
    NewValue := List[NewIndex];

  if (NewIndex <> CurrentIndex) or (NewValue <> resultElement.value) then
  begin
    fCurrentIndex := NewIndex;
    ResultElement.SetReferenceValue(NewValue);
    SubscribeToValue;
    ValueIdentityChanged;
  end;
end;

end.
