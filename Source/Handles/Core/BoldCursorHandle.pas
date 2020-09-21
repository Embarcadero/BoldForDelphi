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
  TBoldCursorHandle = class(TBoldAbstractListHandle)
  private
    fAutoFirst: Boolean;
    fListElement: TBoldIndirectElement;
    fCurrentIndex: Integer;
    fListSubscriber: TBoldPassThroughSubscriber;
    procedure SetAutoFirst(Value: Boolean);
//    function GetListSubscriber: TBoldPassThroughSubscriber;
    procedure _ReceiveFromList(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    function GetCurrentElement: TBoldElement; override;
    function GetCurrentIndex: Integer; override;
    function GetList: TBoldList; override;
    procedure SetCurrentIndex(NewIndex: Integer); override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
//    property ListSubscriber: TBoldPassThroughSubscriber read GetListSubscriber;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoFirst: Boolean read fAutoFirst write SetAutoFirst default true;
  end;

implementation

uses
  SysUtils,
  HandlesConst,
  BoldDefs,
  BoldSystemRT;

{ TBoldCursorhandle }

constructor TBoldCursorhandle.Create(Owner: TComponent);
begin
  inherited;
  fListElement := TBOldIndirectElement.Create;
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
begin
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
    IndexOfOldCurrent := TheList.IndexOf(ResultElement.Value) // Don't ensure current!
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
    ValueIdentityChanged;   // changing index is an identitychange
  end;
end;

end.
