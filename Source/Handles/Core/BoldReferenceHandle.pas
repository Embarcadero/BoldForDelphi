unit BoldReferenceHandle;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldHandles;

type
  { forward declaration of classes }
  TBoldReferenceHandle = class;

  { TBoldReferenceHandle }
  TBoldReferenceHandle = class(TBoldNonSystemHandle)
  private
    fStaticValueTypeName: String;
    fValue: TBoldElement;
    fValueSubscriber: TBoldPassthroughSubscriber;
    FOnValueDestroyed: TNotifyEvent;
    FOnObjectDeleted: TNotifyEvent;
    procedure SetValue(NewValue: TBoldElement);
    procedure SetStaticValueTypeName(Value: string);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetValue: TBoldElement; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Value: TBoldElement read GetValue write SetValue;
  published
    property StaticValueTypeName: string read fStaticValueTypeName write SetStaticValueTypeName;
    property OnObjectDeleted: TNotifyEvent read FOnObjectDeleted write FOnObjectDeleted;
    property OnValueDestroyed: TNotifyEvent read FOnValueDestroyed write FOnValueDestroyed;
  end;

implementation

uses
  SysUtils,
  BoldSystemRT;

const
  breValueDestroyed = 42;
  breObjectDeleted = 43;

{ TBoldReferenceHandle }

constructor TBoldReferenceHandle.Create(Owner: TComponent);
begin
  inherited;
  fValueSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

destructor TBoldReferenceHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fValueSubscriber);
  inherited;
end;

function TBoldReferenceHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if Assigned(StaticSystemTypeInfo) then
    Result := StaticSystemTypeInfo.ElementTypeInfoByExpressionName[StaticValueTypeName]
  else
    Result := nil;
end;

function TBoldReferenceHandle.GetValue: TBoldElement;
begin
  result := fValue;
end;

procedure TBoldReferenceHandle.SetStaticValueTypeName(Value: string);
begin
  if Value <> StaticValueTypeName then
  begin
    fStaticValueTypeName := Value;
    StaticBoldTypeChanged;
  end;
end;

procedure TBoldReferenceHandle.SetValue(NewValue: TBoldElement);
begin
  if fValue <> NewValue then
  begin
    fValue := NewValue;
    fValueSubscriber.CancelAllSubscriptions;
    if Assigned(fValue) then
    begin
      fValue.AddSmallSubscription(fValueSubscriber, [beDestroying], breValueDestroyed);
      fValue.AddSmallSubscription(fValueSubscriber, [beObjectDeleted], breObjectDeleted);
    end;
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

procedure TBoldReferenceHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if requestedEvent = breObjectDeleted then
  begin
    if assigned(OnObjectDeleted) then
      OnObjectDeleted(self)
    else
      Value := nil;
  end;

  if requestedEvent = breValueDestroyed then
  begin
    Value := nil;
    if assigned(OnValueDestroyed) then
      OnValueDestroyed(self);
  end;
end;

end.
