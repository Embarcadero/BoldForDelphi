
{ Global compiler directives }
{$include bold.inc}
unit BoldReferenceHandle;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldHandles,
  BoldSystem;

type
  { forward declaration of classes }
  TBoldReferenceHandle = class;

  { TBoldReferenceHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldReferenceHandle = class(TBoldNonSystemHandle)
  private
    fStaticValueTypeName: String;
    fValue: TBoldElement;
    fValueSubscriber: TBoldPassthroughSubscriber;
    FOnValueDestroyed: TNotifyEvent;
    FOnObjectDeleted: TNotifyEvent;
    procedure SetStaticValueTypeName(Value: string);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetValue: TBoldElement; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    function GetStaticSystemHandle: TBoldAbstractSystemHandle; override;
    function GetBoldSystem: TBoldSystem; override;
    procedure SetValue(NewValue: TBoldElement); override;
    function GetCanSetValue: boolean; override;
    procedure DoAssign(Source: TPersistent); override;
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
  BoldSystemRT,
  BoldDomainElement;

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

procedure TBoldReferenceHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldReferenceHandle then with TBoldReferenceHandle(Source) do
  begin
    self.StaticValueTypeName := StaticValueTypeName;
    // do we want to assign these events ?
    self.OnObjectDeleted := OnObjectDeleted;
    self.OnValueDestroyed := OnValueDestroyed;
  end;
end;

function TBoldReferenceHandle.GetBoldSystem: TBoldSystem;
begin
  if Value is TBoldDomainElement then
    result := TBoldDomainElement(Value).BoldSystem as TBoldSystem
  else
    result := inherited;
end;

function TBoldReferenceHandle.GetCanSetValue: boolean;
begin
  result := true;
end;

function TBoldReferenceHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if Assigned(StaticSystemTypeInfo) and (StaticValueTypeName <> '') then
    Result := StaticSystemTypeInfo.ElementTypeInfoByExpressionName[StaticValueTypeName]
  else
    Result := nil;
end;

function TBoldReferenceHandle.GetStaticSystemHandle: TBoldAbstractSystemHandle;
var
  System: TBoldSystem;
begin
  result := inherited GetStaticSystemHandle;
  if Assigned(result) or not Assigned(fValue) then
    exit;
  System := nil;
  if fValue is TBoldSystem then
    System := TBoldSystem(fValue)
  else
  if fValue is TBoldObject then
    System := TBoldObject(fValue).BoldSystem
  else
  if fValue is TBoldMember then
    System := TBoldMember(fValue).BoldSystem;
  result := TBoldAbstractSystemHandle.FindSystemHandleForSystem(System);
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
    if Assigned(fValue) and (not (fValue is TBoldSystem)) then
    begin
      fValue.AddSmallSubscription(fValueSubscriber, [beDestroying], breValueDestroyed);
      if (fValue is TBoldObject) then
        fValue.AddSmallSubscription(fValueSubscriber, [beObjectDeleted], breObjectDeleted)
    end;
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

procedure TBoldReferenceHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (requestedEvent = breObjectDeleted) and (Originator = Value) then
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