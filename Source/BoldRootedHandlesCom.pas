
{ Global compiler directives }
{$include bold.inc}
unit BoldRootedHandlesCom;

interface

uses
  Classes,
  BoldSubscription,
  BoldComObjectSpace_TLB,
  BoldComClientHandles,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldRootedHandleCom = class;

  {-- TBoldRootedHandleCom --}
  TBoldRootedHandleCom = class(TBoldNonSystemHandleCom)
  private
    FRootHandle: TBoldElementHandleCom;
    FRootHandleSubscriber: TBoldPassthroughSubscriber;
    function GetEnabled: Boolean;
    function GetRootHandleSubscriber: TBoldPassthroughSubscriber;
    function GetRootTypeName: string;
    function GetStaticRootType: IBoldElementTypeInfo;
    function GetSubscribe: Boolean;
    procedure RootHandleSubscriber_Receive(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetEnabled(Value: Boolean);
    procedure SetRootHandle(Value: TBoldElementHandleCom);
    procedure SetRootTypeName(const Value: string);
    procedure SetSubscribe(Value: Boolean);
    property RootHandleSubscriber: TBoldPassthroughSubscriber read GetRootHandleSubscriber;
  protected
    FEnabled: Boolean;
    FRootTypeName: string;
    FStaticRootType: IBoldElementTypeInfo;
    FSubscribe: Boolean;
    function GetEffectiveActive: Boolean; override;
    function GetEffectiveConnectionHandle: TBoldComConnectionHandle; override;
    procedure SetConnectionHandle(Value: TBoldComConnectionHandle); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function IsRootLinkedTo(Handle: TBoldElementHandleCom): Boolean;
    property StaticRootType: IBoldElementTypeInfo read GetStaticRootType;
    property Subscribe: Boolean read GetSubscribe write SetSubscribe default True;
 published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property RootHandle: TBoldElementHandleCom read FRootHandle write SetRootHandle;
    property RootTypeName: string read GetRootTypeName write SetRootTypeName;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldComClient;

const
  breFreeHandle = 42;
  
{-- TBoldRootedHandleCom ------------------------------------------------------}

constructor TBoldRootedHandleCom.Create(Owner: TComponent);
begin
  inherited;
  FEnabled := True;
  FSubscribe := True;
end;

destructor TBoldRootedHandleCom.Destroy;
begin
  FreeAndNil(FRootHandleSubscriber);
  inherited;
end;

function TBoldRootedHandleCom.GetEffectiveActive: Boolean;
begin
  Result := Active;
  if Assigned(RootHandle) then
    Result := Result and RootHandle.EffectiveActive;
end;

function TBoldRootedHandleCom.GetEffectiveConnectionHandle: TBoldComConnectionHandle;
begin
  Result := ConnectionHandle;
  if not Assigned(Result) and Assigned(RootHandle) then
    Result := RootHandle.EffectiveConnectionHandle;
end;

function TBoldRootedHandleCom.GetEnabled: Boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FEnabled;
end;

function TBoldRootedHandleCom.GetRootHandleSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(FRootHandleSubscriber) then
    FRootHandleSubscriber := TBoldPassthroughSubscriber.Create(RootHandleSubscriber_Receive);
  Result := FRootHandleSubscriber;
end;

function TBoldRootedHandleCom.GetRootTypeName: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FRootTypeName;
end;

function TBoldRootedHandleCom.GetStaticRootType: IBoldElementTypeInfo;
begin
  EnsureCurrent;
  Result := FStaticRootType;
end;

function TBoldRootedHandleCom.GetSubscribe: Boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FSubscribe;
end;

function TBoldRootedHandleCom.IsRootLinkedTo(Handle: TBoldElementHandleCom): Boolean;
begin
  Assert(Assigned(Handle));
  if Handle = RootHandle then
    Result := True
  else if RootHandle is TBoldRootedHandleCom then
    Result := TBoldRootedHandleCom(RootHandle).IsRootLinkedTo(Handle)
  else
    Result := False;
end;

procedure TBoldRootedHandleCom.RootHandleSubscriber_Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = RootHandle then
  begin
    Assert(RequestedEvent in [breFreeHandle]);
    case RequestedEvent of
      breFreeHandle:
        RootHandle := nil;
    end;
  end;
end;

procedure TBoldRootedHandleCom.SetConnectionHandle(Value: TBoldComConnectionHandle);
begin
  if (Value <> ConnectionHandle) then
  begin
    inherited;
    if not Assigned(ConnectionHandle) then
    begin
      if Assigned(RootHandle) then
      begin
        RootHandle.AddSmallSubscription(ConnectionSubscriber,[bceHandleInit,bceHandleTerm],0);
        if Connected then
          DoConnect;
      end;
    end;
  end;
end;

procedure TBoldRootedHandleCom.SetRootHandle(Value: TBoldElementHandleCom);
begin
  if (Value <> RootHandle) then
  begin
    if (Value = Self) or ((Value is TBoldRootedHandleCom) and TBoldRootedHandleCom(Value).IsRootLinkedTo(Self)) then
      raise EBold.Create('Circular reference in RootHandle');
    RootHandleSubscriber.CancelAllSubscriptions;
    if not Assigned(ConnectionHandle) then
    begin
      ConnectionSubscriber.CancelAllSubscriptions;
      if Connected then
        DoDisconnect;
    end;
    FRootHandle := Value;
    if Assigned(RootHandle) then
    begin
      RootHandle.AddSmallSubscription(RootHandleSubscriber,[beDestroying], breFreeHandle);
      if not Assigned(ConnectionHandle) then
      begin
        RootHandle.AddSmallSubscription(ConnectionSubscriber,[bceHandleInit,bceHandleTerm],0);
        if Connected then
          DoConnect;
      end;
    end;
    LocalValueChanged;
  end;
end;

procedure TBoldRootedHandleCom.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('Enabled is read-only');
    FEnabled := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldRootedHandleCom.SetSubscribe(Value: Boolean);
begin
  if FSubscribe <> Value then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('Subscribe is read-only');
    FSubscribe := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldRootedHandleCom.SetRootTypeName(const Value: string);
begin
  if Value <> RootTypeName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('RootTypeName is read-only');
    FRootTypeName := Value;
    LocalValueChanged;
  end;
end;

end.
