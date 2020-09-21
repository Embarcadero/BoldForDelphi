unit BoldDeriver;

interface

uses
  BoldDefs,
  BoldSubscription;

type
  { forward declarations }
  TBoldAbstractDeriver = class;
  TBoldEventPluggedDeriver = class;
  TBoldDeriver = class;

  { method prototypes }
  TBoldDeriveAndResubscribe = procedure (DerivedObject: TObject; Subscriber: TBoldSubscriber) of object;
  TBoldReverseDerive = procedure (DerivedObject: TObject) of object;

  { enumerations }
  TBoldDeriverState = (bdsCurrent, bdsOutOfDate, bdsSubscriptionOutOfDate, bdsDeriving, bdsDerivingAndSubscribing, bdsReverseDeriving, bdsReverseDerivingSubscriptionOutOfDate);

  { TBoldAbstractDeriver }
  TBoldAbstractDeriver = class(TBoldSubscriber)
  private
    fDerivedObject: TObject;
    fSubscribe: Boolean;
    procedure SetDeriverState(Value: TBoldDeriverState);
    procedure SetSubscribe(value: boolean);
    function GetIsDeriving: Boolean;
  protected
    procedure SetInternalDeriverState(const Value: TBoldDeriverState); virtual; abstract;
    function GetInternalDeriverState: TBoldDeriverState; virtual; abstract;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    property DeriverState: TBoldDeriverState read GetInternalDeriverState write SetDeriverState;
    procedure DoDeriveAndSubscribe(subscribe: Boolean); virtual; abstract;
    procedure DoNotifyOutOfDate; virtual;
    procedure DoReverseDerive; virtual;
    function GetCanReverseDerive: Boolean; virtual; abstract;
    property InternalDeriverState: TBoldDeriverState read GetInternalDeriverState write SetInternalDeriverState;
  public
    constructor Create(DerivedObject: TObject);
    procedure MarkSubscriptionOutOfdate;
    procedure MarkOutOfdate;
    procedure EnsureCurrent;
    procedure Derive;
    procedure ReverseDerive;
    property DerivedObject: TObject read fDerivedObject;
    property Subscribe: Boolean read fSubscribe write SetSubscribe;
    property IsDeriving: Boolean read GetIsDeriving;
    property CanReverseDerive: Boolean read GetCanReverseDerive;
  end;

  { TBoldEventPluggedDeriver }
  TBoldEventPluggedDeriver = class(TBoldAbstractDeriver)
  private
    fOnDeriveAndSubscribe: TBoldDeriveAndResubscribe;
    fOnReverseDerive: TBoldReverseDerive;
    fOnNotifyOutOfDate: TBoldJustNotifyEvent;
  protected
    procedure DoDeriveAndSubscribe(subscribe: Boolean); override;
    procedure DoNotifyOutOfDate; override;
    procedure DoReverseDerive; override;
    function GetCanReverseDerive: Boolean; override;
  public
    property OnDeriveAndSubscribe: TBoldDeriveAndResubscribe read fOnDeriveAndSubscribe write fOnDeriveAndSubscribe;
    property OnReverseDerive: TBoldReverseDerive read fOnReverseDerive write fOnReverseDerive;
    property OnNotifyOutOfdate: TBoldJustNotifyEvent read fOnNotifyOutOfDate write fOnNotifyOutOfDate;
  end;

  { TBoldDeriver }
  TBoldDeriver = class(TBoldEventPluggedDeriver)
  private
    fInternalDeriverState: TBoldDeriverState;
  protected
    procedure SetInternalDeriverState(const Value: TBoldDeriverState); override;
    function GetInternalDeriverState: TBoldDeriverState; override;
  end;

const
  bdsIsDeriving = [bdsDeriving, bdsDerivingAndSubscribing];

implementation

uses
SysUtils,
  BoldCommonConst;


{ TBoldDeriver }

constructor TBoldAbstractDeriver.Create(DerivedObject: TObject);
begin
  inherited Create;
  fDerivedObject := DerivedObject;
  Subscribe := True;
end;

procedure TBoldAbstractDeriver.Derive;
var
  NewState: TBoldDeriverState;
begin
  NewState := bdsSubscriptionOutOfdate;
  try
    repeat
      if (DeriverState = bdsSubscriptionOutOfdate) then
        DeriverState := bdsDerivingAndSubscribing
      else
        DeriverState := bdsDeriving;
      if not (DeriverState in bdsIsDeriving) then
        CancelAllSubscriptions;
    until DeriverState in bdsIsDeriving;
    NewState := bdsCurrent;
  finally
    DeriverState := NewState;
  end;
end;

procedure TBoldAbstractDeriver.DoNotifyOutOfDate;
begin
  // no action
end;

procedure TBoldAbstractDeriver.DoReverseDerive;
begin
  raise EBold.CreateFmt(sCannotReverseDerive, [ClassName]);
end;

procedure TBoldAbstractDeriver.EnsureCurrent;
begin
  if (DeriverState in [bdsOutOfDate, bdsSubscriptionOutOfDate]) then
    Derive;
end;

function TBoldAbstractDeriver.GetIsDeriving: Boolean;
begin
  Result := DeriverState in bdsIsDeriving;
end;

procedure TBoldAbstractDeriver.MarkSubscriptionOutOfdate;
begin
  DeriverState := bdsSubscriptionOutOfDate;
end;

procedure TBoldAbstractDeriver.MarkOutOfdate;
begin
  // avoids going from Subscriptionoutofdate to OutOfDate
  if DeriverState = bdsCurrent then
    DeriverState := bdsOutOfDate;
end;

procedure TBoldAbstractDeriver.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breReEvaluate:
      if DeriverState = bdsCurrent then
        DeriverState := bdsOutOfDate;
    breReSubscribe:
      case DeriverState of
        bdsCurrent,
        bdsOutOfDate:
          DeriverState := bdsSubscriptionOutOfDate;
        bdsReverseDeriving:
          DeriverState := bdsReverseDerivingSubscriptionOutOfDate;
      end;
  else
    raise EBold.CreateFmt(sUnknownMessageReceived, [ClassName, RequestedEvent]);
  end;
end;

procedure TBoldAbstractDeriver.ReverseDerive;
begin
  DoReverseDerive;
end;

procedure TBoldAbstractDeriver.SetDeriverState(Value: TBoldDeriverState);
var
  OldState: TBoldDeriverState;
begin
  // FIXME check legal transitions;
  if Value <> DeriverState then
  begin
    OldState := InternalDeriverState;
    // exit actions - None...
    InternalDeriverState := Value;

    // entry actions
    if (OldState <> bdsOutOfDate) and // Check on OldState is primarily an optimization not to notify multiple times!
      (value in [bdsOutOfDate, bdsSubscriptionOutOfDate]) then
      DoNotifyOutOfDate;

    case DeriverState of
      bdsSubscriptionOutOfDate,
      bdsReverseDerivingSubscriptionOutOfDate:
        CancelAllSubscriptions;
      bdsDerivingAndSubscribing:
        DoDeriveAndSubscribe(True);
      bdsDeriving:
        DoDeriveAndSubscribe(False);
    end;
  end
end;

procedure TBoldAbstractDeriver.SetSubscribe(value: boolean);
begin
  if value <> Subscribe then
  begin
    fSubscribe := value;
    if value then
      SetDeriverState(bdsSubscriptionOutOfDate)
    else
    begin
      CancelAllSubscriptions;
      if DeriverState = bdsSubscriptionOutOfDate then
        SetDeriverState(bdsOutOfDate);
    end;
  end;
end;

procedure TBoldEventPluggedDeriver.DoDeriveAndSubscribe(subscribe: Boolean);
begin
  if Assigned(fOnDeriveAndSubscribe) then
    if (Subscribe) then
      fOnDeriveAndSubscribe(DerivedObject, Self)
    else
      fOnDeriveAndSubscribe(DerivedObject, nil);
end;

procedure TBoldEventPluggedDeriver.DoNotifyOutOfDate;
begin
  if Assigned(fOnNotifyOutOfDate) then
    fOnNotifyOutOfDate;
end;

procedure TBoldEventPluggedDeriver.DoReverseDerive;
begin
  SetDeriverState(bdsReverseDeriving);
  if Assigned(fOnReverseDerive) then
    fOnReverseDerive(DerivedObject);
  case deriverstate of
    bdsReverseDeriving: SetDeriverState(bdsOutOfDate);
    bdsReverseDerivingSubscriptionOutOfDate: SetDeriverState(bdsSubscriptionOutOfDate);
  end;
end;

function TBoldEventPluggedDeriver.GetCanReverseDerive: Boolean;
begin
  Result := Assigned(OnReverseDerive);
end;

procedure TBoldDeriver.SetInternalDeriverState(
  const Value: TBoldDeriverState);
begin
  FInternalDeriverState := Value;
end;

function TBoldDeriver.GetInternalDeriverState: TBoldDeriverState;
begin
  Result := FInternalDeriverState;
end;

end.
