
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
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
  strict private
    function GetIsDeriving: Boolean;
    function GetIsCurrent: Boolean;
    procedure DeriveAndSubscribe(subscribe: Boolean);
  strict protected
    procedure SetSubscribe(value: boolean); virtual;
    function GetSubscribe: Boolean; virtual;
    function GetDerivedObject: TObject; virtual; abstract;
    procedure SetDeriverState(Value: TBoldDeriverState);
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
    function GetContextString: string; override;    
  public
    procedure MarkSubscriptionOutOfdate;
    procedure MarkOutOfdate;
    procedure EnsureCurrent;
    procedure Derive;
    procedure ReverseDerive;
    property DerivedObject: TObject read GetDerivedObject;
    property Subscribe: Boolean read GetSubscribe write SetSubscribe;
    property IsDeriving: Boolean read GetIsDeriving;
    property IsCurrent: Boolean read GetIsCurrent;
  end;

  TBoldEventPluggedDeriver = class (TBoldAbstractDeriver)
  strict private
    fOnderiveAndSubscribe: TBoldDeriveAndResubscribe;
    fOnReverseDerive: TBoldReverseDerive;
    fOnNotifyOutOfDate: TBoldJustNotifyEvent;
  strict protected
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
  strict private
    fDerivedObject: TObject;
    fInternalDeriverState: TBoldDeriverState;
    fSubscribe: Boolean;
  strict protected
    procedure SetSubscribe(value: boolean); override;
    function GetSubscribe: Boolean; override;
    procedure SetInternalDeriverState(const Value: TBoldDeriverState); override;
    function GetInternalDeriverState: TBoldDeriverState; override;
    function GetDerivedObject: TObject; override;
  public 
    constructor Create(DerivedObject: TObject);
  end;

const
  bdsIsDeriving = [bdsDeriving, bdsDerivingAndSubscribing];

implementation

uses
  SysUtils,
  Classes,
  {$IFDEF ATTRACS}
  AttracsTraceLog,
  AttracsDefs,
  AttracsPerformance,
   {$IFDEF BOLD_PERFORMANCE_COUNTERS}
     BoldSystemPerf,
   {$ENDIF}
  {$ENDIF}
  BoldCoreConsts,
  BoldSystem;

{ TBoldDeriver }
procedure TBoldAbstractDeriver.Derive;
var
  NewState: TBoldDeriverState;
begin
  if Subscribe then
    NewState := bdsSubscriptionOutOfdate
  else
    NewState := bdsOutOfdate;
  try
    repeat
      if (DeriverState = bdsSubscriptionOutOfdate) then
      begin
        CancelAllSubscriptions;
        DeriverState := bdsDerivingAndSubscribing
      end
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

procedure TBoldAbstractDeriver.DeriveAndSubscribe(subscribe: Boolean);
{$IFDEF ATTRACS_NOTNOW}
var
  PerformanceMeasurement,
  SaveSubscriptionsPerformanceMeasurement : TPerformanceMeasurement; //PATCH Performance log
  Member : TBoldMember;
  MemberAsString : String;
  OwningObject : TBoldObject;
  ObjectID : String;

  procedure InitialiseLocalVariablesWithDerivedObjectData;
  begin
    if DerivedObject is TBoldMember then
    begin
      Member := DerivedObject as TBoldMember;
      OwningObject := Member.OwningObject;
      if Assigned(OwningObject) then
        ObjectID := OwningObject.BoldObjectLocator.AsString;
      MemberAsString := Member.BoldMemberRtInfo.AsString;
    end else
      MemberAsString := DerivedObject.ClassName;
  end;

begin
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
  // Perform the derivation
  DoDeriveAndSubscribe(Subscribe);
  // if it took too long, Log the derivation (including prefetching if it happened)
  // and if too long and the SubscriptionSaver is enabled, add this member as slow and save the subscriptions
  PerformanceMeasurement.EndMeasurement;

  {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerfObject.BoldEventPluggedDeriver_DoDeriveAndSubscribe(PerformanceMeasurement.TimeTaken);
  {$ENDIF}
  if not PerformanceMeasurement.AcceptableTimeForSmallComputation then
  begin
    InitialiseLocalVariablesWithDerivedObjectData;
    PerformanceMeasurement.WhatMeasured := 'Deriving ' + MemberAsString ;
    PerformanceMeasurement.WhatMeasuredParameter := 'object ' + ObjectID;

    PerformanceMeasurement.EndMeasurement;
    if DerivedObject is TBoldMember then
    begin
      PerformanceMeasurement.EndMeasurement;
      PerformanceMeasurement.Trace;
    end;
  end;

end;
{$ELSE}
begin;
  DoDeriveAndSubscribe(Subscribe);
end;
{$ENDIF}

procedure TBoldAbstractDeriver.DoNotifyOutOfDate;
begin
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

function TBoldAbstractDeriver.GetContextString: string;
begin
  result := '';
  if Assigned(DerivedObject) then
    if DerivedObject is TComponent then
      result := TComponent(DerivedObject).Name
    else
    if DerivedObject is TBoldSubscribableObject then
      result := TBoldSubscribableObject(DerivedObject).ContextString
    else
      Result := DerivedObject.ClassName;
end;

function TBoldAbstractDeriver.GetIsCurrent: Boolean;
begin
  result := DeriverState = bdsCurrent;
end;

function TBoldAbstractDeriver.GetIsDeriving: Boolean;
begin
  Result := DeriverState in bdsIsDeriving;
end;

function TBoldAbstractDeriver.GetSubscribe: Boolean;
begin
  Result := true;
end;

procedure TBoldAbstractDeriver.MarkSubscriptionOutOfdate;
begin
  DeriverState := bdsSubscriptionOutOfDate;
end;

procedure TBoldAbstractDeriver.MarkOutOfdate;
begin
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
  OldState := InternalDeriverState;
  if Value <> OldState then
  begin
    if not (OldState in [bdsCurrent, bdsOutOfDate, bdsSubscriptionOutOfDate, bdsDeriving, bdsDerivingAndSubscribing, bdsReverseDeriving, bdsReverseDerivingSubscriptionOutOfDate]) then
      raise Exception.Create('TBoldAbstractDeriver.SetDeriverState old state is ' + IntToStr(Integer(oldState)));
    InternalDeriverState := Value;
    if (OldState <> bdsOutOfDate) and
      (value in [bdsOutOfDate, bdsSubscriptionOutOfDate]) then
      DoNotifyOutOfDate;

    case DeriverState of
      bdsSubscriptionOutOfDate,
      bdsReverseDerivingSubscriptionOutOfDate:
        CancelAllSubscriptions;
      bdsDerivingAndSubscribing:
        DeriveAndSubscribe(True);
      bdsDeriving:
        DeriveAndSubscribe(False);
    end;
  end
end;

procedure TBoldAbstractDeriver.SetSubscribe(value: boolean);
begin
  raise Exception.Create('Subscribe not settable for this class');
                                                 
end;

procedure TBoldDeriver.SetSubscribe(value: boolean);
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
  if DeriverState = bdsSubscriptionOutOfDate then begin
    SetDeriverState(bdsReverseDerivingSubscriptionOutOfDate);
  end else begin
    SetDeriverState(bdsReverseDeriving);
  end;
  if Assigned(fOnReverseDerive) then begin
    fOnReverseDerive(DerivedObject);
  end;
  case DeriverState of
    bdsReverseDeriving: SetDeriverState(bdsOutOfDate);
    bdsReverseDerivingSubscriptionOutOfDate: SetDeriverState(bdsSubscriptionOutOfDate);
  end;
end;

function TBoldEventPluggedDeriver.GetCanReverseDerive: Boolean;
begin
  Result := Assigned(OnReverseDerive);
end;

function TBoldDeriver.GetSubscribe: Boolean;
begin
  Result := fSubscribe;
end;

procedure TBoldDeriver.SetInternalDeriverState(
  const Value: TBoldDeriverState);
begin
  FInternalDeriverState := Value;
end;

constructor TBoldDeriver.Create(DerivedObject: TObject);
begin
  inherited Create;
  fDerivedObject := DerivedObject;
  Subscribe := true;
end;

function TBoldDeriver.GetDerivedObject: TObject;
begin
  Result := fDerivedObject;
end;

function TBoldDeriver.GetInternalDeriverState: TBoldDeriverState;
begin
  Result := FInternalDeriverState;
end;

end.
