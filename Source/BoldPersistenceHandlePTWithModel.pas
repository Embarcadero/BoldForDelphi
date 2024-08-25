{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandlePTWithModel;

interface

uses
  BoldPersistenceHandlePassthrough,
  BoldAbstractModel,
  BoldSubscription,
  Classes;

type
  {forward declarations}
  TBoldPersistenceHandlePassthroughWithModel = class;
  TBoldPersistenceHandlePassthroughWithModel = class(TBoldPersistenceHandlePassthrough)
  private
    fBoldModel: TBoldAbstractModel;
    fModelSubscriber: TBoldPassThroughSubscriber;
    procedure SetBoldModel(Value: TBoldAbstractModel);
    procedure ModelChanged;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property BoldModel: TBoldAbstractModel read fBoldModel write SetBoldModel;
  end;

implementation

uses
  SysUtils;

const
  breModelDestroyed = 42;
  breModelChanged = 43;

constructor TBoldPersistenceHandlePassthroughWithModel.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fModelSubscriber := TBoldPassThroughSubscriber.Create(_Receive);
end;

procedure TBoldPersistenceHandlePassthroughWithModel.SetBoldModel(Value: TBoldAbstractModel);
begin
  if Value <> fBoldModel then
  begin
    fModelSubscriber.CancelAllSubscriptions;
    if Assigned(Value) then
    begin
      Value.AddSmallSubscription(fModelSubscriber, [beDestroying], breModelDestroyed);
      Value.AddSmallSubscription(fModelSubscriber, [beModelChanged], breModelChanged);
    end;
    FBoldModel := Value;
    ModelChanged;
  end;
end;

procedure TBoldPersistenceHandlePassthroughWithModel.ModelChanged;
begin
  ReleasePersistenceController;
  SendEvent(self, beValueIdentityChanged);
end;

destructor TBoldPersistenceHandlePassthroughWithModel.Destroy;
begin
  FreeAndNil(fModelSubscriber);
  inherited;
end;

procedure TBoldPersistenceHandlePassthroughWithModel._Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breModelChanged: ModelChanged;
    breModelDestroyed: begin
      fBoldModel := nil;
      fModelSubscriber.CancelAllSubscriptions;
    end;
  end;
end;

end.
