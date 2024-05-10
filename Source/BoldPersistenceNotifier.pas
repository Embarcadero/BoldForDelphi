
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceNotifier;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  BoldId,
  BoldContainers,
  BoldPersistenceHandle,
  BoldSubscription;

type
  {Forward declaration of classes}
  TBoldAbstractPersistenceNotifier = class;
  TBoldPersistenceNotifier = class;
  TBoldPersistenceProgressNotifier = class;

  TBoldAlertExcessiveFetchEvent = procedure(ObjectId: TBoldObjectId; FetchCount: integer) of object;

  { TBoldAbstractPersistenceNotifier }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldAbstractPersistenceNotifier = class(TComponent)
  private
    fEvents: array[bpeMinReserved..bpeMaxReserved] of TBoldExtendedEventHandler;
    FPersistenceHandle: TBoldPersistenceHandle;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fFetchLog: TBoldIntegerArray;
    fOnAlertExcessiveFetch: TBoldAlertExcessiveFetchEvent; 
    procedure SetPersistenceHandle(const Value: TBoldPersistenceHandle);
    function GetEvent(index: integer): TBoldExtendedEventHandler;
    procedure SetEvent(const index: integer; EventHandler: TBoldExtendedEventHandler);
    procedure CallIfAssigned(EventID: integer; Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    function GetFetchLog: TBoldIntegerArray;
  protected
    procedure EndFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure EndUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure EndFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure StartUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure StartFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure UpdateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure DeleteObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure CreateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure FetchObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure FetchMember(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure StartFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure FetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure ProgressStart(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure ProgressEnd(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReceiveExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    procedure LogFetchOneObject(ObjectId: TBoldObjectId);
    procedure AlertExcessiveFetch(ObjectId: TBoldObjectId; FetchCount: integer); virtual;
    procedure Loaded; override;
    property OnEndFetch: TBoldExtendedEventHandler index bpeEndFetch read GetEvent write SetEvent;
    property OnEndUpdate: TBoldExtendedEventHandler index bpeEndUpdate read GetEvent write SetEvent;
    property OnEndFetchID: TBoldExtendedEventHandler index bpeEndFetchID read GetEvent write SetEvent;
    property OnStartUpdate: TBoldExtendedEventHandler index bpeStartUpdate read GetEvent write SetEvent;
    property OnStartFetch: TBoldExtendedEventHandler index bpeStartFetch read GetEvent write SetEvent;
    property OnUpdateObject: TBoldExtendedEventHandler index bpeUpdateObject read GetEvent write SetEvent;
    property OnDeleteObject: TBoldExtendedEventHandler index bpeDeleteObject read GetEvent write SetEvent;
    property OnCreateObject: TBoldExtendedEventHandler index bpeCreateObject read GetEvent write SetEvent;
    property OnFetchObject: TBoldExtendedEventHandler index bpeFetchObject read GetEvent write SetEvent;
    property OnFetchMember: TBoldExtendedEventHandler index bpeFetchMember read GetEvent write SetEvent;
    property OnStartFetchID: TBoldExtendedEventHandler index bpeStartFetchID read GetEvent write SetEvent;
    property OnFetchID: TBoldExtendedEventHandler index bpeFetchID read GetEvent write SetEvent;
    property OnProgressStart: TBoldExtendedEventHandler index bpeProgressStart read GetEvent write SetEvent;
    property OnProgressEnd: TBoldExtendedEventHandler index bpeProgressEnd read GetEvent write SetEvent;
    property OnAlertExcessiveFetch: TBoldAlertExcessiveFetchEvent read fOnAlertExcessiveFetch write fOnAlertExcessiveFetch; 
    property PersistenceHandle: TBoldPersistenceHandle read FPersistenceHandle write SetPersistenceHandle;
    property FetchLog: TBoldIntegerArray read GetFetchLog;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
  end;

  { TBoldPersistenceNotifier }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPersistenceNotifier = class(TBoldAbstractPersistenceNotifier)
  published
    property PersistenceHandle;
    property OnEndFetch;
    property OnEndUpdate;
    property OnEndFetchID;
    property OnStartUpdate;
    property OnStartFetch;
    property OnUpdateObject;
    property OnDeleteObject;
    property OnCreateObject;
    property OnFetchObject;
    property OnFetchMember;
    property OnStartFetchID;
    property OnFetchID;
    property OnProgressStart;
    property OnProgressEnd;
    property OnAlertExcessiveFetch;
  end;

  { TBoldPersistenceProgressNotifier }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPersistenceProgressNotifier = class(TBoldAbstractPersistenceNotifier)
  private
    { Private declarations }
    fLastUpdate: TDateTime;
    FAnimation: TAnimate;
    fAnimationTimer: TTimer;
    FWinControl: TWinControl;
    fMsgLabel: TLabel;
    FProgressBar: TProgressBar;
    fMsgFetchObjects: String;
    fMsgRetrieveIds: String;
    fMsgUpdateDatabase: String;
    procedure AnimationTurnOff(Sender: TObject);
    procedure SetProgressBar(const Value: TProgressBar);
    procedure SetAnimation(const Value: TAnimate);
    procedure SetWinControl(const Value: TWinControl);
    procedure SetMsgLabel(const Value: TLabel);
    function GetAnimationInterval: integer;
    procedure SetAnimationInterval(const Value: integer);
    function GetAnimationTimer: TTimer;
  protected
    { Protected declarations }
    procedure EndEvent;
    procedure StartEvent(Count: integer);
    procedure StepProgress;
    procedure StepAnimation;
    procedure SetMessage(const s: String);
    procedure UpdateProgressBar;
    procedure EndFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure EndUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure EndFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure UpdateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure DeleteObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure CreateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchMember(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property AnimationTimer: TTimer read GetAnimationTimer;    
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property Animation: TAnimate read FAnimation write SetAnimation;
    property WinControl: TWinControl read FWinControl write SetWinControl;
    property MsgLabel: TLabel read fMsgLabel write SetMsgLabel;
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;
    property AnimationInterval: integer read GetAnimationInterval write SetAnimationInterval;
    property PersistenceHandle;
    property MsgFetchObjects: string read fMsgFetchObjects write fMsgFetchObjects;
    property MsgRetrieveIds: string read fMsgRetrieveIds write fMsgRetrieveIds;
    property MsgUpdateDatabase: string read fMsgUpdateDatabase write fMsgUpdateDatabase;
    property OnProgressStart;
    property OnProgressEnd;
    property OnAlertExcessiveFetch;
  end;

implementation

uses
  SysUtils;

const
  brePersistenceHandleDestroying = 100;

const
  c100ms = 1/24/60/60/10;
    
{ TBoldAbstractPersistenceNotifier }

procedure TBoldAbstractPersistenceNotifier.CallIfAssigned(EventID: integer;
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if Assigned(fEvents[EventID]) then
    fEvents[EventID](Originator, OriginalEvent, RequestedEvent, Args);
end;

constructor TBoldAbstractPersistenceNotifier.create(owner: TComponent);
begin
  inherited;
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(ReceiveExtendedEvent);
end;

procedure TBoldAbstractPersistenceNotifier.CreateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  CallIfAssigned(bpeCreateObject, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.DeleteObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  CallIfAssigned(bpeDeleteObject, Originator, OriginalEvent, RequestedEvent, Args);
end;

destructor TBoldAbstractPersistenceNotifier.destroy;
begin
  FreeAndNil(fSubscriber);
  FreeAndNil(fFetchLog);
  inherited;
end;

procedure TBoldAbstractPersistenceNotifier.EndFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeEndFetch, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.EndFetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeEndFetchID, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.EndUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeEndUpdate, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.FetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeFetchID, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.FetchMember(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeFetchMember, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.FetchObject(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeFetchObject, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.LogFetchOneObject(ObjectId: TBoldObjectId);
var
  h, m, s, ms: word;
  i: integer;
  MSecs: integer;
begin
  DecodeTime(now, h, m, s, ms);
  MSecs := ms + s * 1000 + m * 60 * 1000 + h * 24 * 60 * 1000;
  FetchLog.Add(MSecs);
  i := 0;
  while (i < fetchLog.Count) and (FetchLog[i] < (msecs - 10 * 1000)) do
    inc(i);
  if i > 0 then
    FetchLog.DeleteRange(0, i - 1);
  if FetchLog.Count > 10 then
    AlertExcessiveFetch(ObjectId, FetchLog.Count);
end;

function TBoldAbstractPersistenceNotifier.GetEvent(index: integer): TBoldExtendedEventHandler;
begin
  Result := fEvents[Index];
end;

function TBoldAbstractPersistenceNotifier.GetFetchLog: TBoldIntegerArray;
begin
  if not assigned(fFetchLog) then
    fFetchLog := TBoldIntegerArray.Create(64, []);
  result := fFetchLog;
end;

procedure TBoldAbstractPersistenceNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PersistenceHandle) then
    PersistenceHandle := nil;
end;

procedure TBoldAbstractPersistenceNotifier.ProgressEnd(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeProgressEnd, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.ProgressStart(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  CallIfAssigned(bpeProgressStart, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.ReceiveExtendedEvent(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if (OriginalEvent = bpeStartFetch) or (OriginalEvent = bpeStartUpdate) or (OriginalEvent = bpeStartFetchId) then
    ProgressStart(Originator, OriginalEvent, RequestedEvent, args);

  case OriginalEvent of
    bpeEndFetch: EndFetch(Originator, OriginalEvent, RequestedEvent, Args);
    bpeEndUpdate: EndUpdate(Originator, OriginalEvent, RequestedEvent, Args);
    bpeEndFetchID: EndFetchID(Originator, OriginalEvent, RequestedEvent, Args);

    bpeStartUpdate: StartUpdate(Originator, OriginalEvent, RequestedEvent, Args);
    bpeStartFetch: StartFetch(Originator, OriginalEvent, RequestedEvent, Args);
    bpeStartFetchID: StartFetchID(Originator, OriginalEvent, RequestedEvent, Args);

    bpeUpdateObject: UpdateObject(Originator, OriginalEvent, RequestedEvent, Args);
    bpeDeleteObject: DeleteObject(Originator, OriginalEvent, RequestedEvent, Args);
    bpeCreateObject: CreateObject(Originator, OriginalEvent, RequestedEvent, Args);
    bpeFetchObject: FetchObject(Originator, OriginalEvent, RequestedEvent, Args);
    bpeFetchMember: FetchMember(Originator, OriginalEvent, RequestedEvent, Args);
    bpeFetchId: FetchID(Originator, OriginalEvent, RequestedEvent, Args);
  end;

  if (OriginalEvent = bpeEndFetch) or (OriginalEvent = bpeEndUpdate) or (OriginalEvent = bpeEndFetchId) then
    ProgressEnd(Originator, OriginalEvent, RequestedEvent, args);
    
  if (OriginalEvent = beDestroying) and (RequestedEvent = brePersistenceHandleDestroying) then
  begin
    PersistenceHandle := nil;
    fSubscriber.CancelAllSubscriptions;
  end;
end;

procedure TBoldAbstractPersistenceNotifier.SetEvent(const index: integer;
  EventHandler: TBoldExtendedEventHandler);
begin
  fEvents[Index] := EventHandler;
end;

procedure TBoldAbstractPersistenceNotifier.SetPersistenceHandle(const Value: TBoldPersistenceHandle);
begin
  if Value <> fPersistenceHandle then
  begin
    fSubscriber.CancelAllSubscriptions;
    if Assigned(Value) then
    begin
      if not (csDesigning in ComponentState) and
         not (csLoading in ComponentState) then
        Value.AddPersistenceSubscription(fSubscriber);
      Value.AddSmallSubscription(fSubscriber, [beDestroying], brePersistenceHandleDestroying);
    end;
    fPersistenceHandle := Value;
  end;
end;

procedure TBoldAbstractPersistenceNotifier.StartFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
var
  IdList: TBoldObjectIdList;
begin
  CallIfAssigned(bpeStartFetch, Originator, OriginalEvent, RequestedEvent, Args);
  IdList := args[0].vObject as TBoldObjectIdList;
  if idLIst.Count = 1 then
    LogFetchOneObject(IdList[0]);
end;

procedure TBoldAbstractPersistenceNotifier.StartFetchID(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  CallIfAssigned(bpeStartFetchID, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.StartUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  CallIfAssigned(bpeStartUpdate, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.UpdateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  CallIfAssigned(bpeUpdateObject, Originator, OriginalEvent, RequestedEvent, Args);
end;

procedure TBoldAbstractPersistenceNotifier.AlertExcessiveFetch(ObjectId: TBoldObjectId; FetchCount: integer);
begin
  if assigned(OnAlertExcessiveFetch) then
    OnAlertExcessiveFetch(ObjectId, FetchCount);
end;

procedure TBoldAbstractPersistenceNotifier.Loaded;
begin
  inherited;
  if assigned(fPersistenceHandle) and not (csDesigning in ComponentState) then
    FPersistenceHandle.AddPersistenceSubscription(fSubscriber);
end;

{ TBoldPersistenceNotifier }

procedure TBoldPersistenceProgressNotifier.AnimationTurnOff(Sender: TObject);
begin
  if assigned(Animation) then
  begin
    Animation.Stop;
    AnimationTimer.Enabled := false;
  end;
end;

constructor TBoldPersistenceProgressNotifier.create(owner: TComponent);
begin
  inherited;
  fMsgFetchObjects := 'Loading %d objects';
  fMsgRetrieveIds := 'Loading IDs';
  fMsgUpdateDatabase := 'Saving %d objects';
end;

procedure TBoldPersistenceProgressNotifier.CreateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.DeleteObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.EndEvent;
begin
  if assigned(ProgressBar) then
  begin
    ProgressBar.Visible := false;
    ProgressBar.Max := 0;
    UpdateProgressBar;
  end;
  if assigned(Animation) then
  begin
    AnimationTimer.Enabled := true;
    Animation.Refresh;
  end;
  SetMessage('');
end;

procedure TBoldPersistenceProgressNotifier.EndFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.EndFetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.EndUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.FetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
end;

procedure TBoldPersistenceProgressNotifier.FetchMember(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StepAnimation;
end;

procedure TBoldPersistenceProgressNotifier.FetchObject(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StepProgress;
end;

function TBoldPersistenceProgressNotifier.GetAnimationInterval: integer;
begin
  result := AnimationTimer.Interval;
end;

function TBoldPersistenceProgressNotifier.GetAnimationTimer: TTimer;
begin
  if not Assigned(fAnimationTimer) then
  begin
    fAnimationTimer := TTimer.Create(self);
    fAnimationTimer.OnTimer := AnimationTurnOff;
    fAnimationTimer.Interval := 100;
  end;
  result := fAnimationTimer;
end;

{ Handle removing of non-bold components }
procedure TBoldPersistenceProgressNotifier.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = ProgressBar then
      ProgressBar := nil;
    if AComponent = Animation then
      Animation := nil;
    if AComponent = WinControl then
      WinControl := nil;
    if AComponent = MsgLabel then
      MsgLabel := nil;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetAnimation(
  const Value: TAnimate);
begin
  FAnimation := Value;
  if Assigned(fAnimation) then
    fAnimation.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.SetAnimationInterval(
  const Value: integer);
begin
  AnimationTimer.Interval := Value;
end;

type
  TExposedWinControl = class(TWinControl);

procedure TBoldPersistenceProgressNotifier.SetMessage(const s: String);
begin
  if assigned(WinControl) then
  begin
    TExposedWinControl(WinControl).Caption := s;
    WinControl.Refresh;
  end;
  if assigned(MsgLabel) then
  begin
    MsgLabel.Caption := s;
    MsgLabel.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetMsgLabel(
  const Value: TLabel);
begin
  fMsgLabel := Value;
  if Assigned(fMsgLabel) then
    fMsgLabel.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.SetProgressBar(
  const Value: TProgressBar);
begin
  FProgressBar := Value;
  if Assigned(fProgressBar) then
  begin
    fProgressBar.FreeNotification(self);
    FProgressBar.Step := 1;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetWinControl(
  const Value: TWinControl);
begin
  FWinControl := Value;
  if Assigned(fWinControl) then
    fWinControl.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.StartEvent(Count: integer);
begin
  if Assigned(ProgressBar) then
  begin
    ProgressBar.Visible := true;
    ProgressBar.Position := 0;
    ProgressBar.Max := Count;
    UpdateProgressBar;
  end;
  if Assigned(Animation) then
  begin
    AnimationTimer.Enabled := False;
    if not Animation.Active and (Animation.FrameCount > 0) then
      Animation.Play(0, Animation.FrameCount - 1, 0);
    Animation.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.StartFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StartEvent((Args[0].VObject as TBoldObjectIdList).Count);
  SetMessage(MsgFetchObjects);
end;

procedure TBoldPersistenceProgressNotifier.StartFetchID(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepAnimation;
  SetMessage(MsgRetrieveIDs);
end;

procedure TBoldPersistenceProgressNotifier.StartUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StartEvent((Args[0].VObject as TBoldObjectIdList).Count);
  SetMessage(MsgUpdateDatabase);
end;

procedure TBoldPersistenceProgressNotifier.StepAnimation;
begin
  if assigned(Animation) then
  begin
    AnimationTimer.Enabled := false;
    if (Animation.FrameCount > 0) then
      Animation.Play(0, Animation.FrameCount - 1, 0);
    Animation.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.StepProgress;
begin
  if assigned(ProgressBar) then
  begin
    ProgressBar.StepIt;
    UpdateProgressBar;
  end;
end;

procedure TBoldPersistenceProgressNotifier.UpdateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.UpdateProgressBar;
begin
  if now - fLastUpdate > c100ms then
  begin
    ProgressBar.Refresh;
    fLastUpdate := now;
  end;
end;

end.
