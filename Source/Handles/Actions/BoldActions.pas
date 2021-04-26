
{ Global compiler directives }
{$include bold.inc}
unit BoldActions;

interface

uses
  Classes,
  ActnList,
  BoldSubscription,
  BoldSystemHandle,
  BoldHandleAction;

type
  { forward declaration }
  TBoldSystemHandleAction = class;
  TBoldActivateSystemAction = class;
  TBoldUpdateDBAction = class;
  TBoldFailureDetectionAction = class;

  TBoldSaveAction = (saAsk, saYes, saNo, saFail);

  { TBoldSystemHandleAction }
  TBoldSystemHandleAction = class(TBoldHandleAction)
  private
    function GetBoldSystemHandle: TBoldSystemHandle;
  protected
    procedure SetBoldSystemHandle(const Value: TBoldSystemHandle); virtual;
  published
    property BoldSystemHandle: TBoldSystemHandle read GetBoldSystemHandle write SetBoldSystemHandle;
  end;

  { TBoldFailureDetectionAction }
  TBoldFailureDetectionAction = class(TAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TBoldUpdateDBAction }
  TBoldUpdateDBAction = class(TBoldSystemHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TBoldActivateSystemAction }
  TBoldActivateSystemAction = class(TBoldSystemHandleAction)
  private
    fHandleIdentitySubscriber: TBoldPassthroughSubscriber;
    fOnSystemClosed: TNotifyEvent;
    fOnSystemActivated: TNotifyEvent;
    FOpenCaption: string;
    FCloseCaption: string;
    FSaveOnClose: TBoldSaveAction;
    FSaveQuestion: string;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetOpenCaption(const Value: string);
    procedure SetCloseCaption(const Value: string);
    procedure TriggerSwitchEvent;
  protected
    procedure _HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    procedure SetBoldSystemHandle(const Value: TBoldSystemHandle); override;
    procedure UpdateCaption; virtual;
    procedure SystemActivated;
    procedure SystemClosed;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OnSystemOpened: TNotifyEvent read fOnSystemActivated write fOnSystemActivated;
    property OnSystemClosed: TNotifyEvent read fOnSystemClosed write fOnSystemClosed;
    property OpenCaption: string read FOpenCaption write SetOpenCaption;
    property CloseCaption: string read FCloseCaption write SetCloseCaption;
    property SaveQuestion: string read FSaveQuestion write fSaveQuestion;
    property SaveOnClose: TBoldSaveAction read FSaveOnClose write fSaveOnClose;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  Controls,
  Dialogs,
  ComCtrls,
  BoldSystem;

const
  breValueIdentityChanged = 45;

{ TBoldUpdateDBAction }

procedure TBoldUpdateDBAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldSystemHandle.Active and
                    (BoldSystemHandle.System.DirtyObjects.Count > 0);
end;

constructor TBoldUpdateDBAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Update DB';
end;

procedure TBoldUpdateDBAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if Assigned(BoldSystemHandle) then
    BoldSystemHandle.UpdateDatabase;
end;

{ TBoldActivateSystemAction }

constructor TBoldActivateSystemAction.Create(AOwner: TComponent);
begin
  inherited;
  fHandleIdentitySubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  fOpenCaption := 'Open system';
  fCloseCaption := 'Close system';
  fSaveQuestion := 'There are dirty objects. Save them before closing system?';
  UpdateCaption;
end;

destructor TBoldActivateSystemAction.Destroy;
begin
  FreeAndNil(fHandleIdentitySubscriber);
  inherited;
end;

procedure TBoldActivateSystemAction.ExecuteTarget(Target: TObject);
var
  Update: Boolean;
begin
  inherited;
  if Assigned(BoldSystemHandle) then
  begin
    Update := True;
    if BoldSystemHandle.Active then
      case FSaveOnClose of
        saAsk: if BoldSystemHandle.System.DirtyObjects.Count > 0 then
               case MessageDlg(fSaveQuestion, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                 mrYes: BoldSystemHandle.System.UpdateDatabase;
                 mrNo: BoldSystemHandle.System.Discard;
                 mrCancel: Update := False;
               end;
        saYes: BoldSystemHandle.UpdateDatabase;
        saNo: BoldSystemHandle.System.Discard;
        saFail: if BoldSystemHandle.System.DirtyObjects.Count > 0 then
                raise EBold.Create('Closing system with dirty objects!!');
      end;
    if Update then
      BoldSystemHandle.Active := not BoldSystemHandle.Active;
  end;
end;

procedure TBoldActivateSystemAction.SetBoldSystemHandle(
  const Value: TBoldSystemHandle);
begin
  inherited;
  fHandleIdentitySubscriber.CancelAllSubscriptions;
  if Assigned(BoldSystemHandle) then
    BoldSystemHandle.AddSmallSubscription(fHandleIdentitySubscriber,
                                          [beValueIdentityChanged, beDestroying],
                                          breValueIdentityChanged);
  UpdateCaption;
end;

procedure TBoldActivateSystemAction.SetCloseCaption(const Value: string);
begin
  FCloseCaption := Value;
  UpdateCaption;
end;

procedure TBoldActivateSystemAction.SetOpenCaption(const Value: string);
begin
  FOpenCaption := Value;
  UpdateCaption;
end;

procedure TBoldActivateSystemAction.SystemActivated;
begin
  if Assigned(fOnSystemActivated) then
    fOnSystemActivated(Self);
end;

procedure TBoldActivateSystemAction.SystemClosed;
begin
  if Assigned(fOnSystemClosed) then
    fOnSystemClosed(Self);
end;

procedure TBoldActivateSystemAction.TriggerSwitchEvent;
begin
  if Assigned(BoldSystemHandle) and BoldSystemHandle.Active then
    SystemActivated
  else
    SystemClosed;
end;

procedure TBoldActivateSystemAction.UpdateCaption;
begin
  if Assigned(BoldSystemHandle) then
  begin
    if BoldSystemHandle.Active then
      Caption := CloseCaption
    else
      Caption := OpenCaption;
    end
  else
    Caption := Name;
end;

procedure TBoldActivateSystemAction._HandleSubscriberReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  inherited;
  case RequestedEvent of
    breValueIdentityChanged: UpdateCaption;
  end;
end;

procedure TBoldActivateSystemAction._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breValueIdentityChanged:
    begin
      UpdateCaption;
      TriggerSwitchEvent;
    end;
  end;
end;

{ TBoldFailureDetectionAction }

function TBoldFailureDetectionAction.HandlesTarget(
  Target: TObject): Boolean;
begin
  Result := Target is TControl and
            ((Target as TControl).Action = self);
end;

procedure TBoldFailureDetectionAction.UpdateTarget(Target: TObject);
var
  failure: TBoldFailureReason;
begin
  inherited;
  failure := GetBoldLastFailureReason;
  if assigned(failure) then
    Caption := failure.Reason
  else
    Caption := '';


  if Target is TStatusBar then
    (Target as TStatusBar).SimpleText := Caption;
end;

{ TBoldSystemHandleAction }

function TBoldSystemHandleAction.GetBoldSystemHandle: TBoldSystemHandle;
begin
  Result := BoldElementHandle as TBoldSystemHandle;
end;

procedure TBoldSystemHandleAction.SetBoldSystemHandle(
  const Value: TBoldSystemHandle);
begin
  BoldElementHandle := Value;
end;

initialization

end.
