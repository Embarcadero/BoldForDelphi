
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
  TBoldCreateDatabaseAction = class;
  TBoldDiscardChangesAction = class;
  
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

  { TBoldDiscardChangesAction }
  TBoldDiscardChangesAction = class(TBoldSystemHandleAction)
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

  TBoldCreateDatabaseAction = class(TBoldSystemHandleAction)
  private
    fOnSchemaGenerated: TNotifyEvent;
    fIgnoreUnknownTables: boolean;
    fDropExisting: boolean;
    procedure SchemaGenerated;
  protected
    procedure GenerateSchema;
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OnSchemaGenerated: TNotifyEvent read fOnSchemaGenerated write fOnSchemaGenerated;
    property IgnoreUnknownTables: boolean read fIgnoreUnknownTables write fIgnoreUnknownTables default false;
    property DropExisting: boolean read fDropExisting write fDropExisting default false;
  end;

implementation

uses
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Menus, // for TextToShortCut
  SysUtils,
  System.UITypes,

  BoldCoreConsts,
  BoldDefs,
  BoldSystem;

const
  breValueIdentityChanged = 45;

{ TBoldUpdateDBAction }

procedure TBoldUpdateDBAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldSystemHandle.Active
                    and not BoldSystemHandle.System.IsProcessingTransactionOrUpdatingDatabase
                    and (BoldSystemHandle.System.DirtyObjects.Count > 0);
end;

constructor TBoldUpdateDBAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sUpdateDB;
  ShortCut := TextToShortCut('Ctrl+S');
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
  fOpenCaption := sOpenSystem;
  fCloseCaption := sCloseSystem;
  fSaveQuestion := sThereAreDirtyObjects;
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
                raise EBold.Create(sClosingWithDirtyObjects);
      end;
    if Update then
      BoldSystemHandle.Active := not BoldSystemHandle.Active;
  end;
end;

procedure TBoldActivateSystemAction.SetBoldSystemHandle(
  const Value: TBoldSystemHandle);
begin
  inherited SetBoldSystemHandle(Value);
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

type TControlAccess = class(TControl);

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
    (Target as TStatusBar).SimpleText := Caption
  else
  if Target is TControl then
    (Target as TControlAccess).Caption := Caption
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

{ TBoldCreateDatabaseAction }

procedure TBoldCreateDatabaseAction.CheckAllowEnable(var EnableAction: boolean);
begin
  EnableAction := Assigned(BoldSystemHandle) and Assigned(BoldSystemHandle.PersistenceHandleDB) and not BoldSystemHandle.Active;
end;

constructor TBoldCreateDatabaseAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sCreateDB;
end;

procedure TBoldCreateDatabaseAction.ExecuteTarget(Target: TObject);
begin
  GenerateSchema;
end;

procedure TBoldCreateDatabaseAction.GenerateSchema;
begin
  if Assigned(BoldSystemHandle) and Assigned(BoldSystemHandle.PersistenceHandleDB) then
  begin
    Screen.Cursor := crHourGlass;
    try
      BoldSystemHandle.PersistenceHandleDB.CreateDataBase(DropExisting);
      BoldSystemHandle.PersistenceHandleDB.CreateDataBaseSchema(IgnoreUnknownTables);
    finally
      Screen.Cursor := crDefault;
    end;
    SchemaGenerated;
  end;
end;

procedure TBoldCreateDatabaseAction.SchemaGenerated;
begin
  if Assigned(fOnSchemaGenerated) then
    fOnSchemaGenerated(Self);
end;

{ TBoldDiscardChangesAction }

procedure TBoldDiscardChangesAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldSystemHandle.Active
                    and not BoldSystemHandle.System.IsProcessingTransactionOrUpdatingDatabase
                    and (BoldSystemHandle.System.DirtyObjects.Count > 0);
end;

constructor TBoldDiscardChangesAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Discard changes';
end;

procedure TBoldDiscardChangesAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if Assigned(BoldSystemHandle) then
    BoldSystemHandle.Discard;
end;

end.
