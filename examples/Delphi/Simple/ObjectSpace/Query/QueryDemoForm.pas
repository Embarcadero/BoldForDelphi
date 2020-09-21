unit QueryDemoForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  BoldNavigator,
  StdCtrls,
  ActnList,
  Grids,
  BoldGrid,
  BoldListBox,
  Boldhandles,
  BoldSystemHandle,
  BoldSubscription,
  BoldModel,
  BoldElements,
  BoldSystem,
  BoldHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TfrmQueryDemo = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldGrid1: TBoldGrid;
    BoldListBox1: TBoldListBox;
    BoldListBox2: TBoldListBox;
    Label1: TLabel;
    BoldNavigator1: TBoldNavigator;
    BoldListHandle1: TBoldListHandle;
    BoldListHandle2: TBoldListHandle;
    BoldListHandle3: TBoldListHandle;
    btnLock: TButton;
    btnRelease: TButton;
    btnSave: TButton;
    Label2: TLabel;
    Label3: TLabel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    Button1: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnLockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReleaseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    LockSubscriber: TBoldPassthroughSubscriber;
    function AnswerFalse(Originator: TObject; OriginalEvent: TBoldEvent;
                         RequestedEvent: TBoldRequestedEvent;
                         const Args: array of const;
                         Subscriber: TBoldSubscriber) : Boolean;
  public
    { Public declarations }
  end;

var
  frmQueryDemo: TfrmQueryDemo;

implementation

uses
  QueryDemoClasses;

{$R *.DFM}

function TfrmQueryDemo.AnswerFalse(Originator: TObject;
                                   OriginalEvent: TBoldEvent;
                                   RequestedEvent: TBoldRequestedEvent;
                                   const Args: array of const;
                                   Subscriber: TBoldSubscriber): Boolean;
begin
  // Always answer false.
  result := False;
  SetBoldLastFailureReason(TBoldFailureReason.Create('Value is locked', nil));
end;

procedure TfrmQueryDemo.btnLockClick(Sender: TObject);
begin
  // Place a subscription on the MaySetValue query of 'Name' of the current object, with a subscriber that always answers false
  // This will prevent changing (or actually, setting) the name of the object
  (BoldListHandle1.CurrentBoldObject as TThing).M_Name.AddSubscription(LockSubscriber, bqMaySetValue, bqMaySetValue);
end;

procedure TfrmQueryDemo.FormCreate(Sender: TObject);
begin
  // Initialize the Subscriber we will use for locking objects
  LockSubscriber := TBoldPassthroughSubscriber.CreateWithReceiveAndAnswer(nil, AnswerFalse);
end;

procedure TfrmQueryDemo.FormDestroy(Sender: TObject);
begin
  LockSubscriber.Free;
end;

procedure TfrmQueryDemo.btnReleaseClick(Sender: TObject);
begin
  // Release all objects by cancelling the subscriptions
  LockSubscriber.CancelAllSubscriptions;
end;

procedure TfrmQueryDemo.btnSaveClick(Sender: TObject);
begin
  try
    BoldSystemHandle1.UpdateDatabase;
  except
    BoldRaiseLastFailure(BoldSystemHandle1.System, 'SaveToDatabase', 'Update failed');
  end;
end;

procedure TfrmQueryDemo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active then
    if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: BoldSystemHandle1.System.UpdateDatabase;
        mrNo: BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;

end.
