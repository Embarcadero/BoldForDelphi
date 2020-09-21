unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ActnList,
  ComCtrls,
  BoldHandle,
  BoldHandles,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldTypeNameHandle,
  BoldRootedHandles,
  BoldSystemHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldModel,
  BoldSubscription,
  BoldListBox,
  BoldSystem,
  BoldComboBox,
  BoldEdit,
  BoldNavigator,
  BoldAFPDefault,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldListHandle1: TBoldListHandle;
    BoldListBox1: TBoldListBox;
    BoldEdit1: TBoldEdit;
    BoldEdit2: TBoldEdit;
    BoldEdit3: TBoldEdit;
    BoldComboBox1: TBoldComboBox;
    BoldEdit4: TBoldEdit;
    BoldEdit5: TBoldEdit;
    BoldEdit6: TBoldEdit;
    blhAllBudgetStatus: TBoldListHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BoldEdit7: TBoldEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BoldNavigator1: TBoldNavigator;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    Action1: TAction;
    Button1: TButton;
    BoldEdit8: TBoldEdit;
    Label9: TLabel;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button2: TButton;
    Button3: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure Action1Update(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Action1Update(Sender: TObject);
var
  LastFailure: TBoldFailureReason;
begin
  LastFailure:= GetBoldLastFailureReason;
  if assigned( LastFailure ) then
    StatusBar1.SimpleText := LastFailure.Reason
  else
    StatusBar1.SimpleText := '';

  Button1.Enabled :=
    BoldSystemHandle1.Active and
    (BoldSystemHandle1.System.DirtyObjects.Count <> 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := 330;
  Height := 350;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active and
  (BoldSystemHandle1.System.DirtyObjects.Count > 0) then
    case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

end.

