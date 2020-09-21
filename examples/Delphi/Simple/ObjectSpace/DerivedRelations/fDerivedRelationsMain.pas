unit fDerivedRelationsMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldHandles,
  BoldSystemHandle,
  BoldSubscription,
  BoldModel,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  ExtCtrls,
  BoldNavigator,
  BoldEdit,
  BoldListBox,
  Grids,
  BoldGrid,
  BoldExpressionHandle,
  BoldActions,
  ActnList,
  BoldHandleAction,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldGrid1: TBoldGrid;
    BoldGrid2: TBoldGrid;
    lblProject: TLabel;
    lblPerson: TLabel;
    BoldListBox1: TBoldListBox;
    BoldEdit1: TBoldEdit;
    lblProjectLeader: TLabel;
    lblParticipators: TLabel;
    bnavProject: TBoldNavigator;
    BoldNavigator1: TBoldNavigator;
    blhPerson: TBoldListHandle;
    blhProject: TBoldListHandle;
    blhParticipators: TBoldListHandle;
    bsltAllMembers: TBoldListBox;
    lblAllMembers: TLabel;
    blhAllMembers: TBoldListHandle;
    behProjectLeader: TBoldExpressionHandle;
    blhAssociates: TBoldListHandle;
    blstMemberOf: TBoldListBox;
    lblAssociates: TLabel;
    Bevel1: TBevel;
    btnSave: TButton;
    ActionList1: TActionList;
    Button2: TButton;
    Button3: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DerivedHandleExampleClasses;

{$R *.DFM}

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width  := 467;
  Height := 401;
  Randomize;
end;

// Give the user the opportunity to save changes before close.
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active then
  if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
    case MessageDlg( 'There are dirty objects. save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  btnSave.Enabled := True;
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  btnSave.Enabled := False;
end;

end.
