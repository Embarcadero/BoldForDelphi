unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ActnList,
  ExtCtrls,
  Grids,
  BoldSubscription,
  BoldHandle,
  BoldHandles,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldSystemHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldVariableDefinition,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldAbstractModel,
  BoldModel,
  BoldEdit,
  BoldGrid,
  BoldNavigator,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldNavigatorDefs, BoldOclVariables, BoldIBDatabaseAction, DB,
  IBDatabase, BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldGrid1: TBoldGrid;
    btnSave: TButton;
    gboGlobalSettings: TGroupBox;
    lblVAT: TLabel;
    lblPercent: TLabel;
    Label1: TLabel;
    btxtVAT: TBoldEdit;
    blhProduct: TBoldListHandle;
    bnavProduct: TBoldNavigator;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    behVAT: TBoldExpressionHandle;
    BoldOclVariables: TBoldOclVariables;
    Button1: TButton;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure BoldSystemActivator1SystemActivated(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  OclVariableClasses;

{$R *.DFM}

// Ensure there is one instance of GlobalSettings.
procedure TForm1.BoldSystemActivator1SystemActivated(Sender: TObject);
begin
  if BoldSystemHandle1.System.ClassByExpressionName['GlobalSettings'].Count = 0 then
    begin
      TGlobalSettings.Create(BoldSystemHandle1.System);
      BoldSystemHandle1.UpdateDatabase;
    end;
end;

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width  := 305;
  Height := 369;
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
  BoldSystemHandle1.System.UpdateDatabase;
end;

end.
