unit fDerivedAttrMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  BoldHandles,
  BoldSystemHandle,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldModel,
  StdCtrls,
  Grids,
  BoldGrid,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldEdit,
  BoldExpressionHandle,
  BoldActions,
  ActnList,
  BoldHandleAction,
  BoldDBActions,
  BoldAbstractModel,
  BoldListActions, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    blhProduct: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    btnAddProduct: TButton;
    btnDelProduct: TButton;
    btnSave: TButton;
    gboGlobalSettings: TGroupBox;
    btxtVAT: TBoldEdit;
    lblVAT: TLabel;
    blhGlobalSettings: TBoldListHandle;
    lblPercent: TLabel;
    Label1: TLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldListHandleDeleteAction1: TBoldListHandleDeleteAction;
    BoldListHandleAddNewAction1: TBoldListHandleAddNewAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
  private
    { Private declarations }
    procedure SetGlobals;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DerivedAttrExampleClasses;

{$R *.DFM}

// Set Width and Height of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width  := 304;
  Height := 370;
end;

// Give the user the opportunity to save changes before close.
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active and BoldSystemHandle1.System.BoldDirty then
    case MessageDlg( 'There are dirty objects. save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

// Define/Redefine a  global variable 'global_VAT' in the evaluator.
procedure TForm1.SetGlobals;
var
  AGlobalSetting: TGlobalSettings;
begin
  AGlobalSetting := BoldSystemHandle1.System.ClassByExpressionName['GlobalSettings'][0]
                      as TGlobalSettings;
  if Assigned(AGlobalSetting.M_vat) then
    BoldSystemHandle1.System.Evaluator.DefineVariable('global_VAT',
      AGlobalSetting.M_vat, AGlobalSetting.M_vat.BoldType, False);
end;

// Ensure that there is one instance of TGlobalSettings
// in the new database and save it if needed.
// If system is active, set global variables.
procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  if BoldSystemHandle1.Active then
  begin
    if blhGlobalSettings.Count < 1 then
      blhGlobalSettings.List.AddNew;
    if BoldSystemHandle1.System.BoldDirty then
      BoldSystemHandle1.UpdateDatabase;
    SetGlobals;
  end;
end;

end.
