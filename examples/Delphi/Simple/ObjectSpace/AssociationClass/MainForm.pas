unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Dialogs,
  BoldAbstractModel,
  BoldModel,
  DBTables,
  Grids,
  BoldGrid,
  ExtCtrls,
  StdCtrls,
  BoldSubscription,
  BoldAFPDefault,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldPMappers,
  BoldHandles,
  BoldSystemHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldListHandle,
  BoldCursorHandle,
  BoldActions,
  BoldHandleAction,
  BoldDBActions,
  BoldListBox,
  BoldEdit,
  BoldNavigator,
  ActnList,
  Controls,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TfrmMain = class(TForm)
    BoldModel1: TBoldModel;
    AllPersonsGrid: TBoldGrid;
    JobGrid: TBoldGrid;
    CompanyGrid: TBoldGrid;
    AllPersons: TBoldListHandle;
    AllJobs: TBoldListHandle;
    AllCompanies: TBoldListHandle;
    PersonEmployersListBox: TBoldListBox;
    CompanyEmployeesListBox: TBoldListBox;
    PersonEmployers: TBoldListHandle;
    CompanyEmplyees: TBoldListHandle;
    PersonNavigator: TBoldNavigator;
    CompanyNavigator: TBoldNavigator;
    PersonJobs: TBoldListHandle;
    CompanyJobs: TBoldListHandle;
    PersonJobGrid: TBoldGrid;
    CompanyJobGrid: TBoldGrid;
    Label1: TLabel;
    Label2: TLabel;
    BoldSystemHandle1: TBoldSystemHandle;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

// Set size of form before show.
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Height := 444;
  Width  := 693;
end;

// Give the user the opportunity to save changes before close.
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

end.
