unit fRDExamMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  StdCtrls,
  Grids,
  ExtCtrls,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldAbstractModel,
  BoldModel,
  BoldGrid,
  BoldEdit,
  BoldExpressionHandle,
  BoldListBox,
  BoldNavigator,
  BoldCheckBox,
  BoldLabel,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldAFPPluggable,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    blhPersons: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    btnSave: TButton;
    Label2: TLabel;
    Label3: TLabel;
    blhFamilies: TBoldListHandle;
    BoldGrid2: TBoldGrid;
    lblresultString1: TLabel;
    cbxblink1: TBoldCheckBox;
    cbxbold1: TBoldCheckBox;
    cbxunderline1: TBoldCheckBox;
    BoldNavigator1: TBoldNavigator;
    cbxnoValues1: TBoldCheckBox;
    blhFonts: TBoldListHandle;
    BoldLabel1: TBoldLabel;
    BoldEdit1: TBoldEdit;
    Label1: TLabel;
    BoldNavigator2: TBoldNavigator;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
  ReverseDeriveExampleClasses;

{$R *.DFM}

// Save all changes to database.
procedure TForm1.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

// Set Width and Height of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width  := 600;
  Height := 340;
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

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  btnSave.Enabled := BoldSystemHandle1.Active;
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  btnSave.Enabled := BoldSystemHandle1.Active;
end;

end.
