unit MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Grids,
  StdCtrls,
  ComCtrls,
  ActnList,
  BoldHandle,
  BoldHandles,
  BoldRootedHandles,
  BoldSystemHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldNavigator,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldModel,
  BoldCheckBox,
  BoldGrid,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    pbcParentOrChild: TPageControl;
    tsParent: TTabSheet;
    bgrdJet: TBoldGrid;
    bgrdProp: TBoldGrid;
    bnavJet: TBoldNavigator;
    bnavProp: TBoldNavigator;
    blhJet: TBoldListHandle;
    blhProp: TBoldListHandle;
    lblJet: TLabel;
    lblProp: TLabel;
    tsChild: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    lblTruck: TLabel;
    bgrdTruck: TBoldGrid;
    lblBus: TLabel;
    bgrdBus: TBoldGrid;
    bnavBus: TBoldNavigator;
    bnavTruck: TBoldNavigator;
    blhTruck: TBoldListHandle;
    blhBus: TBoldListHandle;
    bchkThrustVector: TBoldCheckBox;
    Button1: TButton;
    ActionList: TActionList;
    BoldActivateSystemAction: TBoldActivateSystemAction;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

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

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width  := 331;
  Height := 395;
end;

end.
