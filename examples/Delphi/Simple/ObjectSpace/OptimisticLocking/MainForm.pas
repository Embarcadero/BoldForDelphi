unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Grids,
  BoldGrid,
  ExtCtrls,
  BoldNavigator,
  StdCtrls,
  BoldActions,
  ActnList,
  BoldAbstractModel,
  BoldModel,
  BoldHandleAction,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldVariableHandle,
  BoldRootedHandles,
  BoldCursorHandle,
  BoldAbstractListHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldSystem,
  BoldListBox,
  BoldAFPPluggable,
  BoldDBActions,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    GroupBox1: TGroupBox;
    cmdCreateDB: TButton;
    cmdOpen1: TButton;
    blhAllPersons1: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    blhAllCars1: TBoldListHandle;
    BoldGrid2: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    BoldNavigator2: TBoldNavigator;
    BoldActivateSystemAction2: TBoldActivateSystemAction;
    cmdUpdate1: TButton;
    bvhFailedObjects1: TBoldVariableHandle;
    bchFailedObjects1: TBoldCursorHandle;
    BoldListBox1: TBoldListBox;
    cmdDiscardApp1: TButton;
    GroupBox2: TGroupBox;
    cmdOpen2: TButton;
    BoldGrid3: TBoldGrid;
    BoldGrid4: TBoldGrid;
    BoldNavigator3: TBoldNavigator;
    BoldNavigator4: TBoldNavigator;
    cmdUpdate2: TButton;
    BoldListBox2: TBoldListBox;
    cmdDiscardApp2: TButton;
    blhAllPersons2: TBoldListHandle;
    blhallCars2: TBoldListHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    bvhFailedObjects2: TBoldVariableHandle;
    bchFailedObjects2: TBoldCursorHandle;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    Label1: TLabel;
    Label2: TLabel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB2: TBoldDatabaseAdapterIB;
    IBDatabase2: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdUpdate1Click(Sender: TObject);
    procedure cmdDiscardApp1Click(Sender: TObject);
    procedure cmdUpdate2Click(Sender: TObject);
    procedure cmdDiscardApp2Click(Sender: TObject);
  private
    { Private declarations }
    procedure DiscardObject(BoldObject: TBoldObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 465;
  Width  := 651;
end;

// Give the user the opportunity to save changes before close.
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if ((BoldSystemHandle1.Active and BoldSystemHandle1.System.BoldDirty) or
      (BoldSystemHandle2.Active and BoldSystemHandle2.System.BoldDirty)) then
    case MessageDlg( 'There are dirty objects. save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: begin
        BoldSystemHandle1.System.UpdateDatabase;
        BoldSystemHandle2.System.UpdateDatabase;
      end;
      mrNo: begin
        BoldSystemHandle1.System.Discard;
        BoldSystemHandle2.System.Discard;
      end;
      mrCancel: CanClose := False;
    end;
end;

procedure TForm1.cmdUpdate1Click(Sender: TObject);
begin
  bchFailedObjects1.List.Clear;
  try
    BoldSystemHandle1.UpdateDataBase;
  except
    on e: EBoldOperationFailedForObjectlist do
    begin
      bchFailedObjects1.List.AddList(e.ObjectList);
      raise;
    end;
  end;
end;

procedure TForm1.cmdDiscardApp1Click(Sender: TObject);
begin
  DiscardObject(bchFailedObjects1.CurrentBoldObject);
end;

procedure TForm1.cmdUpdate2Click(Sender: TObject);
begin
  bchFailedObjects2.List.Clear;
  try
    BoldSystemHandle2.UpdateDataBase;
  except
    on e: EBoldOperationFailedForObjectlist do
    begin
      bchFailedObjects2.List.AddList(e.ObjectList);
      raise;
    end;
  end;
end;

procedure TForm1.cmdDiscardApp2Click(Sender: TObject);
begin
  DiscardObject(bchFailedObjects2.CurrentBoldObject);
end;

procedure TForm1.DiscardObject(BoldObject: TBoldObject);
begin
  if Assigned(BoldObject) then
    BoldObject.Discard
  else
    raise Exception.Create('No object selected to discard');
end;

end.
