unit SimpleTreeViewForm;

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
  BoldPersistenceHandle,
  BoldHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldAbstractModel,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  BoldDBActions,
  ActnList,
  BoldHandleAction,
  BoldActions,
  StdCtrls,
  ExtCtrls,
  BoldNavigatorDefs,
  BoldNavigator,
  Grids,
  BoldGrid,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  ImgList,
  ComCtrls,
  BoldTreeView, BoldPersistenceHandleDB,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    Button2: TButton;
    Button1: TButton;
    btnUpdateDB: TButton;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    blhCompanyList: TBoldListHandle;
    blhDepartmentList: TBoldListHandle;
    blhEmployeeList: TBoldListHandle;
    gridShool: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    gridClass: TBoldGrid;
    BoldNavigator2: TBoldNavigator;
    BoldGrid1: TBoldGrid;
    BoldNavigator3: TBoldNavigator;
    btvCompanyDepartments: TBoldTreeView;
    imlTreeStructure: TImageList;
    btvCompanyEmployees: TBoldTreeView;
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnUpdateDBClick(Sender: TObject);
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

procedure TForm1.btnUpdateDBClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := 741;
  Height := 502;
end;

end.
