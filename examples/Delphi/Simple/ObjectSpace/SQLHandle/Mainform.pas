unit Mainform;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Grids,
  ComCtrls,
  Buttons,
  ActnList,
  BoldSubscription,
  BoldHandle,
  BoldHandles,
  BoldRootedHandles,
  BoldDerivedHandle,
  BoldSystemHandle,
  BoldTypeNameHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldSQLHandle,
  BoldAbstractModel,
  BoldModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldNavigator,
  BoldGrid,
  BoldElements,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    grdAllPersons: TBoldGrid;
    bnAllPersons: TBoldNavigator;
    Label1: TLabel;
    blhAllPersons: TBoldListHandle;
    GroupBox1: TGroupBox;
    btnUpdateDB: TButton;
    bsqlhPersons: TBoldSQLHandle;
    gbSelection: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cmbAssetsOperator: TComboBox;
    cmbFirstnameOperator: TComboBox;
    cmbLastnameOperator: TComboBox;
    edAssetsExpr: TEdit;
    edFirstnameExpr: TEdit;
    edLastnameExpr: TEdit;
    gbSorting: TGroupBox;
    cmbOrderBy1: TComboBox;
    Label5: TLabel;
    cmbOrderBy3: TComboBox;
    cmbOrderBy2: TComboBox;
    Label6: TLabel;
    grdSQLResult: TBoldGrid;
    blhSelectedPersons: TBoldListHandle;
    btExecSQL: TButton;
    Label7: TLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button2: TButton;
    Button3: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btExecSQLClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
  private
    procedure CreateDefaults;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  SQLHandleClasses,
  BoldAttributes;

{$R *.DFM}

procedure TForm1.btExecSQLClick(Sender: TObject);
var
  Conjunction,
  WhereClause,
  OrderByClause: String;
begin
  if BoldSystemHandle1.System.BoldDirty then
    ShowMessage('There are changes that are not saved to the database. Results may not be as expected');
  Conjunction := '';
  // just some code to set up the order by and where statements for this example
  if cmbAssetsOperator.Text <> '' then
  begin
    WhereClause := Format('Assets %s %s', [cmbAssetsOperator.Text, edAssetsExpr.Text]);
    Conjunction := ' and';
  end;
  if cmbFirstNameOperator.Text <> '' then
  begin
    WhereClause := WhereClause + Format('%s Firstname %s ''%s''', [Conjunction, cmbFirstnameOperator.Text, edFirstnameExpr.Text]);
    Conjunction := ' and';
  end;

  if cmbLastnameOperator.Text <> '' then
    WhereClause := WhereClause + Format('%s Lastname %s ''%s''', [Conjunction, cmbLastnameOperator.Text, edLastnameExpr.Text]);

  OrderByClause := cmbOrderBy1.Text + ', ' + cmbOrderBy2.Text + ', ' + cmbOrderBy3.Text;

  bsqlhPersons.SQLOrderByClause := OrderByClause;
  bsqlhPersons.SQLWhereClause := WhereClause;
  bsqlhPersons.ExecuteSQL;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active then
    if BoldSystemHandle1.System.BoldDirty then
      case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: BoldSystemHandle1.System.UpdateDatabase;
        mrNo: BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  CreateDefaults;
  btExecSQL.Enabled := BoldSystemHandle1.Active;
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  btExecSQL.Enabled := BoldSystemHandle1.Active;
end;

procedure TForm1.CreateDefaults;
var
  aPerson: TPerson;
  i: Integer;
const
  ExamplePersons: array [1..10, 1..3] of String = (
  ('1', 'Adam', 'J'),
  ('2', 'Bertil', 'I'),
  ('3', 'Caesar', 'H'),
  ('4', 'David', 'G'),
  ('5', 'Eric', 'F'),
  ('6', 'Fredrik', 'E'),
  ('7', 'Gustaf', 'D'),
  ('8', 'Henrik', 'C'),
  ('9', 'Ivar', 'B'),
  ('10', 'Jacob', 'A')
  );

begin
  // adds some exampel persons if there are none when the system gets activated.
  if blhAllPersons.Count = 0 then
  begin
    for i := 1 to 10 do
    begin
      aPerson := TPerson.Create(nil);
      aPerson.Assets := StrToFloat(ExamplePersons[i, 1]);
      aPerson.firstname := ExamplePersons[i, 2];
      aPerson.lastName := ExamplePersons[i, 3]; 
      (blhAllPersons.List as TPersonList).Add(aPerson);
    end;
  end;
end;

end.
