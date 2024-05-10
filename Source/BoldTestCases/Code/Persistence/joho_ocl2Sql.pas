
{$INCLUDE BOLD.INC}

unit joho_ocl2Sql;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
//  Dialogs,
  BoldSubscription,
  BoldHandles,
  BoldElements,
  BoldSystemHandle,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldHandle,
  BoldPersistenceHandle,
  TestSuite,
  BoldLogHandler,
  BoldLogHandlerForm,
  BoldSystem,
  BoldSoapServerPersistenceHandles,
  BoldSoapClientPersistenceHandles,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldClientHandles,
  BoldComClientHandles,
  BoldComServerHandles,
  BoldServerHandles,
  BoldSQLSymbols,
  Ocl2SqlXMLServer,
  BoldDBActions,
  BoldAbstractModel,
  TestFramework,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  BoldDatabaseAdapterUniDAC,
  DBAccess,
  Uni;

type
  Tdm_ocl2sql = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    SQLListHandle: TBoldListHandle;
    BoldListHandle1: TBoldListHandle;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    UniConnection1: TUniConnection;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    BoldSOAPServerPersistenceHandle2: TBoldSOAPServerPersistenceHandle;
    BoldSOAPClientPersistenceHandle2: TBoldSOAPClientPersistenceHandle;
  end;

  Tjoho_ocl2Sql = class(TBoldTestCase)
  protected
    procedure ResetSystem;
    procedure TestExpression(Expr: String; CheckSize: Boolean = true; AssertSizeDifferent: Boolean = false);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  Tjoho_o2sNav = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Nav;
  end;

  Tjoho_o2sList = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure List;
  end;

  Tjoho_o2sBool = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Bool;
  end;

  Tjoho_o2sArith = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Arith;
  end;

  Tjoho_o2sOther = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Other;
  end;

  Tjoho_o2sCombo = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Combo;
  end;

  Tjoho_o2sBagSet = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure BagSet;
  end;

  TTestOCL2SQL = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  end;

  Tjoho_o2sNewStuff = class(Tjoho_ocl2Sql)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure NewStuff;
  end;


var
  dm_ocl2sql: Tdm_ocl2sql;
  G_TestXmlStreamingOfNodes: Boolean = true;

implementation

uses ocl2SqlTest;

procedure InitializeObjects;
var
  p1, p2, p3, p4, p5: TPerson;
  b1, b2, b3: TBuilding;
  rb1, rb2, rb3, rb4: TResidentialBuilding;
begin
  while dm_ocl2sql.BoldSystemHandle1.System.Classes[0].Count > 0 do
    dm_ocl2sql.BoldSystemHandle1.System.Classes[0][0].Delete;

  p1 := TPerson.Create(dm_ocl2sql.BoldSystemHandle1.System);
  p1.FirstName := 'Jonas';
  p1.LastName := 'Högström';
  p1.Assets := 10000;

  p2 := TPerson.Create(dm_ocl2sql.BoldSystemHandle1.System);
  p2.FirstName := 'Åsa';
  p2.LastName := 'Högström';
  p2.Assets := 20000;
  p2.isMarried := true;
  p1.Mother := p2;

  p3 := TPerson.Create(dm_ocl2sql.BoldSystemHandle1.System);
  p3.FirstName := 'Jesper';
  p3.LastName := 'Högström';
  p3.Assets := 5000;
  p3.Mother := p2;
  p3.isMarried := true;

  p4 := TPerson.Create(dm_ocl2sql.BoldSystemHandle1.System);
  p4.FirstName := 'Joanna';
  p4.LastName := 'Högström';
  p4.Assets := 20;

  p5 := TPerson.Create(dm_ocl2sql.BoldSystemHandle1.System);
  p5.FirstName := 'Lotta';
  p5.LastName := 'Lindberg';
  p5.Assets := 20000;
  p5.isMarried := true;
  p4.Mother := p5;

  b1 := TBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  b1.Address := 'Drakens Gränd';
  b1.Owners.Add(p1);
  b1.Owners.Add(p3);

  b2 := TBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  b2.Address := 'husvagnen';
  b2.Owners.Add(p2);

  b3 := TBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  b3.Address := 'Lekstugan';
  b3.Owners.Add( p4 );

  rb1 := TResidentialBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  rb1.Address := 'Lintrådsvägen';
  rb1.TotalRent := 10000;
  rb1.Residents.Add( p2 );
  rb1.Owners.Add( p2 );

  rb2 := TResidentialBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  rb2.Address := 'Skorpvägen';
  rb2.TotalRent := 5000;
  rb2.Residents.Add( p3);
  rb2.Residents.Add( p4);
  rb2.Residents.Add( p5);
  rb2.Owners.Add( p3);
  rb2.Owners.Add( p5);

  rb3 := TResidentialBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  rb3.address := 'Heleneborgsg';
  rb3.TotalRent := 5000;
  rb3.Residents.Add( p1 );

  rb4 := TResidentialBuilding.Create(dm_ocl2sql.BoldSystemHandle1.System);
  rb4.Address := 'Birger Sjöbergs väg';
  rb4.TotalRent := 4000;
  rb4.Owners.Add( p1 );
end;

procedure ActivateCOM;
begin
  if G_TestXmlStreamingOfNodes then
  begin
    dm_ocl2sql.BoldComConnectionHandle1.Connected := true;
    dm_ocl2sql.BoldPersistenceHandleDB1.Active := true;
  end;
end;

procedure DeActivateCOM;
begin
  if dm_ocl2sql.BoldComConnectionHandle1.Connected then
    dm_ocl2sql.BoldComConnectionHandle1.Connected := false;
end;

procedure EnsureDB;
begin
  if not dm_ocl2sql.BoldSystemHandle1.Active then
  begin
    dm_ocl2sql.BoldDatabaseAdapterUniDAC1.CreateDatabase;
    dm_ocl2sql.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    ActivateCOM;
    dm_ocl2sql.BoldSystemHandle1.Active := True;
    InitializeObjects;
    dm_ocl2sql.BoldSystemHandle1.UpdateDatabase;
  end;
end;

procedure EnsureDM;
begin
  if not Assigned(dm_ocl2sql) then
  begin
    Application.CreateForm(Tdm_ocl2sql, dm_ocl2sql);
  end;
  dm_ocl2sql.BoldSystemHandle1.PersistenceHandle := dm_ocl2sql.BoldPersistenceHandleDB1;

  if G_TestXmlStreamingOfNodes then
    dm_ocl2sql.BoldSystemHandle1.PersistenceHandle := dm_ocl2sql.BoldSOAPClientPersistenceHandle2;

  EnsureDB;

  ActivateCom;
end;

procedure FreeDM;
begin
  DeActivateCOM;
  FreeAndNil(dm_ocl2sql);
end;


{$R *.DFM}

{ Tjoho_o2s }

procedure Tjoho_ocl2sql.ResetSystem;
begin
  if dm_ocl2sql.BoldSystemHandle1.Active then
  begin
    dm_ocl2sql.BoldSystemHandle1.System.Discard;
    dm_ocl2sql.BoldSystemHandle1.Active := false;
  end;

  dm_ocl2sql.BoldSystemHandle1.Active := True;
end;

procedure Tjoho_ocl2sql.TestExpression( Expr: String; CheckSize: Boolean = true; AssertSizeDifferent: Boolean = false);
var
  sqlList : TBoldList;
  oclList: TBoldList;
  i: integer;
  LocatorTraverser: TBoldLocatorListTraverser;
begin
  ResetSystem;
  dm_ocl2sql.SQLListHandle.MarkOutOfDate;
  dm_ocl2sql.SQLListHandle.Expression := Expr;
  SqlList := dm_ocl2sql.SQLListHandle.list;
  sqlList.EnsureRange(0, sqlList.count-1);

  LocatorTraverser := dm_ocl2sql.BoldSystemHandle1.System.Locators.CreateTraverser;
  i := 0;
  while LocatorTraverser.MoveNext do
    if assigned(LocatorTraverser.Locator.BoldObject) then
      inc(i);

  LocatorTraverser.Free;
  assert( i = sqlList.Count, 'SQL loaded more objects than necessary: ' + Expr);

  dm_ocl2sql.BoldListHandle1.MarkOutOfDate;
  dm_ocl2sql.BoldListHandle1.Expression := Expr;
  oclList := dm_ocl2sql.BoldListHandle1.List;

  BoldLog.Log( Expr );
  BoldLog.Log( 'SQL:' );
  for i := 0 to sqlList.Count-1 do
    BoldLog.Log( '* ' + sqlList[i].AsString);

  BoldLog.Log( 'OCL:' );
  for i := 0 to oclList.Count-1 do
    BoldLog.Log( '* ' + oclList[i].AsString);

  BoldLog.Log( '---' );


  try
    if CheckSize then
      assert( oclList.count = sqlList.Count, 'Count mismatch: '+Expr );

    if assertSizeDifferent then
      assert( oclList.count > sqlList.Count, 'Count mismatch: '+Expr );

    for i := 0 to sqlList.Count-1 do
      assert( oclList.IndexOf( sqlList[i] ) <> -1, 'ObjectMismatch: ' +Expr);
  except
    on e: exception do
    begin
      BoldLog.Log( 'Exception: '+e.message);
      raise;
    end;
  end;

end;

procedure Tjoho_ocl2sql.SetUp;
begin
  EnsureDM;
end;

{ Tjoho_o2sNav }

procedure Tjoho_o2sNav.Nav;
var
  p1: TPerson;
  ie: TBoldIndirectElement;
begin
  // SingleLink, then Boolean attr
  testExpression('Person.allInstances->select(mother.ismarried)' );

  // linkclass
  testExpression('Person.allInstances->select(ownership->select(ownedBuildings.address <> '''')->notempty)' );
  testExpression('Ownership.allInstances.owners.ownership', false);

  // Single-relation
  TestExpression('Person.allInstances.home', false);
  // indirect multirelation
  TestExpression('Person.allInstances.ownedBuildings', false);
  // direct multirelation
  TestExpression('ResidentialBuilding.allInstances.residents', false);
  // attributes
  TestExpression('Person.allInstances->select(assets > 1000)');

  // some combinations
  testExpression('Person.allInstances->select(home.totalRent > assets).home', false);
  testExpression('Person.allInstances->select(home.totalRent > assets)');
  TestExpression('Person.allInstances->select(mother.firstName = ''Lotta'')');
  TestExpression('Person.allInstances->select(mother.firstName.isNull)');

  testExpression('Person.allInstances->select(mother->isEmpty)' );

  // Jonas Owns Drakens gränd and Birger Sjönbergs väg;
  p1 := dm_ocl2sql.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Person.AllInstances->select(firstname = ''Jonas'')->first') as TPerson;
  ie := TBoldIndirectElement.Create;
  p1.OwnedBuildings.EvaluateExpression('Building.allInstances->difference(self)', ie, true);
  assert(ie.value is TBoldObjectList);
  assert((ie.value as TBoldObjectList).Count = 7-2);
  ie.free;
end;

class procedure Tjoho_o2sNav.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Nav', '2.1.1'));
end;

class function Tjoho_o2sNav.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Nav', '2.1.1');
end;

{ Tjoho_o2sList }

procedure Tjoho_o2sList.List;
begin
  // allInstances
  TestExpression('Person.allInstances');

  // select + size
  TestExpression('Person.allInstances->select(ownedbuildings->size > 1)');

  // size on singlelink
  testExpression('Person.allInstances->select(home->size > 0)');

  // reject
  TestExpression('Person.allInstances->reject(assets > 1000)');

  // OrderBy
  TestExpression('Person.allInstances->orderby(firstname)');
{  // OrderDescending
  TestExpression('Person.allInstances->orderDescending(firstname)');
}

  // minValue
  TestExpression('Building.allInstances->select(owners.assets->minValue > 1000)');
  // maxValue
  TestExpression('Building.allInstances->select(owners.assets->maxValue > 1000)');
  // average
  TestExpression('Building.allInstances->select(owners.assets->average > 1000)');
  // sum
  TestExpression('Building.allInstances->select(owners.assets->sum > 1000)');
  // exists, forall
  TestExpression('Building.allInstances->select(owners->exists(assets > 1000))');
  TestExpression('Building.allInstances->select(owners->forall(assets > 1000))');
  // isEmpty, NotEmpty
  TestExpression('Building.allInstances->select(owners->isEmpty)');
  TestExpression('Building.allInstances->select(owners->notEmpty)');

  // includes
  testExpression('Person.allInstances->select(ownedbuildings->includes(home))');
end;

class procedure Tjoho_o2sList.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('List', '2.1.2.1'));
end;

class function Tjoho_o2sList.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'List', '2.1.2.1');
end;

{ Tjoho_o2sBool }

procedure Tjoho_o2sBool.Bool;
begin
  TestExpression('Person.allInstances->select((firstname = lastname) and (lastname = firstname))');
  TestExpression('Person.allInstances->select(isMarried)');
//  TestExpression('Person.allInstances->select(isMarried = true)');
//  TestExpression('Person.allInstances->select(isMarried = isMarried)');
  TestExpression('Person.allInstances->select((isMarried and isMarried) or ((not isMarried) and (not isMarried)))');
  // =, <>, <, >, <=, >=
  TestExpression('Person.allInstances->select(firstName = lastName)');
  TestExpression('Person.allInstances->select(firstName <> lastName)');
  TestExpression('Person.allInstances->select(firstName < lastName)');
  TestExpression('Person.allInstances->select(firstName > lastName)');
  TestExpression('Person.allInstances->select(firstName <= lastName)');
  TestExpression('Person.allInstances->select(firstName >= lastName)');
  // and, or, not, implies
  TestExpression('Person.allInstances->select(ownedBuildings->isEmpty and home->notEmpty)');
  TestExpression('Person.allInstances->select(ownedBuildings->isEmpty or home->notEmpty)');
  TestExpression('Person.allInstances->select(ownedBuildings->isEmpty implies home->notEmpty)');
  TestExpression('Person.allInstances->select(not (ownedBuildings->isEmpty and home->notEmpty))');

  // xor - not implemented in interbase
//  TestExpression('Person.allInstances->select(ownedBuildings->isEmpty xor home->notEmpty)');

  // sqlLike, sqlLikeCaseInsensitive
  TestExpression('Person.allInstances->select(firstName.sqlLike( ''Jo%''))');
  TestExpression('Person.allInstances->select(firstName.sqlLikeCaseInsensitive( ''jO%''))');
end;

class procedure Tjoho_o2sBool.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Bool', '2.1.2.2'));
end;

class function Tjoho_o2sBool.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Bool', '2.1.2.2');
end;

{ Tjoho_o2sArith }

procedure Tjoho_o2sArith.Arith;
begin
  // +, *, /, -
  TestExpression('Person.allInstances->select(assets > -1)');

  TestExpression('Person.allInstances->select(assets + 3000 > 5000)');
  TestExpression('Person.allInstances->select(assets * 2 > 5000)');
  TestExpression('Person.allInstances->select(assets / 2 > 5000)');
  TestExpression('Person.allInstances->select(assets - 5000 > 5000)');

  // div - not implemented in interbase
//  TestExpression('ResidentialBuilding.allInstances->select((residents->size div 2) = 0)');

  // mod - Not implemented in interbase
//  TestExpression('ResidentialBuilding.allInstances->select((residents->size mod 2) = 0)');
end;

class procedure Tjoho_o2sArith.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Arith', '2.1.2.3'));
end;

class function Tjoho_o2sArith.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Arith', '2.1.2.3');
end;

{ Tjoho_o2sOther }

procedure Tjoho_o2sOther.Other;
begin
  // isNull
  TestExpression('Person.allInstances->select(firstName.isNull)');
  TestExpression('Person.allInstances->select(not firstName.isNull)');

  // toUpper
  TestExpression('Person.allInstances->select(firstName.toUpper = ''JESPER'')');

  // Operation on Query-carrying nodes
  TestExpression('Person.allInstances->select(home.address.sqlLikeCaseInsensitive(''Lin%''))');
  TestExpression('Person.allInstances->select(home.address.sqlLike(''Lin%''))');
end;

class procedure Tjoho_o2sOther.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Other', '2.1.2.4'));
end;

class function Tjoho_o2sOther.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Other', '2.1.2.4');
end;

{ Tjoho_o2sCombo }

procedure Tjoho_o2sCombo.Combo;
begin
  // various expressions that are combinations of the above, and should all work

  // many navigations
  testExpression('Person.allInstances->select(mother.children.mother->includes(mother))');

  // other
  testExpression('Person.allInstances->select(ownedbuildings->forall(owners->size = 1))');
  testExpression('Person.allInstances->select((assets > 10000) or (home->size > 0))');
  testExpression('ResidentialBuilding.allInstances->reject(address.isNull)');
  testExpression('ResidentialBuilding.allInstances->reject(residents->isEmpty)');
  testExpression('ResidentialBuilding.allInstances->reject(residents->notEmpty)');
  testExpression('Person.allInstances->select((firstName = ''Jonas'') and (lastName = ''Högström''))');
  testExpression('Person.allInstances->select(firstName <> ''Jonas'')');
  testExpression('Person.allInstances->select((home.totalRent > 1000) and (home.totalRent < 3000))');
end;

class procedure Tjoho_o2sCombo.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Combo'));
end;

class function Tjoho_o2sCombo.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Combo', '');  
end;

{ Tjoho_o2sBagSet }

procedure Tjoho_o2sBagSet.BagSet;
begin
  TestExpression('Person.allInstances.home', false, true);
end;

class procedure Tjoho_o2sBagSet.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('BagSet', '2.2.5'));
end;

class function Tjoho_o2sBagSet.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'BagSet', '2.2.5');
end;

{ TTestOCL2SQL }

class procedure TTestOCL2SQL.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  Tjoho_o2sNav.Suit(aSuite);
  Tjoho_o2sList.Suit(aSuite);
  Tjoho_o2sBool.Suit(aSuite);
  Tjoho_o2sArith.Suit(aSuite);
  Tjoho_o2sOther.Suit(aSuite);
  Tjoho_o2sBagSet.Suit(aSuite);
  Tjoho_o2sCombo.Suit(aSuite);
end;

class function TTestOCL2SQL.Suite: ITestSuite;
begin
  result := inherited Suite;
end;

{ Tjoho_o2sNewStuff }

procedure Tjoho_o2sNewStuff.NewStuff;
begin
  // typecasting

  testExpression('Building.allInstances->select(oclIsKindOf(ResidentialBuilding))');
  testExpression('Building.allInstances->select(oclIsKindOf(ResidentialBuilding) and (oclAsType(ResidentialBuilding).address = ''lekstugan''))');
  testExpression('Person.allInstances->select(home.oclIsKindOf(ResidentialBuilding))');
  testExpression('Building.allInstances->filterOnType(ResidentialBuilding)');
  testExpression('BusinessClassesRoot.allInstances->select(oclIsTypeOf(Building))');

  // multiselect
  testExpression('Person.allInstances->select(assets > 1000)->select(assets < 1000)');
  testExpression('Person.allInstances->select(assets > 1000)->orderby(firstName)');

  // union
  testExpression('Person.allInstances->union(Building.allInstances)');
  testExpression('Person.allInstances->select(ownedbuildings->union(home.residents)->size > 3)');

  testExpression('ResidentialBuilding.allInstances->select(owners->intersection(residents)->exists(firstName = ''Jonas''))');
  testExpression('ResidentialBuilding.allInstances->select(owners->difference(residents)->exists(firstName = ''Jonas''))');

//  testExpression('ResidentialBuilding.allInstances->select(owners->symmetricDifference(residents)->exists(firstName = ''Jonas''))');


end;

class procedure Tjoho_o2sNewStuff.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('NewStuff', '---'));
end;

procedure Tjoho_ocl2Sql.TearDown;
begin
  FreeDM;
  inherited;
end;

procedure Tdm_ocl2sql.DataModuleCreate(Sender: TObject);
begin
  BoldSOAPServerPersistenceHandle2 := TBoldSOAPServerPersistenceHandle.Create(self);
  BoldSOAPServerPersistenceHandle2.BoldHandle := BoldPersistenceHandleDB1;
  BoldSOAPServerPersistenceHandle2.BoldModel := BoldModel1;
  BoldSOAPServerPersistenceHandle2.name := 'BoldSOAPServerPersistenceHandle2';
  BoldSOAPServerPersistenceHandle2.ObjectName := 'BoldSOAPServerPersistenceHandle2';
  BoldSOAPServerPersistenceHandle2.ServerClass := 'Ocl2SqlXMLtestServer';
  BoldSOAPServerPersistenceHandle2.ServerHandle := BoldComServerHandle1;

  BoldSOAPClientPersistenceHandle2 := TBoldSOAPClientPersistenceHandle.Create(self);
  BoldSOAPClientPersistenceHandle2.BoldModel := BoldModel1;
  BoldSOAPClientPersistenceHandle2.ConnectionHandle := BoldComConnectionHandle1;
  BoldSOAPClientPersistenceHandle2.Name := 'BoldSOAPClientPersistenceHandle2';
  BoldSOAPClientPersistenceHandle2.Objectname := 'BoldSOAPServerPersistenceHandle2';
end;

class function Tjoho_o2sNewStuff.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'NewStuff', '---');
end;

initialization
  TestGlobal.RegisterTestCase(Tjoho_o2sNewStuff);

  TestGlobal.RegisterTestCase(Tjoho_o2sNav);
  TestGlobal.RegisterTestCase(Tjoho_o2sList);
  TestGlobal.RegisterTestCase(Tjoho_o2sBool);
  TestGlobal.RegisterTestCase(Tjoho_o2sArith);
  TestGlobal.RegisterTestCase(Tjoho_o2sOther);
  TestGlobal.RegisterTestCase(Tjoho_o2sBagSet);
  TestGlobal.RegisterTestCase(Tjoho_o2sCombo);

  TestGlobal.RegisterTestCase(TTestOCL2SQL);

{ChapitrePM.allInstances->select(
  (
    (statutc = ' ') and
    (dclenvoyeef = 0) and
    (contribancientbadr.languec = '1')
  )
  and
  (
    (contribancientbadr.codeannexeno = 01) or
    (contribancientbadr.codeannexeno = 02)
  )
  and
  (
    tiers.oclAsType(Personne).rcinscriptiond >= '01.07.2000'
  )
)
}
end.




