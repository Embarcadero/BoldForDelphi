
unit fdmTestCase;

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
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  TestSuite, BoldAbstractModel, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB, DBAccess, Uni, BoldDatabaseAdapterUniDAC ;

type
  TdmTestCase = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLocalTestCase = class(TBoldTestCase)
  public
    procedure Setup; override;
  end;

  TYourTestCaseHere = class(TLocalTestCase)
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    // If Setup is used, remember to invoke inherited!
  published
    //procedure TestCase1;
  end;


var
  dmTestCase: TdmTestCase = nil;

implementation

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmTestCase absolute dmTestCase;
  DoneSetup: boolean = false;

{$R *.DFM}

procedure EnsureDataModule;
begin
  // Uncomment EnsureDM_Model if the model in MAIN is used.
  //Ensuredm_Model;

  if not Assigned(TheDataModule) then
  begin
    TheDataModule := TdmTestCase.Create(Application);
    (TheDataModule.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDatabase;
    (TheDataModule.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBaseSchema;
    TheDataModule.BoldSystemHandle1.Active := True;
  end;
end;

procedure EnsureSetup;
begin
  if DoneSetup then Exit;

  EnsureDataModule;

  // Your global setup code here

  DoneSetup := True;
end;

{ TLocalTestCase }

procedure TLocalTestCase.Setup;
begin
  inherited;
  EnsureSetup;
end;

{ TYourTestCaseHere }

class procedure TYourTestCaseHere.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  // Testcases are registered with name of published method
  // and a string reference to the tested feature (specification, requirement, bug

//  --Add tests here--
  //ASuite.AddTest(Create('name_of_published_method1' , 'string_reference_to_specs'));
end;

initialization
  //TestGlobal.RegisterTestCase({your testcase-class here});
  //--Register testcase classes here--

end.
