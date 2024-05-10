
unit dmNullableMomentAttributes;

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
  TestSuite,
  NullableMomentAttributes,
  BoldSystem,
  BoldAttributes,
  TestFrameWork,
  BoldDBActions, BoldAbstractModel, DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB, DBAccess, Uni, BoldDatabaseAdapterUniDAC, System.Actions;

type
  TdmTestCase1 = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    ActionList1: TActionList;
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
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TNullableMomentAttributes = class(TLocalTestCase)
  private
    procedure AttemptSetToNull(anAttribute: TBoldAttribute; Nullable: boolean);
    procedure CheckInitialValue(anAttribute: TBoldAttribute; Null: boolean);
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    // If Setup is used, remember to invoke inherited!
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckNullableAttributes;
    procedure CheckNotNullAttributes;
    procedure CheckInitialValues;
    procedure SetParts;
  end;


var
  dmTestCase1: TdmTestCase1 = nil;

implementation

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmTestCase1 absolute dmTestCase1;
  DoneSetup: boolean = false;
  aDateClass: TClassWithDates;

{$R *.DFM}

procedure EnsureDataModule;
begin
  // Uncomment EnsureDM_Model if the model in MAIN is used.
  //Ensuredm_Model;

  if not Assigned(TheDataModule) then
  begin
    TheDataModule := TdmTestCase1.Create(Application);
    TheDataModule.BoldDatabaseAdapterUniDac1.CreateDatabase;
    TheDataModule.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    TheDataModule.BoldSystemHandle1.Active := True;
  end;
end;

procedure FreeDatamodule;
begin
  FreeAndNil(TheDataModule);
end;

procedure EnsureSetup;
begin
  if DoneSetup then Exit;

  EnsureDataModule;

  // Your global setup code here

  DoneSetup := True;
end;

procedure EnsureTeardown;
begin
  FreeDataModule;
  DoneSetUp := False;
end;

{ TLocalTestCase }

procedure TLocalTestCase.SetUp;
begin
  inherited;
  EnsureSetup;
end;

procedure TLocalTestCase.TearDown;
begin
  inherited;
  EnsureTearDown;
end;


{ TNullableMomentAttributes }

procedure TNullableMomentAttributes.AttemptSetToNull(anAttribute: TBoldAttribute;
  Nullable: boolean);
var
  name: string;
begin
  Name := anAttribute.BoldMemberRTInfo.ModelName;
  try
    anAttribute.SetToNull;
    Assert(Nullable, Name + ' is not null');
  except
    Assert(not Nullable, Name + ' is null');
  end;
  Assert(anAttribute.IsNull = Nullable, Name + ' wrong null state');
end;

procedure TNullableMomentAttributes.CheckInitialValues;
begin
  CheckInitialValue(aDateClass.M_aDate, false);
  CheckInitialValue(aDateClass.M_aTime, false);
  CheckInitialValue(aDateClass.M_aDateTime, false);

  CheckInitialValue(aDateClass.M_aNullableDate, true);
  CheckInitialValue(aDateClass.M_aNullableTime, true);
  CheckInitialValue(aDateClass.M_aNullableDateTime, true);
end;

procedure TNullableMomentAttributes.CheckInitialValue(
  anAttribute: TBoldAttribute; Null: boolean);
var
  name: string;
begin
  Name := anAttribute.BoldMemberRTInfo.ModelName;
  if Null then
    Assert(anAttribute.isNull, Name + ' is not null')
  else
    Assert(not anAttribute.isNull and (anAttribute.AsVariant = 0) , Name + ' is null');
end;

procedure TNullableMomentAttributes.CheckNotNullAttributes;
begin
  AttemptSetToNull(aDateClass.M_aDate, False);
  AttemptSetToNull(aDateClass.M_aTime, False);
  AttemptSetToNull(aDateClass.M_aDateTime, False);
end;

procedure TNullableMomentAttributes.CheckNullableAttributes;
begin
  AttemptSetToNull(aDateClass.M_aNullableDate, True);
  AttemptSetToNull(aDateClass.M_aNullableTime, True);
  AttemptSetToNull(aDateClass.M_aNullableDateTime, True);
end;

procedure TNullableMomentAttributes.SetParts;
begin
  // Check ability to set time part of DateTime
  try
    aDateClass.M_aNullableDateTime.SetToNull;
    aDateClass.M_aNullableDateTime.AsTime := Now;
    Assert(True, 'Could set time part');
  except
    Assert(False, 'Could not set time part');
  end;

  // Check ability to set date part of DateTime
  try
    aDateClass.M_aNullableDateTime.SetToNull;
    aDateClass.M_aNullableDateTime.AsDate := Now;
    Assert(True, 'Could set date part');
  except
    Assert(False, 'Could not set date part');
  end;

  // Check ability to set datetime part of DateTime
  try
    aDateClass.M_aNullableDateTime.SetToNull;
    aDateClass.M_aNullableDateTime.AsDateTime := Now;
    Assert(True, 'Could set datetime part');
  except
    Assert(False, 'Could not set datetime part');
  end;

end;

procedure TNullableMomentAttributes.SetUp;
begin
  inherited;
  aDateClass := TClasswithDates.Create(TheDataModule.BoldSystemHandle1.System);
end;

class procedure TNullableMomentAttributes.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  // Testcases are registered with name of published method
  // and a string reference to the tested feature (specification, requirement, bug

  ASuite.AddTest(CreateWithComment('CheckInitialValues' , 'B00088: Invalid initial value'));
  ASuite.AddTest(CreateWithComment('CheckNotNullAttributes' , 'B00088: Can not set to null'));
  ASuite.AddTest(CreateWithComment('CheckNullableAttributes' , 'B00088: Can set to null'));
  ASuite.AddTest(CreateWithComment('SetParts' , 'B00088: Set parts of attributes'));
end;


class function TNullableMomentAttributes.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'CheckInitialValues' , 'B00088: Invalid initial value');
  SetCommentForTest(Result, 'CheckNotNullAttributes' , 'B00088: Can not set to null');
  SetCommentForTest(Result, 'CheckNullableAttributes' , 'B00088: Can set to null');
  SetCommentForTest(Result, 'SetParts' , 'B00088: Set parts of attributes');
end;

procedure TNullableMomentAttributes.TearDown;
begin
  if Assigned(aDateClass) then
    aDateClass.Delete;
  inherited;
end;

initialization
  TestGlobal.RegisterTestCase(TNullableMomentAttributes);

end.
