unit MainDataModule;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  BoldHandles,
  BoldSystemHandle,
  BoldAbstractModel,
  BoldModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldElements,
  BoldManipulators,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB, DB, IBDatabase, BoldDatabaseAdapterIB;

type
  TdmMain = class(TDataModule)
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    OrgChartSystem: TBoldSystemHandle;
    SystemTypeInfo: TBoldSystemTypeInfoHandle;
    BoldManipulator1: TBoldManipulator;
    OrgchartModel: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure IBDatabase1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GenerateData;
  end;

var
  dmMain: TdmMain;

implementation

uses
  OrgChartClasses,
  BoldAttributes,
  BoldUtils;

{$R *.DFM}

{ TdmMain }

procedure TdmMain.GenerateData;
var
  aCompany: TCompany;
  anOffice: TOffice;
  aDepartment: TDepartment;
  aPerson: TPerson;

  function CreateDepartment(theName: string): TDepartment;
  begin
    Result := TDepartment.Create(OrgChartSystem.System);
    Result.Name := theName;
    Result.Office := anOffice;
  end;

  function CreatePerson(theFirstName, theLastName: string): TPerson;
  begin
    Result := TPerson.Create(OrgChartSystem.System);
    Result.FirstName := theFirstName;
    Result.LastName := theLastName;
    Result.Employer.Add(aCompany);
  end;

begin
  if not (OrgChartSystem.Active) then
    Exit;
  aCompany := TCompany.Create(OrgChartSystem.System);
  aCompany.CompanyName := 'BoldSoft MDE';
  if FileExists('Bold.bmp') then
    aCompany.M_Logo.  CreateBlobStream(bmReadWrite).LoadFromFile('bold.bmp');
  anOffice := TOffice.Create(OrgChartSystem.System);
  aCompany.Offices.Add(anOffice);
  anOffice.Name := 'BoldSoft Sweden';
  anOffice.EstablishedDate := EncodeDate(1998, 04, 08);
  anOffice.phone := '+46-(0)8-545 182 40';
  anOffice.fax := '+46-(0)8-545 182 41';
  anOffice.Email := 'info@borland.com';
  anOffice.address := TAddress.Create(OrgChartSystem.System);
  anOffice.address.street := 'Drakens Gränd 8';
  anOffice.address.city := 'Stockholm';
  anOffice.address.country := 'Sweden';
  anOffice.address.postcode := '111 30';

  aDepartment := CreateDepartment('R&D');
  aPerson := CreatePerson('Mary', 'Ananian');
  aPerson.Employment[0].Department := aDepartment;
  aPerson.Employment[0].Title := 'Developer';

  aPerson := CreatePerson('Isabel', 'Jemio');
  aPerson.Employment[0].Department := aDepartment;
  aPerson.Employment[0].Title := 'Developer';

  aDepartment := CreateDepartment('Sales');
  aPerson := CreatePerson('Dan', 'Nygren');
  aPerson.Employment[0].Department := aDepartment;
  aPerson.Employment[0].Title := 'Account Manager';

  aDepartment := CreateDepartment('Management');
  aPerson := CreatePerson('Henrik', 'Jondell');
  aPerson.Employment[0].Department := aDepartment;
  aPerson.Employment[0].Title := 'CEO';
end;

procedure TdmMain.IBDatabase1BeforeConnect(Sender: TObject);
begin
  IBDatabase1.DatabaseName := 'localhost:' + ExtractFilePath(GetModuleFileNameAsString(True)) + ExtractFileName(IBDatabase1.DatabaseName);
end;

end.
