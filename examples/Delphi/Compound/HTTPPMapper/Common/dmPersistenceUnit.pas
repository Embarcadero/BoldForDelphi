unit dmPersistenceUnit;

interface

uses
  SysUtils, Classes, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldSubscription, BoldHandle,
  BoldPersistenceHandle, BoldAbstractPersistenceHandleDB,
  BoldPersistenceHandleDB;

type
  TdmPersistence = class(TDataModule)
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure IBDatabase1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmPersistence: TdmPersistence;

implementation

uses
  dmCoreUnit,
  BoldUtils;

{$R *.dfm}

procedure TdmPersistence.IBDatabase1BeforeConnect(Sender: TObject);
begin
  IBDatabase1.DatabaseName := 'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(IBDatabase1.DatabaseName);
end;

end.
