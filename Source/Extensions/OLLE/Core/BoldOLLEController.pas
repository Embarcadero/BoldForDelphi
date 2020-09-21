unit BoldOLLEController;

interface

uses
  classes,
  BoldDefs,
  BoldOLLEdmmain,
  BoldSystem,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldAbstractPersistenceHandleDB;

type
  TBoldPHandleMimic = class(TBoldAbstractPersistenceHandleDB)
  private
    fPHandle: TBoldAbstractPersistenceHandleDB;
    fSQLDatabaseConfig: TBoldSQLDatabaseConfig;
  protected
    function GetDataBaseInterface: IBoldDatabase; override;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig; override;
  public
    destructor Destroy; override;
  end;

  TBoldOLLEController = class
  private
    fOlleDM: TdmOll;
    fMimicPHandle: TBoldPHandleMimic;
    function GetOLLESystem: TBoldSystem;
    function GetPersistent: Boolean;
    procedure SetPersistent(const Value: Boolean);
  public
    constructor Create(PersistenceHandle: TBoldAbstractPersistenceHandleDB);
    procedure GenerateDatabase;
    property OLLESystem: TBoldSystem read GetOLLESystem;
    property Persistent: Boolean read GetPersistent write SetPersistent;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  OLLEConsts;

{ TBoldOLLEController }

constructor TBoldOLLEController.Create(PersistenceHandle: TBoldAbstractPersistenceHandleDB);
begin
  fOlleDM := TdmOLL.Create(nil);
  fMimicPHandle := TBoldPHandleMimic.create(fOlleDM);
  fMimicPHandle.fPHandle := PersistenceHandle;
  fMimicPHandle.BoldModel := fOlleDM.BoldModel1;
  fMimicPHandle.SQLDatabaseConfig.AssignConfig(PersistenceHandle.SQLDatabaseConfig);
  fMimicPHandle.SQLDataBaseConfig.SystemTablePrefix := 'OLLE'; // do not localize
  fOlleDM.BoldObjectInfoSystem.PersistenceHandle := fMimicPHandle;
end;

procedure TBoldOLLEController.GenerateDatabase;
begin
  fMimicPHandle.CreateDataBaseSchema(True);
end;

function TBoldOLLEController.GetOLLESystem: TBoldSystem;
begin
  fOlleDM.BoldObjectInfoSystem.Active := True;
  result := fOlleDM.BoldObjectInfoSystem.System;
end;

function TBoldOLLEController.GetPersistent: Boolean;
begin
  result := assigned(fOlleDM.BoldObjectInfoSystem.PersistenceHandle);
end;

procedure TBoldOLLEController.SetPersistent(const Value: Boolean);
begin
  if value <> Persistent then
  begin
    if fOlleDm.BoldObjectInfoSystem.Active then
      raise Exception.CreateFmt(sCannotChangePersistenceWhenActive, [ClassName]);
    if Value then
      fOlleDM.BoldObjectInfoSystem.PersistenceHandle := fMimicPHandle
    else
      fOlleDM.BoldObjectInfoSystem.PersistenceHandle := nil;
  end;
end;

{ TBoldPHandleMimic }

destructor TBoldPHandleMimic.destroy;
begin
  FreeAndNil(fSQLDatabaseConfig);
  inherited;
end;

function TBoldPHandleMimic.GetDataBaseInterface: IBoldDatabase;
begin
  result := fPHandle.DataBaseInterface;
end;

function TBoldPHandleMimic.GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  if not assigned(fSQLDatabaseConfig) then
    fSQLDatabaseConfig := TBoldSQLDataBaseConfig.Create;
  result := fSQLDatabaseConfig;
end;

end.
