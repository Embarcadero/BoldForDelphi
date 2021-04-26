
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEController;

interface

uses
  classes,
  BoldDefs,
  BoldOLLEdmmain,
  BoldSystem,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldIndexCollection,
  BoldAbstractPersistenceHandleDB;

type
  TBoldPHandleMimic = class(TBoldAbstractPersistenceHandleDB)
  private
    fPHandle: TBoldAbstractPersistenceHandleDB;
    fSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    fCustomIndexes: TBoldIndexCollection;
  protected
    function GetDataBaseInterface: IBoldDatabase; override;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig; override;
    function GetCustomIndexes: TBoldIndexCollection; override;
  public
    destructor destroy; override;
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
  BoldUtils;


{ TBoldOLLEController }

constructor TBoldOLLEController.Create(PersistenceHandle: TBoldAbstractPersistenceHandleDB);
begin
  fOlleDM := TdmOLL.Create(nil);
  fMimicPHandle := TBoldPHandleMimic.create(fOlleDM);
  fMimicPHandle.fPHandle := PersistenceHandle;
  fMimicPHandle.BoldModel := fOlleDM.BoldModel1;
  fMimicPHandle.SQLDatabaseConfig.AssignConfig(PersistenceHandle.SQLDatabaseConfig);
  fMimicPHandle.SQLDataBaseConfig.SystemTablePrefix := 'OLLE';
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
      raise Exception.Create( 'TBoldOLLEController: Can not change Persistent-property when the OLLE system is active' );
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
  FreeAndNil(fCustomIndexes);
  inherited;
end;

function TBoldPHandleMimic.GetCustomIndexes: TBoldIndexCollection;
begin
  if not assigned(fCustomIndexes) then
    fCustomIndexes := TBoldIndexCollection.Create(nil);
  result := fCustomIndexes;
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

initialization

end.
