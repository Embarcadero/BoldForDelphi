
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleAdvantage;

interface
uses
  Classes,
  adscnnct,
  BoldDBInterfaces,
  BoldPersistenceHandleDB_deprecated,
  BoldAdvantageInterfaces;

type
  TBoldPersistenceHandleAdvantage = class(TBoldDBPersistenceHandle)
  private
    FAdsConnection: TADSConnection;
    FDatabaseAdapter: TBoldAdvantageDatabase;
    FEffectiveDataBase: TAdsConnection;
    function GetEffectiveDatabase: TAdsConnection;
    procedure SetDatabase(const Value: TAdsConnection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DatabaseAdapter: TBoldAdvantageDatabase read FDatabaseAdapter;
    property EffectiveDatabase: TAdsConnection read GetEffectiveDatabase;
  public
    destructor destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property AdsConnection: TAdsConnection read FAdsConnection write SetDatabase;
  end deprecated;

implementation

uses
  SysUtils;

{ TBoldPersistenceHandleAdvantage }

destructor TBoldPersistenceHandleAdvantage.destroy;
begin
  Active := false;
  FreeAndNil(FEffectiveDatabase);
  FreeAndNil(FDatabaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleAdvantage.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(FDatabaseAdapter) then
    FDatabaseAdapter := TBoldAdvantageDatabase.create(EffectiveDatabase, SQLDataBaseConfig);
  result := FDataBaseAdapter;
end;

function TBoldPersistenceHandleAdvantage.GetEffectiveDatabase: TAdsConnection;
begin
  if assigned(FAdsConnection) then
    result := FAdsConnection
  else
    raise Exception.Create('TBoldPersistenceHandleAdvantage.GetEffectiveDatabase: AdsConnection property must be assigned');
end;

procedure TBoldPersistenceHandleAdvantage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = FAdsConnection) and (Operation = opRemove) then begin
    if aComponent = EffectiveDatabase then begin
      Active := false;
      FDatabaseAdapter := nil;
    end;
    FAdsConnection := nil;
  end;
end;

procedure TBoldPersistenceHandleAdvantage.SetDataBase(const Value: TAdsConnection);
begin
  if FAdsConnection <> Value then begin
    if assigned(FEffectiveDatabase) then begin
      FreeAndNil(FEffectiveDatabase);
      FreeAndNil(FDatabaseAdapter);
    end;

    FAdsConnection := Value;

    if assigned(FAdsConnection) then
      FAdsConnection.FreeNotification(self);
  end;
end;

end.
