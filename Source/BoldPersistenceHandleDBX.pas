
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDBX;

interface

uses
  Classes,
  SQLExpr,
  BoldDBInterfaces,
  BoldDBXInterfaces,
  BoldPersistenceHandleDB_deprecated;

type
  { forward declarations }
  TBoldPersistenceHandleDBX = class;

  { TBoldPersistenceHandleDBX }
  TBoldPersistenceHandleDBX = class(TBoldDBPersistenceHandle)
  private
    fSQLConnection: TSQLConnection;
    fSQLConnectionAdapter: TBoldDBXDatabase;
    procedure SetSQLConnection(const Value: TSQLConnection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
  public
    destructor destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property SQLConnection: TSQLConnection read fSQLConnection write SetSQLConnection;
  end deprecated;

implementation

uses
  SysUtils;

{ TBoldPersistenceHandleDBX }

destructor TBoldPersistenceHandleDBX.destroy;
begin
  Active := false;
  inherited;
end;

function TBoldPersistenceHandleDBX.GetDatabaseInterface: IBoldDatabase;
begin
  if not assigned(fSQLConnectionAdapter) then
    fSQLConnectionAdapter := TBoldDBXDataBase.create(fSQLConnection, SQLDataBaseConfig);
  result := fSQLConnectionAdapter;
end;

procedure TBoldPersistenceHandleDBX.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = fSQLConnection) and (Operation = opRemove) then
  begin
    Active := false;
    fSQLConnectionAdapter := nil;
    fSQLConnection := nil;
  end;
end;

procedure TBoldPersistenceHandleDBX.SetSQLConnection(const Value: TSQLConnection);
begin
  if fSQLConnection <> Value then
  begin
    fSQLConnection := Value;
    FreeAndNil(fSQLConnectionAdapter);
    if assigned(fSQLConnection) then
      fSQLConnection.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleDBX.SetPassword(const Value: string);
begin
  raise Exception.Create('Can not set password directly on PersistenceHandleDBX, set it on the SQLConnection');
end;   

procedure TBoldPersistenceHandleDBX.SetUserName(const Value: string);
begin
  raise Exception.Create('Can not set username directly on PersistenceHandleDBX, set it on the SQLConnection');
end;

end.
