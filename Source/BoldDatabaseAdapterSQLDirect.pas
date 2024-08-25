{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterSQLDirect;

interface

uses
  SDEngine,
  SDCommon,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldSQLDirectInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterSQLDirect = class;

  { TBoldDatabaseAdapterSQLDirect }
  TBoldDatabaseAdapterSQLDirect = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldSQLDirectDataBase;
    procedure SetDataBase(const Value: TSDDataBase);
    function GetDataBase: TSDDataBase;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
    procedure LostConnectError(Database: TSDDatabase;
        E: ESDEngineError; var Action: TSDLostConnectAction);
    procedure ReconnectError(Database: TSDDatabase;
        E: ESDEngineError; var Action: TSDLostConnectAction);
  published
    property DataBase: TSDDataBase read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterSQLDirect }

destructor TBoldDatabaseAdapterSQLDirect.Destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

function TBoldDatabaseAdapterSQLDirect.GetDataBase: TSDDataBase;
begin
  result := InternalDatabase as TSDDataBase;
end;

function TBoldDatabaseAdapterSQLDirect.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to a database', [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldSQLDirectDataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterSQLDirect.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterSQLDirect.LostConnectError(
  Database: TSDDatabase; E: ESDEngineError;
  var Action: TSDLostConnectAction);
begin
  Action:=lcWaitReconnect;
  DoOnDisconnect(self);
end;

procedure TBoldDatabaseAdapterSQLDirect.ReconnectError(
  Database: TSDDatabase; E: ESDEngineError;
  var Action: TSDLostConnectAction);
begin
  Action:=lcWaitReconnect;
end;

procedure TBoldDatabaseAdapterSQLDirect.SetDataBase(const Value: TSDDataBase);
begin
  value.OnLostConnectError:=LostConnectError;
  value.OnReconnectError:=ReconnectError;
  value.AfterConnect:= DoOnConnect;
  value.AfterReConnect:= DoOnReconnect;
  Value.AfterDisconnect := DoOnDisconnect;
  InternalDatabase := value;
end;

end.