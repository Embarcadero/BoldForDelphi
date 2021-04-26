{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterUniDAC;

interface

uses
  Classes,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldUniDACInterfaces,
  Uni;

type
  { forward declarations }
  TBoldDatabaseAdapterUniDAC = class;

  { TBoldDatabaseAdapterUniDAC }
  TBoldDatabaseAdapterUniDAC = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldUniDACConnection: TBoldUniDACConnection;
    procedure SetConnection(const Value: TUniConnection);
    function GetConnection: TUniConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
    procedure CreateDatabase; override;    
  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  BoldSQLDatabaseConfig,
  SysUtils,
  BoldDefs,
  UniDACConsts;

{ TBoldDatabaseAdapterUniDAC }

destructor TBoldDatabaseAdapterUniDAC.Destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldUniDACConnection);
  inherited;
end;

procedure TBoldDatabaseAdapterUniDAC.CreateDatabase;
begin
  DatabaseInterface.CreateDatabase;
end;

function TBoldDatabaseAdapterUniDAC.GetConnection: TUniConnection;
begin
  Result := InternalDatabase as TUniConnection;
end;

function TBoldDatabaseAdapterUniDAC.GetDataBaseInterface: IBoldDatabase;
begin
  if not Assigned(Connection) then
  begin
    raise EBold.CreateFmt(sAdapterNotConnected, [ClassName]);
  end;
  if not Assigned(fBoldUniDACConnection) then
  begin
    fBoldUniDACConnection := TBoldUniDACConnection.Create(Connection, SQLDataBaseConfig);
  end;
  Result := fBoldUniDACConnection;
end;

procedure TBoldDatabaseAdapterUniDAC.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldUniDACConnection);
end;

procedure TBoldDatabaseAdapterUniDAC.SetConnection(const Value: TUniConnection);
begin
  InternalDatabase := Value;
end;

end.

