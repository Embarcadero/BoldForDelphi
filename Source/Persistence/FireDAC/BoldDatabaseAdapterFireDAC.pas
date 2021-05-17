{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterFireDAC;

interface

uses
  Classes,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldFireDACInterfaces,
  FireDAC.Comp.Client;

type
  { forward declarations }
  TBoldDatabaseAdapterFireDAC = class;

  { TBoldDatabaseAdapterFireDAC }
  TBoldDatabaseAdapterFireDAC = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldFireDACConnection: TBoldFireDACConnection;
    procedure SetConnection(const Value: TFDConnection);
    function GetConnection: TFDConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
    procedure CreateDatabase; override;
  published
    property Connection: TFDConnection read GetConnection write SetConnection;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  BoldSQLDatabaseConfig,
  SysUtils,
  BoldDefs,
  BoldFireDACConsts;

{ TBoldDatabaseAdapterFireDAC }

destructor TBoldDatabaseAdapterFireDAC.Destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldFireDACConnection);
  inherited;
end;

procedure TBoldDatabaseAdapterFireDAC.CreateDatabase;
begin
  DatabaseInterface.CreateDatabase;
end;

function TBoldDatabaseAdapterFireDAC.GetConnection: TFDConnection;
begin
  Result := InternalDatabase as TFDConnection;
end;

function TBoldDatabaseAdapterFireDAC.GetDataBaseInterface: IBoldDatabase;
begin
  if not Assigned(Connection) then
  begin
    raise EBold.CreateFmt(sAdapterNotConnected, [ClassName]);
  end;
  if not Assigned(fBoldFireDACConnection) then
  begin
    fBoldFireDACConnection := TBoldFireDACConnection.Create(Connection, SQLDataBaseConfig);
  end;
  Result := fBoldFireDACConnection;
end;

procedure TBoldDatabaseAdapterFireDAC.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldFireDACConnection);
end;

procedure TBoldDatabaseAdapterFireDAC.SetConnection(const Value: TFDConnection);
begin
  InternalDatabase := Value;
end;

end.

