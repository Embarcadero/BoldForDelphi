
{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterDBX;

interface

uses
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  SQLExpr,
  BoldDBXInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterDBX = class;

  { TBoldDatabaseAdapterDBX }
  TBoldDatabaseAdapterDBX = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldDBXDatabase;
    procedure SetDataBase(const Value: TSQLConnection);
    function GetDataBase: TSQLConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
  published
    property Connection: TSQLConnection read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterDBX }

destructor TBoldDatabaseAdapterDBX.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

function TBoldDatabaseAdapterDBX.GetDataBase: TSQLConnection;
begin
  result := InternalDatabase as TSQLConnection;
end;

function TBoldDatabaseAdapterDBX.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Connection) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to a DBX connection', [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldDBXDatabase.create(Connection, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterDBX.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterDBX.SetDataBase(const Value: TSQLConnection);
begin
  InternalDatabase := value;
end;

end.
