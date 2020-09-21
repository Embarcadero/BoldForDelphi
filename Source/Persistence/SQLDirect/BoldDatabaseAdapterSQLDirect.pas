unit BoldDatabaseAdapterSQLDirect;

interface

uses
  SDEngine,
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

destructor TBoldDatabaseAdapterSQLDirect.destroy;
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

procedure TBoldDatabaseAdapterSQLDirect.SetDataBase(const Value: TSDDataBase);
begin
  InternalDatabase := value;
end;

end.
