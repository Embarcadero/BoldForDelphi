unit BoldDatabaseAdapterDOA;

interface

uses
  Oracle,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldDOAInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterDOA = class;

  { TBoldDatabaseAdapterDOA }
  TBoldDatabaseAdapterDOA = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldDOADataBase;
    procedure SetDataBase(const Value: TOracleSession);
    function GetDataBase: TOracleSession;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
  published
    property DataBase: TOracleSession read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  DOAConsts;

{ TBoldDatabaseAdapterDOA }

destructor TBoldDatabaseAdapterDOA.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

function TBoldDatabaseAdapterDOA.GetDataBase: TOracleSession;
begin
  result := InternalDatabase as TOracleSession;
end;

function TBoldDatabaseAdapterDOA.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt(sAdapterNotConnected, [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldDOADataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterDOA.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterDOA.SetDataBase(const Value: TOracleSession);
begin
  InternalDatabase := value;
end;

end.
