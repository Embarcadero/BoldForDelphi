
{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterBDE;

interface

uses
  DbTables,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldBDEInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterBDE = class;

  { TBoldDatabaseAdapterBDE }
  TBoldDatabaseAdapterBDE = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldBDEDataBase;
    procedure SetDataBase(const Value: TDataBase);
    function GetDataBase: TDataBase;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor destroy; override;
  published
    property DataBase: TDataBase read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterBDE }      

destructor TBoldDatabaseAdapterBDE.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;                   
end;

function TBoldDatabaseAdapterBDE.GetDataBase: TDataBase;
begin
  result := InternalDatabase as TDataBase;
end;

function TBoldDatabaseAdapterBDE.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to a database', [classname]); 
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldBDEDataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterBDE.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterBDE.SetDataBase(const Value: TDataBase);
begin
  InternalDatabase := value;
end;

end.
