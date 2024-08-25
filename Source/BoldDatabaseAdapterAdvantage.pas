
{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterAdvantage;

interface

uses
  adscnnct,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldAdvantageInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterAdvantage = class;

  { TBoldDatabaseAdapterAdvantage }
  TBoldDatabaseAdapterAdvantage = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldAdvantageDataBase;
    procedure SetDataBase(const Value: TADSConnection);
    function GetDataBase: TADSConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor destroy; override;
  published
    property DataBase: TADSConnection read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterAdvantage }      

destructor TBoldDatabaseAdapterAdvantage.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;                   
end;

function TBoldDatabaseAdapterAdvantage.GetDataBase: TADSConnection;
begin
  result := InternalDatabase as TADSConnection;
end;

function TBoldDatabaseAdapterAdvantage.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to a database', [classname]); 
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldAdvantageDataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterAdvantage.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterAdvantage.SetDataBase(const Value: TADSConnection);
begin
  InternalDatabase := value;
end;

end.
