
{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterADO;

interface

uses
  ADODB,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldADOInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterADO = class;

  { TBoldDatabaseAdapterADO }
  TBoldDatabaseAdapterADO = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldADOConnection;
    procedure SetDataBase(const Value: TADOConnection);
    function GetDataBase: TADOConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor destroy; override;
  published
    property Connection: TADOConnection read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterADO }

destructor TBoldDatabaseAdapterADO.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

function TBoldDatabaseAdapterADO.GetDataBase: TADOConnection;
begin
  result := InternalDatabase as TADOConnection;
end;

function TBoldDatabaseAdapterADO.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Connection) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to an ADO connection', [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldADOConnection.create(Connection, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterADO.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterADO.SetDataBase(const Value: TADOConnection);
begin
  InternalDatabase := value;
end;

end.
