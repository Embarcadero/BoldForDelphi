
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterIB;

interface

uses
  Classes,
  Windows,
  IBX.IBDatabase,
  BoldSQLDatabaseConfig,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldIBInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterIB = class;

  { TBoldDatabaseAdapterIB }
  TBoldDatabaseAdapterIB = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldIBDataBase;
    procedure SetDataBase(const Value: TIBDataBase);
    function GetDataBase: TIBDataBase;
  protected
    procedure ReleaseBoldDatabase; override;
    procedure SetDataBaseEngine(const Value: TBoldDataBaseEngine); override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateInterbaseDatabase(PageSize: integer = 4096);
    procedure EnsureInterbaseDatabase(PageSize: integer = 4096);
    procedure CreateDatabase(DropExisting: boolean = true); override;
  published
    property DataBase: TIBDataBase read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDatabaseAdapterIB }

constructor TBoldDatabaseAdapterIB.Create(aOwner: TComponent);
begin
  inherited;
  DatabaseEngine := dbeInterbaseSQLDialect3;
end;

procedure TBoldDatabaseAdapterIB.CreateInterbaseDatabase(PageSize: integer = 4096);
begin
  DatabaseInterface.CreateDatabase;
end;

destructor TBoldDatabaseAdapterIB.Destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

procedure TBoldDatabaseAdapterIB.EnsureInterbaseDatabase(
  PageSize: integer);
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.EnsureInterbaseDatbase: Unable to complete operation without an IBDatabase', [classname]);

  if not FileExists(Database.DatabaseName) then
    CreateInterbaseDatabase(PageSize);
end;

procedure TBoldDatabaseAdapterIB.CreateDatabase(DropExisting: boolean);
begin
  CreateInterbaseDatabase;
end;

function TBoldDatabaseAdapterIB.GetDataBase: TIBDataBase;
begin
  result := InternalDatabase as TIBDataBase;
end;

function TBoldDatabaseAdapterIB.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.GetDatabaseInterface: The adapter is not connected to a database', [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldIBDataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterIB.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterIB.SetDataBase(const Value: TIBDataBase);
begin
  InternalDatabase := value;
end;

procedure TBoldDatabaseAdapterIB.SetDataBaseEngine(
  const Value: TBoldDataBaseEngine);
begin
  if value in [dbeUnknown, dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3] then
    inherited
  else
    raise EBold.CreateFmt(
      '%s.SetDatabaseEngine: value not allowed. This adapter only supports' + BOLDCRLF +
      'dbeUnknown, dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3', [classname]);
end;

end.