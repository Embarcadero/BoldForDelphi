unit BoldDatabaseAdapterDBISAM;

interface

uses
  classes,
  dbisamtb,
  BoldSQLDatabaseconfig,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldDBISAMInterfaces;

type
  { forward declarations }
  TBoldDatabaseAdapterDBISAM = class;

  { TBoldDatabaseAdapterDBISAM }
  TBoldDatabaseAdapterDBISAM = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldDatabase: TBoldDBISAMDataBase;
    procedure SetDataBase(const Value: TDBISAMDataBase);
    function GetDataBase: TDBISAMDataBase;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataBase: TDBISAMDataBase read GetDataBase write SetDataBase;
    {$IFNDEF T2H}
    property DatabaseEngine;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  DBISAMConsts;

{ TBoldDatabaseAdapterDBISAM }

constructor TBoldDatabaseAdapterDBISAM.create(aOwner: TComponent);
begin
  inherited;
  DatabaseEngine := dbeDBISAM;
end;

destructor TBoldDatabaseAdapterDBISAM.destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldDatabase);
  inherited;
end;

function TBoldDatabaseAdapterDBISAM.GetDataBase: TDBISAMDataBase;
begin
  result := InternalDatabase as TDBISAMDataBase;
end;

function TBoldDatabaseAdapterDBISAM.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt(sAdapterNotConnected, [classname]);
  if not assigned(fBoldDatabase) then
    fBoldDatabase := TBoldDBISAMDataBase.create(Database, SQLDataBaseConfig);
  result := fBoldDatabase;
end;

procedure TBoldDatabaseAdapterDBISAM.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldDatabase);
end;

procedure TBoldDatabaseAdapterDBISAM.SetDataBase(const Value: TDBISAMDataBase);
begin
  InternalDatabase := value;
end;

end.
