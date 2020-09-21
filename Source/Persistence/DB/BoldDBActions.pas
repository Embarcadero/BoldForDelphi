unit BoldDBActions;

interface

uses
  Classes,
  BoldPersistenceHandleDB,
  ActnList;

type
  TBoldGenerateSchemaAction = class;

  { TBoldGenerateSchemaAction }
  TBoldGenerateSchemaAction = class(TAction)
  private
    FBoldPersistenceHandleDB: TBoldPersistenceHandleDB;
    fIgnoreUnknownTables: boolean;
    procedure SetBoldPersistenceHandleDB(const Value: TBoldPersistenceHandleDB);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property BoldPersistenceHandleDB: TBoldPersistenceHandleDB read FBoldPersistenceHandleDB write SetBoldPersistenceHandleDB;
    property IgnoreUnknownTables: boolean read fIgnoreUnknownTables write fIgnoreUnknownTables;
  end;

implementation

uses
  BoldDefs,
  SysUtils,
  BoldActionDefs,
  BoldUtils,
  PersistenceConsts;


{ TBoldGenerateSchemaAction }

constructor TBoldGenerateSchemaAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sGenerateSchema;
end;

procedure TBoldGenerateSchemaAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if HandlesTarget(nil) then
    BoldPersistenceHandleDB.CreateDataBaseSchema(IgnoreUnknownTables);
end;

function TBoldGenerateSchemaAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(BoldPersistenceHandleDB) and
            Assigned(BoldPersistenceHandleDB.DatabaseAdapter) and
            not BoldPersistenceHandleDB.Active;
end;

procedure TBoldGenerateSchemaAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = BoldPersistenceHandleDB) and (operation = opRemove) then
    fBoldPersistenceHandleDB := nil;
end;

procedure TBoldGenerateSchemaAction.SetBoldPersistenceHandleDB(const Value: TBoldPersistenceHandleDB);
begin
  FBoldPersistenceHandleDB := Value;
  if Assigned(fBoldPersistenceHandleDB) then
    fBoldPersistenceHandleDB.FreeNotification(Self);
end;

end.
