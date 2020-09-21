unit BoldPersistenceHandleDOA;

interface

uses
  Classes,
  Oracle,
  BoldDBInterfaces,
  BoldDOAInterfaces,
  BoldPersistenceHandleDB_deprecated;

type
  { forward declarations }
  TBoldPersistenceHandleDOA = class;

  { TBoldPersistenceHandleDOA }
  TBoldPersistenceHandleDOA = class(TBoldDBPersistenceHandle)
  private
    fOracleSession: TOracleSession;
    fDataBaseAdapter: TBoldDOADataBase;
    procedure SetOracleSession(const NewValue: TOracleSession);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
  public
    destructor Destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property OracleSession: TOracleSession read fOracleSession write SetOracleSession;
  end deprecated;

implementation

uses
  SysUtils;

{ TBoldPersistenceHandleDOA }

destructor TBoldPersistenceHandleDOA.destroy;
begin
  Active := false;
  FreeAndNil(fDataBaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleDOA.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(fDataBaseAdapter) then
  begin
    if not assigned(fOracleSession) then
      raise Exception.CreateFmt(sNoOracleSession, [classname]);
    fDataBaseAdapter := TBoldDOADataBase.create(fOracleSession, SQLDataBaseConfig);
  end;
  result := fDataBaseAdapter;
end;

procedure TBoldPersistenceHandleDOA.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fOracleSession) and (Operation = opRemove) then
  begin
    Active := false;
    FreeAndNil(fDataBaseAdapter);
    fOracleSession := nil;
  end;
end;

procedure TBoldPersistenceHandleDOA.SetOracleSession(const NewValue: TOracleSession);
begin
  if fOracleSession <> NewValue then
  begin
    CheckInactive('SetDataBase'); // do not localize
    fOracleSession := NewValue;
    if assigned(fOracleSession) then
      fOracleSession.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleDOA.SetPassword(const Value: string);
begin
  raise Exception.CreateFmt(sSetOnOracleSession, [classname, 'SetPassword']); // do not localize
end;

procedure TBoldPersistenceHandleDOA.SetUserName(const Value: string);
begin
  raise Exception.CreateFmt(sSetOnOracleSession, [classname, 'SetUserName']); // do not localize
end;

end.
