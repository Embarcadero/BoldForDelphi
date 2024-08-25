
{ Global compiler directives }
{$include bold.inc}
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
    destructor destroy; override;
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
      raise Exception.CreateFmt('%s.GetDatabaseInterface: There is no OracleSession, can''t create a database interface', [classname]); 
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
    CheckInactive('SetDataBase');
    fOracleSession := NewValue;
    if assigned(fOracleSession) then
      fOracleSession.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleDOA.SetPassword(const Value: string);
begin
  raise Exception.CreateFmt('%s.SetPassword: Not supported, set the password directly on your OracleSession-object', [classname]);
end;

procedure TBoldPersistenceHandleDOA.SetUserName(const Value: string);
begin
  raise Exception.CreateFmt('%s.SetUserName: Not supported, set the Username directly on your OracleSession-object', [classname]);
end;

end.
