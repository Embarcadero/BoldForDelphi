unit BoldPersistenceHandleAsta;

interface

uses
  Classes,
  BoldDefs,
  AstaStringLine,
  AstaClientSocket,
  BoldDBInterfaces,
  BoldAstaInterfaces,
  BoldSQLDatabaseConfig,
  BoldPersistenceController,
  BoldPersistenceHandleDB;

type
  { forward declarations }
  TBoldPersistenceHandleAsta = class;

  { TBoldPersistenceHandleAsta }
  TBoldPersistenceHandleAsta = class(TBoldDBPersistenceHandle)
  private
    fAstaClientSocket: TAstaClientSocket;
    fOwnDataBase: TAstaClientSocket;
    fDataBaseAdapter: TBoldAstaDataBase;
    fAddress: string;
    fPort: Integer;
    fHost: string;
    function getEffectiveDataBase: TAstaClientSocket;
    procedure SetAstaClientSocket(const Value: TAstaClientSocket);
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetHost(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
    property EffectiveDataBase: TAstaClientSocket read getEffectiveDataBase;
   public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property Port: integer read fPort write SetPort;
    property Address: string read fAddress write SetAddress;
    property Host: string read fHost write SetHost;
    property AstaClientSocket: TAstaClientSocket read FAstaClientSocket write SeTAstaClientSocket;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldPersistenceHandleAsta }

constructor TBoldPersistenceHandleAsta.create(owner: TComponent);
begin
  inherited;
  UserName := 'SYSDBA';
  Password := 'masterkey';
end;

destructor TBoldPersistenceHandleAsta.destroy;
begin
  Active := false;
  FreeAndNil(fOwnDataBase);
  FreeAndNil(fDataBaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleAsta.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(fDataBaseAdapter) then
    fDataBaseAdapter := TBoldAstaDataBase.create(EffectiveDataBase, EmptyStringMarker);
  result := fDataBaseAdapter;
end;

function TBoldPersistenceHandleAsta.getEffectiveDataBase: TAstaClientSocket;
begin
  if assigned(fAstaClientSocket) then
    result := fAstaClientSocket
  else
  begin
    if not assigned(fOwnDataBase) then
    begin
      fOwnDataBase := TAstaClientSocket.Create(nil);
      fOwnDataBase.UserName := Username;
      fOwnDataBase.Password := Password;
      fOwnDataBase.Port := Port;
      fOwnDataBase.Address := Address;
      fOwnDataBase.Host := Host;
      if PassWord <> '' then
        fOwnDataBase.AutoLoginDlg := ltLoginNoDlg;
    end;
    result := fOwnDataBase;
  end;
end;

procedure TBoldPersistenceHandleAsta.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fAstaClientSocket) and (Operation = opRemove) then
  begin
    if aComponent = EffectiveDataBase then
    begin
      Active := false;
      fDataBaseAdapter := nil;
    end;
    fAstaClientSocket := nil;
  end;
end;

procedure TBoldPersistenceHandleAsta.SetAstaClientSocket(const Value: TAstaClientSocket);
begin
  if fAstaClientSocket <> Value then
  begin
    CheckInactive('SetDataBase');
    if assigned(fOwnDataBase) then
    begin
      FreeAndNil(FOwnDataBase);
      FreeAndNil(fdataBaseAdapter);
    end;

    fAstaClientSocket := Value;

    if assigned(fAstaClientSocket) then
      fAstaClientSocket.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleAsta.SetPassword(const Value: string);
begin
  inherited;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Password := Password;
end;

procedure TBoldPersistenceHandleAsta.SetUserName(const Value: string);
begin
  inherited;
  if assigned(fOwnDataBase) then
    fOwnDataBase.UserName := UserName;
end;

procedure TBoldPersistenceHandleAsta.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

procedure TBoldPersistenceHandleAsta.SetAddress(const Value: string);
begin
  fAddress := Value;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Address := Value;
end;

procedure TBoldPersistenceHandleAsta.SetPort(const Value: Integer);
begin
  fPort := Value;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Port := Value;
end;

procedure TBoldPersistenceHandleAsta.SetHost(const Value: string);
begin
  fHost := Value;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Host := Value;
end;

end.
