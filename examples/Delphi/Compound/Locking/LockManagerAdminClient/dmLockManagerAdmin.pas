unit dmLockManagerAdmin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldAbstractLockManagerAdminHandle, BoldLockManagerAdminHandleCom,
  BoldSubscription, BoldHandle, BoldClientHandles, BoldComClientHandles;

type

  TClientInfo = class
  private
    fIdString: string;
    fLocks: TStringList;
  public
    constructor Create(IdString: string);
    destructor Destroy; override;
    property IdString: string read fIdString write fIdString;
    property Locks: TStringList read fLocks write fLocks;
  end;

  TLockInfo = class
  private
    fDuration: string;
    fLockName: string;
    fClients: TStringList;
  public
    constructor Create(Duration: string; LockName:string);
    destructor Destroy; override;
    property Duration: string read fDuration write fDuration;
    property LockName: string read fLockName write fLockName;
  end;

  TdmMain = class(TDataModule)
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldLockManagerAdminHandleCom1: TBoldLockManagerAdminHandleCom;
  private
    { Private declarations }
    fClients: TStringList;
    fLocks: TStringList;
    fViewAll: Boolean;
    procedure SetLocks(const Value: TStringList);
    procedure SetClients(const Value: TStringList);
    procedure SetViewAll(const Value: Boolean);
    function GetLockManagerSuspended: Boolean;
    procedure SetLockManagerSuspended(const Value: Boolean);
    function getServerName: string;
  public
    { Public declarations }
    procedure GetClients;
    procedure KillClient(ClientId: integer);
    property LockManagerSuspended: Boolean read GetLockManagerSuspended write SetLockManagerSuspended;
    property Clients: TStringList read fClients write SetClients;
    property Locks: TStringList read fLocks write SetLocks;
    property ViewAll: Boolean read fViewAll write SetViewAll;
    property ServerName: string read getServerName;
  end;

  procedure FreeObjects(const List: TStringList);
  procedure AssignObjects(const Source, Destination: TStringList);

var
  dmMain: TdmMain;


implementation

uses
  BoldUtils,
  MainForm;
{$R *.DFM}

procedure FreeObjects(const List: TStringList);
var
  i: integer;
  obj : TObject;
begin
  if not Assigned(List) then
    Exit;
  for i:= 0 to list.Count - 1 do
  begin
    obj := list.Objects[i];
    FreeAndNil(obj);
  end;
end;

procedure AssignObjects(const Source, Destination: TStringList );
var
  i: integer;
begin
  if not Assigned(Source) or not Assigned(Destination) then
    Exit;
  for i:= 0 to source.Count - 1 do
  begin
    Destination.Objects[i] := Source.Objects[i];
  end;
end;

{ TClientInfo }

constructor TClientInfo.Create(IdString: string);
begin
  inherited Create;
  fIdString := IdString;
  fLocks := TStringList.Create;
end;

destructor TClientInfo.Destroy;
begin
  FreeObjects(fLocks);
  FreeAndNil(fLocks);
  inherited;
end;

{ TLockInfo }

constructor TLockInfo.Create(Duration: string; LockName: string);
begin
  inherited Create;
  fDuration:= Duration;
  fLockName := LockName;
  fClients := TStringList.Create;
end;

destructor TLockInfo.Destroy;
begin
  FreeObjects(fClients);
  FreeAndNil(fClients);
  inherited;
end;

{ TdmMain }

procedure TdmMain.GetClients;
var
  lClients, lLocks, lLockDurations: TStringList;
  vClients, vLocks, vlockDurations: OleVariant;
  clientInfo: TClientInfo;
  lockInfo: TLockInfo;
  i, j, idx: integer;
begin
  if BoldComConnectionHandle1.Connected then
  begin
    lClients := TStringList.Create;
    lLocks := TStringList.Create;
    lLockDurations := TStringList.Create;
    try
      if not Assigned(fClients) then
        fClients := TStringList.Create;
      if not Assigned(fLocks) then
        fLocks := TStringList.Create;
      if not BoldLockManagerAdminHandleCom1.Active then
        BoldLockManagerAdminHandleCom1.Active := True;

      // listallclients         (1)
      if ViewAll then
        BoldLockManagerAdminHandleCom1.LockManagerAdmin.ListAllClients(vClients)
      else
        BoldLockManagerAdminHandleCom1.LockManagerAdmin.ListLockingClients(vclients);
      BoldVariantToStrings(vClients, lClients);

      // get locks for clients
      BoldLockManagerAdminHandleCom1.LockManagerAdmin.LocksForClients(vclients, vLocks, vLockDurations);
      BoldVariantToStrings(vLocks, lLocks);
      BoldVariantToStrings(vLockDurations, lLockDurations);
      for i := 0 to lLocks.Count - 1 do
      begin
        lockInfo := TLockInfo.Create(lLockDurations[i], lLocks.Values[lLocks.Names[i]]);
        lLocks[i] := lLocks.Names[i];
        lLocks.Objects[i] := lockinfo;
      end;

      for i:= 0 to (lClients.Count - 1) do
      begin
        clientInfo := TClientInfo.Create(lClients.Values[lClients.Names[i]]);
        lClients[i] := lClients.Names[i];
        lClients.Objects[i]:= clientinfo;
        j := lLocks.IndexOf(lClients[i]);
        while j <> -1 do
        begin
          idx := clientinfo.Locks.Add(lLocks[j]);
          clientinfo.Locks[j] := (lLocks.Objects[j] as TLockInfo).LockName;
          clientinfo.Locks.Objects[idx] := lLocks.Objects[j];
          lLocks.Delete(j);
          j := lLocks.IndexOf(lClients[i]);
        end
      end;

      Clients := lClients;
    finally
      FreeAndNil(lClients);
      FreeAndNil(lLocks);
      FreeAndNil(lLockDurations);
    end;
  end;
end;

function TdmMain.GetLockManagerSuspended: Boolean;
begin
  if BoldComConnectionHandle1.Connected then
  begin
    if not BoldLockManagerAdminHandleCom1.Active then
      BoldLockManagerAdminHandleCom1.Active := True;
    Result := BoldLockManagerAdminHandleCom1.LockManagerAdmin.LockManagerSuspended;
  end
  else
    Result := false;
end;

function TdmMain.getServerName: string;
begin
  Result := trim(fMain.edServerName.Text);
end;

procedure TdmMain.KillClient(ClientId: integer);
begin
  try
    if BoldComConnectionHandle1.Connected then
    begin
      if not BoldLockManagerAdminHandleCom1.Active then
        BoldLockManagerAdminHandleCom1.Active := True;
      BoldLockManagerAdminHandleCom1.LockManagerAdmin.KillClient(ClientId);
      dmMain.GetClients;
    end;
  finally
  end;
end;

procedure TdmMain.SetClients(const Value: TStringList);
var
  i, idx: integer;
begin
  FreeObjects(fClients);
  fClients.Clear;
  fClients.Assign(Value);
  AssignObjects(Value, fClients);
  if Assigned(MainForm.fMain) and (MainForm.FMain.PageControl1.ActivePage.Name = 'tsClients') then
  begin
    MainForm.FMain.lbclients.Clear;
    for i:= 0 to fClients.Count - 1 do
    begin
      idx := MainForm.FMain.lbclients.Items.Add(Format('%s=%s', [fClients[i], (fClients.Objects[i] as TClientInfo).IdString]));
      MainForm.FMain.lbclients.Items.Objects[idx] := fClients.Objects[i];
    end;
  end;
end;

procedure TdmMain.SetLockManagerSuspended(const Value: Boolean);
begin
  if BoldComConnectionHandle1.Connected then
  begin
    if not BoldLockManagerAdminHandleCom1.Active then
      BoldLockManagerAdminHandleCom1.Active := True;
    BoldLockManagerAdminHandleCom1.LockManagerAdmin.LockManagerSuspended := Value;
  end;
end;

procedure TdmMain.SetLocks(const Value: TStringList);
begin
  FreeObjects(fLocks);
  fLocks.Clear;
  fLocks.Assign(Value);
  AssignObjects(Value, fLocks);
end;

procedure TdmMain.SetViewAll(const Value: Boolean);
begin
  if (Value <> fViewAll) then
  begin
    fViewAll := Value;
    getClients;
  end;
end;

end.
