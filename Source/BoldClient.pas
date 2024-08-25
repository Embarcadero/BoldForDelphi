
{ Global compiler directives }
{$include bold.inc}
unit BoldClient;

interface

uses
  BoldBase,
  BoldContainers,
  BoldSubscription;

type
  { forward declarations }
  TBoldClient = class;
  TBoldClientApplication = class;

  {-- TBoldClient --}
  TBoldClient = class(TBoldSubscribableObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {-- TBoldClientApplication --}
  TBoldClientApplication = class(TBoldNonRefCountedObject)
  private
    FClients: TBoldObjectArray;
    procedure AddClient(Client: TBoldClient);
    procedure RemoveClient(Client: TBoldClient);
    function GetClientByIndex(Index: Integer): TBoldClient;
    function GetClientCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TBoldClientApplication;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TBoldClient read GetClientByIndex;
  end;

implementation

uses
  SysUtils;

var
  G_BoldClientApplication: TBoldClientApplication = nil;

{-- TBoldClient ---------------------------------------------------------------}

constructor TBoldClient.Create;
begin
  inherited;
  TBoldClientApplication.Instance.AddClient(Self);
end;

destructor TBoldClient.Destroy;
begin
  TBoldClientApplication.Instance.RemoveClient(Self);
  inherited;
end;

{-- TBoldClientApplication ----------------------------------------------------}

constructor TBoldClientApplication.Create;
begin
  inherited Create;
  FClients := TBoldObjectArray.Create(1,[]);
end;

destructor TBoldClientApplication.Destroy;
begin
  FreeAndNil(FClients);
  inherited;
end;

procedure TBoldClientApplication.AddClient(Client: TBoldClient);
begin
  FClients.Add(Client);
end;

function TBoldClientApplication.GetClientByIndex(Index: Integer): TBoldClient;
begin
  Result := TBoldClient(FClients[Index]);
end;

function TBoldClientApplication.GetClientCount: Integer;
begin
  Result := FClients.Count;
end;

class function TBoldClientApplication.Instance: TBoldClientApplication;
begin
  if not Assigned(G_BoldClientApplication) then
    G_BoldClientApplication := TBoldClientApplication.Create;
  Result := G_BoldClientApplication;
end;

procedure TBoldClientApplication.RemoveClient(Client: TBoldClient);
begin
  FClients.Remove(Client);
end;

initialization

finalization
  FreeAndNil(G_BoldClientApplication);

end.
