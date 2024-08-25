
{ Global compiler directives }
{$include bold.inc}
unit BoldServer;

interface

uses
  BoldBase,
  BoldContainers,
  BoldSubscription;

type
  { forward declarations }
  TBoldServer = class;
  TBoldServerApplication = class;

  {-- TBoldServer --}
  TBoldServer = class(TBoldSubscribableObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {-- TBoldServerApplication --}
  TBoldServerApplication = class(TBoldNonRefCountedObject)
  private
    FServers: TBoldObjectArray;
    procedure AddServer(Server: TBoldServer);
    procedure RemoveServer(Server: TBoldServer);
    function GetServerByIndex(Index: Integer): TBoldServer;
    function GetServerCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TBoldServerApplication;
    property ServerCount: Integer read GetServerCount;
    property Servers[Index: Integer]: TBoldServer read GetServerByIndex;
  end;

implementation

uses
  SysUtils;

var
  G_BoldServerApplication: TBoldServerApplication = nil;

{-- TBoldServer --------------------------------------------------------}

constructor TBoldServer.Create;
begin
  inherited;
  TBoldServerApplication.Instance.AddServer(Self);
end;

destructor TBoldServer.Destroy;
begin
  TBoldServerApplication.Instance.RemoveServer(Self);
  inherited;
end;

{-- TBoldServerApplication ----------------------------------------------------}

constructor TBoldServerApplication.Create;
begin
  inherited Create;
  FServers := TBoldObjectArray.Create(1,[]);
end;

destructor TBoldServerApplication.Destroy;
begin
  FreeAndNil(FServers);
  inherited;
end;

procedure TBoldServerApplication.AddServer(Server: TBoldServer);
begin
  FServers.Add(Server);
end;

function TBoldServerApplication.GetServerByIndex(Index: Integer): TBoldServer;
begin
  Result := TBoldServer(FServers[Index]);
end;

function TBoldServerApplication.GetServerCount: Integer;
begin
  Result := FServers.Count;
end;

class function TBoldServerApplication.Instance: TBoldServerApplication;
begin
  if not Assigned(G_BoldServerApplication) then
    G_BoldServerApplication := TBoldServerApplication.Create;
  Result := G_BoldServerApplication;
end;

procedure TBoldServerApplication.RemoveServer(Server: TBoldServer);
begin
  FServers.Remove(Server);
end;

initialization

finalization
  FreeAndNil(G_BoldServerApplication);

end.
