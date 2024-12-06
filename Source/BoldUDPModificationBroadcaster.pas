
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUDPModificationBroadcaster;

interface

uses
  { RTL/VCL }
  SysUtils,
  Classes,

  { Bold }
  BoldThreadSafeQueue,
  BoldAbstractModificationPropagator,

  { Indy }
  IdUDPClient,
  IdUDPServer,
  IdSocketHandle;

type
  { forward declarations }
  TBoldUDPModificationBroadcaster = class;

  { function prototypes }
  TActivationErrorEvent = procedure(Sender: TObject; E: Exception) of object;

  { TBoldUDPModificationBroadcaster }
  TBoldUDPModificationBroadcaster = class(TBoldAbstractNotificationPropagator)
  private
    fUDPClient: TIdUDPClient;
    fUDPServer: TIdUDPServer;
    FOnActivationError: TActivationErrorEvent;
    function GetPort: Integer;
    procedure SetPort(Value: Integer);
  protected
    procedure OnSendQueueNotEmpty(Sender: TBoldThreadSafeQueue); override;
    procedure SetActive(Value: Boolean); override;
    procedure InternalUDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Port: Integer read GetPort write SetPort;
    property OnActivationError: TActivationErrorEvent read FOnActivationError write FOnActivationError;
    {$IFNDEF T2H}
    property NextPersistenceHandle;
    property Active;
    property Dequeuer;
    property OnReceiveEvent;
    property SystemHandle;
    {$ENDIF}
  end;

implementation

uses
  BoldEnvironment;

const
  { Signature that is sent with every UDP propagation to identify ourselves }
  SIdentification = '#BOLDUDP#'#13#10;

{ TBoldUDPModificationBroadcaster }

constructor TBoldUDPModificationBroadcaster.Create(AOwner: TComponent);
begin
  inherited;
  fUDPClient := TIdUDPClient.Create(Self);
  fUDPServer := TIdUDPServer.Create(Self);
  fUDPClient.BroadcastEnabled := True;
  fUDPServer.OnUDPRead := InternalUDPRead;

  fUDPClient.Host := '255.255.255.255';
  fUDPClient.Port := 4098;

  fUDPServer.DefaultPort := 4098;
  fUDPServer.Bindings.Add;
end;

destructor TBoldUDPModificationBroadcaster.Destroy;
begin
  FreeAndNil(fUDPClient);
  FreeAndNil(fUDPServer);
  inherited;
end;

function TBoldUDPModificationBroadcaster.GetPort: Integer;
begin
  if Assigned(fUDPClient) then
    Result := fUDPClient.Port
  else
    Result := 0;
end;

procedure TBoldUDPModificationBroadcaster.SetActive(Value: Boolean);
begin
  Assert(Assigned(fUDPServer));
  inherited SetActive(Value);
  try
    fUDPServer.Active := Value;
  except
    on E: Exception do
      if Assigned(FOnActivationError) then
        FOnActivationError(Self, E)
      else
        raise;
  end;
end;

procedure TBoldUDPModificationBroadcaster.SetPort(Value: Integer);
begin
  Assert(Assigned(fUDPClient));
  Assert(Assigned(fUDPServer));
  if Active then
    Active := False;

  fUDPClient.Port := Value;

  fUDPServer.Bindings.Clear;
  fUDPServer.Bindings.DefaultPort := Value;
  fUDPServer.Bindings.Add.Port := Value;
end;

procedure TBoldUDPModificationBroadcaster.InternalUDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
  S: String;
begin
  if AData.Size - AData.Position > 0 then
  begin
    SetLength(S, AData.Size - AData.Position);
    AData.Read(S[1], Length(S));
    if Length(S) > 0 then
      if SameText(Copy(S, 1, Length(SIdentification)), SIdentification) then
        ReceiveEvent(Copy(S, Length(SIdentification) + 1, Length(S)));
  end;
end;

procedure TBoldUDPModificationBroadcaster.OnSendQueueNotEmpty(Sender: TBoldThreadSafeQueue);
begin
  { Idle once }
  BoldEffectiveEnvironment.ProcessMessages;

  while not SendQueue.Empty do
    fUDPClient.Send('255.255.255.255',
                   fUDPClient.Port,
                   SIdentification + SendQueue.Dequeue);
end;

end.
