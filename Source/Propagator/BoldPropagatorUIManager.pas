
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorUIManager;

interface

uses
  BoldClientHandler,
  BoldSubscription,
  BoldGuard,
  BoldDefs,
  syncobjs;

type
  { forward declarations }
  TUIManager = class;

  { TUIManager }
  TUIManager = class(TBoldExtendedPassthroughSubscriber)
  private
    fClientHandler: TBoldClientHandler;
    fEnabled: Boolean;
    procedure UpdateDisplay(All: Boolean);
    procedure setClientHandler(const Value: TBoldClientHandler);
    procedure RefreshClick(Sender: TObject);
    procedure ShutDownClick(Sender: TObject);
    procedure OnFormDestroy(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnGetExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    property ClientHandler: TBoldClientHandler read fClientHandler write setClientHandler;
    procedure SetDequeueIndicator(value: Boolean);
    procedure ClientInfoChanged(const ClientId: TBoldClientId; const ClientSubscriptions, OutQ: integer);
    procedure GlobalInfoChanged(const InQ, Added, Sent: integer);
    procedure RequestGlobalInfo;
    procedure RequestClientInfo(const ClientId: TBoldClientId; const CalculateSubscriptions: Boolean);
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldPropagatorMainform,
  BoldPropagatorConstants,
  BoldPropagatorServer,
  Forms,
  windows,
  ComServ,
  classes,
  comctrls;

{ UIManager }

constructor TUIManager.Create;
begin
  inherited CreateWithExtendedReceive(OnGetExtendedEvent);
  fEnabled := false;
  if ComServ.ComServer.UIInteractive then
  begin
    Application.CreateForm(TPropagatorMainForm, PropagatorMainForm);
    PropagatorMainForm.mnuRefreshGUI.OnClick := RefreshClick;
    PropagatorMainForm.mnuShutdown.OnClick := ShutDownClick;

    PropagatorMainForm.OnDestroy := OnFormDestroy;
    fEnabled := true;
  end;
end;

destructor TUIManager.Destroy;
begin
  if assigned(PropagatorMainForm) then
    PropagatorMainForm.OnDestroy := nil;
  CancelAllSubscriptions;
  inherited;
end;

procedure TUIManager.OnGetExtendedEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  if fEnabled and (RequestedEvent = breUpdateDisplay) then
    UpdateDisplay(false);
end;

procedure TUIManager.RefreshClick(Sender: TObject);
begin
  PropagatorMainForm.mnuRefreshGUI.Caption := 'Hang on...';
  UpdateDisplay(true);
end;

procedure TUIManager.setClientHandler(const Value: TBoldClientHandler);
begin
  fClientHandler := Value;
  if assigned(PropagatorMainForm) then
    PropagatorMainForm.SetClientHandler(value);
  if fEnabled then
  begin
    ClientHandler.AddSubscription(self, BOLD_PROPAGATOR_CLIENT_REGISTERED, breUpdateDisplay);
    ClientHandler.AddSubscription(self, BOLD_PROPAGATOR_CLIENT_REMOVED, breUpdateDisplay);
  end;
end;

procedure TUIManager.SetDequeueIndicator(value: Boolean);
begin
  if fEnabled then
  begin
    if Value then
      PostMessage(PropagatorMainForm.Handle, BM_PROPAGATOR_DISPLAY_STARTDEQUEUE, 0, 0)
    else
      PostMessage(PropagatorMainForm.Handle, BM_PROPAGATOR_DISPLAY_DONEDEQUEUE, 0, 0)
  end;
end;

procedure TUIManager.ClientInfoChanged(const ClientId: TBoldClientId; const ClientSubscriptions, OutQ: integer);
var
  PInfo: PPropagatorStateInfo;
begin
  if fEnabled then
  begin
    New(PInfo);
    Fillchar(PInfo^, SizeOf(PInfo^), 0);
    PInfo^.ClientId := ClientId;
    PInfo^.Events := ClientSubscriptions;
    PInfo^.OutQ := OutQ;
    if Assigned(PropagatorMainForm) then
      PostMessage(PropagatorMainForm.Handle, BM_UPDATE_CLIENT_INFO, 0, Integer(PInfo));
  end;
end;

procedure TUIManager.GlobalInfoChanged(const InQ, Added, Sent: integer);
var
  PInfo: PPropagatorStateInfo;
begin
  if fEnabled then
  begin
    New(PInfo);
    Fillchar(PInfo^, SizeOf(PInfo^), 0);
    PInfo^.Added := Added;
    PInfo^.Sent := Sent;
    if Assigned(PropagatorMainForm) then
      PostMessage(PropagatorMainForm.Handle, BM_UPDATE_GLOBAL_INFO, 0, Integer(PInfo));
  end;
end;

procedure TUIManager.UpdateDisplay(All: Boolean);
var
  i: integer;
  ClientInfoList: TStringList;
  ClientInfo: TStringList;
  ClientId: Integer;
  Guard: IBoldGuard;
begin
  if fEnabled then
  begin
    Guard := TBoldGuard.Create(ClientInfoList, ClientInfo);

    if not Assigned(PropagatorMainForm) then
      Exit;

    PropagatorMainForm.ClearAllUsers;

    ClientInfoList := TStringList.Create;
    ClientInfo := TStringList.Create;
    ClientHandler.GetRegisteredClientInfos(ClientInfoList);
    for i := 0 to ClientInfoList.Count - 1 do
    begin
      ClientInfo.CommaText := ClientInfoList[i];
      ClientID := StrToIntDef(ClientInfo.values['ID'], -1);
      if ClientID = -1 then
      begin
        PropagatorMainForm.SetClientHandlerStats(
        StrToIntDef(ClientInfo.Values['TotalClients'], -1),
        StrToIntDef(ClientInfo.Values['PeakClients'], -1),
        StrToIntDef(ClientInfo.Values['TotalLostEvents'], -1));
      end
      else
      begin
        if All then
          RequestClientInfo(ClientID, PropagatorMainForm.mnuCountSubscriptions.Checked);

        PropagatorMainForm.AddUser(
          ClientID,
          ClientInfo.values['IDString'],
          StrToDateFmt(ClientInfo.values['RegistrationTime'], 'yyyy-mm-dd', 'hh:nn:ss', '-'),
          StrToDateFmt(ClientInfo.values['LeaseTimeout'], 'yyyy-mm-dd', 'hh:nn:ss', '-'),
          StrToIntDef(ClientInfo.values['Packages'], -1),
          StrToIntDef(ClientInfo.values['Events'], -1),
          StrToDateFmt(ClientInfo.values['LastSend'], 'yyyy-mm-dd', 'hh:nn:ss', '-'),
          StrToDateFmt(ClientInfo.values['LongestInterval'], 'yyyy-mm-dd', 'hh:nn:ss', '-'),
          ClientInfo.values['Status'],
          StrToIntDef(ClientInfo.values['LostEvents'], 0));
      end;
    end;
    if assigned(TBoldPropagatorServer.Instance.AdvancedPropagator) then
    begin
      PropagatorMainForm.InQ := TBoldPropagatorServer.Instance.AdvancedPropagator.Dequeuer.InQueueCount;
      PropagatorMainForm.InQPeak := TBoldPropagatorServer.Instance.AdvancedPropagator.Dequeuer.InQueueCountPeak;
    end;
    PropagatorMainForm.UpdateUptime;
    if all then
      PropagatorMainForm.UpdateMemoryStats;
    RequestGlobalInfo;
  end;
end;

procedure TUIManager.RequestClientInfo(const ClientId: TBoldClientId; const CalculateSubscriptions: Boolean);
begin
  if assigned(TBoldPropagatorServer.Instance.AdvancedPropagator) then
    PostThreadMessage(TBoldPropagatorServer.Instance.AdvancedPropagator.Dequeuer.ThreadId, BM_REQUEST_CLIENT_INFO, wParam(CalculateSubscriptions), lParam(ClientId));
end;

procedure TUIManager.RequestGlobalInfo;
begin
  if assigned(TBoldPropagatorServer.Instance.AdvancedPropagator) then
    PostThreadMessage(TBoldPropagatorServer.Instance.AdvancedPropagator.Dequeuer.ThreadId, BM_REQUEST_GLOBAL_INFO, 0, 0);
end;

procedure TUIManager.OnFormDestroy(Sender: TObject);
begin
   fEnabled := false;
end;

procedure TUIManager.ShutDownClick(Sender: TObject);
var
  ClientInfoList: TStringList;
  ClientInfo: TStringList;
  i: integer;
  ClientId: integer;
begin
  ClientInfoList := TStringList.Create;
  ClientInfo := TStringList.Create;
  ClientHandler.GetRegisteredClientInfos(ClientInfoList);
  for i := 0 to ClientInfoList.Count - 1 do
  begin
    ClientInfo.CommaText := ClientInfoList[i];
    ClientID := StrToIntDef(ClientInfo.values['ID'], -1);
    ClientHandler.SendDisconnectRequest(ClientId, 'Propagator must be shut down', 60000);
  end;
  ClientInfoList.Free;
  ClientInfo.Free;
end;

initialization

end.
