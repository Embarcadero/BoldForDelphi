
{ Global compiler directives }
{$include bold.inc}
unit BoldLowEndPropagatorClasses;

interface

uses
  Variants,
  BoldBase,
  ComObj,
  BoldPropagatorInterfaces_TLB,
  Classes;

type

  TBoldInternalPropagator = class
   private
    ClientList: TInterfaceList;
    function  RegisterClient(const ListenerInterface: IBoldListener; out Value: Integer): HResult; stdcall;
    function  UnregisterClient(ClientID: Integer): HResult; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendEvents(BoldClientID: integer; Events: OleVariant);
    procedure RefreshDisplay;
    property PublicClientList: TInterfaceList read ClientList;
   end;

  TBoldClientPropagator = class(TTypedComObject, IBoldClientHandler, IBoldEventPropagator)
  private
      {IBoldClientHandler}
    function  RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
    function  ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
    function  UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
      {IBoldPropagator}
    function  SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function  CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldLowEndpropagatorMainForm,
  ComServ,
  Windows;

var
  G_InternalPropagator: TBoldInternalPropagator;


constructor TBoldInternalPropagator.Create;
begin
  inherited;
  ClientList := TInterfaceList.Create;
end;

destructor TBoldInternalPropagator.Destroy;
begin
  FreeAndNil(ClientList);
  inherited;
end;

function  TBoldInternalPropagator.RegisterClient(const ListenerInterface: IBoldListener; out Value: Integer): HResult; stdcall;
var
  Registered: Boolean;
  i: integer;
begin
  Registered := false;
  for i:= 0 to ClientList.Count - 1 do
    if not Assigned(ClientList[i]) then
    begin
      ClientList[i] := ListenerInterface;
      Value := i + 1;
      Registered := true;
      Break;
    end;
  if not Registered then
    Value := ClientList.Add(ListenerInterface) + 1;
  RefreshDisplay;
  Result := S_OK;
end;

procedure TBoldInternalPropagator.SendEvents(BoldClientID: Integer; Events: OleVariant);
var
  i: Integer;
begin
  for i := 0 to ClientList.Count - 1 do
  begin
    if Assigned(ClientList[i]) and (BoldClientID <> (i + 1))then
      (ClientList[i] as IBoldListener).ReceiveEvents(Events);
  end;
end;

procedure TBoldInternalPropagator.RefreshDisplay;
var
  Count, i: integer;
  Clients: TList;
begin
  count := 0;
  Clients := nil;
  for i := 0 to ClientList.Count - 1 do
  begin
    if not Assigned(Clients) then Clients := TList.Create;
    if Assigned(ClientList[i]) then
    begin
      Inc(Count);
      Clients.Add(Pointer(true));
    end
    else
      Clients.Add(Pointer(false));
  end;
  BoldLEPropagatorMainForm.UpdateClientCount(Count);
  BoldLEPropagatorMainForm.UpdateClientList(Clients);
  FreeAndNil(Clients);
end;

function TBoldInternalPropagator.UnregisterClient(ClientID: Integer): HResult;
begin
  Result := S_OK;
  try
    ClientList[ClientID - 1] := nil;
    RefreshDisplay;
  except
    Result := S_FALSE;
  end;
end;

{TBoldClientPropagator}

function TBoldClientPropagator.RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
begin
  Result := G_InternalPropagator.RegisterClient(BoldClientListener,BoldClientID);
end;

function TBoldClientPropagator.ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
begin
  Result := S_OK;
end;

function TBoldClientPropagator.UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
begin
  Result := G_InternalPropagator.UnregisterClient(BoldClientID);
end;

function  TBoldClientPropagator.SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
var
  EventList: TStringList;
  i: integer;
begin
  Result := S_OK;
  try
    if VarIsArray(Events) then
    begin
      EventList := TStringList.Create;
      for i := VarArrayLowBound(Events, 1) to VarArrayHighBound(Events, 1) do
      begin
        EventList.Add(Events[i]);
        if BoldLEPropagatorMainForm.DoLogEvents then
          BoldLEPropagatorMainForm.Memo1.Lines.Add(Format('ClientID=%d: EVENT: %s',[BoldClientID, EventList[i]]));
      end;
      G_InternalPropagator.SendEvents(BoldClientID, Events);
    end;
  except
    Result := S_FALSE;
  end;
end;

function TBoldClientPropagator.CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult;
begin
  Result := S_OK;
end;

function  TBoldClientPropagator.AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult;
var
  i: integer;
begin
  Result := S_OK;
  try
    if VarIsArray(Subscriptions) then
      if BoldLEPropagatorMainForm.DoLogEvents then
        for i := VarArrayLowBound(Subscriptions, 1) to VarArrayHighBound(Subscriptions, 1) do
          BoldLEPropagatorMainForm.Memo1.Lines.Add(Format('ClientID=%d: SUBSCRIPTION: %s',[BoldClientID, Subscriptions[i]]));
  except
    Result := S_FALSE;
  end;
end;

initialization
  G_InternalPropagator := TBoldInternalPropagator.Create;
  TTypedComObjectFactory.Create(ComServer, TBoldClientPropagator, CLASS_BoldPropagator,
    ciMultiInstance, tmFree);

finalization
  FreeAndNil(G_InternalPropagator);
end.
