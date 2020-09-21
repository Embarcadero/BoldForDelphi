unit datamod;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  DB,
  stdctrls,
  Graphics,
  BoldSystem,
  BoldSystemRT,
  BoldHandles,
  ComServ,
  BoldListenerCom,
  BoldModel,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldStringControlPack,
  BoldSubscription,
  BoldElements,
  BoldSystemHandle,
  BoldHandle,
  BoldPersistenceHandle,
  BoldTypeNameHandle,
  BoldBDEInterfaces,
  BoldPersistenceHandleDB,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link, BoldIDAdderHandle, BoldListenerHandle,
  BoldPersistenceHandlePassthrough, BoldSnooperHandle,
  BoldAbstractDequeuer, BoldExternalObjectSpaceEventHandler,
  BoldAbstractModel, BoldAbstractPropagatorHandle, BoldPropagatorHandleCOM,
  BoldClientHandles, BoldComClientHandles, ExtCtrls,
  BoldPersistenceHandlePTWithModel, BoldPersistenceHandleIB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TDataModule1 = class(TDataModule)
    BoldModel1: TBoldModel;
    FullNameRenderer: TBoldAsStringRenderer;
    IsRichRenderer: TBoldAsCheckBoxStateRenderer;
    IsRichFilter: TBoldFilter;
    NameComparer: TBoldComparer;
    NegativeRedRenderer: TBoldAsStringRenderer;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldListenerHandle1: TBoldListenerHandle;
    BoldIDAdderHandle1: TBoldIDAdderHandle;
    BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    bcchEnterprisePropagator: TBoldComConnectionHandle;
    Timer1: TTimer;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    function FullNameRendererGetAsString(Element: TBoldElement;
      representation: Integer; Expression: String): string;
    procedure FullNameRenderStubscribe(Element: TBoldElement;
      representation: Integer; Expression: String;
      subscriber: TBoldSubscriber);
    function IsRichRendererGetAsCheckBoxState(element: TBoldElement;
      representation: Integer; Expression: String): TCheckBoxState;
    function NameComparerCompare(item1,
      item2: TBoldElement): Integer;
    procedure NameComparerSubscribe(boldElement: TBoldElement;
      subscriber: TBoldSubscriber);
    function IsRichFilterFilter(element: TBoldElement): Boolean;
    procedure NegativeRedRendererHoldsChangedValue(
      element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererSetFont(element: TBoldElement;
      aFont: TFont; representation: Integer; Expression: String);
    procedure IsRichFilterSubscribe(element: TBoldElement;
      subscriber: TBoldSubscriber);
    procedure IsRichRendererSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure BoldExternalObjectSpaceEventHandler1Conflict(
      BoldObject: TBoldObject);
    procedure BoldListenerHandle1RegistrationFailed(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure bcchEnterprisePropagatorBeforeConnect(Sender: TObject);
    procedure BoldPropagatorHandleCOM1PropagatorCallFailed(Sender: TObject; const ErrorMessage: String);
    procedure bcchEnterprisePropagatorConnectFailed(Sender: TObject);
    function BoldListenerHandle1ThreadError(aMessage: String): Boolean;
    procedure BoldListenerHandle1ExtendLeaseFailed(
      res: TBoldExtendLeaseResult; const Msg: String);
    procedure BoldExternalObjectSpaceEventHandler1DoDisconnect(
      aMessage: String; RemainDisconnected: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

uses
  BldOwnClasses,
  Dialogs;

{$R *.DFM}

function TDataModule1.FullNameRendererGetAsString(
  Element: TBoldElement; representation: Integer;
  Expression: String): string;
begin
  result := '';
  if assigned( element ) then
    with Element as TPerson do
      Result := Format('%s, %s', [LastName, FirstName])
end;

procedure TDataModule1.FullNameRenderStubscribe(Element: TBoldElement;
  representation: Integer; Expression: String; Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('firstName', Subscriber, false);
  Element.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichRendererGetAsCheckBoxState(
  element: TBoldElement; representation: Integer;
  Expression: String): TCheckBoxState;
begin
  result := cbGrayed;
  if assigned(element) then
    with element as TPerson do
      if Assets > 10000 then
         Result := cbChecked
      else
         Result := cbUnChecked
end;

function TDataModule1.NameComparerCompare(item1,
  item2: TBoldElement): Integer;
begin
  Result := AnsiCompareText((Item1 as TPerson).LastName, (Item2 as TPerson).LastName);
  if Result = 0 then
    Result := AnsiCompareText((Item1 as TPerson).FirstName, (Item2 as TPerson).FirstName);
end;

procedure TDataModule1.NameComparerSubscribe(boldElement: TBoldElement;
  subscriber: TBoldSubscriber);
begin
  boldElement.SubscribeToExpression('firstName', Subscriber, false);
  boldElement.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichFilterFilter(
  element: TBoldElement): Boolean;
begin
  Result := False;
  if assigned( element ) then
    with element as TPerson do
      Result := Assets > 10000;
end;

procedure TDataModule1.NegativeRedRendererHoldsChangedValue(
  element: TBoldElement; representation: Integer;
  Expression: String; Subscriber: TBoldSubscriber);
begin
  if assigned( element ) then
    with NegativeRedRenderer do
      DefaultHoldsChangedValue(element, representation, Expression, nil, subscriber);
end;

procedure TDataModule1.NegativeRedRendererSetFont(
  element: TBoldElement; aFont: TFont; representation: Integer;
  Expression: String);
begin
  if assigned( element ) then
    with element as TPerson do
      if Assets < 0 then
        aFont.Color :=  clRed
      else
        aFont.Color := clBlue;
end;

procedure TDataModule1.IsRichFilterSubscribe(element: TBoldElement;
  subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.IsRichRendererSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.BoldExternalObjectSpaceEventHandler1Conflict(
  BoldObject: TBoldObject);
begin
  ShowMessage(BoldObject.AsString + 'Conflict with a modification in another database!!');
end;

procedure TDataModule1.BoldListenerHandle1RegistrationFailed(
  Sender: TObject);
begin
  showmessage('Failed to register with propagator');
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  BoldListenerHandle1.ListenerThread.ExtendLease;
end;

procedure TDataModule1.bcchEnterprisePropagatorBeforeConnect(Sender: TObject);
begin
  bcchEnterprisePropagator.ServerHost := InputBox('Propagator Server', 'Enter the Machine Name', 'localhost');
end;

procedure TDataModule1.BoldPropagatorHandleCOM1PropagatorCallFailed(
  Sender: TObject; const ErrorMessage: String);
begin
  showmessage('Propagator call failed: ' + ErrorMessage);
end;

procedure TDataModule1.bcchEnterprisePropagatorConnectFailed(Sender: TObject);
begin
  showmessage('Failed to connect to the Propagator');
end;

function TDataModule1.BoldListenerHandle1ThreadError(aMessage: String): Boolean;
begin
  Result := False;
  showmessage('Thread error in the ListenerThread: '+ aMessage);
end;


procedure TDataModule1.BoldListenerHandle1ExtendLeaseFailed(
  res: TBoldExtendLeaseResult; const Msg: String);
var
  s: string;
  s2: string;
begin
  case res of
    elrFailed: s := 'Failed';
    elrFailedExpired: s := 'Failed (Expired)';
    elrDenied: s := 'Denied';
    elrNotRegistered: s := 'Denied (Not registered)';
  end;
  if msg = '' then
    s2 := 'Reason unknown'
  else
    s2 := msg;

  showmessage(format('Extend lease %s: %s', [s, s2]));
end;

procedure TDataModule1.BoldExternalObjectSpaceEventHandler1DoDisconnect(
  aMessage: String; RemainDisconnected: Integer);
begin
  bcchEnterprisePropagator.Connected := false;
end;

initialization
  TBoldListenerComFactory.Create(ComServer);

end.
