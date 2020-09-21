unit BoldIDAdderHandle;

interface

uses
  BoldListenerHandle,
  BoldPersistenceHandlePassthrough,
  BoldPersistenceController,
  BoldPersistenceControllerPassthrough,
  BoldPropagatorConstants,
  BoldPersistenceHandle,
  BoldSubscription,
  comobj,
  classes;

type
  { forward declarations }
  TBoldIdAdderHandle = class;

  { TBoldIDAdderHandle }
  TBoldIDAdderHandle = class(TBoldPersistenceHandlePassthrough)
  private
    fBoldListenerHandle: TBoldListenerHandle;
    fPTSubscriber: TBoldPassThroughSubscriber;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure ReadObsoleteAutoStartProperty(Reader: TReader);
    procedure ReadObsoleteLeaseDurationProperty(Reader: TReader);
    procedure ReadObsoleteAutoExtendLeaseProperty(Reader: TReader);
    procedure ReadObsoletePollingIntervalProperty(Reader: TReader);
    procedure ReadObsoleteMachineNameProperty(Reader: TReader);
    procedure ReadObsoleteOnRegistrationFailedEventHandler(Reader: TReader);
    procedure ReadObsoleteProperty(Reader: TReader; const PropertyName, NewPropertyName, OldPropertyValue, ComponentName: string);
    procedure SetBoldListenerHandle(const Value: TBoldListenerHandle);
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure SetActive(Value: Boolean); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property NextPersistenceHandle;
    property BoldListener: TBoldListenerHandle read fBoldListenerHandle write SetBoldListenerHandle;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldIDAdder,
  dialogs,
  BoldPMConsts;

{ TBoldIDAdderHandle }

constructor TBoldIDAdderHandle.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fPTSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

function TBoldIDAdderHandle.CreatePersistenceController: TBoldPersistenceController;
var
  Temp: TBoldIDAdder;
begin
  temp := TBoldIDAdder.Create;
  ChainPersistenceController(Temp);
  if Assigned(fBoldListenerHandle) then
    Temp.Listener := fBoldListenerHandle.ListenerThread;
  result := Temp;
end;

procedure TBoldIDAdderHandle.DefineProperties(Filer: TFiler);
begin
  inherited;
  // property AutoStart moved to TBoldListenerHandle
  Filer.DefineProperty('AutoStart', ReadObsoleteAutoStartProperty, nil, True); // do not localize
  // property AutoExtendLease moved to TBoldListenerHandle
  Filer.DefineProperty('AutoExtendLease', ReadObsoleteAutoExtendLeaseProperty, nil, True); // do not localize
  // property LeaseDuration moved to TBoldListenerHandle
  Filer.DefineProperty('LeaseDuration', ReadObsoleteLeaseDurationProperty, nil, True); // do not localize
  // property Polling interval moved to TBoldListenerHandle
  Filer.DefineProperty('PollingInterval', ReadObsoletePollingIntervalProperty, nil, True); // do not localize
  // property MachineName moved to TBoldPropagatorHandleCOM
  Filer.DefineProperty('MachineName', ReadObsoleteMachineNameProperty, nil, True); // do not localize
  // event handler OnRegistrationFailed moved to TBoldListenerHandle
  Filer.DefineProperty('OnRegistrationFailed', ReadObsoleteOnRegistrationFailedEventHandler, nil, True); // do not localize
end;

destructor TBoldIDAdderHandle.Destroy;
begin
  FreeAndNil(fPTSubscriber);
  inherited;
end;

procedure TBoldIDAdderHandle.ReadObsoleteAutoExtendLeaseProperty(Reader: TReader);
var
  OldPropertyValue: Boolean;
  ValueAsString: string;
begin
  OldPropertyValue := Reader.ReadBoolean;
  if OldPropertyValue then
    ValueAsString := 'True' // do not localize
  else
    ValueAsString := 'False'; // do not localize
  ReadObsoleteProperty(Reader, 'AutoExtendLease', 'AutoExtendLease', ValueAsString, 'TBoldListenerHandle'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoleteAutoStartProperty(Reader: TReader);
var
  OldPropertyValue: Boolean;
  ValueAsString: string;
begin
  OldPropertyValue := Reader.ReadBoolean;
  if OldPropertyValue then
    ValueAsString := 'True' // do not localize
  else
    ValueAsString := 'False'; // do not localize
  ReadObsoleteProperty(Reader, 'AutoStart', 'AutoStart', ValueAsString, 'TBoldListenerHandle'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoleteLeaseDurationProperty(Reader: TReader);
var
  OldPropertyValue: Integer;
  ValueAsString: string;
begin
  OldPropertyValue := Reader.ReadInteger;
  ValueAsString := IntToStr(OldPropertyValue);
  ReadObsoleteProperty(Reader, 'LeaseDuration', 'LeaseDuration', ValueAsString, 'TBoldListenerHandle'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoleteMachineNameProperty(Reader: TReader);
var
  OldPropertyValue: string;
begin
  OldPropertyValue := Reader.ReadString;
  ReadObsoleteProperty(Reader, 'MachineName', 'ServerHost', OldPropertyValue, 'TBoldPropagatorHandleCom'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoleteOnRegistrationFailedEventHandler(
  Reader: TReader);
var
  OldPropertyValue: string;
begin
  OldPropertyValue := Reader.ReadString;
  ReadObsoleteProperty(Reader, 'OnRegistrationFailed', 'OnRegistrationFailed', OldPropertyValue, 'TBoldListenerHandle'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoletePollingIntervalProperty(Reader: TReader);
var
  OldPropertyValue: Integer;
  ValueAsString: string;
begin
  OldPropertyValue := Reader.ReadInteger;
  ValueAsString := IntToStr(OldPropertyValue);
  ReadObsoleteProperty(Reader, 'PollingInterval', 'PollingInterval', ValueAsString, 'TBoldListenerHandle'); // do not localize
end;

procedure TBoldIDAdderHandle.ReadObsoleteProperty(Reader: TReader;
  const PropertyName, NewPropertyName, OldPropertyValue, ComponentName: string);
begin
  if (csDesigning in ComponentState) then
    MessageDlg(Format(sPropertyHasMoved,
                      [ClassName, PropertyName, ComponentName, NewPropertyName, OldPropertyValue]), mtWarning, [mbOK], 0);
end;

procedure TBoldIDAdderHandle.SetActive(Value: Boolean);
begin
  inherited;
  BoldListener.SetActive(Value);
end;

procedure TBoldIDAdderHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fBoldListenerHandle) and (RequestedEvent = beDestroying) then
    BoldListener := nil;
end;

procedure TBoldIDAdderHandle.SetBoldListenerHandle(
  const Value: TBoldListenerHandle);
begin
  if (fBoldListenerHandle <> Value) then
  begin
    Subscribe(False);
    fBoldListenerHandle := Value;
    Subscribe(True);
  end;
end;

procedure TBoldIDAdderHandle.Subscribe(const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fBoldListenerHandle) then
        fBoldListenerHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;

end.
