
{ Global compiler directives }
{$include bold.inc}
unit BoldSnooperHandle;

interface

uses
  BoldPersistenceController,
  BoldPersistenceHandlePTWithModel,
  BoldSubscription,
  BoldAbstractLockManagerHandle,
  BoldAbstractPropagatorHandle,
  BoldSnooper,
  Classes;

type
  {forward declarations}
  TBoldSnooperHandle = class;
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldSnooperHandle = class(TBoldPersistenceHandlePassthroughWithModel)
  private
    fPTSubscriber: TBoldPassThroughSubscriber;
    fLockManagerHandle: TBoldAbstractLockManagerHandle;
    FCheckDatabaseLock: Boolean;
    fPropagatorHandle: TBoldAbstractPropagatorHandle;
    fClassesToIgnore: string;
    fUseSubscriptions: boolean;
    fUseClassEvents: boolean;
    fUseMemberLevelOSS: boolean;
    procedure SetLockManagerHandle(Value: TBoldAbstractLockManagerHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
    function GetConnected: Boolean;
    procedure ReadObsoleteMachineNameProperty(Reader: TReader);
    procedure ReadObsoleteProperty(Reader: TReader; const PropertyName, NewPropertyName, OldPropertyValue, ComponentName: string);
    function GetIsLoaded: Boolean;
    function GetSnooper: TBoldSnooper;
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure DefineProperties(Filer: TFiler); override;
    property IsLoaded: Boolean read GetIsLoaded;
    property Snooper: TBoldSnooper read GetSnooper;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Connected: Boolean read GetConnected;
  published
    property NextPersistenceHandle;
    property BoldModel;
    property LockManagerHandle: TBoldAbstractLockManagerHandle read fLockManagerHandle write SetLockManagerHandle;
    property CheckDatabaseLock: Boolean read fCheckDatabaseLock write fCheckDatabaseLock;
    property PropagatorHandle: TBoldAbstractPropagatorHandle read fPropagatorHandle write SetPropagatorHandle;
    property UseClassEvents: boolean read fUseClassEvents write fUseClassEvents;
    property UseMemberLevelOSS: boolean read fUseMemberLevelOSS write fUseMemberLevelOSS;
    property UseSubscriptions: boolean read fUseSubscriptions write fUseSubscriptions;
    property ClassesToIgnore: string read fClassesToIgnore write fClassesToIgnore;
  end;

implementation

uses
  Dialogs,
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldPersistenceHandlePassThrough;

function TBoldSnooperHandle.CreatePersistenceController: TBoldPersistenceController;
var
  Snooper: TBoldSnooper;
begin
  if not Assigned(BoldModel) then
    raise EBold.CreateFmt('%s.CreatePersistenceController: cannot find a BoldModel', [ClassName]);
  Snooper := TBoldSnooper.Create(BoldModel.MoldModel, self);
  ChainPersistenceController(Snooper);
  if Assigned(PropagatorHandle) then
    Snooper.OnPropagatorFailure := PropagatorHandle.DoPropagatorCallFailed
  else
    raise EBold.CreateFmt(sPropagatorHandleNotAssigned, [ClassName]);
  result := Snooper;
  Snooper.UseClassEvents := UseClassEvents;
  Snooper.UseMemberLevelOSS := UseMemberLevelOSS;
  Snooper.ClassesToIgnore := ClassesToIgnore;
  Subscribe(True);
end;

constructor TBoldSnooperHandle.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fPTSubscriber := TBoldPassThroughSubscriber.Create(_Receive);
  fCheckDatabaseLock := false;
end;

destructor TBoldSnooperHandle.Destroy;
begin
  FreeAndNil(fPTSubscriber);
  inherited;
end;

procedure TBoldSnooperHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fLockManagerHandle) and (RequestedEvent = beDestroying) then
    LockManagerHandle := nil;
  if (Originator = fPropagatorHandle) and (RequestedEvent = beDestroying) then
    PropagatorHandle := nil;
end;

procedure TBoldSnooperHandle.SetLockManagerHandle(
  Value: TBoldAbstractLockManagerHandle);
begin
  if (fLockManagerHandle <> Value) then
  begin
    Subscribe(False);
    fLockManagerHandle := Value;
    Subscribe(True);
  end;
end;

procedure TBoldSnooperHandle.SetPropagatorHandle(
  Value: TBoldAbstractPropagatorHandle);
begin
  if Value <> fPropagatorHandle then
  begin
    Subscribe(False);
    fPropagatorHandle := Value;
    if IsLoaded then
    begin
      if Assigned(fPropagatorHandle) then
        Snooper.OnPropagatorFailure := fPropagatorHandle.DoPropagatorCallFailed
      else if not (csDestroying in ComponentState) then
        Snooper.OnPropagatorFailure := nil;
      if Assigned(Value) then
      begin
        Snooper.UseClassEvents := UseClassEvents;
        Snooper.UseMemberLevelOSS := UseMemberLevelOSS;
        Snooper.ClassesToIgnore := ClassesToIgnore;
      end;
    end;
    Subscribe(True);
  end;
end;

function TBoldSnooperHandle.GetConnected: Boolean;
begin
  Result := Active;
  if Result and IsLoaded then
    Result := Assigned(Snooper.Propagator);
end;

procedure TBoldSnooperHandle.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MachineName', ReadObsoleteMachineNameProperty, nil, True);
end;

procedure TBoldSnooperHandle.ReadObsoleteMachineNameProperty(
  Reader: TReader);
var
  OldPropertyValue: string;
begin
  OldPropertyValue := Reader.ReadString;
  ReadObsoleteProperty(Reader, 'MachineName', 'ServerHost', OldPropertyValue, 'TBoldPropagatorHandleCom');
end;

procedure TBoldSnooperHandle.ReadObsoleteProperty(Reader: TReader;
  const PropertyName, NewPropertyName, OldPropertyValue, ComponentName: string);
begin
  if (csDesigning in ComponentState) then
    MessageDlg(Format('%s.%s has been moved to component (%s.%s). Old value was "%s"',
                      [ClassName, PropertyName, ComponentName, NewPropertyName, OldPropertyValue]), mtWarning, [mbOK], 0);
end;

function TBoldSnooperHandle.GetIsLoaded: Boolean;
begin
  Result := not (csDesigning in ComponentState) and not (csLoading in ComponentState);
end;

function TBoldSnooperHandle.GetSnooper: TBoldSnooper;
begin
  result := PersistenceController as TBoldSnooper;
end;

procedure TBoldSnooperHandle.Subscribe(const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fLockManagerHandle) then
        fLockManagerHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
      if Assigned(fPropagatorHandle) then
        fPropagatorHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;

end.
