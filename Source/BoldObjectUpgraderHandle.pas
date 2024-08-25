{ Global compiler directives }
{$include bold.inc}
unit BoldObjectUpgraderHandle;

interface

uses
  classes,
  BoldAbstractObjectUpgrader,
  BoldObjectUpgrader,
  BoldAbstractPersistenceHandleDB,
  BoldHandles,
  BoldSystemRT,
  BoldPersistenceControllerDefault,
  BoldSubscription,
  BoldAbstractObjectUpgraderHandle;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldObjectUpgraderHandle = class(TBoldAbstractObjectUpgraderHandle)
  private
    FPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    fComponentSubscriber: TBoldPassThroughSubscriber;
    procedure SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetSystemTypeInfoHandle(const Value: TBoldSystemTypeInfoHandle);
    procedure _ReceiveComponentEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure PlaceComponentSubscriptions;
    function GetObjectUpgrader: TBoldObjectUpgrader;
    function GetSystemTypeInfo: TBoldSystemTypeInfo;
    function GetPersistenceController: TBoldPersistenceControllerDefault;
    function GetOnUpgradeObject: TBoldUpgradeEvent;
    procedure SetOnUpgradeObject(const Value: TBoldUpgradeEvent);
  protected
    function CreateObjectUpgrader: TBoldAbstractObjectUpgrader; override;
    function ConfigClass: TBoldObjectUpgraderConfigClass; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    property ObjectUpgrader: TBoldObjectUpgrader read GetObjectUpgrader;
  published
    property PersistenceHandle: TBoldAbstractPersistenceHandleDB read FPersistenceHandle write SetPersistenceHandle;
    property SystemTypeInfoHandle: TBoldSystemTypeInfoHandle read FSystemTypeInfoHandle write SetSystemTypeInfoHandle;
    property OnUpgradeObject: TBoldUpgradeEvent read GetOnUpgradeObject write SetOnUpgradeObject;
  end;

implementation

uses
  SysUtils;

const
  breSystemTypeInfoHandleDestroying = 100;
  brePersistenceHandleDestroying = 101;
  breSystemTypeInfoHandleChanged = 102;
  brePersistenceHandleDeactivating = 103;

{ TBoldObjectUpgraderHandle }

function TBoldObjectUpgraderHandle.ConfigClass: TBoldObjectUpgraderConfigClass;
begin
  result := TBoldObjectUpgraderConfigurationWithEvent;
end;

constructor TBoldObjectUpgraderHandle.Create(owner: TComponent);
begin
  inherited;
  fComponentSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveComponentEvents);
end;

function TBoldObjectUpgraderHandle.CreateObjectUpgrader: TBoldAbstractObjectUpgrader;
begin
  result := TBoldObjectUpgrader.Create(Config, GetSystemTypeInfo, GetPersistenceController);
end;

destructor TBoldObjectUpgraderHandle.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  inherited;                       
end;

function TBoldObjectUpgraderHandle.GetObjectUpgrader: TBoldObjectUpgrader;
begin
  result := inherited ObjectUpgrader as TBoldObjectUpgrader;
end;

function TBoldObjectUpgraderHandle.GetOnUpgradeObject: TBoldUpgradeEvent;
begin
  result := ObjectUpgrader.OnUpgradeObject;
end;

function TBoldObjectUpgraderHandle.GetPersistenceController: TBoldPersistenceControllerDefault;
begin
  if assigned(PersistenceHandle) then
    result := PersistenceHandle.PersistenceControllerDefault
  else
    result := nil;
end;

function TBoldObjectUpgraderHandle.GetSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(SystemTypeInfoHandle) then
    result := SystemTypeInfoHandle.StaticSystemTypeInfo
  else
    result := nil;
end;

procedure TBoldObjectUpgraderHandle.PlaceComponentSubscriptions;
begin
  fComponentSubscriber.CancelAllSubscriptions;
  if assigned(fSystemTypeInfoHandle) then
  begin
    FSystemTypeInfoHandle.AddSmallSubscription(fComponentSubscriber, [beDestroying], breSystemTypeInfoHandleDestroying);
    FSystemTypeInfoHandle.AddSmallSubscription(fComponentSubscriber, [beValueIdentityChanged], breSystemTypeInfoHandleChanged);
  end;
  if assigned(fPersistenceHandle) then
  begin
    FPersistenceHandle.AddSmallSubscription(fComponentSubscriber, [beDestroying], brePersistenceHandleDestroying);
    FPersistenceHandle.AddSmallSubscription(fComponentSubscriber, [beDeactivating], brePersistenceHandleDeactivating);
  end;
end;

procedure TBoldObjectUpgraderHandle.SetOnUpgradeObject(
  const Value: TBoldUpgradeEvent);
begin
  ObjectUpgrader.OnUpgradeObject := Value;
end;

procedure TBoldObjectUpgraderHandle.SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  FPersistenceHandle := Value;
  PlaceComponentSubscriptions;
end;

procedure TBoldObjectUpgraderHandle.SetSystemTypeInfoHandle(const Value: TBoldSystemTypeInfoHandle);
begin
  FSystemTypeInfoHandle := Value;
  PlaceComponentSubscriptions;
end;


procedure TBoldObjectUpgraderHandle._ReceiveComponentEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breSystemTypeInfoHandleDestroying: SystemTypeInfoHandle := nil;
    brePersistenceHandleDestroying: PersistenceHandle := nil;
  end;
  if requestedEvent in [
    breSystemTypeInfoHandleDestroying, brePersistenceHandleDestroying,
    breSystemTypeInfoHandleChanged, brePersistenceHandleDeactivating] then
    ObjectUpgrader.ReleaseBoldSystem;
end;

end.
