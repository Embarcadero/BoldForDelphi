{ Global compiler directives }
{$include bold.inc}
unit BoldSystemHandle;

interface

uses
  Classes,
  BoldSystem,
  BoldElements,
  BoldPersistenceHandle,
  BoldSubscription,
  BoldHandles,
  BoldLockRegions,
  BoldAbstractPersistenceHandleDB;

type
  { Forward declaration of classes }
  TBoldSystemHandle = class;

  { TBoldSystemHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldSystemHandle = class(TBoldAbstractSystemHandle)
  private
    fBoldSystem: TBoldSystem;
    fStreamedActive: Boolean;
    fAutoActivate: Boolean;
    fPersistenceHandle: TBoldPersistenceHandle;
    fPersistenceHandleSubscriber: TBoldPassThroughSubscriber;
    fRegionDefinitionSubscriber: TBoldPassThroughSubscriber;
    fOnLookupOclDefinition: TBoldLookUpOclDefinition;
    fOnOptimisticLockingFailed: TBoldOptimisticLockingFailedEvent;
    FOnPreUpdate: TNotifyEvent;
    fRegionFactory: TBoldRegionFactory;
    procedure PanicShutDownSystem(Message: String);
    procedure _ReceivePersistenceHandle(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure _ReceiveRegionDefinitions(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetAutoActivate: Boolean;
    procedure SetAutoActivate(Value: Boolean);
    procedure SetPersistenceHandle(NewHandle: TBoldPersistenceHandle);
    function GetPersistent: Boolean;
    procedure ReadTrackBold(Reader: TReader);
    procedure SetOnOptimisticLockingFailed(const Value: TBoldOptimisticLockingFailedEvent);
    procedure SetOnPreUpdate(Value: TNotifyEvent);
    function GetPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
  protected
    function GetValue: TBoldElement; override;
    procedure Loaded; override;
    function GetSystem: TBoldSystem; override;
    procedure SetActive(Value: Boolean); override;
    function GetActive: Boolean; override;
    procedure ModelChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateDatabase;
    procedure InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    procedure Discard;
    property Persistent: Boolean read GetPersistent;
    property RegionFactory: TBoldRegionFactory read fRegionFactory;
    property PersistenceHandleDB: TBoldAbstractPersistenceHandleDB read GetPersistenceHandleDB;
  published
    property AutoActivate: Boolean read GetAutoActivate write SetAutoActivate default False;
    {$IFNDEF T2H}
    property SystemTypeInfoHandle;
    property Active;
    {$ENDIF}
    property PersistenceHandle: TBoldPersistenceHandle read fPersistenceHandle write SetPersistenceHandle;
    property OnPreUpdate: TNotifyEvent read FOnPreUpdate write SetOnPreUpdate;
    property OnOptimisticLockingFailed: TBoldOptimisticLockingFailedEvent read fOnOptimisticLockingFailed write SetOnOptimisticLockingFailed;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldRegionDefinitions,
  BoldEnvironment,
  BoldPersistenceController,
  BoldPersistenceHandlePassthrough;

const
  brePersistenceHandleDestroying = 100;
  breRegiondefinitionsDestroying = 101;
  breRegionDefinitionClearing = 102;

{ TBoldSystemHandle }

destructor TBoldSystemHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fBoldSystem);
  FreeAndNil(fPersistenceHandleSubscriber);
  FreeAndNil(fRegiondefinitionSubscriber);
  FreeAndNil(fRegionFactory);
  inherited Destroy;
end;

procedure TBoldSystemHandle.Discard;
begin
  if Assigned(System) then
    System.Discard;
end;

procedure TBoldSystemHandle.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive or ((not (csdesigning in ComponentState)) and AutoActivate) then
      SetActive(True);
  except
    if csDesigning in ComponentState then
      BoldEffectiveEnvironment.HandleDesigntimeException(Self)
    else
      raise;
  end;
end;

procedure TBoldSystemHandle.UpdateDatabase;
begin
  if Assigned(System) then
    System.UpdateDatabase
  else
    raise EBold.CreateFmt(sCannotUpdateDatebaseWithoutSystem, [ClassName, Name]);
end;

function TBoldSystemHandle.GetSystem: TBoldSystem;
begin
  Result := fBoldSystem;
end;

procedure TBoldSystemHandle.SetAutoActivate(Value: Boolean);
begin
  if Value <> fAutoActivate then
  begin
    fAutoActivate := Value;
    if (csReading in ComponentState) and
      not (csDesigning in ComponentState) then
      Active := AutoActivate;
  end;
end;

function TBoldSystemHandle.GetAutoActivate: Boolean;
begin
  Result := fAutoActivate;
end;

procedure TBoldSystemHandle.SetActive(Value: Boolean);
var
  PController: TBoldPersistenceController;
  vBoldSystem: TBoldSystem;
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Value <> Active then
    begin
      if Value then
      begin
        if (csDesigning in ComponentState) then
          exit;
        if not assigned(SystemTypeInfoHandle) then
          raise EBold.CreateFmt(sUnableToActivateSystemWithoutTypeInfoHandle, [name]);

        if not assigned(StaticSystemTypeInfo) then
          raise EBold.Create(sUnableToFindTypeInfoHandle);

        if not StaticSystemTypeInfo.SystemIsRunnable then
          raise EBold.CreateFmt(sUnableToActivateSystem, [BOLDCRLF, StaticSystemTypeInfo.InitializationLog.Text]);

        if Persistent then
        begin
          PersistenceHandle.Active := true;
          PController := PersistenceHandle.PersistenceController;
        end
        else
          PController := nil;

        if assigned(SystemTypeInfoHandle.RegionDefinitions) and not assigned(fRegionFactory) then
        begin
          SystemTypeInfoHandle.RegionDefinitions.AddSmallSubscription(fRegionDefinitionSubscriber, [beDestroying], breRegionDefinitionsDestroying);
          SystemTypeInfoHandle.RegionDefinitions.AddSubscription(fRegionDefinitionSubscriber, beRegionDefinitionClearing, breRegionDefinitionClearing);
          fRegionFactory := TBoldRegionFactory.Create(SystemTypeInfoHandle.RegionDefinitions);
        end;

        try
          fBoldSystem := TBoldSystem.CreatewithTypeInfo(nil, StaticSystemTypeInfo, PController, fRegionFactory);
        except
          fBoldSystem := nil;
          FreeAndNil(fRegionFactory);
          if Persistent then
            PersistenceHandle.Active := False;
          raise;
        end;

        if Active then
        begin
          fBoldSystem.Evaluator.SetLookupOclDefinition(fOnLookupOclDefinition);
          fBoldSystem.IsDefault := IsDefault;
          fBoldSystem.OnPreUpdate := OnPreUpdate;
          fBoldSystem.OnOptimisticLockingFailed := OnOptimisticLockingFailed;
        end;
      end
      else
      begin
        fBoldSystem.EnsureCanDestroy;
        vBoldSystem := fBoldSystem;
        fBoldSystem := nil;
        if Persistent then
          PersistenceHandle.Active := false;
        FreeAndNil(vBoldSystem);
        FreeAndNil(fRegionFactory);
      end;
      SendEvent(Self, beValueIdentityChanged);
    end;
end;

function TBoldSystemHandle.GetActive: Boolean;
begin
  Result := Assigned(fBoldSystem);
end;

procedure TBoldSystemHandle.SetPersistenceHandle(NewHandle: TBoldPersistenceHandle);
begin
  if NewHandle <> fPersistenceHandle then
  begin
    fPersistenceHandleSubscriber.CancelAllSubscriptions;
    if Active then
      PanicShutDownSystem(sPersistenceHandleChangedOnRunningSystem);
    fPersistenceHandle := NewHandle;
    if assigned(fPersistenceHandle) then
      fPersistenceHandle.AddSmallSubscription(fPersistenceHandleSubscriber, [beDestroying], brePersistenceHandleDestroying);
  end;
end;

function TBoldSystemHandle.GetPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
var
  Handle: TBoldPersistenceHandle;
begin
  result := nil;
  Handle := PersistenceHandle;
  repeat
    if Handle is TBoldAbstractPersistenceHandleDB then
      result := TBoldAbstractPersistenceHandleDB(Handle)
    else
    if Handle is TBoldPersistenceHandlePassthrough then
      Handle := TBoldPersistenceHandlePassthrough(Handle).NextPersistenceHandle
    else
      exit;
  until Assigned(result) or not Assigned(Handle);
end;

function TBoldSystemHandle.GetPersistent: Boolean;
begin
  result := assigned(PersistenceHandle);
end;

procedure TBoldSystemHandle.ModelChanged;
//var
//  WasActive: Boolean;
begin
//  WasActive := Active;
  if Active then
    PanicShutDownSystem(sModelChangedOnRunningSystem);

  Active := False;
{  if WasActive then
    Active := True
}
    SendEvent(self, beValueIdentityChanged);
end;

function TBoldSystemHandle.GetValue: TBoldElement;
begin
  Result := fBoldSystem;
end;

procedure TBoldSystemHandle.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TrackBold', ReadTrackBold, nil, False);
end;

procedure TBoldSystemHandle.ReadTrackBold(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TBoldSystemHandle._ReceivePersistenceHandle(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    brePersistenceHandleDestroying: PersistenceHandle := nil;
  end;
end;

procedure TBoldSystemHandle._ReceiveRegionDefinitions(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breRegiondefinitionsDestroying,
    beRegionDefinitionClearing:
    begin
      if Active then
        PanicShutDownSystem(sRegionDefinitionsRemovedFromRunningSystem);
      FreeAndNil(fRegionFactory);
    end;
  end;
end;

constructor TBoldSystemHandle.Create(owner: TComponent);
begin
  inherited;
  fPersistenceHandleSubscriber := TBoldPassthroughSubscriber.Create(_ReceivePersistenceHandle);
  fRegionDefinitionSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveRegionDefinitions);
end;

procedure TBoldSystemHandle.PanicShutDownSystem(Message: String);
var
  DirtyCount: integer;
begin
  if csDestroying in ComponentState then
    exit;
  try
    DirtyCount := system.DirtyObjects.Count;
    System.Discard;
    Active := False;
    if DirtyCount >0 then
      raise EBold.CreateFmt(sPanicShutDown, [Message, BOLDCRLF, system.DirtyObjects.Count]);
  except
    on e: Exception do
     if BoldEffectiveEnvironment.RunningInIDE then
       BoldEffectiveEnvironment.HandleDesigntimeException(Self)
     else
       raise;  
  end;
end;

procedure TBoldSystemHandle.InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
begin
  fOnLookupOclDefinition := Value;
  if assigned(SystemTypeInfoHandle) then
    SystemTypeInfoHandle.InstallOclDefinitionLookUp(Value);
     
  if Active then
    System.Evaluator.SetLookupOclDefinition(value);
end;

function TBoldSystemHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = PersistenceHandle;
end;

procedure TBoldSystemHandle.SetOnOptimisticLockingFailed(
  const Value: TBoldOptimisticLockingFailedEvent);
begin
  fOnOptimisticLockingFailed := Value;
  if Active then
    System.OnOptimisticLockingFailed := Value;
end;

procedure TBoldSystemHandle.SetOnPreUpdate(Value: TNotifyEvent);
begin
  FOnPreUpdate := Value;
  if Active then
    fBoldSystem.OnPreUpdate := Value;
end;

initialization

end.