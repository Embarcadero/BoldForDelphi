unit BoldHandles;

interface

uses
  Classes,
  BoldRegionDefinitions,
  BoldAbstractModel,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldSystemRT;

type
  { forward declaration of classes }
  TBoldAbstractSystemHandle = class;
  TBoldElementHandle = class;
  TBoldSystemTypeInfoHandle = class;
  TBoldNonSystemHandle = class;

  {---TBoldElementHandle---}
  TBoldElementHandle = class(TBoldSubscribableComponent)
  private
    fStrictType: Boolean;
    function GetDynamicBoldType: TBoldElementTypeInfo;
    function GetBoldType: TBoldElementTypeInfo;
  protected
    function GetValue: TBoldElement; virtual; abstract;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; virtual; abstract;
    function GetStaticBoldType: TBoldElementTypeInfo; virtual; abstract;
    procedure StaticBoldTypeChanged; virtual;
  public
    destructor Destroy; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; virtual;
    property StaticSystemTypeInfo: TBoldSystemTypeInfo read GetStaticSystemTypeInfo;
    property BoldType: TBoldElementTypeInfo read GetBoldType;
    property DynamicBoldType: TBoldElementTypeInfo read GetDynamicBoldType;
    property StaticBoldType: TBoldElementTypeInfo read GetStaticBoldType;
    property Value: TBoldElement read GetValue;
    property StrictType: Boolean read fStrictType write fStrictType;
  end;

  { TBoldSystemTypeInfoHandle }
  TBoldSystemTypeInfoHandle = class(TBoldElementHandle)
  private
    fBoldModel: TBoldAbstractModel;
    fModelSubscriber: TBoldPassThroughSubscriber;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fUseGeneratedCode: Boolean;
    fRegionDefinitions: TBoldRegionDefinitions;
    fOnLookupOclDefinition: TBoldLookUpOclDefinition;
    fCheckCodeCheckSum: Boolean;
    procedure _Recieve(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetBoldModel(Value: TBoldAbstractModel);
    procedure ModelChanged;
    function GetRegionDefinitions: TBoldRegionDefinitions;
  protected
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    function GetValue: TBoldElement; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    property RegionDefinitions: TBoldRegionDefinitions read GetRegionDefinitions;
  published
    property BoldModel: TBoldAbstractModel read fBoldModel write SetBoldModel;
    property UseGeneratedCode: Boolean read fUseGeneratedCode write fUseGeneratedCode default True;
    property CheckCodeCheckSum: Boolean read fCheckCodeCheckSum write fCheckCodeCheckSum default True;
  end;

  { TBoldAbstractSystemHandle }
  TBoldAbstractSystemHandle = class(TBoldElementHandle)
  private
    fIsDefault: Boolean;
    fSystemTypeInfoHandleSubscriber: TBoldPassThroughSubscriber;
    fSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    procedure SetSystemTypeInfoHandle(Value: TBoldSystemTypeInfoHandle);
    procedure _Recieve(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure setIsDefault(Value: Boolean);
    function GetIsDefault: Boolean;
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(Value: Boolean); virtual; abstract;
    function GetSystem: TBoldSystem; virtual; abstract;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure ModelChanged; virtual; abstract;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    property System: TBoldSystem read GetSystem;
    property Active: Boolean read GetActive write SetActive;
    property SystemTypeInfoHandle: TBoldSystemTypeInfoHandle read fSystemTypeInfoHandle write SetSystemTypeInfoHandle;
    class function DefaultBoldSystemTypeInfo: TBoldSystemTypeInfo;
    class function DefaultBoldSystemHandle: TBoldAbstractSystemHandle;
  published
    property IsDefault: Boolean read GetIsDefault write setIsDefault nodefault; {Always save}
  end;

  { TBoldNonSystemHandle }
  TBoldNonSystemHandle = class(TBoldElementHandle)
  private
    fStaticSystemHandle: TBoldAbstractSystemHandle;
    fStaticSystemHandleSubscriber: TBoldPassthroughSubscriber;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); virtual;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
 published
    property StaticSystemHandle: TBoldAbstractSystemHandle read fStaticSystemHandle write SetStaticSystemHandle;
  end;

implementation

uses
  SysUtils,
  HandlesConst,
  BoldDefs,
  BoldregionDefinitionParser;

const
  breModelDestroyed = 42;
  breModelChanged = 43;
  breFreeHandle = 44;
  breValueIdentityChanged = 45;

var
  G_DefaultBoldSystemHandle: TBoldAbstractSystemHandle = nil;

{---TBoldElementHandle---}

destructor TBoldElementHandle.Destroy;
begin
  FreePublisher;
  inherited Destroy;
end;

function TBoldElementHandle.GetBoldType: TBoldElementTypeInfo;
begin
  Result := DynamicBoldType;
  if not Assigned(DynamicBoldType) then
    Result := StaticBoldType;
end;

function TBoldElementHandle.GetDynamicBoldType: TBoldElementTypeInfo;
begin
  if Assigned(Value) then
    Result := Value.BoldType
  else
    Result := nil;
end;

function TBoldElementHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := false;
end;

procedure TBoldElementHandle.StaticBoldTypeChanged;
begin
  SendEvent(self, beValueIdentityChanged);
end;

{ TBoldNonSystemHandle }

procedure TBoldNonSystemHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Assert(Originator=StaticSystemHandle);
  Assert(RequestedEvent in [breFreeHandle, breValueIdentityChanged]);
  case RequestedEvent of
    breFreeHandle: StaticSystemHandle := nil;
    breValueIdentityChanged: StaticBoldTypeChanged;
  end;
end;

procedure TBoldNonSystemHandle.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if (fStaticSystemHandle <> Value) then
  begin
    fStaticSystemHandleSubscriber.CancelAllSubscriptions;
    fStaticSystemHandle := Value;
    if Assigned(StaticSystemHandle) then
    begin
      StaticSystemHandle.AddSmallSubscription(fStaticSystemHandleSubscriber, [beDestroying], breFreeHandle);
      StaticSystemHandle.AddSmallSubscription(fStaticSystemHandleSubscriber, [beValueIDentityChanged], breValueIdentityChanged);
    end;
    StaticBoldTypeChanged;
  end;
end;

function TBoldNonSystemHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if Assigned(StaticSystemHandle) then
    Result := StaticSystemHandle.StaticSystemTypeInfo
  else if TBoldAbstractSystemHandle.DefaultBoldSystemHandle <> nil then
    Result := TBoldAbstractSystemHandle.DefaultBoldSystemTypeInfo
  else
    Result := nil;
end;

constructor TBoldNonSystemHandle.Create(Owner: TComponent);
begin
  inherited;
  fStaticSystemHandleSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

destructor TBoldNonSystemHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fStaticSystemHandleSubscriber);
  inherited;
end;

{ TBoldAbstractSystemHandle }

constructor TBoldAbstractSystemHandle.create(owner: TComponent);
begin
  inherited;
  fSystemTypeInfoHandleSubscriber := TBoldPassThroughSubscriber.Create(_Recieve);
  if not Assigned(G_DefaultBoldSystemHandle) and (csdesigning in ComponentState) then  {only make first default at design time}
    IsDefault := True;
end;

class function TBoldAbstractSystemHandle.DefaultBoldSystemHandle: TBoldAbstractSystemHandle;
begin
  Result := G_DefaultBoldSystemHandle;
end;

class function TBoldAbstractSystemHandle.DefaultBoldSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if DefaultBoldSystemHandle <> nil then
    Result := DefaultBoldSystemHandle.StaticSystemTypeInfo
  else
    Result := nil;
end;

destructor TBoldAbstractSystemHandle.Destroy;
begin
  FreePublisher;
  if G_DefaultBoldSystemHandle = self then
    G_DefaultBoldSystemHandle := nil;
  IsDefault := False;
  FreeAndNil(fSystemTypeInfoHandleSubscriber);
  inherited;
end;

function TBoldAbstractSystemHandle.GetIsDefault: Boolean;
begin
  result := fIsDefault;
end;

function TBoldAbstractSystemHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  result := StaticSystemTypeInfo;
end;

function TBoldAbstractSystemHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(SystemTypeInfoHandle) then
    result := SystemTypeInfoHandle.StaticSystemTypeInfo
  else
    result := nil;
end;

function TBoldAbstractSystemHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = SystemTypeInfoHandle;
end;

procedure TBoldAbstractSystemHandle.setIsDefault(Value: Boolean);
begin
  if (Value <> IsDefault) then
  begin
    fIsDefault := Value;
    if Value then
      G_DefaultBoldSystemHandle := Self
    else if Isdefault then
      G_DefaultBoldSystemHandle := nil;
    if Active then
      System.IsDefault := Value;
  end;
end;

procedure TBoldAbstractSystemHandle.SetSystemTypeInfoHandle(Value: TBoldSystemTypeInfoHandle);
begin
  if Value <> fSystemTypeInfoHandle then
  begin
    if Active then
      raise EBold.CreateFmt(sNotAllowedOnActiveHandle, [Name]);
    fSystemTypeInfoHandleSubscriber.CancelAllSubscriptions;
    if Assigned(Value) then
    begin
      Value.AddSmallSubscription(fSystemTypeInfoHandleSubscriber, [beDestroying], breModelDestroyed);
      Value.AddSmallSubscription(fSystemTypeInfoHandleSubscriber, [beValueIdentityChanged], breValueIdentityChanged);
    end;
    fSystemTypeInfoHandle := Value;
    ModelChanged;
  end;
end;

procedure TBoldAbstractSystemHandle._Recieve(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breModelDestroyed:
    begin
      Active := false;
      SystemTypeInfoHandle:= nil;
    end;
    breValueIdentityChanged: ModelChanged;
  end;
end;

{ TBoldSystemTypeInfoHandle }

procedure TBoldSystemTypeInfoHandle._Recieve(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breModelChanged: ModelChanged;
    breModelDestroyed:
    begin
      fBoldModel := nil;
      fModelSubscriber.CancelAllSubscriptions;
    end;
  end;
end;

function TBoldSystemTypeInfoHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  result := GetStaticSystemTypeInfo;
end;

function TBoldSystemTypeInfoHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if not assigned(fSystemTypeInfo) then
  begin
    if Assigned(BoldModel) and assigned(BoldModel.MoldModel) then
    begin
      fSystemTypeInfo := TBoldSystemTypeInfo.Create(
        BoldModel.MoldModel,
        UseGeneratedCode and not (csDesigning in ComponentState),
        CheckCodeCheckSum,
        BoldModel.TypeNameDictionary);
      fSystemTypeInfo.Evaluator.SetLookupOclDefinition(fOnLookupOclDefinition);
      SendEvent(Self, beValueIdentityChanged);
    end;
  end;
  result := fSystemTypeInfo;
end;

procedure TBoldSystemTypeInfoHandle.ModelChanged;
begin
  FreeAndNil(fSystemTypeInfo);
  FreeAndNil(fRegionDefinitions);
  SendEvent(self, beValueIdentityChanged);  // type change regarded as idenitychange
end;

procedure TBoldSystemTypeInfoHandle.SetBoldModel(Value: TBoldAbstractModel);
begin
  if Value <> fBoldModel then
  begin
    fModelSubscriber.CancelAllSubscriptions;
    if Assigned(Value) then
    begin
      Value.AddSmallSubscription(fModelSubscriber, [beDestroying], breModelDestroyed);
      Value.AddSmallSubscription(fModelSubscriber, [beModelChanged], breModelChanged);
    end;
    FBoldModel := Value;
    ModelChanged;
  end;
end;

constructor TBoldSystemTypeInfoHandle.Create(owner: TComponent);
begin
  inherited;
  fUseGeneratedCode := True;
  CheckCodeCheckSum := True;
  fModelSubscriber := TBoldPassThroughSubscriber.Create(_Recieve);
end;

destructor TBoldSystemTypeInfoHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fModelSubscriber);
  FreeAndNil(fSystemTypeInfo);
  FreeAndNil(fRegiondefinitions);
  inherited;
end;

function TBoldSystemTypeInfoHandle.GetValue: TBoldElement;
begin
  result := StaticSystemTypeInfo;
end;

procedure TBoldSystemTypeInfoHandle.InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
begin
  fOnLookupOclDefinition := Value;
  if assigned(fSystemTypeInfo) then
    fSystemTypeInfo.Evaluator.SetLookupOclDefinition(fOnLookupOclDefinition);
end;

function TBoldSystemTypeInfoHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = BoldModel;
end;

function TBoldSystemTypeInfoHandle.GetRegionDefinitions: TBoldRegionDefinitions;
var
  Parser: TBoldRegionParser;
  definitions: TStringList;
begin
  if not assigned(fRegionDefinitions) and assigned(BoldModel) then
  begin
    fRegionDefinitions := TBoldRegionDefinitions.Create;
    Parser := TBoldRegionParser.Create(fRegionDefinitions, StaticSystemTypeInfo);
    Definitions := TStringList.Create;
    try
      definitions.Text := BoldModel.MoldModel.RegionDefinitions;
      if not Parser.Parse(Definitions) then
        raise EBoldBadLockExpression.Create(Parser.Errors.Text);
    finally
      Definitions.free;
      Parser.Free;
    end;
  end;
  result := fRegionDefinitions;
end;

end.
