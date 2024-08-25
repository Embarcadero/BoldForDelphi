
{ Global compiler directives }
{$include bold.inc}
unit BoldHandles;

interface

uses
  Classes,
  BoldRegionDefinitions,
  BoldAbstractModel,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldSystemRT,
  BoldHandle,
  BoldContainers,
  BoldComponentValidator; // maybe move IBoldValidateableComponent here instead ?

type
  { forward declaration of classes }
  TBoldAbstractSystemHandle = class;
  TBoldElementHandle = class;
  TBoldSystemTypeInfoHandle = class;
  TBoldNonSystemHandle = class;
  TBoldSystemExtensionComponent = class;

  TBoldElementHandleClass = class of TBoldElementHandle;
  TBoldNonSystemHandleClass = class of TBoldNonSystemHandle;

  {---TBoldElementHandle---}
  TBoldElementHandle = class(TBoldSubscribableComponent, IBoldValidateableComponent)
  private
    fStrictType: Boolean;
    function GetDynamicBoldType: TBoldElementTypeInfo;
    function GetBoldType: TBoldElementTypeInfo;
    function GetValueAsString: String;
    function GetValueAsVariant: variant;
  protected
    function GetBoldSystem: TBoldSystem; virtual; abstract;
    { IBoldValidateableComponent}
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean; virtual;
    function GetValue: TBoldElement; virtual; abstract;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; virtual; abstract;
    function GetStaticBoldType: TBoldElementTypeInfo; virtual; abstract;
    procedure StaticBoldTypeChanged; virtual;
    function GetCanSetValue: boolean; virtual;
    procedure SetValue(NewValue: TBoldElement); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; virtual;
    property StaticSystemTypeInfo: TBoldSystemTypeInfo read GetStaticSystemTypeInfo;
    property BoldType: TBoldElementTypeInfo read GetBoldType;
    property DynamicBoldType: TBoldElementTypeInfo read GetDynamicBoldType;
    property StaticBoldType: TBoldElementTypeInfo read GetStaticBoldType;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property Value: TBoldElement read GetValue;
    property StrictType: Boolean read fStrictType write fStrictType;
    property AsString: String read GetValueAsString;
    property AsVariant: Variant read GetValueAsVariant;
    property CanSetValue: boolean read GetCanSetValue;
  end;

  { TBoldSystemTypeInfoHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
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
    function GetIsSystemTypeInfoAvailable: boolean;
  protected
    function GetBoldSystem: TBoldSystem; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    function GetValue: TBoldElement; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    property RegionDefinitions: TBoldRegionDefinitions read GetRegionDefinitions;
    property IsSystemTypeInfoAvailable: boolean read GetIsSystemTypeInfoAvailable;
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
    procedure SetIsDefault(Value: Boolean);
    function GetIsDefault: Boolean;
  protected
    function GetBoldSystem: TBoldSystem; override;
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
    class function FindSystemHandleForSystem(ABoldSystem: TBoldSystem): TBoldAbstractSystemHandle;
  published
    property IsDefault: Boolean read GetIsDefault write SetIsDefault nodefault; {Always save}
  end;

  { TBoldNonSystemHandle }
  TBoldNonSystemHandle = class(TBoldElementHandle)
  private
    fStaticSystemHandle: TBoldAbstractSystemHandle;
    fSubscriber: TBoldPassthroughSubscriber;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetSubscriber: TBoldPassthroughSubscriber;
  protected
    function GetBoldSystem: TBoldSystem; override;
    function GetStaticSystemHandle: TBoldAbstractSystemHandle; virtual;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); virtual;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    procedure DoAssign(Source: TPersistent); virtual;
    function IsStaticSystemHandleStored: boolean; virtual;
    property Subscriber: TBoldPassthroughSubscriber read GetSubscriber;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
 published
    property StaticSystemHandle: TBoldAbstractSystemHandle read GetStaticSystemHandle write SetStaticSystemHandle;
  end;

  TBoldSystemExtensionComponent = class(TBoldHandle)
  private
    fStaticSystemHandle: TBoldAbstractSystemHandle;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    function GetBoldSystem: TBoldSystem;
    function GetSubscriber: TBoldExtendedPassthroughSubscriber;
  protected
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    function GetHandledObject: TObject; override;
    function GetStaticSystemHandle: TBoldAbstractSystemHandle; virtual;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); virtual;
    procedure StaticBoldTypeChanged; virtual;
    procedure PlaceSubscriptions; virtual;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property Subscriber: TBoldExtendedPassthroughSubscriber read GetSubscriber;
  public
    destructor Destroy; override;
 published
    property StaticSystemHandle: TBoldAbstractSystemHandle read GetStaticSystemHandle write SetStaticSystemHandle;
  end;

var
  BoldElementHandleList: TBoldObjectArray;

implementation

uses
  SysUtils,
  Variants,

  BoldCoreConsts,
  BoldDefs,
  BoldRegionDefinitionParser;

const
  breModelDestroyed = 42;
  breModelChanged = 43;
  breFreeHandle = 44;
  breValueIdentityChanged = 45;

var
  G_DefaultBoldSystemHandle: TBoldAbstractSystemHandle = nil;
  G_BoldSystemHandleList: TBoldObjectArray;

{---TBoldElementHandle---}

procedure TBoldElementHandle.AfterConstruction;
begin
  inherited;
  if assigned(BoldHandleList) then
    BoldHandleList.Add(self);
end;

procedure TBoldElementHandle.BeforeDestruction;
begin
  inherited;
  if assigned(BoldHandleList) then
    BoldHandleList.Remove(self);
end;

destructor TBoldElementHandle.Destroy;
begin
  FreePublisher;
  inherited Destroy;
end;

function TBoldElementHandle.GetBoldType: TBoldElementTypeInfo;
begin
  Result := DynamicBoldType;
  if not Assigned(Result) then
    Result := StaticBoldType;
end;

function TBoldElementHandle.GetCanSetValue: boolean;
begin
  result := false;
end;

function TBoldElementHandle.GetDynamicBoldType: TBoldElementTypeInfo;
begin
  if Assigned(Value) then
    Result := Value.BoldType
  else
    Result := nil;
end;

function TBoldElementHandle.GetValueAsString: String;
begin
  if Assigned(Value) then
    result := Value.AsString
  else
    result := '';
end;

function TBoldElementHandle.GetValueAsVariant: variant;
begin
  if Assigned(Value) then
    result := Value.AsVariant
  else
    result := null;
end;

function TBoldElementHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := false;
end;

procedure TBoldElementHandle.SetValue(NewValue: TBoldElement);
begin
  raise EBold.CreateFmt('%s: SetValue Not supported', [ClassName]);
end;

procedure TBoldElementHandle.StaticBoldTypeChanged;
begin
  SendEvent(self, beValueIdentityChanged);
end;

function TBoldElementHandle.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  vBoldOCLComponent: IBoldOCLComponent;
begin
  result := Assigned(BoldType);
  if result and Supports(self, IBoldOCLComponent, vBoldOCLComponent) then
    result := ComponentValidator.ValidateOCLComponent(vBoldOCLComponent, NamePrefix+Name);
end;

{ TBoldNonSystemHandle }

procedure TBoldNonSystemHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Assert(RequestedEvent in [breFreeHandle, breValueIdentityChanged], IntToStr(OriginalEvent) + ',' + IntToStr(RequestedEvent));
  case RequestedEvent of
    breFreeHandle: StaticSystemHandle := nil;
    breValueIdentityChanged: StaticBoldTypeChanged;
  end;
end;

procedure TBoldNonSystemHandle.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if (fStaticSystemHandle <> Value) then
  begin
    FreeAndNil(fSubscriber);
    fStaticSystemHandle := Value;
    if Assigned(fStaticSystemHandle) then
    begin
      fStaticSystemHandle.AddSmallSubscription(Subscriber, [beDestroying], breFreeHandle);
      fStaticSystemHandle.AddSmallSubscription(Subscriber, [beValueIDentityChanged], breValueIdentityChanged);
    end;
    StaticBoldTypeChanged;
  end;
end;

function TBoldNonSystemHandle.GetBoldSystem: TBoldSystem;
begin
  if Assigned(StaticSystemHandle) then
    result := StaticSystemHandle.System
  else
  if TBoldAbstractSystemHandle.DefaultBoldSystemHandle <> nil then
    result := TBoldAbstractSystemHandle.DefaultBoldSystemHandle.System
  else
    result := nil;
end;

function TBoldNonSystemHandle.GetStaticSystemHandle: TBoldAbstractSystemHandle;
begin
  result := fStaticSystemHandle;
end;

function TBoldNonSystemHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if Assigned(StaticSystemHandle) then
    Result := StaticSystemHandle.StaticSystemTypeInfo
  else
    Result := TBoldAbstractSystemHandle.DefaultBoldSystemTypeInfo
end;

function TBoldNonSystemHandle.GetSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  result := fSubscriber;
end;

function TBoldNonSystemHandle.IsStaticSystemHandleStored: boolean;
begin
  result := true;
end;

procedure TBoldNonSystemHandle.Assign(Source: TPersistent);
begin
  if Source is TBoldNonSystemHandle then
    DoAssign(Source)
  else
    inherited Assign(Source);
end;

procedure TBoldNonSystemHandle.DoAssign(Source: TPersistent);
begin
  if (Source is TBoldNonSystemHandle) then
  begin
    StaticSystemHandle := TBoldNonSystemHandle(Source).StaticSystemHandle;
  end
end;

destructor TBoldNonSystemHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fSubscriber);
  inherited;
end;

{ TBoldAbstractSystemHandle }

constructor TBoldAbstractSystemHandle.create(owner: TComponent);
begin
  inherited;
  fSystemTypeInfoHandleSubscriber := TBoldPassThroughSubscriber.Create(_Recieve);
  if not Assigned(G_DefaultBoldSystemHandle) and (csdesigning in ComponentState) then  {only make first default at design time}
    IsDefault := True;
  G_BoldSystemHandleList.Add(self);
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
  G_BoldSystemHandleList.Remove(self);
  if G_DefaultBoldSystemHandle = self then
    G_DefaultBoldSystemHandle := nil;
  IsDefault := False;
  FreeAndNil(fSystemTypeInfoHandleSubscriber);
  inherited;
end;

class function TBoldAbstractSystemHandle.FindSystemHandleForSystem(ABoldSystem: TBoldSystem): TBoldAbstractSystemHandle;
var
  i: integer;
begin
  for I := 0 to G_BoldSystemHandleList.Count - 1 do
  begin
    result := TBoldAbstractSystemHandle(G_BoldSystemHandleList[i]);
    if ABoldSystem = result.System then
      exit;
  end;
  result := nil;
end;

function TBoldAbstractSystemHandle.GetBoldSystem: TBoldSystem;
begin
  result := self.System;
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

function TBoldAbstractSystemHandle.GetIsDefault: Boolean;
begin
  if (csdesigning in ComponentState) then
    result := fIsDefault
  else
    result :=  G_DefaultBoldSystemHandle = Self;
end;

procedure TBoldAbstractSystemHandle.SetIsDefault(Value: Boolean);
begin
  fIsDefault := Value;
  if Value then
    G_DefaultBoldSystemHandle := Self
  else
  if G_DefaultBoldSystemHandle = Self then
    G_DefaultBoldSystemHandle := nil;
  if Active then
    System.IsDefault := G_DefaultBoldSystemHandle = Self;
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
        UseGeneratedCode {and not (csDesigning in ComponentState)},
        CheckCodeCheckSum,
        BoldModel.TypeNameDictionary);
      fSystemTypeInfo.Evaluator.SetLookupOclDefinition(fOnLookupOclDefinition);
      SendEvent(Self, beValueIdentityChanged);
    end;
  end;
  result := fSystemTypeInfo;
end;

procedure TBoldSystemTypeInfoHandle.ModelChanged;
var
  WasActive: boolean;
begin
  WasActive := Assigned(fSystemTypeInfo);
  if WasActive then
    SendEvent(self, beValueIdentityChanged);
  FreeAndNil(fSystemTypeInfo);
  FreeAndNil(fRegionDefinitions);
  if not WasActive then
    SendEvent(self, beValueIdentityChanged);
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

destructor TBoldSystemTypeInfoHandle.destroy;
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

function TBoldSystemTypeInfoHandle.GetBoldSystem: TBoldSystem;
begin
  result := nil;
end;

function TBoldSystemTypeInfoHandle.GetIsSystemTypeInfoAvailable: boolean;
begin
  result := Assigned(fSystemTypeInfo);
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

{ TBoldSystemExtensionComponent }

destructor TBoldSystemExtensionComponent.Destroy;
begin
  FreeAndNil(fSubscriber);
  inherited;
end;

function TBoldSystemExtensionComponent.GetBoldSystem: TBoldSystem;
begin
  if Assigned(fStaticSystemHandle) then
    result := fStaticSystemHandle.System
  else
    result := nil;
end;

function TBoldSystemExtensionComponent.GetHandledObject: TObject;
begin
  result := GetStaticSystemHandle;
end;

function TBoldSystemExtensionComponent.GetStaticSystemHandle: TBoldAbstractSystemHandle;
begin
  result := fStaticSystemHandle;
end;

function TBoldSystemExtensionComponent.GetSubscriber: TBoldExtendedPassthroughSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(_Receive);
  result := fSubscriber;
end;

procedure TBoldSystemExtensionComponent.PlaceSubscriptions;
begin
  FreeAndNil(fSubscriber);
  if Assigned(fStaticSystemHandle) then
  begin
    fStaticSystemHandle.AddSmallSubscription(Subscriber, [beDestroying], breFreeHandle);
    fStaticSystemHandle.AddSmallSubscription(Subscriber, [beValueIDentityChanged], breValueIdentityChanged);
  end;
end;

procedure TBoldSystemExtensionComponent.SetStaticSystemHandle(
  Value: TBoldAbstractSystemHandle);
begin
  if (fStaticSystemHandle <> Value) then
  begin
    fStaticSystemHandle := Value;
    StaticBoldTypeChanged;
  end;
end;

procedure TBoldSystemExtensionComponent.StaticBoldTypeChanged;
begin
  PlaceSubscriptions;
end;

procedure TBoldSystemExtensionComponent._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  Assert(RequestedEvent in [breFreeHandle, breValueIdentityChanged], IntToStr(OriginalEvent) + ',' + IntToStr(RequestedEvent));
  case RequestedEvent of
    breFreeHandle: StaticSystemHandle := nil;
    breValueIdentityChanged: StaticBoldTypeChanged;
  end;
end;

initialization
  G_BoldSystemHandleList := TBoldObjectArray.Create(128, []);

finalization
  Assert(G_BoldSystemHandleList.Count=0, Format('There are %d unreleased handles in finalization', [G_BoldSystemHandleList.Count]));
  FreeAndNil(G_BoldSystemHandleList);

end.
