
{ Global compiler directives }
{$include bold.inc}
unit BoldControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  SysUtils,
  Controls,
  {$IFDEF BOLD_DELPH6_OR_LATER}
  Types,
  {$ENDIF}
{$IFNDEF BOLDCOMCLIENT}
  BoldSystemRT,
  BoldSystem,
{$ENDIF}
  Menus,
  Graphics,
  Windows,
  BoldDefs,
  BoldControlPackDefs,
  BoldBase,
  BoldContainers,
  BoldSubscription,
  BoldElements,
  BoldQueue,
  BoldHandles,
  BoldOclVariables;

const
  befFollowerResultElementOutOfDate = BoldElementFlag4;
  befFollowerEnsured                = BoldElementFlag5;

type
  {Forward declarations of all classes}
  TBoldRendererData = class;
  TBoldRenderer = class;
  TBoldFollower = class;
  TBoldFollowerSubscriber = class;
  TBoldFollowerController = class;
  TBoldSingleFollowerController = class;
  TBoldSingleRenderer = class;
  TBoldPopup = class;
  TBoldAbstractHandleFollower = class;

  TBoldRendererDataClass = class of TBoldRendererData;
  TBoldFollowerControllerClass = class of TBoldFollowerController;

  TBoldFollowerEvent = procedure (Follower: TBoldFollower) of object;
  TBoldGetContextTypeEvent = function: TBoldElementTypeInfo of object;
  TBoldSubFollowerEvent = procedure (index: Integer; OwningFollower: TBoldFollower) of object;
  TBoldValidateString = function (aFollower: TBoldFollower; const Value: string): Boolean of object;
  TBoldApplyExceptionEvent = function (E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean of object;
  TBoldDisplayExceptionEvent = function(E: Exception; Elem: TBoldElement): Boolean of object;

  TBoldFollowerArray = array of TBoldFollower;

  { TBoldAbstractHandleFollower }
  TBoldAbstractHandleFollower = class(TBoldQueueable)
  private
    fFollower: TBoldFollower;
    fSubscriber: TBoldSubscriber;
  protected
    function GetBoldHandle: TBoldElementHandle; virtual; abstract;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual; abstract;
    property Subscriber: TBoldSubscriber read fSubscriber;    
  public
    constructor Create(AMatchObject: TObject; Controller: TBoldFollowerController);
    destructor Destroy; override;
    property BoldHandle: TBoldElementHandle read GetBoldHandle;
    property Follower: TBoldFollower read fFollower;
  end;

  { TBoldFollowerSubscriber }
  TBoldFollowerSubscriber = class(TBoldSubscriber)
  private
    fFollower: TBoldFollower;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    function GetContextString: string; override;
    function ContextObject: TObject; override;
  public
    constructor Create(Follower: TBoldFollower);
    destructor Destroy; override;
    property Follower: TBoldFollower read fFollower;
  end;

  { TBoldRendererData }
  TBoldRendererData = class(TBoldMemoryManagedObject)
  private
    fOwningFollower: TBoldFollower;
  protected
    procedure EnsureSubfollowersDisplayable; virtual;
    function GetSubFollowerCount: Integer; virtual;
    function GetSubFollower(index: Integer): TBoldFollower; virtual;
    function GetEnsuredSubFollower(Index: Integer): TBoldFollower; virtual;
    function GetCurrentSubFollowerIndex: Integer; virtual;
    procedure SetCurrentSubFollowerIndex(index:integer); virtual;
    function GetSubFollowerAssigned(Index: Integer): boolean; virtual;
  public
    constructor Create(OwningFollower: TBoldFollower); virtual;
    property OwningFollower: TBoldFollower read fOwningFollower;
  end;

  { TBoldRenderer }
  TBoldMayModify = function(aFollower: TBoldFollower): Boolean of object;
  TBoldHoldsChangedValue = procedure(aFollower: TBoldFollower) of object;
  TBoldReleaseChangedValue = procedure(aFollower: TBoldFollower) of object;
  TBoldSubscribe = procedure(aFollower: TBoldFollower; Subscriber: TBoldSubscriber) of object;
  TBoldEnsureFetched = procedure (List: TBoldObjectList; Expression: TBoldExpression) of object;
  TBoldStartDrag = procedure (Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData) of object;
  TBoldEndDrag = procedure (DragMode: TBoldDragMode; InternalDrag: Boolean) of object;
  TBoldDragOver = function (Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean of object;
  TBoldDragDrop = procedure (Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer) of object;
  TBoldValidateCharacter = function (AFollower: TBoldFollower; C: Char): Boolean of object;

  TBoldRenderer = class(TBoldSubscribableComponentViaBoldElem)
  private
    FOnStartDrag: TBoldStartDrag;
    FOnEndDrag: TBoldEndDrag;
    FOnDragOver: TBoldDragOver;
    FOnDragDrop: TBoldDragDrop;
    FOnMayModify: TBoldMayModify;
    FOnHoldsChangedValue: TBoldHoldsChangedValue;
    FOnReleaseChangedValue: TBoldReleaseChangedValue;
    FOnSubscribe: TBoldSubscribe;
    FOnEnsureFetched: TBoldEnsureFetched;
    FRepresentations: TStringList;
    FOnValidateCharacter: TBoldValidateCharacter;
    function GetRepresentations: TStrings;
    procedure SetRepresentations(Value: TStrings);
    function StoreRepresentations: Boolean;
    procedure SetOnSubscribe(Value: TBoldSubscribe);
  protected
    class function GetExpressionAsDirectElement(Element: TBoldElement; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): TBoldElement;
    function GetRendererDataClass: TBoldRendererDataClass; virtual;
    function GetSupportsMulti: Boolean; virtual;
    procedure DefaultHoldsChangedValue(aFollower: TBoldFollower); virtual;
    procedure DefaultReleaseChangedValue(aFollower: TBoldFollower); virtual;
    function DefaultMayModify(aFollower: TBoldFollower): Boolean; virtual;
    function DefaultValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean; virtual;
    procedure DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData); virtual;
    procedure DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean; virtual;
    procedure DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Aligment: TAlignment; Margins: TPoint); virtual;
    function HasSetValueEventOverrides: boolean; virtual;
    function ValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean; virtual;
    function GetDefaultRepresentationStringList: TStringList; virtual;
    property OnStartDrag: TBoldStartDrag read FOnStartDrag write FOnStartDrag;
    property OnEndDrag: TBoldEndDrag read FOnEndDrag write FOnEndDrag;
    property OnDragOver: TBoldDragOver read FOnDragOver write FOnDragOver;
    property OnDragDrop: TBoldDragDrop read FOnDragDrop write FOnDragDrop;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed;
    function MayModify(aFollower: TBoldFollower): Boolean; virtual;
    procedure HoldsChangedValue(Follower: TBoldFollower); virtual;
    procedure ReleaseChangedValue(Follower: TBoldFollower); virtual;
    procedure StartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData); virtual;
    procedure EndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure SubscribeToElement(aFollower: TBoldFollower);
    procedure EnsureFetched(List: TBoldObjectList; BoldType: TBoldClassTypeInfo; Expression: TBoldExpression);
    property RendererDataClass: TBoldRendererDataClass read GetRendererDataClass;
    property SupportsMulti: Boolean read GetSupportsMulti;
  published
    property Representations: TStrings read GetRepresentations write SetRepresentations stored StoreRepresentations;
    property OnMayModify: TBoldMayModify read FOnMayModify write FOnMayModify;
    property OnHoldsChangedValue: TBoldHoldsChangedValue read FOnHoldsChangedValue write FOnHoldsChangedValue;
    property OnReleaseChangedValue: TBoldReleaseChangedValue read FOnReleaseChangedValue write FOnReleaseChangedValue;
    property OnSubscribe: TBoldSubscribe read FOnSubscribe write SetOnSubscribe;
    property OnEnsureFetched: TBoldEnsureFetched read FOnEnsureFetched write FOnEnsureFetched;
    property OnValidateCharacter: TBoldValidateCharacter read FOnValidateCharacter write FOnValidateCharacter;
  end;

  { TBoldFollower }
  TBoldFollower = class(TBoldQueueable)
  private
    fIndex: Integer;
    fOwningFollower: TBoldFollower;
    fState: TBoldFollowerState;
    fElement: TBoldElement;
    fRendererData: TBoldRendererData;
    fController: TBoldFollowerController;
    fControlData: TObject;
    FSubscriber: TBoldSubscriber;
    FIndirectElement: TBoldIndirectElement;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetElementValid: Boolean;
    procedure SetElementValid(Value: Boolean);
    function GetSubFollower(index: Integer): TBoldFollower;
    function GetEnsuredSubFollower(Index: Integer): TBoldFollower;
    function GetSubFollowerCount: Integer;
    function GetCurrentIndex: Integer;
    procedure SetCurrentIndex(index: integer);
    procedure SetElement(AElement: TBoldElement);
    procedure SetState(AValue: TBoldFollowerState);
    function GetRendererData: TBoldRendererData;
    function CollectMatchingDownwards(Followers: TBoldFollowerArray; MatchController: TBoldFollowerController): TBoldFollowerArray;
    function GetAssertedController: TBoldFollowerController;
    function GetCurrentSubFollower: TBoldFollower;
    function GetIsDirty: Boolean;
    function GetValue: TBoldElement;
    function GetSubFollowerAssigned(index: Integer): boolean;
  protected
    function GetDebugInfo: string; override;
    procedure AddToDisplayList; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure MakeUptodateAndSubscribe;
    class procedure MultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray);
    procedure MakeClean;
    {State handling}
    procedure MarkDirty;
    procedure MarkClean;
    procedure MarkEnsured;
    function CollectMatching(MatchController: TBoldFollowerController): TBoldFollowerArray;
    function CollectMatchingSiblings(WindowSize: Integer): TBoldFollowerArray;
    property Ensured: Boolean index befFollowerEnsured read GetElementFlag write SetElementFlag;
  public
    constructor Create(MatchObject: TObject; Controller: TBoldFollowerController);
    constructor CreateSubFollower(
      OwningFollower: TBoldFollower;
      aController: TBoldFollowerController;
      aElement: TBoldElement;
      aActive: boolean;
      aIndex: integer);
    destructor Destroy; override;
    procedure SetElementAndMakeCurrent(AElement: TBoldElement; AActive: boolean);
    procedure Display; override;
    procedure Apply; override;
    procedure MarkValueOutOfDate;
    procedure MarkSubscriptionOutOfDate;
    function CheckIfInHierarchy(aElement: TBoldElement; aController: TBoldFollowerController): Boolean;
    procedure ControlledValueChanged;
    procedure DiscardChange; override;
    function Displayable: Boolean;
    procedure EnsureDisplayable;
    function ExistInOwner: Boolean;
    function MayChange: Boolean;
    function MayModify: Boolean;
    procedure EnsureMulti;
    procedure EnsureSiblings;
    property Active: Boolean read GetActive write SetActive;
    property ElementValid: Boolean read GetElementValid write SetElementValid;
    property Controller: TBoldFollowerController read fController;
    property AssertedController: TBoldFollowerController read GetAssertedController;
    property ControlData: TObject read FControlData write FControlData;
    property CurrentIndex: Integer read GetCurrentIndex write SetCurrentIndex;
    property Element: TBoldElement read FElement write SetElement;
    property index: Integer read fIndex write fIndex;
    property OwningFollower: TBoldFollower read FOwningFollower;
    property RendererData: TBoldRendererData read GetRendererData;
    property State: TBoldFollowerState read fState;
    property IsDirty: boolean read GetIsDirty;
    property Selected: Boolean index befFollowerSelected read GetElementFlag write SetElementFlag;
    property ResultElementOutOfDate: Boolean index befFollowerResultElementOutOfDate read GetElementFlag write SetElementFlag;
    property SubFollowerCount: Integer read GetSubFollowerCount;
    property SubFollowers[index: Integer]: TBoldFollower read GetSubFollower;
    property SubFollowerAssigned[index: Integer]: boolean read GetSubFollowerAssigned;
    property CurrentSubFollower : TBoldFollower read GetCurrentSubFollower;
    property EnsuredSubFollowers[Index: Integer]: TBoldFollower read GetEnsuredSubFollower;
    property Subscriber: TBoldSubscriber read fSubscriber;
    property Value: TBoldElement read GetValue;
  end;

  { TBoldFollowerController }
  TBoldFollowerController = class(TBoldSubscribablePersistent)
  private
    fUntypedRenderer: TBoldRenderer;
    fInternalDrag: Boolean;
    fOwningComponent: TComponent;
    fApplyPolicy: TBoldApplyPolicy;
    fDragMode: TBoldDragMode;
    fDropMode: TBoldDropMode;
    fCleanOnEqual: Boolean;
    fPopup: TBoldPopup;
    fRepresentation: TBoldRepresentation;
    fExpression: TBoldExpression;
    fAfterMakeUptoDate: TBoldFollowerEvent;
    fBeforeMakeUptoDate: TBoldFollowerEvent;
    fComponentSubscriber: TBoldPassthroughSubscriber;
    FOnGetContextType: TBoldGetContextTypeEvent;
    fVariables: TBoldOclVariables;
    fApplyException: TBoldApplyExceptionEvent;
    fDisplayException: TBoldDisplayExceptionEvent;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetRendererDataClass: TBoldRendererDataClass;
    procedure SetRepresentation(Value: TBoldRepresentation);
    procedure SetExpression(const Value: string);
    procedure SetUntypedRenderer(NewRender: TBoldRenderer);
    procedure SetVariables(const Value: TBoldOclVariables);
    procedure Resubscribe;
    function GetSupportsMulti: Boolean;
    function HandleApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
    function HandleDisplayException(E: Exception; Elem: TBoldElement): Boolean;
  protected
    function GetOwner: TPersistent; override;
    function GetEffectiveRenderer: TBoldRenderer; virtual;
    function GetContextType: TBoldElementTypeInfo; virtual;
    function GetVariableList: TBoldExternalVariableList; virtual;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); virtual;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray); virtual;
    procedure Changed;
    procedure DoAssign(Source: TPersistent); virtual;
    function DoApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
    function DoDisplayException(E: Exception; Elem: TBoldElement): Boolean;
    procedure CleanRendererData(RendererData: TBoldRendererData); virtual;
    procedure MultiMakeEnsure(Followers: TBoldFollowerArray);
    function GetSupportsMultiEnsure: Boolean; virtual;
    property EffectiveRenderer: TBoldRenderer read GetEffectiveRenderer;
    property RendererDataClass: TBoldRendererDataClass read GetRendererDataClass;
    property InternalDrag: Boolean read FInternalDrag write FInternalDrag;
    property UntypedRenderer: TBoldRenderer read fUntypedRenderer write SetUntypedRenderer;
    property Variables: TBoldOclVariables read fVariables write SetVariables;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MakeClean(Follower: TBoldFollower); virtual;
    function MayModify(Follower: TBoldFollower): Boolean;
    procedure HoldsChangedValue(Follower: TBoldFollower);
    procedure ReleaseChangedValue(Follower: TBoldFollower);
    function GetVariableListAndSubscribe(Subscriber: TBoldSubscriber): TBoldExternalVariableList;
    procedure StartDrag(Follower: TBoldFollower);
    procedure EndDrag;
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscribe: Boolean);
    procedure SubscribeToElement(aFollower: TBoldFollower);
    procedure MultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray);
    function DragOver(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); virtual;
    function SubFollowersActive: boolean; virtual;
    property OwningComponent: TComponent read FOwningComponent;    
    property ApplyPolicy: TBoldApplyPolicy read FApplyPolicy write FApplyPolicy default bapExit;
    property CleanOnEqual: Boolean read FCleanOnEqual write FCleanOnEqual default true;
    property Popup: TBoldPopup read FPopup write FPopup; {temporarily}
    property AfterMakeUptoDate: TBoldFollowerEvent read fAfterMakeUptoDate write fAfterMakeUptoDate;
    property BeforeMakeUptoDate: TBoldFollowerEvent read fBeforeMakeUptoDate write fBeforeMakeUptoDate;
    property OnGetContextType: TBoldGetContextTypeEvent read fOnGetContextType write fOnGetContextType;
    property ContextType: TBoldElementTypeInfo read GetContextType;
    property VariableList: TBoldExternalVariableList read GetVariableList;
    property SupportsMulti: Boolean read GetSupportsMulti;
    property SupportsMultiEnsure : Boolean read GetSupportsMultiEnsure;
    property Expression: TBoldExpression read FExpression write SetExpression nodefault;
    property Representation: TBoldRepresentation read FRepresentation write SetRepresentation default brDefault;
  published
    property DragMode: TBoldDragMode read FDragMode write FDragMode default DefaultBoldDragMode;
    property DropMode: TBoldDropMode read FDropMode write FDropMode default DefaultBoldDropMode;
    property OnApplyException: TBoldApplyExceptionEvent read fApplyException write fApplyException;
    property OnDisplayException: TBoldDisplayExceptionEvent read fDisplayException write fDisplayException;
  end;

  { TBoldSingleRenderer }
  TBoldSingleRenderer = class(TBoldRenderer)
  public
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); virtual; abstract;
  end;

  { TBoldSingleFollowerController }
  TBoldSingleFollowerController = class(TBoldFollowerController)
  public
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
  {$IFNDEF T2H}
    property RendererDataClass;
  published
    property Representation;
    property Expression;
    property Variables;
    property ApplyPolicy;
    property CleanOnEqual;
    {$ENDIF}
  end;

  { TBoldPopup }
  TBoldPopup = class(TPersistent)
  private
    FEnable: Boolean;
    FInsertNew: Boolean;
    FDelete: TBoldPopupDeleteType;
    FMove: Boolean;
  public
    function GetMenu(CONTROL: TControl; Element: TBoldElement): TPopupMenu;
    procedure Assign(Source: TPersistent); override;
  published
    property Enable: Boolean read FEnable write FEnable default False;
    property InsertNew: Boolean read FInsertNew write FInsertNew default False;
    property Delete: TBoldPopupDeleteType read FDelete write FDelete default bpdNone;
    property Move: Boolean read FMove write FMove default False;
  end;

{$IFDEF BOLDCOMCLIENT}
type
  TBoldClientableList = TBoldInterfaceArray;

function BoldTestType(element: IUnknown; const TypeOrInterface: TGUID): Boolean;
{$ELSE}
function BoldTestType(element: TObject; TypeOrInterface: TClass): Boolean;

type
  TBoldClientableList = TBoldObjectArray;
{$ENDIF}

implementation

uses
  BoldCoreConsts,
  BoldExceptionHandlers,
  BoldGuiResourceStrings,
{$IFNDEF BOLDCOMCLIENT}
  BoldGUI,
{$ENDIF}
{$IFDEF SpanFetch}
  AttracsSpanFetchManager,
{$ENDIF}
  BoldGuard,
  BoldListControlPack,
  BoldDomainElement;

const
  breVariablesRemoved = 42;
  breVariablesChanged = 43;
  breRendererRemoved = 44;
  breRendererChanged = 45;
  breControllerChanged = 46;

var
  DefaultRenderer: TBoldRenderer;

{$IFDEF BOLDCOMCLIENT}
function BoldTestType(element: IUnknown; const TypeOrInterface: TGUID): Boolean;
var
  Res: IUnknown;
begin
  result := element.QueryInterface(TypeOrInterface, res) = S_OK;
end;
{$ELSE}
function BoldTestType(element: TObject; TypeOrInterface: TClass): Boolean;
begin
  result := element is TypeOrInterface;
end;
{$ENDIF}

{ TBoldRendererData }

constructor TBoldRendererData.Create(OwningFollower: TBoldFollower);
begin
  FOwningFollower := OwningFollower;
end;

function TBoldRendererData.GetSubFollowerCount: Integer;
begin
  Result := 0;
end;

function TBoldRendererData.GetSubFollower(index: Integer): TBoldFollower;
begin
  raise EBold.CreateFmt('%s: This class has no subfollowers', [ClassName]);
end;

function TBoldRendererData.GetCurrentSubFollowerIndex: Integer;
begin
  Result := -1;
end;

function TBoldRendererData.GetEnsuredSubFollower(
  Index: Integer): TBoldFollower;
begin
  result := GetSubFollower(Index);
end;

{ TBoldFollowerController }

constructor TBoldFollowerController.Create(aOwningComponent: TComponent);
begin
  inherited Create;
  fComponentSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  FOwningComponent := aOwningComponent;
  FApplyPolicy := bapExit;
  FDragMode := DefaultBoldDragMode;
  FDropMode := DefaultBoldDropMode;
  FPopup := TBoldPopup.Create;
  FRepresentation := brDefault;
  fCleanOnEqual := true;
end;

destructor TBoldFollowerController.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  FreeAndNil(FPopup);
  fBeforeMakeUptodate := nil;
  fAfterMakeUptodate := nil;
  fVariables := nil;
  inherited;
end;

procedure TBoldFollowerController.Assign(Source: TPersistent);
begin
  if Source is ClassType then
    DoAssign(Source)
  else
    inherited Assign(Source);
end;

function TBoldFollowerController.DoApplyException(E: Exception;
  Elem: TBoldElement; var Discard: Boolean): Boolean;
begin
  result := false;
  if Assigned(fApplyException) then
    result := fApplyException(E, Elem, Discard);
end;

procedure TBoldFollowerController.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldFollowerController);
  DragMode := TBoldFollowerController(Source).DragMode;
  DropMode := TBoldFollowerController(Source).DropMode;
  InternalDrag := TBoldFollowerController(Source).InternalDrag;
  Representation := TBoldFollowerController(Source).Representation;
  Expression := TBoldFollowerController(Source).Expression;
  ApplyPolicy := TBoldFollowerController(Source).ApplyPolicy;
  CleanOnEqual := TBoldFollowerController(Source).CleanOnEqual;
  Popup.Assign(TBoldFollowerController(Source).Popup);
  if Assigned(TBoldFollowerController(Source).UntypedRenderer) then
    UntypedRenderer := (TBoldFollowerController(Source).UntypedRenderer);
end;

function TBoldFollowerController.DoDisplayException(E: Exception;
  Elem: TBoldElement): Boolean;
begin
  result := false;
  if Assigned(fApplyException) then
    result := fDisplayException(E, Elem);
end;

procedure TBoldFollower.MarkClean;
begin
  SetState(bfsCurrent);
end;

procedure TBoldFollowerController.MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscribe: Boolean);
begin
  if Assigned(BeforeMakeUptoDate) then
    BeforeMakeUptoDate(aFollower);
  try
    DoMakeUptodateAndSubscribe(aFollower, Subscribe);
  finally
    aFollower.MarkClean;
    if Assigned(AfterMakeUptoDate) then
      AfterMakeUptoDate(aFollower);
  end;
end;

procedure TBoldFollowerController.MakeClean(Follower: TBoldFollower);
begin
  raise EBoldInternal.CreateFmt(sNotImplemented, [ClassName, 'MakeClean']); // do not localize
end;

procedure TBoldFollowerController.Changed;
begin
  SendEvent(Self, beValueChanged);
end;

function TBoldFollowerController.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := EffectiveRenderer.RendererDataClass;
end;

function TBoldFollowerController.GetOwner: TPersistent;
begin
  Result := OwningComponent;
end;

function TBoldFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := DefaultRenderer;
end;

procedure TBoldFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
end;

function TBoldFollower.GetAssertedController: TBoldFollowerController;
begin
  if not assigned(fController) then
    raise EBold.CreateFmt('%s.GetAssertedController: Controller not assigned', [classname]);
  result := fController;
end;

function TBoldFollower.GetActive: Boolean;
begin
  Result := not (State in [bfsInactiveValidElement, bfsInactiveInvalidElement]);
end;

function TBoldFollower.GetElementValid: Boolean;
begin
  Result := State <> bfsInactiveInValidElement;
end;

function TBoldFollower.GetRendererData: TBoldRendererData;
begin
  if not Active then
    raise EBold.Create(SBoldInactiveFollowerNoRenderData);
  if not Assigned(FRendererData) then
    FRendererData := AssertedController.RendererDataClass.Create(Self);
  Result := FRendererData;
end;

function TBoldFollower.GetSubFollower(index: Integer): TBoldFollower;
begin
  Result := RendererData.GetSubFollower(index);
end;

function TBoldFollower.GetEnsuredSubFollower(
  Index: Integer): TBoldFollower;
begin
  Result := RendererData.GetEnsuredSubFollower(index);
end;

function TBoldFollower.GetSubFollowerCount: Integer;
begin
  if Active then
    Result := RendererData.GetSubFollowerCount
  else
    result := 0
end;

function TBoldFollower.GetCurrentIndex: Integer;
begin
  Result := RendererData.GetCurrentSubFollowerIndex;
end;

function TBoldFollower.GetCurrentSubFollower: TBoldFollower;
begin
  if CurrentIndex = -1 then
    result := nil
  else
    Result := SubFollowers[CurrentIndex];
end;

procedure TBoldFollowerController.StartDrag(Follower: TBoldFollower);
begin
  EffectiveRenderer.StartDrag(Follower.Element, DragMode, Follower.RendererData);
end;

procedure TBoldFollowerController.EndDrag;
begin
  EffectiveRenderer.EndDrag(DragMode, InternalDrag);
end;

function TBoldFollowerController.DragOver(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer): Boolean;
begin
  Result := EffectiveRenderer.DragOver(ReceivingElement, DropMode, InternalDrag, Follower.RendererData, dropindex);
end;

procedure TBoldFollowerController.DragDrop(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer);
var
  {$IFNDEF BOLDCOMCLIENT}
  Success: Boolean;
  {$ENDIF}
  Discard: Boolean;
  Handled: Boolean;
begin
  {$IFNDEF BOLDCOMCLIENT}
  if BoldGuiHandler.ActivateTargetFormOnDrop then
  begin
    Success := false;
    if assigned(OwningComponent) then
      Success := BoldGuiHandler.TryToFocusHostingForm(OwningComponent);
    if not success and (Follower.MatchObject is TComponent) then
      BoldGuiHandler.TryToFocusHostingForm(Follower.MatchObject as TComponent);
  end;
  {$ENDIF}
  try
    EffectiveRenderer.DragDrop(ReceivingElement, DropMode, dropindex);
  except
    on E: Exception do
    begin
      Handled := HandleApplyException(E, ReceivingElement, Discard);
      if Discard then
        Follower.DiscardChange;
      if not Handled then
        raise;
    end;
  end;
end;

procedure TBoldFollowerController.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  EffectiveRenderer.DrawOnCanvas(Follower, Canvas, Rect, Alignment, Margins);
end;

function TBoldFollowerController.MayModify(Follower: TBoldFollower): Boolean;
begin
  result := EffectiveRenderer.MayModify(Follower)
end;

procedure TBoldFollowerController.SetRepresentation(Value: TBoldRepresentation);
begin
  if Value <> Representation then
  begin
    FRepresentation := Value;
    Changed;
  end;
end;

procedure TBoldFollowerController.SetExpression(const Value: string);
begin
  if Value <> Expression then
  begin
    FExpression := Value;
    Changed;
  end;
end;

procedure TBoldFollowerController.SetUntypedRenderer(NewRender: TBoldRenderer);
begin
  if NewRender <> UntypedRenderer then
  begin
    fUntypedRenderer := NewRender;
    Resubscribe;
    Changed;
  end;
end;

procedure TBoldFollowerController.HoldsChangedValue(Follower: TBoldFollower);
begin
  EffectiveRenderer.HoldsChangedValue(Follower);
end;

procedure TBoldFollowerController.ReleaseChangedValue(Follower: TBoldFollower);
begin
  EffectiveRenderer.ReleaseChangedValue(Follower);
end;

{ TBoldSingleFollowerController }
procedure TBoldSingleFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  with EffectiveRenderer as TBoldSingleRenderer do
  begin
    if Subscribe then
      MakeUptodateAndSubscribe(Follower, Follower.Subscriber)
    else
      MakeUptodateAndSubscribe(Follower, nil);
  end;
end;

{ TBoldRenderer }
destructor TBoldRenderer.Destroy;
begin
  FreeAndNil(fRepresentations);
  inherited Destroy;
end;

function TBoldRenderer.GetRepresentations: TStrings;
begin
  if not Assigned(FRepresentations) then
    FRepresentations := GetDefaultRepresentationStringList;
  Result := TStrings(FRepresentations);
end;

procedure TBoldRenderer.SetRepresentations(Value: TStrings);
begin
  if not Assigned(FRepresentations) then
    FRepresentations := TStringList.Create;
  FRepresentations.Assign(Value);
end;

function TBoldRenderer.StoreRepresentations: Boolean;
begin
  Result := False;
  if Assigned(FRepresentations) and (FRepresentations.Count > 0) then
    with GetDefaultRepresentationStringList do
    try
      Result := not Equals(FRepresentations);
    finally
      Free;
    end;
end;

class function TBoldRenderer.GetExpressionAsDirectElement(Element: TBoldElement; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): TBoldElement;
begin
  Result := nil;
  if Assigned(Element) then
  begin
    if Expression = '' then
    begin
      Result := Element;
    end
    else
      {$IFDEF BOLDCOMCLIENT}
      Result := Element.EvaluateExpression(Expression);
      {$ELSE}
      Result := Element.EvaluateExpressionAsDirectElement(Expression, VariableList);
      {$ENDIF}
  end;
end;

function TBoldRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldRendererData;
end;

function TBoldRenderer.GetDefaultRepresentationStringList: TStringList;
begin
  Result := TStringList.Create;
  try
    with Result do
    begin
      Add(IntToStr(brDefault) + '=' + SRepresentationDefault);
      Add(IntToStr(brShort) + '=' + SRepresentationShort);
      Add(IntToStr(brLong) + '=' + SRepresentationLong);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TBoldRenderer.DefaultMayModify(aFollower: TBoldFollower): Boolean;

  function CheckPessimisticLocking(AElement: TBoldDomainElement): boolean;
  var
    BoldSystem: TBoldSystem;
  begin
    result := true;
    BoldSystem := TBoldDomainElement(AElement).BoldSystem as TBoldSystem;
    if Assigned(BoldSystem) and Assigned(BoldSystem.PessimisticLockHandler) then
      result := BoldSystem.PessimisticLockHandler.LockElement(AElement);
  end;

var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
  begin
    {$IFDEF BOLDCOMCLIENT}
    result := ValueElement.mutable;
    {$ELSE}
    Result := ValueElement.ObserverMayModify(aFollower.Subscriber);
    {$ENDIF}
    if (ValueElement is TBoldDomainElement) then
      result := result and CheckPessimisticLocking(ValueElement as TBoldDomainElement);
  end
  else
    Result := False;
end;

procedure TBoldRenderer.DefaultHoldsChangedValue(aFollower: TBoldFollower);
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    ValueElement.RegisterModifiedValueHolder(aFollower.Subscriber)
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TBoldRenderer.DefaultReleaseChangedValue(aFollower: TBoldFollower);
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    ValueElement.UnRegisterModifiedValueHolder(aFollower.Subscriber)
end;
{$ELSE}
begin
end;
{$ENDIF}

function TBoldRenderer.MayModify(aFollower: TBoldFollower): Boolean;
begin
  if Assigned(aFollower.Value) then
  begin
    Result := aFollower.Value.ObserverMayModify(aFollower.Subscriber);
    if not result then
      Result := HasSetValueEventOverrides and
       ((aFollower.Value.ModifiedValueHolder = nil) or (aFollower.Value.ModifiedValueHolder = aFollower.Subscriber));
  end
  else
    Result := HasSetValueEventOverrides;
  if Assigned(FOnMayModify) then
    Result := Result and OnMayModify(aFollower)
  else
    Result := Result and HasSetValueEventOverrides or DefaultMayModify(aFollower);
end;

procedure TBoldRenderer.EnsureFetched(List: TBoldObjectList; BoldType: TBoldClassTypeInfo; Expression: TBoldExpression);
{$IFNDEF SpanFetch}
var
  ListType: TBoldListTypeInfo;
  RealObjectList: TBoldObjectList;
  ie: TBoldIndirectElement;
{$ENDIF}
begin
  if assigned(FOnEnsureFetched) then
    FOnEnsureFetched(List, expression)
  else if Expression <> '' then
  {$IFDEF SpanFetch}
    FetchOclSpan(List, Expression);
  {$ELSE}
    if (List.Count > 1) then
    begin
      try
        List.EnsureObjects;
        ListType := BoldType.ListTypeInfo;
        RealObjectlist := TBoldMemberFactory.CreateMemberFromBoldType(ListType) as TBoldObjectList;
        RealObjectList.AddList(List);
        ie := TBoldIndirectElement.Create;
        RealObjectList.EvaluateExpression('self->collect(' + Expression + ')', ie);
      finally
        FreeAndNil(RealObjectList);
        FreeAndNil(ie);
      end;
    end;
{$ENDIF}
end;

function TBoldRenderer.GetSupportsMulti: Boolean;
begin
  Result := false;
end;

procedure TBoldRenderer.HoldsChangedValue(Follower: TBoldFollower);
begin
  if Assigned(FOnHoldsChangedValue) then
    OnHoldsChangedValue(Follower)
  else
    DefaultHoldsChangedValue(Follower)
end;

procedure TBoldRenderer.ReleaseChangedValue(Follower: TBoldFollower);
begin
  if Assigned(FOnReleaseChangedValue) then
    OnReleaseChangedValue(Follower)
  else
    DefaultReleaseChangedValue(Follower)
end;

procedure TBoldRenderer.DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData);
{$IFNDEF BOLDCOMCLIENT}
var
  Obj: TBoldObject;
{$ENDIF}  
begin
  {$IFNDEF BOLDCOMCLIENT}
  if BoldGUIHandler.DraggedObjects.Count <> 0 then
    raise EBold.Create(SDraggedObjectsNotCleared);

  if DragMode = bdgSelection then
  begin
    if (Element is TBoldObject) then
      Obj := element as TBoldObject
    else if (Element is TBoldObjectReference) then
      Obj := (Element as TBoldObjectReference).BoldObject
    else
      obj := nil;

    if Assigned(obj) then
      BoldGUIHandler.DraggedObjects.Add(Obj);
  end;
  {$ENDIF}
end;

function TBoldRenderer.DefaultValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateCharacter(C, aFollower.AssertedController.Representation)
  else
    Result := True;
end;

procedure TBoldRenderer.DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean);
begin
  {$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.DraggedObjects.Clear;
  {$ENDIF}
end;

function TBoldRenderer.DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  result := false;
  {$ELSE}
  Result := Assigned(Element) and Element.ObserverMayModify(Self) and
    (BoldGUIHandler.DraggedObjects.Count > 0) and
    BoldGUIHandler.DraggedObjectsAssignable(Element, DropMode);
  {$ENDIF}
end;

procedure TBoldRenderer.DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
{$IFNDEF BOLDCOMCLIENT}
var
  i: integer;
  offset,
  prevIndex: integer;
  BoldObject: TBoldObject;
  TheLink: TBoldObjectReference;
  TheList: TBoldObjectList;
  DraggedObjects: TBoldObjectList;
begin
  DraggedObjects := BoldGUIHandler.DraggedObjects;
  if element is TBoldObjectReference then
  begin
    TheLink := Element as TBoldObjectReference;
    if DraggedObjects.Count = 0 then
        BoldObject := nil
    else if DraggedObjects.Count = 1 then
      BoldObject := DraggedObjects[0]
      else
        raise EBold.Create(SCannotDragOverMultipleObjects);
    case DropMode of
      bdpInsert, bdpAppend:
        if Assigned(TheLink.BoldObject) and Assigned(BoldObject) then
          raise EBold.Create(SLinkAlreadyAssigned)
        else
          TheLink.BoldObject := BoldObject;
      bdpReplace:
        TheLink.BoldObject := BoldObject;
    end;
  end
  else if element is TBoldObjectList then
  begin
    TheList := Element as TBoldObjectlist;
    case DropMode of
      bdpAppend:
        for i := 0 to DraggedObjects.Count - 1 do
          if TheList.IndexOf(DraggedObjects[I]) = -1 then
            TheList.Add(DraggedObjects[i]);
      bdpReplace:
        raise EBoldFeatureNotImplementedYet.CreateFmt('%s.DefaultDragDrop: Replace not implemented yet', [ClassName]);
      bdpInsert:
      begin
        if dropindex < 0
          then dropindex := 0;
        for I := 0 to DraggedObjects.Count - 1 do
        begin
          prevIndex := TheList.IndexOf(DraggedObjects[I]);
          Offset := 0;
          if prevIndex = -1 then
          begin
            if dropindex < TheList.Count then
              TheList.Insert(dropindex + Offset, DraggedObjects[I])
            else
              TheList.Add(DraggedObjects[I]);
            INC(dropindex);
          end
          else
          begin
            TheList.Move(prevIndex, dropindex);
            if prevIndex > dropindex then
              Inc(dropindex)
          end;
        end
      end;
    end;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TBoldRenderer.StartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData);
begin
  if Assigned(FOnStartDrag) then
    OnStartDrag(Element, DragMode, RendererData)
  else
    DefaultStartDrag(Element, DragMode, RendererData);
end;

procedure TBoldRenderer.EndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean);
begin
  if Assigned(FOnEndDrag) then
    OnEndDrag(DragMode, InternalDrag)
  else
    DefaultEndDrag(DragMode, InternalDrag);
end;

function TBoldRenderer.DragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean;
begin
  if Assigned(FOnDragOver) then
    Result := FOnDragOver(Element, DropMode, InternalDrag, RendererData, dropindex)
  else
    Result := DefaultDragOver(Element, DropMode, InternalDrag, RendererData, dropindex)
end;

procedure TBoldRenderer.SubscribeToElement(aFollower: TBoldFollower);
begin
  if Assigned(aFollower.Element) then
  begin
    if assigned(fOnSubscribe) then
      fOnSubscribe(aFollower, aFollower.Subscriber)
    else
    {$IFDEF BOLDCOMCLIENT}
      Element.SubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
    {$ELSE}
      aFollower.Element.SubscribeToExpression(aFollower.AssertedController.Expression, aFollower.subscriber, false, false, aFollower.Controller.GetVariableListAndSubscribe(aFollower.subscriber));
    {$ENDIF}
  end;
end;

function TBoldRenderer.ValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean;
begin
  if Assigned(FOnValidateCharacter) then
    Result := OnValidateCharacter(aFollower, C)
  else
    Result := DefaultValidateCharacter(aFollower, C);
end;

procedure TBoldRenderer.DragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
begin
  if Assigned(FOnDragDrop) then
    OnDragDrop(Element, DropMode, dropindex)
  else
    DefaultDragDrop(Element, DropMode, dropindex)
end;

procedure TBoldRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Aligment: TAlignment; Margins: TPoint);
begin
end;

function TBoldRenderer.HasSetValueEventOverrides: boolean;
begin
  result := false;
end;

{ TBoldFollower }

procedure TBoldFollower.SetElementAndMakeCurrent(AElement: TBoldElement; AActive: boolean);
begin
  if {not assigned(AElement) or} (AElement <> fElement) then
  begin
    fElement := AElement;
    if IsInDisplayList then
      RemoveFromDisplayList(false);
    if AActive then
    begin
      SetState(bfsActivating);
      MakeUptodateAndSubscribe;
    end
    else
    begin
      if Assigned(fElement) then
        fState := bfsInactiveValidElement
      else
        fState := bfsInactiveInvalidElement;
    end;
  end;
end;

constructor TBoldFollower.Create(MatchObject: TObject; Controller: TBoldFollowerController);
begin
  inherited Create(BoldGuiHandler.FindHostingForm(MatchObject as TComponent));
  Assert(assigned(Controller));
  fController := Controller;
  fSubscriber := TBoldFollowerSubscriber.Create(Self);
  fIndirectElement := TBoldIndirectElement.Create;
  ResultElementOutOfDate := true;
  fIndex := -1;
end;

type TBoldAsFollowerListControllerAccess = class(TBoldAsFollowerListController);

constructor TBoldFollower.CreateSubFollower(OwningFollower: TBoldFollower;
    aController: TBoldFollowerController;
    aElement: TBoldElement;
    aActive: boolean;
    aIndex: integer);

begin
  Assert(assigned(aController));
  inherited Create(OwningFollower.MatchObject);
  FOwningFollower := OwningFollower;
  PrioritizedQueuable := OwningFollower;
  fController := aController;
  fSubscriber := TBoldFollowerSubscriber.Create(Self);
  fIndirectElement := TBoldIndirectElement.Create;
  fElement := aElement;
  fIndex := aIndex;
  ResultElementOutOfDate := true;
  if aActive {and not PrioritizedQueuable.IsInDisplayList} and // check MostPrioritizedQueuable instead of PrioritizedQueuable ?
    not ((aController is TBoldAsFollowerListController) and TBoldAsFollowerListControllerAccess(aController).PrecreateFollowers) then
  begin
    fState := bfsActivating;
    MakeUptodateAndSubscribe;
  end
  else
  if aActive then
  begin
//    MarkSubscriptionOutOfDate
    fState := bfsSubscriptionOutOfDate;
    AddToDisplayList;
    if Assigned(fElement) then
      fElement.AddSubscription(Subscriber, beDestroying, beDestroying);
  end
  else
  begin
    if Assigned(fElement) then
      fState := bfsInactiveValidElement
    else
      fState := bfsInactiveInvalidElement;
    if Assigned(fElement) then
      fElement.AddSubscription(Subscriber, beDestroying, beDestroying);
  end;
end;

destructor TBoldFollower.Destroy;
begin
  FreeAndNil(FIndirectElement);
  FreeAndNil(FRendererData);
  FreeAndNil(fSubscriber);
  fCOntroller := nil;
  fControlData := nil;
  fElement := nil;
  fOwningFollower := nil;
  inherited;
end;

function TBoldFollower.ExistInOwner: Boolean;
var
  Follower: TBoldFollower;
  aElement: TBoldElement;
begin
  Result := False;
  if Assigned(Element) then
  begin
    aElement := Element;
    Follower := OwningFollower;
    while Assigned(Follower) do
    begin
      if Assigned(Follower.Element) and
        (Follower.Element = aElement) and
        (Follower.Controller = Controller) then
      begin
        Result := True;
        Exit;
      end;
      Follower := Follower.OwningFollower;
    end;
  end;
end;

function TBoldFollower.CheckIfInHierarchy(aElement: TBoldElement; aController: TBoldFollowerController): Boolean;
var
  Follower: TBoldFollower;
begin
  Result := False;
  Follower := Self;
  while Assigned(Follower) do
  begin
    if Assigned(Follower.Element) and
      (Follower.Element = aElement) and
      (Follower.Controller = aController) then
    begin
      Result := True;
      Exit;
    end;
    Follower := Follower.OwningFollower;
  end;
end;

function TBoldFollower.GetIsDirty: Boolean;
begin
  result := fState = bfsDirty;
end;

type
  TBoldFlaggedObjectAccess = class(TBoldFlaggedObject);

procedure TBoldFollower.SetState(AValue: TBoldFollowerState);
begin
  if not (fState in [bfsEmpty, bfsCurrent, bfsDirty,
    bfsValueOutOfDate, bfsSubscriptionOutOfDate,
    bfsInactiveValidElement, bfsInactiveInvalidElement, bfsActivating]) then
    raise Exception.Create('TBoldFollower.SetState old state is ' + IntToStr(Integer(fState)));
  {action when leaving state}
  case State of
    bfsValueOutOfDate, bfsSubscriptionOutOfDate: {bfsOutOfDate}
      RemoveFromDisplayList(false);
    bfsDirty:
    begin
      if not ResultElementOutOfDate and Assigned(fIndirectElement.Value)
         and TBoldFlaggedObjectAccess(fIndirectElement.Value).GetElementFlag(befHasModifiedValueHolder)
         and (fIndirectElement.Value.ModifiedValueHolder = Subscriber) then
           AssertedController.ReleaseChangedValue(self);
      RemoveFromApplyList;
    end;
  end;

  if AValue = bfsDirty then // process Dirty before changeing fState, since HoldsChangedValue can fail.
  begin
    AssertedController.HoldsChangedValue(Self);
    AddToApplyList;
  end;

  fState := AValue;

  {action when entering state}
  case State of
    bfsValueOutOfDate:
    begin
      AddToDisplayList;
      Ensured := false;
      ResultElementOutOfDate := true;
    end;
    bfsSubscriptionOutOfDate:
      begin
        AddToDisplayList;
        Ensured := false;
        Subscriber.CancelAllSubscriptions;
        ResultElementOutOfDate := true;
      end;
    bfsInactiveValidElement, bfsInactiveInvalidElement:
      begin
        Subscriber.CancelAllSubscriptions;
        FreeAndNil(fRendererData);
        ResultElementOutOfDate := true;
        if Assigned(fElement) then
          Element.AddSubscription(Subscriber, beDestroying, beDestroying);
      end;
    bfsEmpty, bfsCurrent, bfsActivating, bfsDirty:
      {no action}
    else
      raise EBoldInternal.CreateFmt('%s: Unknown FollowerState', [ClassName]);
  end;
end;

function TBoldFollower.GetSubFollowerAssigned(index: Integer): boolean;
begin
  result := Active and RendererData.GetSubFollowerAssigned(Index);
end;

procedure TBoldFollower.SetActive(Value: Boolean);
begin
  if (Value <> GetActive) then
  begin
    if Value then
    begin
      Assert(State <> bfsInactiveInvalidElement);
      SetState(bfsActivating);
      MakeUptodateAndSubscribe;
    end
    else
      if (State <> bfsDirty) then
        SetState(bfsInactiveValidElement);
  end;
end;

function TBoldFollower.GetValue: TBoldElement;
begin
  if ResultElementOutOfDate then
  begin
    fIndirectElement.SetReferenceValue(nil);
    if {ElementValid and} Assigned(Element) then
    try
      ResultElementOutOfDate := false;
      Element.EvaluateAndSubscribeToExpression(Controller.Expression, Subscriber, FIndirectElement, false, false, Controller.GetVariableListAndSubscribe(Subscriber));
    except
      // perhaps raise exception
    end;
    if (state = bfsInactiveValidElement) then
      Active := true;
  end;
  result := fIndirectElement.Value;
end;

procedure TBoldFollower.SetElementValid(Value: Boolean);
begin
  if value <> elementValid then
  begin
    if value then
      SetState(bfsInactivevalidElement)
    else
      SetState(bfsInactiveInvalidElement);
  end;
end;

function TBoldFollower.MayChange: Boolean;
begin
  if State = bfsDirty then
    raise EBold.Create(SCannotChangeStateWithModifiedValue)
  else
    Result := True;
end;

function TBoldFollower.MayModify: Boolean;
begin
  result := AssertedController.MayModify(self);
end;

procedure TBoldFollower.DiscardChange;
begin
  if State = bfsDirty then
  begin
    AssertedController.ReleaseChangedValue(Self);
    MarkValueOutOfDate;
  end;
end;

procedure TBoldFollower.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent);
begin
{$IFNDEF BOLDCOMCLIENT}
  if (OriginalEvent = beDestroying) and (Originator = Element) then
    FElement := nil;
{$ENDIF}
  case RequestedEvent of
    breReEvaluate:
      MarkValueOutOfDate;
    breReSubscribe:
      MarkSubscriptionOutOfDate;
    breControllerChanged:
      begin
        if OriginalEvent = beDestroying then
        begin
          RemoveFromDisplayList(false);
          if assigned(fRendererData) then
            AssertedController.CleanRendererData(fRendererData);
          FreeAndNil(fRendererData);
          fController := nil;
        end;
        if Assigned(FRendererData) and not (FRendererData is AssertedController.RendererDataClass) then
          FreeAndNil(fRendererData);
        MarkSubscriptionOutOfDate;
        if OriginalEvent <> beDestroying then
          AssertedController.AddSmallSubscription(Subscriber, [beValueChanged, beDestroying], breControllerChanged);
      end;
  end;
end;

procedure TBoldFollower.MakeUptodateAndSubscribe;
begin
  if State in bfdNeedResubscribe then
  begin
    Subscriber.CancelAllSubscriptions;
    AssertedController.AddSmallSubscription(Subscriber, [beValueChanged, beDestroying], breControllerChanged);
    Controller.MakeUptodateAndSubscribe(Self, true);
    if Assigned(Element) then
      Element.AddSubscription(Subscriber, beDestroying, beDestroying);
    if Assigned(Element) and (State in bfdNeedResubscribe) then
    Controller.SubscribeToElement(self);
  end
  else
    AssertedController.MakeUptodateAndSubscribe(Self, State in bfdNeedResubscribe);
end;

procedure TBoldFollower.MakeClean;
begin
  AssertedController.MakeClean(Self);
  if State = bfsDirty then
    SetState(bfsSubscriptionOutOfDate);
end;

procedure TBoldFollower.ControlledValueChanged;
begin
  if (State <> bfsDirty) then
  begin
    // if ApplyPolicy is bapChange and there are no events assigned for HoldsChangedValue and ReleaseChangedValue
    // then there's no need to call HoldsChangedValue and ReleaseChangedValue
    // no need to call Controller.HoldsChangedValue(Self); here as it will be called in SetState as a result of MarkDirty bellow
    MarkDirty;
  end;
  if AssertedController.ApplyPolicy = bapChange then
  begin
    Apply;
  end;
end;

procedure TBoldFollower.MarkValueOutOfDate;
begin
  case State of
    bfsEmpty, bfsCurrent, bfsDirty:
      SetState(bfsValueOutOfDate);
    bfsInactiveValidElement, bfsValueOutOfDate,
    bfsSubscriptionOutOfDate, bfsActivating:
      {no action}
  else
    raise EBoldInternal.CreateFmt('%s.MarkOutOfDate: Follower state error', [ClassName]);
  end;
end;

procedure TBoldFollower.MarkSubscriptionOutOfDate;
begin
  case State of
    bfsEmpty, bfsCurrent, bfsDirty, bfsValueOutOfDate:
      SetState(bfsSubscriptionOutOfDate);

    bfsSubscriptionOutOfDate, bfsActivating :
      {no action};


    bfsInactiveValidElement, bfsInactiveInvalidElement:
    begin
      SetState(State);{no action}
    end
  else
    raise EBoldInternal.CreateFmt('%s.MarkSubscriptionOutOfDate: Follower state error', [ClassName]);
  end;
end;

procedure TBoldFollower.SetElement(AElement: TBoldElement);
begin
  if not assigned(AElement) or (AElement <> fElement) then
  begin
    Assert(not (not ResultElementOutOfDate and Assigned(fIndirectElement.Value)
       and TBoldFlaggedObjectAccess(fIndirectElement.Value).GetElementFlag(befHasModifiedValueHolder)
       and (fIndirectElement.Value.ModifiedValueHolder = Subscriber)));
    fElement := AElement;
    ElementValid := true;
    MarkSubscriptionOutOfDate;
    if Assigned(AElement) then
      AssertedController.SubscribeToElement(self);
    ResultElementOutOfDate := true;
  end;
end;

procedure TBoldFollower.MarkDirty;
begin
  case State of
    bfsCurrent:
      SetState(bfsDirty);
    bfsDirty:
      {no action}
  else
    raise EBoldInternal.CreateFmt('%s.MarkDirty: Follower state error', [Classname]);
  end;
end;


procedure TBoldFollower.MarkEnsured;
begin
  Ensured := true;
end;

function TBoldFollower.Displayable: Boolean;
begin
  Result := State in bfsDisplayable;
end;

procedure TBoldFollower.Display;
  procedure DisplaySelf;
  begin
{.$IFNDEF BoldQueue_Optimization}
//  This check is already done in Queue.DisplayOne, so it only serves to protect direct calls to Display. That shouldn't be done anyway.
      if (MostPrioritizedQueuable <> nil) then
        raise EBold.CreateFmt('%s.Display: Can not display because there is an owning follower that must be displayed before', [ClassName]);
{.$ENDIF}
      MakeUptodateAndSubscribe;
  end;

  procedure DisplayMulti;
  var
    Followers: TBoldFollowerArray;
  begin
    Followers := CollectMatching( Controller);
    if Length(Followers) > 1 then
        MultiMakeUptodateAndSubscribe(Followers)
    else
      DisplaySelf;
  end;
begin
  try
    if AssertedController.SupportsMulti then
      DisplayMulti
    else
      DisplaySelf;
  except
    on E: Exception do
    begin
      if assigned(Controller) and not Controller.HandleDisplayException(E, Element) then
      else
      begin
        if assigned(Controller) then
          E.message := Format('%s' + BOLDCRLF + 'occured when displaying component %s', [E.message, Controller.GetNamePath]);
        raise;
      end;
    end;
  end;
end;

procedure TBoldFollower.EnsureMulti;
var
  Followers: TBoldFollowerArray;
begin
  Followers := CollectMatching(Controller);
  if Length(Followers) > 1 then
  try
    Controller.MultiMakeEnsure(Followers);
  except
    ;
  end
end;

procedure TBoldFollower.EnsureSiblings;
var
  Followers: TBoldFollowerArray;
begin
  if (not Assigned(fOwningFollower)) and (not Ensured) then
  begin
    SetLength(Followers, 1);
    Followers[0] := self;
  end
  else
    Followers := CollectMatchingSiblings(20);
  if Length(Followers) > 0 then
    try
      Controller.MultiMakeEnsure(Followers);
    except
      ;
    end

end;

procedure TBoldFollower.EnsureDisplayable;
begin
  if not Displayable then
  begin
    Active := True;
    if not Displayable then
      MakeUptodateAndSubscribe;
  end;
  RendererData.EnsureSubfollowersDisplayable;
end;

procedure TBoldFollower.Apply;
var
  Discard: Boolean;
  Handled: Boolean;  
begin
  if State = bfsDirty then
  begin
    try
      AssertedController.ReleaseChangedValue(Self);
      MakeClean;
    except
      on E: Exception do
        begin
          Handled := assigned(Controller) and Controller.HandleApplyException(E, Element, Discard);
          if Discard then
            DiscardChange
          else
          if State = bfsDirty  then
            AssertedController.HoldsChangedValue(self);
          if not Handled then
            raise;
        end;
    end;
  end;
end;

{ TBoldPopUp }
function TBoldPopup.GetMenu(CONTROL: TControl; Element: TBoldElement): TPopupMenu;
begin
  Result := nil;
  {$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.PopupElement := Element;
  BoldGUIHandler.PopupControl := CONTROL;
  if not Enable then
    Result := BoldPopupMenu;
  {$ENDIF}
end;

procedure TBoldPopup.Assign(Source: TPersistent);
begin
  if Source is TBoldPopup then
  begin
    Enable := TBoldPopup(Source).Enable;
    InsertNew := TBoldPopup(Source).InsertNew;
    Delete := TBoldPopup(Source).Delete;
    Move := TBoldPopup(Source).Move;
  end
  else
    inherited Assign(Source);
end;

constructor TBoldFollowerSubscriber.Create(follower: TBoldFollower);
begin
  inherited create;
  fFollower := Follower;
end;

destructor TBoldFollowerSubscriber.Destroy;
begin
  fFollower := nil;
  inherited;
end;

function TBoldFollowerSubscriber.GetContextString: string;
begin
  Result := Follower.AssertedController.GetNamepath
end;

function TBoldFollowerSubscriber.ContextObject: TObject;
begin
  result := Follower;
end;

procedure TBoldFollowerController._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breRendererRemoved then
    UntypedRenderer := nil;
  if RequestedEvent = breVariablesRemoved then
    fVariables := nil;
  Changed;
end;

procedure TBoldFollower.SetCurrentIndex(index: integer);
begin
  if index <> CurrentIndex then
  begin
    RendererData.SetCurrentSubFollowerIndex(index);
    MarkValueOutOfDate;
  end;
end;

procedure TBoldRendererData.SetCurrentSubFollowerIndex(index: integer);
begin
end;

function TBoldRendererData.GetSubFollowerAssigned(Index: Integer): boolean;
begin
  raise EBold.CreateFmt('%s: This class has no subfollowers', [ClassName]);
end;

function TBoldFollowerController.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(OnGetContextType) then
    result := OnGetContextType
  else
    result := nil;
end;

procedure TBoldRenderer.Assign(Source: TPersistent);
begin
  inherited;
  With Source as TBoldRenderer do
  begin
    self.OnMayModify := OnMayModify;
    self.OnHoldsChangedValue := OnHoldsChangedValue;
    self.OnReleaseChangedValue := OnReleaseChangedValue;
    self.OnSubscribe := OnSubscribe;
    self.OnEnsureFetched := OnEnsureFetched;
    self.OnValidateCharacter := OnValidateCharacter;
    self.Representations.Assign(Representations);
  end;
end;

procedure TBoldRenderer.Changed;
begin
  SendEvent(Self, beValueChanged);
end;

procedure TBoldRenderer.SetOnSubscribe(Value: TBoldSubscribe);
begin
  if @fOnSubscribe <> @Value then
  begin
    FOnSubscribe := Value;
    Changed;
  end;
end;

procedure TBoldRendererData.EnsureSubfollowersDisplayable;
begin

end;

procedure TBoldFollowerController.SetVariables(const Value: TBoldOclVariables);
begin
  if Value <> fVariables then
  begin
    fVariables := Value;
    Resubscribe;
    Changed;
  end;
end;

procedure TBoldFollowerController.Resubscribe;
begin
  fComponentSubscriber.CancelAllSubscriptions;
  if Assigned(Variables) then
  begin
    Variables.AddSmallSubscription(fComponentSubscriber, [beDestroying], breVariablesRemoved);
    Variables.AddSmallSubscription(fComponentSubscriber, [beValueChanged], breVariablesChanged);
  end;
  if Assigned(UntypedRenderer) then
  begin
    UntypedRenderer.AddSmallSubscription(fComponentSubscriber, [beDestroying], breRendererRemoved);
    UntypedRenderer.AddSmallSubscription(fComponentSubscriber, [beValueChanged], breRendererChanged);
  end;
end;

function TBoldFollowerController.GetVariableList: TBoldExternalVariableList;
begin
{$IFDEF BOLDCOMCLIENT}
  result := nil;
{$ELSE}
  if Assigned(Variables) then
    Result := Variables.VariableList
  else
    Result := nil;
{$ENDIF}
end;

function TBoldFollowerController.GetVariableListAndSubscribe(Subscriber: TBoldSubscriber): TBoldExternalVariableList;
begin
  result := GetVariableList;
  {$IFNDEF BOLDCOMCLIENT}
  if assigned(Subscriber) and assigned(Variables) then
    Variables.SubscribeToHandles(Subscriber, Expression);
  {$ENDIF}
end;

function TBoldFollower.CollectMatching(
  MatchController: TBoldFollowerController): TBoldFollowerArray;
begin
  if Assigned(fOwningFollower) then
    Result := fOwningFollower.CollectMatching(MatchController)
  else
    Result := CollectMatchingDownwards(Result, MatchController);
end;

function TBoldFollower.CollectMatchingSiblings(WindowSize: Integer): TBoldFollowerArray;
var
  I, FirstToEnsure, LastToEnsure, found, RowIndex: integer;
  CellFollower: TBoldFollower;
  RowFollower, ListFollower: TBoldFollower;
begin
  FirstToEnsure := 0;
  LastToEnsure := -1;
  // This actully only works for an array, and maybe a treewiew, but that is what we are trying to optimize anyway
  RowFollower := OwningFollower;
  if not Assigned(RowFollower) or (RowFollower.state in bfsOutOfDate) then
    Exit;
  ListFollower := RowFollower.OwningFollower;
  if not Assigned(ListFollower) or (ListFollower.state in bfsOutOfDate) then
    Exit;
  RowIndex :=  RowFollower.index;
  if WindowSize > 1 then
  begin
    FirstToEnsure := (RowIndex DIV WindowSize -1) * WindowSize;
    LastToEnsure := ((RowIndex DIV WindowSize) + 1) * WindowSize;
  end;
  if FirstToEnsure < 0 then
    FirstToEnsure := 0;
  if FirstToEnsure >= ListFollower.SubfollowerCount then
    FirstToEnsure := ListFollower.SubfollowerCount - 1;
  if LastToEnsure >=  ListFollower.SubfollowerCount then
    LastToEnsure := ListFollower.SubfollowerCount - 1;

  SetLength(Result,LastToEnsure- FirstToEnsure+1);
  Found := 0;
  for I := FirstToEnsure to LastToEnsure do
  begin
    RowFollower := ListFollower.SubFollowers[I];
    if (RowFollower = nil) or (not RowFollower.Active) or (RowFollower.SubfollowerCount <= Index) then
      Continue;
    CellFollower := RowFollower.SubFollowers[Index];
    if Assigned(CellFollower) and (CellFollower.Controller = self.Controller) and Assigned(CellFollower.Element) and
      (not CellFollower.Ensured) then
    begin
      Result[Found] := CellFollower;
      Inc(Found);
    end;
  end;
  SetLength(Result, Found);
end;

function TBoldFollower.CollectMatchingDownwards(Followers: TBoldFollowerArray;
  MatchController: TBoldFollowerController): TBoldFollowerArray;
var
  I: integer;
begin
  if (Controller = MatchController) and Assigned(Element) and
    (State in bfsOutOfDate) and (MostPrioritizedQueuable = nil) then
  begin
    SetLength(Result, Length(Followers) + 1);
    for i := 0 to Length(Followers) - 1 do
      Result[i] := Followers[i];
    Result[Length(Followers)] := Self;
  end
  else
   Result := Followers;
  if not (state in bfsOutOfDate) then
  begin
    for I := 0 to SubfollowerCount - 1 do
       if Assigned(SubFollowers[I]) then SubFollowers[I].CollectMatchingDownwards(Followers, MatchController);
  end;
end;

function TBoldFollowerController.GetSupportsMulti: Boolean;
begin
  Result := EffectiveRenderer.SupportsMulti;
end;

class procedure TBoldFollower.MultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
var
  F: TBoldFollower;
  Controller: TBoldFollowerController;
begin
  Assert(Length(Followers) > 0);
  Controller :=  TBoldFollower(Followers[0]).AssertedController;
  for F in Followers do
    if F.State in bfdNeedResubscribe then
    begin
      F.Subscriber.CancelAllSubscriptions;
      Controller.AddSmallSubscription(F.Subscriber, [beValueChanged, beDestroying], breControllerChanged);
    end;
  Controller.MultiMakeUptodateAndSubscribe(Followers);
end;

procedure TBoldFollowerController.MultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
var
  F: TBoldFollower;
begin
  if Assigned(BeforeMakeUptoDate) then
    for F in Followers  do
      BeforeMakeUptoDate(F);
  try
    DoMultiMakeUptodateAndSubscribe(Followers);
  finally
    for F in Followers  do  
      F.MarkClean;
    if Assigned(AfterMakeUptoDate) then
      for F in Followers  do
        AfterMakeUptoDate(F);
  end;
end;

procedure TBoldFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
begin
  raise EBoldInternal.Create('DoMultiMakeUptodateAndSubscribe: called when Multi not supported');
end;

procedure TBoldFollowerSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if not Assigned(Follower) then
    raise EBold.Create('TBoldFollowerSubscriber.Receive called after Destroy');
  Follower.Receive(Originator, OriginalEvent, RequestedEvent);
end;

procedure TBoldFollowerController.SubscribeToElement(aFollower: TBoldFollower);
begin
  EffectiveRenderer.SubscribeToElement(aFollower);
end;

function TBoldFollowerController.HandleApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
var
  ExceptionHandler: TBoldExceptionHandler;
begin
  Result := false;
  Discard := False;
  if Assigned(fApplyException) then
    Result := DoApplyException(E, Elem, Discard);
  if not result and not Discard then
  begin
    ExceptionHandler := TBoldExceptionHandler.FindExceptionHandler(fOwningComponent);
    if assigned(ExceptionHandler) then
      ExceptionHandler.HandleApplyException(E, fOwningComponent, Elem, Discard, Result);
  end;
end;

function TBoldFollowerController.HandleDisplayException(E: Exception;
  Elem: TBoldElement): Boolean;
var
  ExceptionHandler: TBoldExceptionHandler;
begin
  Result := false;
  ExceptionHandler := TBoldExceptionHandler.FindExceptionHandler(fOwningComponent);
  if Assigned(ExceptionHandler) then
    ExceptionHandler.HandleDisplayException(E, fOwningComponent, Elem, Result);
end;

procedure TBoldFollowerController.CleanRendererData(RendererData: TBoldRendererData);
begin
end;

procedure TBoldFollowerController.MultiMakeEnsure(Followers: TBoldFollowerArray);
{$IFNDEF BOLDCOMCLIENT}
var
  BoldType: TBoldClassTypeInfo;
  ObjectList: TBoldObjectList;
  follower: TBoldFollower;
  procedure AddObject(Obj: TBoldObject);
  begin
    ObjectList.Add(Obj);
    if not assigned(BoldType) then
      BoldType := Obj.BoldClassTypeInfo
    else
      BoldType := BoldType.LeastCommonSuperClass(Obj.BoldClassTypeInfo);
  end;

var
  BoldGuard: IBoldGuard;

begin
  BoldGuard := TBoldGuard.Create(ObjectList);
  ObjectList := TBoldObjectList.Create;
  BoldType := nil;
  for Follower in Followers do
  begin
    Follower.MarkEnsured;
    if Follower.element is TBoldObject then
    begin
      AddObject(Follower.element as TBoldObject);
    end;
  end;

  if assigned(BoldType) then
    TBoldFollower(Followers[0]).Controller.EffectiveRenderer.EnsureFetched(ObjectList,BoldType, Expression);
end;

{$ELSE}
begin
end;
{$ENDIF}

function TBoldFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  result := false;
end;

function TBoldFollowerController.SubFollowersActive: boolean;
begin
  result := true;
end;

procedure TBoldFollower.AddToDisplayList;
begin
  if Assigned(Controller) then
    if not ((MatchObject is TComponent) and (csDestroying in TComponent(MatchObject).ComponentState)) then
      inherited AddToDisplayList;
end;

function TBoldFollower.GetDebugInfo: string;
begin
  result := '';
  if Assigned(MatchObject) then
  begin
    if MatchObject is TComponent then
      result := TComponent(MatchObject).Name + ':' + TComponent(MatchObject).ClassName
    else
    if MatchObject is TBoldElement then
      result := TBoldElement(MatchObject).DebugInfo
    else
      Assert(false, MatchObject.ClassName);
  end;
end;

{ TBoldAbstractHandleFollower }

constructor TBoldAbstractHandleFollower.Create(AMatchObject: TObject;
  Controller: TBoldFollowerController);
begin
  inherited Create(BoldGuiHandler.FindHostingForm(AMatchObject as TComponent));
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  fFollower := TBoldFollower.Create(MatchObject, Controller);
  fFollower.PrioritizedQueuable := Self;
end;

destructor TBoldAbstractHandleFollower.Destroy;
begin
  RemoveFromDisplayList(true);
  FreeAndNil(fFollower);
  FreeAndNil(fSubscriber);
  inherited;
end;

initialization
  DefaultRenderer := TBoldRenderer.Create(nil);

finalization
  FreeAndNil(DefaultRenderer);

end.
