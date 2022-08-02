unit BoldQControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  SysUtils,
  QControls,
  QGraphics,
  QMenus,
  {$IFDEF LINUX}
  Types,
  {$ENDIF}
  Types, {QGUI}
  Forms,
  BoldDefs,
  BoldControlPackDefs,
  BoldBase,
  BoldContainers,
  BoldSubscription,
  BoldElements,
  BoldQueue,
  BoldOclVariables;

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

  TBoldRendererDataClass = class of TBoldRendererData;

  TBoldFollowerEvent = procedure (Follower: TBoldFollower) of object;
  TBoldGetContextTypeEvent = function: TBoldElementTypeInfo of object;
  TBoldSubFollowerEvent = procedure (index: Integer; OwningFollower: TBoldFollower) of object;

  { TBoldFollowerSubscriber }
  TBoldFollowerSubscriber = class(TBoldSubscriber)
  private
    fFollower: TBoldFollower;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    function GetContextString: string; override;
  public
    constructor Create(follower: TBoldFollower);
    property Follower: TBoldFollower read fFollower;
  end;

  { TBoldRendererData }
  // Abstract class, concrete versions defined with typed renderer
  TBoldRendererData = class(TBoldMemoryManagedObject)
  private
    fOwningFollower: TBoldFollower;
    fMayModify: Boolean;
  protected
    procedure EnsureSubfollowersDisplayable; virtual;
    function GetSubFollowerCount: Integer; virtual;
    function GetSubFollower(index: Integer): TBoldFollower; virtual;
    function GetCurrentSubFollowerIndex: Integer; virtual;
    procedure SetCurrentSubFollowerIndex(index:integer); virtual;
  public
    constructor Create(OwningFollower: TBoldFollower); virtual;
    property OwningFollower: TBoldFollower read fOwningFollower;
    property MayModify: Boolean read fMayModify write fMayModify;
  end;

  { TBoldRenderer }
  TBoldMayModify = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber): Boolean of object;
  TBoldHoldsChangedValue = procedure (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber) of object;
  TBoldReleaseChangedValue = procedure (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber) of object;
  TBoldSubscribe = procedure (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber) of object;

  TBoldStartDrag = procedure (Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData) of object;
  TBoldEndDrag = procedure (DragMode: TBoldDragMode; InternalDrag: Boolean) of object;
  TBoldDragOver = function (Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean of object;
  TBoldDragDrop = procedure (Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer) of object;

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
    FRepresentations: TStringList;
    function GetRepresentations: TStrings;
    procedure SetRepresentations(Value: TStrings);
    function StoreRepresentations: Boolean;
    procedure SetOnSubscribe(Value: TBoldSubscribe);
  protected
    class function GetExpressionAsDirectElement(Element: TBoldElement; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): TBoldElement;
    function GetRendererDataClass: TBoldRendererDataClass; virtual;
    function GetSupportsMulti: Boolean; virtual;
    procedure DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData); virtual;
    procedure DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean; virtual;
    procedure DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Aligment: TAlignment; Margins: TPoint); virtual;
    function HasEventOverrides: boolean; virtual;
    function GetDefaultRepresentationStringList: TStringList; virtual;
    property OnStartDrag: TBoldStartDrag read FOnStartDrag write FOnStartDrag;
    property OnEndDrag: TBoldEndDrag read FOnEndDrag write FOnEndDrag;
    property OnDragOver: TBoldDragOver read FOnDragOver write FOnDragOver;
    property OnDragDrop: TBoldDragDrop read FOnDragDrop write FOnDragDrop;
  public
    destructor Destroy; override;
    procedure Changed;
    function DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure DefaultHoldsChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber); virtual;
    procedure DefaultReleaseChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList;Subscriber: TBoldSubscriber); virtual;
    function MayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure HoldsChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber); virtual;
    procedure ReleaseChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber); virtual;
    procedure StartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData); virtual;
    procedure EndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure SubscribeToElement(Element: TBoldElement;  Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber; VariableList: TBoldExternalVariableList = nil);
    property RendererDataClass: TBoldRendererDataClass read GetRendererDataClass;
    property SupportsMulti: Boolean read GetSupportsMulti;
  published
    property Representations: TStrings read GetRepresentations write SetRepresentations stored StoreRepresentations;
    property OnMayModify: TBoldMayModify read FOnMayModify write FOnMayModify;
    property OnHoldsChangedValue: TBoldHoldsChangedValue read FOnHoldsChangedValue write FOnHoldsChangedValue;
    property OnReleaseChangedValue: TBoldReleaseChangedValue read FOnReleaseChangedValue write FOnReleaseChangedValue;
    property OnSubscribe: TBoldSubscribe read FOnSubscribe write SetOnSubscribe;
  end;

  { TBoldFollower }
  TBoldFollower = class(TBoldQueueable)
  private
    fIndex: Integer;
    fOwningFollower: TBoldFollower;
//    fSelected: Boolean;
    fState: TBoldFollowerState;
    fElement: TBoldElement;
    fRendererData: TBoldRendererData;
    fController: TBoldFollowerController;
    fControlData: TObject;
    FSubscriber: TBoldSubscriber;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetElementValid: Boolean;
    procedure SetElementValid(Value: Boolean);
    function GetSubFollower(index: Integer): TBoldFollower;
    function GetSubFollowerCount: Integer;
    function GetCurrentIndex: Integer;
    procedure SetCurrentIndex(index: integer);
    procedure SetElement(theElement: TBoldElement);
    procedure SetState(Value: TBoldFollowerState);
    function GetRendererData: TBoldRendererData;
    procedure CollectMatchingDownwards(Followers: TBoldObjectArray; MatchController: TBoldFollowerController);
    function GetAssertedController: TBoldFollowerController;
  protected
    procedure AddToDisplayList; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    {The following two are virtual to allow overriden in the old hierarchy}
    procedure MakeUptodateAndSubscribe; // Displays, i.e. moves from B.O. to RendereData, also resubscribes if needed
    class procedure MultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray);
    procedure MakeClean; // Applies, i.e. moves from rendererdata to B.O.
    {State handling}
    procedure MarkDirty;
    procedure MarkClean;
    procedure CollectMatching(Followers: TBoldObjectArray; MatchController: TBoldFollowerController);
  public
    constructor Create(MatchObject: TObject; Controller: TBoldFollowerController);
    constructor CreateSubFollower(
      OwningFollower: TBoldFollower;
      Controller: TBoldFollowerController;
      Element: TBoldElement);
    destructor Destroy; override;
    procedure Display; override;
    procedure Apply; override;
    procedure MarkValueOutOfDate;
    procedure MarkSubscriptionOutOfDate;
    function CheckIfInHierarchy(aElement: TBoldElement; aController: TBoldFollowerController): Boolean;
    procedure ControlledValueChanged(IsChanged: Boolean);
    procedure DiscardChange; override;
    function Displayable: Boolean;
    procedure EnsureDisplayable;
    function ExistInOwner: Boolean;
    function MayChange: Boolean;
    procedure EnsureMulti;
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
    property Selected: Boolean index befFollowerSelected read GetElementFlag write SetElementFlag;
    property SubFollowerCount: Integer read GetSubFollowerCount;
    property SubFollowers[index: Integer]: TBoldFollower read GetSubFollower;
    property Subscriber: TBoldSubscriber read fSubscriber;
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
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetRendererDataClass: TBoldRendererDataClass;
    procedure SetRepresentation(Value: TBoldRepresentation);
    procedure SetExpression(Value: string);
    procedure SetUntypedRenderer(NewRender: TBoldRenderer);
    procedure SetVariables(const Value: TBoldOclVariables);
    procedure Resubscribe;
    function GetVariableList: TBoldExternalVariableList;
    function GetSupportsMulti: Boolean;
    function HandleApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
    function HandleDisplayException(E: Exception; Elem: TBoldElement): Boolean;
  protected
    function GetOwner: TPersistent; override;
    function GetEffectiveRenderer: TBoldRenderer; virtual;
    function GetContextType: TBoldElementTypeInfo; virtual;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); virtual;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray); virtual;
    procedure Changed;
    procedure DoAssign(Source: TPersistent); virtual;
    procedure CleanRendererData(RendererData: TBoldRendererData); virtual;
    procedure MultiMakeEnsure(Followers: TBoldObjectArray);
    function GetSupportsMultiEnsure: Boolean; virtual;
    property EffectiveRenderer: TBoldRenderer read GetEffectiveRenderer;
    property RendererDataClass: TBoldRendererDataClass read GetRendererDataClass;
    property OwningComponent: TComponent read FOwningComponent;
    property InternalDrag: Boolean read FInternalDrag write FInternalDrag;
    property Representation: TBoldRepresentation read FRepresentation write SetRepresentation default brDefault;
    property Expression: TBoldExpression read FExpression write SetExpression nodefault;
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
    procedure MakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
    procedure SubscribeToElement(Element: TBoldElement; Subscriber: TBoldSubscriber);
    procedure MultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray);
    function DragOver(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); virtual;
    property ApplyPolicy: TBoldApplyPolicy read FApplyPolicy write FApplyPolicy default bapExit;
    property CleanOnEqual: Boolean read FCleanOnEqual write FCleanOnEqual default false;
    property Popup: TBoldPopup read FPopup write FPopup; {temporarily}
    property AfterMakeUptoDate: TBoldFollowerEvent read fAfterMakeUptoDate write fAfterMakeUptoDate;
    property BeforeMakeUptoDate: TBoldFollowerEvent read fBeforeMakeUptoDate write fBeforeMakeUptoDate;
    property OnGetContextType: TBoldGetContextTypeEvent read fOnGetContextType write fOnGetContextType;
    property ContextType: TBoldElementTypeInfo read GetContextType;
    property VariableList: TBoldExternalVariableList read GetVariableList;
    property SupportsMulti: Boolean read GetSupportsMulti;
    property SupportsMultiEnsure : Boolean read GetSupportsMultiEnsure;
  published
    property DragMode: TBoldDragMode read FDragMode write FDragMode default bdgNone;
    property DropMode: TBoldDropMode read FDropMode write FDropMode default bdpNone;
    //   property Popup: TBoldPopup read fPopup write fPopup;
  end;

  { TBoldSingleRenderer }
  TBoldSingleRenderer = class(TBoldRenderer)
  public
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); virtual; abstract;
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
    FEnable: Boolean; // FIXME notify owner of change here
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

{$IFDEF BOLDCOMCLIENT} // List/InterfaceArray
type
  TBoldClientableList = TBoldInterfaceArray;

function BoldTestType(element: IUnknown; TypeOrInterface: TGUID): Boolean;
{$ELSE}
function BoldTestType(element: TObject; TypeOrInterface: TClass): Boolean;

type
  TBoldClientableList = TBoldObjectArray;
{$ENDIF}

implementation

uses
  BoldUtils,
  BoldExceptionHandlers,
{$IFNDEF BOLDCOMCLIENT} // uses
  BoldSystem,
  BoldSystemRT,
  BoldQGUI,
{$ENDIF}
  BoldGuiConsts,
  BoldGuard;

const
  breVariablesRemoved = 42;
  breVariablesChanged = 43;
  breRendererRemoved = 44;
  breRendererChanged = 45;
  breControllerChanged = 46;

var
  DefaultRenderer: TBoldRenderer;

{$IFDEF BOLDCOMCLIENT} // BoldTestType
function BoldTestType(element: IUnknown; TypeOrInterface: TGUID): Boolean;
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

{ TBoldFollowerController }

constructor TBoldFollowerController.Create(aOwningComponent: TComponent);
begin
  inherited Create;
  fComponentSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  FOwningComponent := aOwningComponent;
  FApplyPolicy := bapExit;
  FDragMode := bdgNone;
  FDropMode := bdpNone;
  FPopup := TBoldPopup.Create;
  FRepresentation := brDefault;
end;

destructor TBoldFollowerController.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  FreeAndNil(FPopup);
  inherited;
end;

procedure TBoldFollowerController.Assign(Source: TPersistent);
begin
  if Source is ClassType then
    DoAssign(Source)
  else
    inherited Assign(Source);
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

procedure TBoldFollowerController.MakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  if Assigned(BeforeMakeUptoDate) then
    BeforeMakeUptoDate(Follower);
  try
    DoMakeUptodateAndSubscribe(Follower, Subscribe);
  finally
    if Assigned(AfterMakeUptoDate) then
      AfterMakeUptoDate(Follower);
  end;
end;

procedure TBoldFollowerController.MakeClean(Follower: TBoldFollower);
begin
  raise EBoldInternal.CreateFmt('%s.MakeClean not implemented', [ClassName]);
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
  Follower.RendererData.MayModify := EffectiveRenderer.MayModify(Follower.Element, Representation, Expression, GetVariableListAndSubscribe(Follower.Subscriber), Follower.Subscriber);
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
begin
  {$IFNDEF BOLDCOMCLIENT}
  if BoldGUIHandler.ActivateTargetFormOnDrop then
  begin
    Success := false;
    if assigned(OwningComponent) then
      Success := BoldGUIHandler.TryToFocusHostingForm(OwningComponent);
    if not success and (Follower.MatchObject is TComponent) then
      BoldGUIHandler.TryToFocusHostingForm(Follower.MatchObject as TComponent);
  end;
  {$ENDIF}
  try
    EffectiveRenderer.DragDrop(ReceivingElement, DropMode, dropindex);
  except
    on E: Exception do
    begin
      if not HandleApplyException(E, ReceivingElement, Discard) then
        raise;
      if Discard then
        Follower.DiscardChange;
    end;
  end;
end;

procedure TBoldFollowerController.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  EffectiveRenderer.DrawOnCanvas(Follower, Canvas, Rect, Alignment, Margins);
end;

function TBoldFollowerController.MayModify(Follower: TBoldFollower): Boolean;
begin
  Result := Follower.RendererData.MayModify;
end;

procedure TBoldFollowerController.SetRepresentation(Value: TBoldRepresentation);
begin
  if Value <> Representation then
  begin
    FRepresentation := Value;
    Changed;
  end;
end;

procedure TBoldFollowerController.SetExpression(Value: string);
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
  EffectiveRenderer.HoldsChangedValue(Follower.Element, Representation, Expression, VariableList, Follower.Subscriber);
end;

procedure TBoldFollowerController.ReleaseChangedValue(Follower: TBoldFollower);
begin
  EffectiveRenderer.ReleaseChangedValue(Follower.Element, Representation, Expression, VariableList, Follower.Subscriber);
end;

{ TBoldSingleFollowerController }
procedure TBoldSingleFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  with EffectiveRenderer as TBoldSingleRenderer do
  begin
    if Subscribe then
      MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, Follower.Subscriber)
    else
      MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, nil);
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
  // Don't store the stringlist if it's empty or if it's equal to the default representation stringlist.
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
    {$IFDEF BOLDCOMCLIENT} // GetAsDirectElement // FIXME: VariableList is lost
    Result := Element.EvaluateExpression(Expression);
    {$ELSE}
    Result := Element.EvaluateExpressionAsDirectElement(Expression, VariableList);
    {$ENDIF}
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

function TBoldRenderer.DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    {$IFDEF BOLDCOMCLIENT}  // DefaultMayModify // fixme
    result := ValueElement.mutable
    {$ELSE}
    Result := ValueElement.ObserverMayModify(Subscriber)
    {$ENDIF}
  else
    Result := False;
end;

procedure TBoldRenderer.DefaultHoldsChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber);
{$IFNDEF BOLDCOMCLIENT} // DefaultHoldsChangedValue
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    ValueElement.RegisterModifiedValueHolder(Subscriber)
  else
    raise EBold.CreateFmt('%s.DefaultHoldsChangedValue: Can''t Modify Value', [ClassName]);
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TBoldRenderer.DefaultReleaseChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber);
{$IFNDEF BOLDCOMCLIENT} // defaultReleaseChangedValue
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    ValueElement.UnRegisterModifiedValueHolder(Subscriber)
end;
{$ELSE}
begin
end;
{$ENDIF}

function TBoldRenderer.MayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean;
begin
  if Assigned(FOnMayModify) and
    Assigned(Element) then
    Result := OnMayModify(Element, Representation, Expression, Subscriber)
  else if HasEventOverrides then
    // this forces readonly of renderers that has an OnSubscribeEvent but no OnMayModify
    // OnMayModify is mandatory for a writeable renderer.
    result := false
  else
    Result := DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber)
end;

function TBoldRenderer.GetSupportsMulti: Boolean;
begin
  Result := false;
end;

procedure TBoldRenderer.HoldsChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber);
begin
  if Assigned(FOnHoldsChangedValue) then
    OnHoldsChangedValue(Element, Representation, Expression, Subscriber)
  else
    DefaultHoldsChangedValue(Element, Representation, Expression, VariableList, Subscriber)
end;

procedure TBoldRenderer.ReleaseChangedValue(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber);
begin
  if Assigned(FOnReleaseChangedValue) then
    OnReleaseChangedValue(Element, Representation, Expression, Subscriber)
  else
    DefaultReleaseChangedValue(Element, Representation, Expression, VariableList, Subscriber)
end;

procedure TBoldRenderer.DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData);
{$IFNDEF BOLDCOMCLIENT} // DragDrop
var
  Obj: TBoldObject;
{$ENDIF}
begin
  {$IFNDEF BOLDCOMCLIENT} // DragDrop
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

procedure TBoldRenderer.DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean);
begin
  {$IFNDEF BOLDCOMCLIENT} // dragdrop
  BoldGUIHandler.DraggedObjects.Clear;
  {$ENDIF}
end;

function TBoldRenderer.DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean;
begin
  {$IFDEF BOLDCOMCLIENT} // dragdrop
  result := false;
  {$ELSE}
  Result := Assigned(Element) and Element.ObserverMayModify(Self) and
    (BoldGUIHandler.DraggedObjects.Count > 0) and
    BoldGUIHandler.DraggedObjectsAssignable(Element, DropMode);
  {$ENDIF}
end;

procedure TBoldRenderer.DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
{$IFNDEF BOLDCOMCLIENT} // dragdrop
var
  i: integer;
  offset,
  prevIndex: integer;
  BoldObject: TBoldObject;
  TheLink: TBoldObjectReference;
  TheList: TBoldObjectList;
begin
  if element is TBoldObjectReference then
  begin
    TheLink := Element as TBoldObjectReference;
    with BoldGUIHandler.DraggedObjects do
      if Count = 0 then
        BoldObject := nil
      else if Count = 1 then
        BoldObject := BoldObjects[0]
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
        for i := 0 to BoldGUIHandler.DraggedObjects.Count - 1 do
          if TheList.IndexOf(BoldGUIHandler.DraggedObjects[I]) = -1 then
            TheList.Add(BoldGUIHandler.DraggedObjects[i]);
      bdpReplace:
        raise EBoldFeatureNotImplementedYet.CreateFmt('%s.DefaultDragDrop: Replace not implemented yet', [ClassName]);
      bdpInsert:
      begin
        if dropindex < 0
          then dropindex := 0;
        for I := 0 to BoldGUIHandler.DraggedObjects.Count - 1 do
        begin
          prevIndex := TheList.IndexOf(BoldGUIHandler.DraggedObjects[I]);
          Offset := 0;
          if prevIndex = -1 then
          begin
            if dropindex < TheList.Count then
              TheList.Insert(dropindex + Offset, BoldGUIHandler.DraggedObjects[I])
            else
              TheList.Add(BoldGUIHandler.DraggedObjects[I]);
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

procedure TBoldRenderer.SubscribeToElement(Element: TBoldElement;  Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber; VariableList: TBoldExternalVariableList = nil);
begin
  if assigned(fOnSubscribe) then
    fOnSubscribe(element, representation, expression, subscriber)
  else if assigned(Element) then
  {$IFDEF BOLDCOMCLIENT}
    Element.SubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
  {$ELSE}
    Element.SubscribeToExpression(expression, subscriber, false, false, variableList);
  {$ENDIF}
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

function TBoldRenderer.HasEventOverrides: boolean;
begin
  result := assigned(fOnSubscribe);
end;

{ TBoldFollower }

constructor TBoldFollower.Create(MatchObject: TObject; Controller: TBoldFollowerController);
begin
  inherited Create(MatchObject);
  Assert(assigned(Controller));
  fController := Controller;
  fSubscriber := TBoldFollowerSubscriber.Create(Self);
  fIndex := -1;
end;

constructor TBoldFollower.CreateSubFollower(OwningFollower: TBoldFollower;
    Controller: TBoldFollowerController;
    Element: TBoldElement);
begin
  inherited Create(OwningFollower.MatchObject);
  FOwningFollower := OwningFollower;
  PrioritizedQueuable := OwningFollower;
  Assert(assigned(Controller));
  fController := Controller;
  fSubscriber := TBoldFollowerSubscriber.Create(Self);
  fElement := Element;
  fIndex := -1;
  MarkSubscriptionOutOfDate;
end;

destructor TBoldFollower.Destroy;
begin
  FreeAndNil(FRendererData);
  FreeAndNil(fSubscriber);
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

function TBoldFollower.GetSubFollower(index: Integer): TBoldFollower;
begin
  Result := RendererData.GetSubFollower(index);
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

function TBoldFollower.GetActive: Boolean;
begin
  Result := not (State in [bfsInactiveValidElement, bfsInactiveInvalidElement]);
end;

procedure TBoldFollower.SetActive(Value: Boolean);
begin
  if (Value <> GetActive) then
  begin
    if Value then
    begin
      Assert(State <> bfsInactiveInvalidElement);
      SetState(bfsActivating);
      MakeUptodateAndSubscribe; //CHECKME This could cause errors if an owning follower is in bfsOutOfDate
      MarkClean;
    end
    else
      if (State <> bfsDirty) then
        SetState(bfsInactiveValidElement);
  end;
end;

function TBoldFollower.GetElementValid: Boolean;
begin
  Result := State <> bfsInactiveInValidElement;
end;

procedure TBoldFollower.SetElementValid(Value: Boolean);
begin
   Assert(not Active or value);
   if value <> elementValid then
   begin
     if value then
       SetState(bfsInactivevalidElement)
     else
       SetState(bfsInactiveInvalidElement);
   end;
end;

function TBoldFollower.GetRendererData: TBoldRendererData;
begin
  if not Active then
    raise EBold.Create(SBoldInactiveFollowerNoRenderData);
  if not Assigned(FRendererData) then
    FRendererData := AssertedController.RendererDataClass.Create(Self);
  Result := FRendererData;
end;

function TBoldFollower.MayChange: Boolean;
begin
  if State = bfsDirty then
    raise EBold.Create(SCannotChangeStateWithModifiedValue)
  else
    Result := True;
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
{$IFNDEF BOLDCOMCLIENT} // CHECKME
  if (OriginalEvent = beDestroying) and (Originator = Element) then
    FElement := nil; //CHECKME Is this necesary? /frha
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
          if assigned(fRendererData) then
            AssertedController.CleanRendererData(fRendererData);
          FreeAndNil(fRendererData);
          fController := nil;
        end;
        if Assigned(FRendererData) and not (FRendererData is AssertedController.RendererDataClass) then
          FreeAndNil(fRendererData);
        MarkSubscriptionOutOfDate;
      end;
    else
      raise EBoldInternal.CreateFmt('%s.Receive: Unknown RequestedEvent (%d)', [Classname, RequestedEvent]);
  end;
end;

procedure TBoldFollower.MakeUptodateAndSubscribe;
begin
  if State in bfdNeedResubscribe then
  begin
    Subscriber.CancelAllSubscriptions;
    AssertedController.AddSmallSubscription(Subscriber, [beValueChanged, beDestroying], breControllerChanged);
  end;
  AssertedController.MakeUptodateAndSubscribe(Self, State in bfdNeedResubscribe);
end;

procedure TBoldFollower.MakeClean;
begin
  AssertedController.MakeClean(Self);
  if State = bfsDirty then
    SetState(bfsSubscriptionOutOfDate);
end;

procedure TBoldFollower.ControlledValueChanged(IsChanged: Boolean);
begin
  if IsChanged then
  begin
    if (State <> bfsDirty) then
    begin
      AssertedController.HoldsChangedValue(Self);
      MarkDirty;
      if AssertedController.ApplyPolicy = bapChange then
        Apply;
    end
  end
  else
  begin
    if (State = bfsDirty) and AssertedController.CleanOnEqual then
      DiscardChange;
  end;
end;

procedure TBoldFollower.SetElement(theElement: TBoldElement);
begin
  // if the fElement is nil and the new element is nil aswell we still need
  // to mark the follower out of date since other properties of the controller
  // might have changed (especially the nilstringrepresentation)
  if not assigned(theElement) or (theElement <> fElement) then
  begin
    fElement := theElement;
    ElementValid := true;
    MarkSubscriptionOutOfDate;
  end;
end;

procedure TBoldFollower.MarkValueOutOfDate;
begin
  case State of
    bfsEmpty, bfsCurrent, bfsDirty:
      SetState(bfsValueOutOfDate);
    bfsInactiveValidElement, bfsValueOutOfDate,
    bfsSubscriptionOutOfDate, bfsActivating: // FIXME bfsActivating is a temporary fix for the delayd fetch problem
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

    // these two should not happen, but it is safe to ignore them
    // a bug in the grid seems to cause these when the grid is not displayed
    // right after creation (if it is on an invisible pagecontrol)
    bfsInactiveValidElement, bfsInactiveInvalidElement:
    begin
      // DebugCode below - can safely be removed
      SetState(State);{no action}
    end
  else
    raise EBoldInternal.CreateFmt('%s.MarkSubscriptionOutOfDate: Follower state error', [ClassName]);
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

procedure TBoldFollower.MarkClean;
begin
  SetState(bfsCurrent);
end;

function TBoldFollower.Displayable: Boolean;
begin
  Result := State in bfsDisplayable;
end;

procedure TBoldFollower.SetState(Value: TBoldFollowerState);
begin
  {action when leaving state}
  case State of
    bfsValueOutOfDate, bfsSubscriptionOutOfDate: {bfsOutOfDate}
      RemoveFromDisplayList;
    bfsDirty:
      RemoveFromApplyList;
  end;

  fState := Value;

  {action when entering state}
  case State of
    bfsValueOutOfDate:
      AddToDisplayList;
    bfsSubscriptionOutOfDate:
      begin
        AddToDisplayList;
        Subscriber.CancelAllSubscriptions;
      end;
    bfsDirty:
      begin
        AssertedController.HoldsChangedValue(Self);
        AddToApplyList;
      end;
    bfsInactiveValidElement, bfsInactiveInvalidElement:
      begin
        Subscriber.CancelAllSubscriptions;
        FreeAndNil(fRendererData);
      end;
    bfsEmpty, bfsCurrent, bfsActivating:
      {no action}
    else
      raise EBoldInternal.CreateFmt('%s: Unknown FollowerState', [ClassName]);
  end;
end;

procedure TBoldFollower.Display;
  procedure DisplaySelf;
  begin
    try
      if (MostPrioritizedQueuable <> nil) then
        raise EBold.CreateFmt('%s.Display: Can not display because there is an owning follower that must be displayed before', [ClassName]);
      MakeUptodateAndSubscribe;
    finally
      MarkClean;
    end;
  end;

  procedure DisplayMulti;
  var
    i: integer;
    Followers: TBoldObjectArray;
    BoldGuard: IBoldGuard;
  begin
    BoldGuard := TBoldGuard.Create(Followers);
    Followers := TBoldObjectArray.Create(10, []);
    CollectMatching(Followers, Controller);
    if Followers.Count > 1 then
      try
        MultiMakeUptodateAndSubscribe(Followers);
      finally
        for i := 0 to Followers.Count - 1 do
          TBoldFollower(Followers[i]).MarkClean;
      end
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
      if assigned(Controller) and Controller.HandleDisplayException(E, Element) then
      // don't re-raise
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
  Followers: TBoldObjectArray;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Followers);
  Followers := TBoldObjectArray.Create(10, []);
  CollectMatching(Followers, Controller);

  if Followers.Count > 1 then
  try
    Controller.MultiMakeEnsure(Followers);
  except
    ; // silence any exceptions
  end
end;

procedure TBoldFollower.EnsureDisplayable;
//EnsureDisplayable may only be called when within Display or when ALL owning followers not is in bfsOutOfDate!
begin
  if not Displayable then
  begin
    Active := True;
    MakeUptodateAndSubscribe;
    MarkClean;
  end;
  RendererData.EnsureSubfollowersDisplayable;
end;

procedure TBoldFollower.Apply;
var
  Discard: Boolean;
begin
  if State = bfsDirty then
  begin
    try
      AssertedController.ReleaseChangedValue(Self);
      MakeClean;
    except
      on E: Exception do
        begin
          if (not assigned(Controller)) or (not Controller.HandleApplyException(E, Element, Discard)) then
            raise;
          if Discard then
            DiscardChange;
        end;
    end;
  end;
end;

{ TBoldPopUp }
function TBoldPopup.GetMenu(CONTROL: TControl; Element: TBoldElement): TPopupMenu;
begin
  Result := nil;
  {$IFNDEF BOLDCOMCLIENT} // popup
  BoldGUIHandler.PopupElement := Element;
  BoldGUIHandler.PopupControl := CONTROL;
  // fixme build actual menu
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
  fFollower := Follower;
end;

function TBoldFollowerSubscriber.GetContextString: string;
begin
    Result := Follower.AssertedController.Getnamepath
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
  // just ignore;
end;

function TBoldFollowerController.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(OnGetContextType) then
    result := OnGetContextType
  else
    result := nil;
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

procedure TBoldFollowerController.SetVariables(
  const Value: TBoldOclVariables);
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
    Variables.SubscribeToHandles(Subscriber);
  {$ENDIF}
end;

procedure TBoldFollower.CollectMatching(Followers: TBoldObjectArray;
  MatchController: TBoldFollowerController);
begin
  if Assigned(fOwningFollower) then
    fOwningFollower.CollectMatching(Followers, MatchController)
  else
    CollectMatchingDownwards(Followers, MatchController);
end;

procedure TBoldFollower.CollectMatchingDownwards(Followers: TBoldObjectArray;
  MatchController: TBoldFollowerController);
var
  I: integer;
begin
  if (Controller = MatchController) and Assigned(Element) and
    (State in bfsOutOfDate) and (MostPrioritizedQueuable = nil) then
    Followers.Add(Self);
  if not (state in bfsOutOfDate) then
  begin
    for I := 0 to SubfollowerCount - 1 do
       SubFollowers[I].CollectMatchingDownwards(Followers, MatchController);
  end;
end;

function TBoldFollowerController.GetSupportsMulti: Boolean;
begin
  Result := EffectiveRenderer.SupportsMulti;
end;

class procedure TBoldFollower.MultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  I: integer;
  Controller: TBoldFollowerController;
begin
  Assert(Followers.Count > 0);
  Controller :=  TBoldFollower(Followers[0]).AssertedController;
  for I := 0 to Followers.Count - 1 do
    if TBoldFollower(Followers[i]).State in bfdNeedResubscribe then
    begin
      TBoldFollower(Followers[i]).Subscriber.CancelAllSubscriptions;  // CHECKME ever needed?
      Controller.AddSmallSubscription(TBoldFollower(Followers[i]).Subscriber, [beValueChanged, beDestroying], breControllerChanged);
    end;
  Controller.MultiMakeUptodateAndSubscribe(Followers);
end;

procedure TBoldFollowerController.MultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  I: integer;
begin
  if Assigned(BeforeMakeUptoDate) then
    for i := 0 to Followers.Count - 1  do
      BeforeMakeUptoDate(TBoldFollower(Followers[I]));
  try
    DoMultiMakeUptodateAndSubscribe(Followers);
  finally
    if Assigned(AfterMakeUptoDate) then
      for i := 0 to Followers.Count - 1  do
      AfterMakeUptoDate(TBoldFollower(Followers[I]));
  end;
end;

procedure TBoldFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
begin
  raise EBoldInternal.Create('DoMultiMakeUptodateAndSubscribe: called when Multi not supported');
end;

procedure TBoldFollowerSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Follower.Receive(Originator, OriginalEvent, RequestedEvent);
end;

function TBoldFollower.GetAssertedController: TBoldFollowerController;
begin
  if not assigned(fController) then
    raise EBold.CreateFmt('%s.GetAssertedController: Controller not assigned', [classname]);
  result := fController;
end;

procedure TBoldFollowerController.SubscribeToElement(Element: TBoldElement; Subscriber: TBoldSubscriber);
begin
  EffectiveRenderer.SubscribeToElement(Element, representation, Expression, Subscriber, VariableList);
end;

function TBoldFollowerController.HandleApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
var
  ExceptionHandler: TBoldExceptionHandler;
begin
  Discard := False;
  ExceptionHandler := TBoldExceptionHandler.FindExceptionHandler(fOwningComponent);
  Result := assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleApplyException(E, fOwningComponent, Elem, Discard, Result);
end;

procedure TBoldFollowerController.CleanRendererData(RendererData: TBoldRendererData);
begin
  // do nothing
end;

function TBoldFollowerController.HandleDisplayException(E: Exception;
  Elem: TBoldElement): Boolean;
var
  ExceptionHandler: TBoldExceptionHandler;
begin
  ExceptionHandler := TBoldExceptionHandler.FindExceptionHandler(fOwningComponent);
  Result := Assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleDisplayException(E, fOwningComponent, Elem, Result);
end;

procedure TBoldFollowerController.MultiMakeEnsure(Followers: TBoldObjectArray);
{$IFNDEF BOLDCOMCLIENT}
var
  BoldType: TBoldClassTypeInfo;
  ListType: TBoldListTypeInfo;
  ObjectList: TBoldObjectList;
  RealObjectList: TBoldObjectLIst;
  ie: TBoldIndirectElement;
  i: integer;
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
  BoldGuard := TBoldGuard.Create(ie);
  ObjectList := TBoldObjectList.Create;
  BoldType := nil;
  for i := 0 to Followers.Count - 1 do
  begin
    Follower := TBoldFollower(Followers[i]);
    if Follower.element is TBoldObject then
    begin
      AddObject(Follower.element as TBoldObject);
    end;
  end;

  if assigned(BoldType) then
  begin
    ObjectList.EnsureObjects;
    if (ObjectList.Count > 1) and (Expression <> '') then
    begin
      ListType := BoldType.SystemTypeInfo.ListTypeInfoByElement[BoldType];
      RealObjectlist := TBoldMemberFactory.CreateMemberFromBoldType(ListType) as TBoldObjectList;
      RealObjectList.AddList(ObjectList);

      ie := TBoldIndirectElement.Create;
      RealObjectList.EvaluateExpression(Expression, ie);
    end;
  end;
end;

{$ELSE}
begin
end;
{$ENDIF}

function TBoldFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  result := false;
end;

procedure TBoldFollower.AddToDisplayList;
begin
  if Assigned(Controller) then
    inherited AddToDisplayList;
end;

initialization
  DefaultRenderer := TBoldRenderer.Create(nil);

finalization
  FreeAndNil(DefaultRenderer);

end.

