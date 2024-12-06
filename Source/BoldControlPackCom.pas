
{ Global compiler directives }
{$include bold.inc}
unit BoldControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

interface

uses
  Classes,
  SysUtils,
  Controls,
  {$IFDEF BOLD_DELPH6_OR_LATER}
  Types,
  {$ENDIF}
  Menus,
  Graphics,
  Windows,
  BoldDefs,
  BoldControlPackDefs,
  BoldBase,
  BoldContainers,
  BoldSubscription,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldQueue,
  BoldOclVariables;

type
  {Forward declarations of all classes}
  TBoldFollowerDataCom = class;
  TBoldRendererCom = class;
  TBoldFollowerCom = class;
  TBoldFollowerSubscriberCom = class;
  TBoldFollowerControllerCom = class;
  TBoldSingleFollowerControllerCom = class;
  TBoldSingleRendererCom = class;
  TBoldPopupCom = class;

  TBoldRendererDataClassCom = class of TBoldFollowerDataCom;

  TBoldFollowerEventCom = procedure (Follower: TBoldFollowerCom) of object;
  TBoldGetContextTypeEventCom = function: IBoldElementTypeInfo of object;
  TBoldSubFollowerEventCom = procedure (index: Integer; OwningFollower: TBoldFollowerCom) of object;

  { TBoldFollowerSubscriberCom }
  TBoldFollowerSubscriberCom = class(TBoldComClientSubscriber)
  private
    fFollower: TBoldFollowerCom;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    function GetContextString: string; override;
  public
    constructor Create(follower: TBoldFollowerCom);
    property Follower: TBoldFollowerCom read fFollower;
  end;

  { TBoldFollowerDataCom }
  TBoldFollowerDataCom = class(TBoldMemoryManagedObject)
  private
    fOwningFollower: TBoldFollowerCom;
    fMayModify: Boolean;
  protected
    procedure EnsureSubfollowersDisplayable; virtual;
    function GetSubFollowerCount: Integer; virtual;
    function GetSubFollower(index: Integer): TBoldFollowerCom; virtual;
    function GetCurrentSubFollowerIndex: Integer; virtual;
    procedure SetCurrentSubFollowerIndex(index:integer); virtual;
  public
    constructor Create(OwningFollower: TBoldFollowerCom); virtual;
    property OwningFollower: TBoldFollowerCom read fOwningFollower;
    property MayModify: Boolean read fMayModify write fMayModify;
  end;

  { TBoldRendererCom }
  TBoldMayModifyCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber): Boolean of object;
  TBoldHoldsChangedValueCom = procedure (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber) of object;
  TBoldReleaseChangedValueCom = procedure (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber) of object;
  TBoldSubscribeCom = procedure (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber) of object;

  TBoldStartDragCom = procedure (Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom) of object;
  TBoldEndDragCom = procedure (DragMode: TBoldDragMode; InternalDrag: Boolean) of object;
  TBoldDragOverCom = function (Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean of object;
  TBoldDragDropCom = procedure (Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer) of object;

  TBoldRendererCom = class(TBoldSubscribableComponent)
  private
    FOnStartDrag: TBoldStartDragCom;
    FOnEndDrag: TBoldEndDragCom;
    FOnDragOver: TBoldDragOverCom;
    FOnDragDrop: TBoldDragDropCom;
    FOnMayModify: TBoldMayModifyCom;
    FOnHoldsChangedValue: TBoldHoldsChangedValueCom;
    FOnReleaseChangedValue: TBoldReleaseChangedValueCom;
    FOnSubscribe: TBoldSubscribeCom;
    FRepresentations: TStringList;
    function GetRepresentations: TStrings;
    procedure SetRepresentations(Value: TStrings);
    function StoreRepresentations: Boolean;
    procedure SetOnSubscribe(Value: TBoldSubscribeCom);
  protected
    class function GetExpressionAsDirectElement(Element: IBoldElement; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): IBoldElement;
    function GetRendererDataClass: TBoldRendererDataClassCom; virtual;
    function GetSupportsMulti: Boolean; virtual;
    procedure DefaultStartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom); virtual;
    procedure DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DefaultDragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean; virtual;
    procedure DefaultDragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Aligment: TAlignment; Margins: TPoint); virtual;
    function HasEventOverrides: boolean; virtual;
    function GetDefaultRepresentationStringList: TStringList; virtual;
    property OnStartDrag: TBoldStartDragCom read FOnStartDrag write FOnStartDrag;
    property OnEndDrag: TBoldEndDragCom read FOnEndDrag write FOnEndDrag;
    property OnDragOver: TBoldDragOverCom read FOnDragOver write FOnDragOver;
    property OnDragDrop: TBoldDragDropCom read FOnDragDrop write FOnDragDrop;
  public
    destructor Destroy; override;
    procedure Changed;
    function DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean; virtual;
    procedure DefaultHoldsChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber); virtual;
    procedure DefaultReleaseChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList;Subscriber: TBoldComClientSubscriber); virtual;
    function MayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean; virtual;
    procedure HoldsChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber); virtual;
    procedure ReleaseChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber); virtual;
    procedure StartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom); virtual;
    procedure EndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean); virtual;
    function DragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer); virtual;
    procedure SubscribeToElement(Element: IBoldElement;  Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber; VariableList: IBoldExternalVariableList = nil);
    property RendererDataClass: TBoldRendererDataClassCom read GetRendererDataClass;
    property SupportsMulti: Boolean read GetSupportsMulti;
  published
    property Representations: TStrings read GetRepresentations write SetRepresentations stored StoreRepresentations;
    property OnMayModify: TBoldMayModifyCom read FOnMayModify write FOnMayModify;
    property OnHoldsChangedValue: TBoldHoldsChangedValueCom read FOnHoldsChangedValue write FOnHoldsChangedValue;
    property OnReleaseChangedValue: TBoldReleaseChangedValueCom read FOnReleaseChangedValue write FOnReleaseChangedValue;
    property OnSubscribe: TBoldSubscribeCom read FOnSubscribe write SetOnSubscribe;
  end;

  { TBoldFollowerCom }
  TBoldFollowerCom = class(TBoldQueueable)
  private
    fIndex: Integer;
    fOwningFollower: TBoldFollowerCom;
    fState: TBoldFollowerState;
    fElement: IBoldElement;
    fRendererData: TBoldFollowerDataCom;
    fController: TBoldFollowerControllerCom;
    fControlData: TObject;
    FSubscriber: TBoldComClientSubscriber;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetElementValid: Boolean;
    procedure SetElementValid(Value: Boolean);
    function GetSubFollower(index: Integer): TBoldFollowerCom;
    function GetSubFollowerCount: Integer;
    function GetCurrentIndex: Integer;
    procedure SetCurrentIndex(index: integer);
    procedure SetElement(theElement: IBoldElement);
    procedure SetState(Value: TBoldFollowerState);
    function GetRendererData: TBoldFollowerDataCom;
    procedure CollectMatchingDownwards(Followers: TBoldObjectArray; MatchController: TBoldFollowerControllerCom);
    function GetAssertedController: TBoldFollowerControllerCom;
  protected
    procedure AddToDisplayList; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    {The following two are virtual to allow overriden in the old hierarchy}
    procedure MakeUptodateAndSubscribe;
    class procedure MultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray);
    procedure MakeClean;
    {State handling}
    procedure MarkDirty;
    procedure MarkClean;
    procedure CollectMatching(Followers: TBoldObjectArray; MatchController: TBoldFollowerControllerCom);
  public
    constructor Create(MatchObject: TObject; Controller: TBoldFollowerControllerCom);
    constructor CreateSubFollower(
      OwningFollower: TBoldFollowerCom;
      Controller: TBoldFollowerControllerCom;
      Element: IBoldElement);
    destructor Destroy; override;
    procedure Display; override;
    procedure Apply; override;
    procedure MarkValueOutOfDate;
    procedure MarkSubscriptionOutOfDate;
    function CheckIfInHierarchy(aElement: IBoldElement; aController: TBoldFollowerControllerCom): Boolean;
    procedure ControlledValueChanged(IsChanged: Boolean);
    procedure DiscardChange; override;
    function Displayable: Boolean;
    procedure EnsureDisplayable;
    function ExistInOwner: Boolean;
    function MayChange: Boolean;
    procedure EnsureMulti;
    property Active: Boolean read GetActive write SetActive;
    property ElementValid: Boolean read GetElementValid write SetElementValid;
    property Controller: TBoldFollowerControllerCom read fController;
    property AssertedController: TBoldFollowerControllerCom read GetAssertedController;
    property ControlData: TObject read FControlData write FControlData;
    property CurrentIndex: Integer read GetCurrentIndex write SetCurrentIndex;
    property Element: IBoldElement read FElement write SetElement;
    property index: Integer read fIndex write fIndex;
    property OwningFollower: TBoldFollowerCom read FOwningFollower;
    property RendererData: TBoldFollowerDataCom read GetRendererData;
    property State: TBoldFollowerState read fState;
    property Selected: Boolean index befFollowerSelected read GetElementFlag write SetElementFlag;
    property SubFollowerCount: Integer read GetSubFollowerCount;
    property SubFollowers[index: Integer]: TBoldFollowerCom read GetSubFollower;
    property Subscriber: TBoldComClientSubscriber read fSubscriber;
  end;

  { TBoldFollowerControllerCom }
  TBoldFollowerControllerCom = class(TBoldSubscribablePersistent)
  private
    fUntypedRenderer: TBoldRendererCom;
    fInternalDrag: Boolean;
    fOwningComponent: TComponent;
    fApplyPolicy: TBoldApplyPolicy;
    fDragMode: TBoldDragMode;
    fDropMode: TBoldDropMode;
    fCleanOnEqual: Boolean;
    fPopup: TBoldPopupCom;
    fRepresentation: TBoldRepresentation;
    fExpression: TBoldExpression;
    fAfterMakeUptoDate: TBoldFollowerEventCom;
    fBeforeMakeUptoDate: TBoldFollowerEventCom;
    fComponentSubscriber: TBoldComClientPassthroughSubscriber;
    FOnGetContextType: TBoldGetContextTypeEventCom;
    fVariables: TBoldOclVariables;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetRendererDataClass: TBoldRendererDataClassCom;
    procedure SetRepresentation(Value: TBoldRepresentation);
    procedure SetExpression(Value: string);
    procedure SetUntypedRenderer(NewRender: TBoldRendererCom);
    procedure SetVariables(const Value: TBoldOclVariables);
    procedure Resubscribe;
    function GetVariableList: IBoldExternalVariableList;
    function GetSupportsMulti: Boolean;
    function HandleApplyException(E: Exception; Elem: IBoldElement; var Discard: Boolean): Boolean;
    function HandleDisplayException(E: Exception; Elem: IBoldElement): Boolean;
  protected
    function GetOwner: TPersistent; override;
    function GetEffectiveRenderer: TBoldRendererCom; virtual;
    function GetContextType: IBoldElementTypeInfo; virtual;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); virtual;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray); virtual;
    procedure Changed;
    procedure DoAssign(Source: TPersistent); virtual;
    procedure CleanRendererData(RendererData: TBoldFollowerDataCom); virtual;
    procedure MultiMakeEnsure(Followers: TBoldObjectArray);
    function GetSupportsMultiEnsure: Boolean; virtual;
    property EffectiveRenderer: TBoldRendererCom read GetEffectiveRenderer;
    property RendererDataClass: TBoldRendererDataClassCom read GetRendererDataClass;
    property OwningComponent: TComponent read FOwningComponent;
    property InternalDrag: Boolean read FInternalDrag write FInternalDrag;
    property Representation: TBoldRepresentation read FRepresentation write SetRepresentation default brDefault;
    property Expression: TBoldExpression read FExpression write SetExpression nodefault;
    property UntypedRenderer: TBoldRendererCom read fUntypedRenderer write SetUntypedRenderer;
    property Variables: TBoldOclVariables read fVariables write SetVariables;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MakeClean(Follower: TBoldFollowerCom); virtual;
    function MayModify(Follower: TBoldFollowerCom): Boolean;
    procedure HoldsChangedValue(Follower: TBoldFollowerCom);
    procedure ReleaseChangedValue(Follower: TBoldFollowerCom);
    function GetVariableListAndSubscribe(Subscriber: TBoldComClientSubscriber): IBoldExternalVariableList;
    procedure StartDrag(Follower: TBoldFollowerCom);
    procedure EndDrag;
    procedure MakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
    procedure SubscribeToElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber);
    procedure MultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray);
    function DragOver(Follower: TBoldFollowerCom; ReceivingElement: IBoldElement; dropindex: Integer): Boolean; virtual;
    procedure DragDrop(Follower: TBoldFollowerCom; ReceivingElement: IBoldElement; dropindex: Integer); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); virtual;
    property ApplyPolicy: TBoldApplyPolicy read FApplyPolicy write FApplyPolicy default bapExit;
    property CleanOnEqual: Boolean read FCleanOnEqual write FCleanOnEqual default false;
    property Popup: TBoldPopupCom read FPopup write FPopup; {temporarily}
    property AfterMakeUptoDate: TBoldFollowerEventCom read fAfterMakeUptoDate write fAfterMakeUptoDate;
    property BeforeMakeUptoDate: TBoldFollowerEventCom read fBeforeMakeUptoDate write fBeforeMakeUptoDate;
    property OnGetContextType: TBoldGetContextTypeEventCom read fOnGetContextType write fOnGetContextType;
    property ContextType: IBoldElementTypeInfo read GetContextType;
    property VariableList: IBoldExternalVariableList read GetVariableList;
    property SupportsMulti: Boolean read GetSupportsMulti;
    property SupportsMultiEnsure : Boolean read GetSupportsMultiEnsure;
  published
    property DragMode: TBoldDragMode read FDragMode write FDragMode default bdgNone;
    property DropMode: TBoldDropMode read FDropMode write FDropMode default bdpNone;
  end;

  { TBoldSingleRendererCom }
  TBoldSingleRendererCom = class(TBoldRendererCom)
  public
    procedure MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); virtual; abstract;
  end;

  { TBoldSingleFollowerControllerCom }
  TBoldSingleFollowerControllerCom = class(TBoldFollowerControllerCom)
  public
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); override;
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

  { TBoldPopupCom }
  TBoldPopupCom = class(TPersistent)
  private
    FEnable: Boolean;
    FInsertNew: Boolean;
    FDelete: TBoldPopupDeleteType;
    FMove: Boolean;
  public
    function GetMenu(CONTROL: TControl; Element: IBoldElement): TPopupMenu;
    procedure Assign(Source: TPersistent); override;
  published
    property Enable: Boolean read FEnable write FEnable default False;
    property InsertNew: Boolean read FInsertNew write FInsertNew default False;
    property Delete: TBoldPopupDeleteType read FDelete write FDelete default bpdNone;
    property Move: Boolean read FMove write FMove default False;
  end;

{$IFDEF BOLDCOMCLIENT}
type
  TBoldClientableListCom = TBoldInterfaceArray;

function BoldTestType(element: IUnknown; const TypeOrInterface: TGUID): Boolean;
{$ELSE}
function BoldTestType(element: TObject; TypeOrInterface: TClass): Boolean;

type
  TBoldClientableListCom = TBoldObjectArray;
{$ENDIF}

implementation

uses
  BoldExceptionHandlersCom,
  BoldGuiResourceStringsCom,
{$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {!! DO NOT REMOVE !! BoldSystemRT ,}
  BoldGUI,
{$ENDIF}
  BoldGuard;

const
  breVariablesRemoved = 42;
  breVariablesChanged = 43;
  breRendererRemoved = 44;
  breRendererChanged = 45;
  breControllerChanged = 46;

var
  DefaultRenderer: TBoldRendererCom;

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

{ TBoldFollowerDataCom }

constructor TBoldFollowerDataCom.Create(OwningFollower: TBoldFollowerCom);
begin
  FOwningFollower := OwningFollower;
end;

function TBoldFollowerDataCom.GetSubFollowerCount: Integer;
begin
  Result := 0;
end;

function TBoldFollowerDataCom.GetSubFollower(index: Integer): TBoldFollowerCom;
begin
  raise EBold.CreateFmt('%s: This class has no subfollowers', [ClassName]);
end;

function TBoldFollowerDataCom.GetCurrentSubFollowerIndex: Integer;
begin
  Result := -1;
end;

{ TBoldFollowerControllerCom }

constructor TBoldFollowerControllerCom.Create(aOwningComponent: TComponent);
begin
  inherited Create;
  fComponentSubscriber := TBoldComClientPassthroughSubscriber.Create(_Receive);
  FOwningComponent := aOwningComponent;
  FApplyPolicy := bapExit;
  FDragMode := bdgNone;
  FDropMode := bdpNone;
  FPopup := TBoldPopupCom.Create;
  FRepresentation := brDefault;
end;

destructor TBoldFollowerControllerCom.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  FreeAndNil(FPopup);
  inherited;
end;

procedure TBoldFollowerControllerCom.Assign(Source: TPersistent);
begin
  if Source is ClassType then
    DoAssign(Source)
  else
    inherited Assign(Source);
end;

procedure TBoldFollowerControllerCom.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldFollowerControllerCom);
  DragMode := TBoldFollowerControllerCom(Source).DragMode;
  DropMode := TBoldFollowerControllerCom(Source).DropMode;
  InternalDrag := TBoldFollowerControllerCom(Source).InternalDrag;
  Representation := TBoldFollowerControllerCom(Source).Representation;
  Expression := TBoldFollowerControllerCom(Source).Expression;
  ApplyPolicy := TBoldFollowerControllerCom(Source).ApplyPolicy;
  CleanOnEqual := TBoldFollowerControllerCom(Source).CleanOnEqual;
  Popup.Assign(TBoldFollowerControllerCom(Source).Popup);
  if Assigned(TBoldFollowerControllerCom(Source).UntypedRenderer) then
    UntypedRenderer := (TBoldFollowerControllerCom(Source).UntypedRenderer);
end;

procedure TBoldFollowerControllerCom.MakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
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

procedure TBoldFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  raise EBoldInternal.CreateFmt('%s.MakeClean not implemented', [ClassName]);
end;

procedure TBoldFollowerControllerCom.Changed;
begin
  SendEvent(Self, beValueChanged);
end;

function TBoldFollowerControllerCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := EffectiveRenderer.RendererDataClass;
end;

function TBoldFollowerControllerCom.GetOwner: TPersistent;
begin
  Result := OwningComponent;
end;

function TBoldFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := DefaultRenderer;
end;

procedure TBoldFollowerControllerCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
begin
  Follower.RendererData.MayModify := EffectiveRenderer.MayModify(Follower.Element, Representation, Expression, GetVariableListAndSubscribe(Follower.Subscriber), Follower.Subscriber);
end;

procedure TBoldFollowerControllerCom.StartDrag(Follower: TBoldFollowerCom);
begin
  EffectiveRenderer.StartDrag(Follower.Element, DragMode, Follower.RendererData);
end;

procedure TBoldFollowerControllerCom.EndDrag;
begin
  EffectiveRenderer.EndDrag(DragMode, InternalDrag);
end;

function TBoldFollowerControllerCom.DragOver(Follower: TBoldFollowerCom; ReceivingElement: IBoldElement; dropindex: Integer): Boolean;
begin
  Result := EffectiveRenderer.DragOver(ReceivingElement, DropMode, InternalDrag, Follower.RendererData, dropindex);
end;

procedure TBoldFollowerControllerCom.DragDrop(Follower: TBoldFollowerCom; ReceivingElement: IBoldElement; dropindex: Integer);
var
  {$IFNDEF BOLDCOMCLIENT}
  Success: Boolean;
  {$ENDIF}
  Discard: Boolean;
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
      if not HandleApplyException(E, ReceivingElement, Discard) then
        raise;
      if Discard then
        Follower.DiscardChange;
    end;
  end;
end;

procedure TBoldFollowerControllerCom.DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  EffectiveRenderer.DrawOnCanvas(Follower, Canvas, Rect, Alignment, Margins);
end;

function TBoldFollowerControllerCom.MayModify(Follower: TBoldFollowerCom): Boolean;
begin
  Result := Follower.RendererData.MayModify;
end;

procedure TBoldFollowerControllerCom.SetRepresentation(Value: TBoldRepresentation);
begin
  if Value <> Representation then
  begin
    FRepresentation := Value;
    Changed;
  end;
end;

procedure TBoldFollowerControllerCom.SetExpression(Value: string);
begin
  if Value <> Expression then
  begin
    FExpression := Value;
    Changed;
  end;
end;

procedure TBoldFollowerControllerCom.SetUntypedRenderer(NewRender: TBoldRendererCom);
begin
  if NewRender <> UntypedRenderer then
  begin
    fUntypedRenderer := NewRender;
    Resubscribe;
    Changed;
  end;
end;

procedure TBoldFollowerControllerCom.HoldsChangedValue(Follower: TBoldFollowerCom);
begin
  EffectiveRenderer.HoldsChangedValue(Follower.Element, Representation, Expression, VariableList, Follower.Subscriber);
end;

procedure TBoldFollowerControllerCom.ReleaseChangedValue(Follower: TBoldFollowerCom);
begin
  EffectiveRenderer.ReleaseChangedValue(Follower.Element, Representation, Expression, VariableList, Follower.Subscriber);
end;

{ TBoldSingleFollowerControllerCom }
procedure TBoldSingleFollowerControllerCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  with EffectiveRenderer as TBoldSingleRendererCom do
  begin
    if Subscribe then
      MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, Follower.Subscriber)
    else
      MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, nil);
  end;
end;

{ TBoldRendererCom }
destructor TBoldRendererCom.Destroy;
begin
  FreeAndNil(fRepresentations);
  inherited Destroy;
end;

function TBoldRendererCom.GetRepresentations: TStrings;
begin
  if not Assigned(FRepresentations) then
    FRepresentations := GetDefaultRepresentationStringList;
  Result := TStrings(FRepresentations);
end;

procedure TBoldRendererCom.SetRepresentations(Value: TStrings);
begin
  if not Assigned(FRepresentations) then
    FRepresentations := TStringList.Create;
  FRepresentations.Assign(Value);
end;

function TBoldRendererCom.StoreRepresentations: Boolean;
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

class function TBoldRendererCom.GetExpressionAsDirectElement(Element: IBoldElement; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): IBoldElement;
begin
  Result := nil;
  if Assigned(Element) then
    {$IFDEF BOLDCOMCLIENT}
    Result := Element.EvaluateExpression(Expression);
    {$ELSE}
    Result := Element.EvaluateExpressionAsDirectElement(Expression, VariableList);
    {$ENDIF}
end;

function TBoldRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldFollowerDataCom;
end;

function TBoldRendererCom.GetDefaultRepresentationStringList: TStringList;
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

function TBoldRendererCom.DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean;
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    {$IFDEF BOLDCOMCLIENT}
    result := ValueElement.mutable
    {$ELSE}
    Result := ValueElement.ObserverMayModify(Subscriber)
    {$ENDIF}
  else
    Result := False;
end;

procedure TBoldRendererCom.DefaultHoldsChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber);
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: IBoldElement;
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

procedure TBoldRendererCom.DefaultReleaseChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber);
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    ValueElement.UnRegisterModifiedValueHolder(Subscriber)
end;
{$ELSE}
begin
end;
{$ENDIF}

function TBoldRendererCom.MayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean;
begin
  if Assigned(FOnMayModify) and
    Assigned(Element) then
    Result := OnMayModify(Element, Representation, Expression, Subscriber)
  else if HasEventOverrides then

    result := false
  else
    Result := DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber)
end;

function TBoldRendererCom.GetSupportsMulti: Boolean;
begin
  Result := false;
end;

procedure TBoldRendererCom.HoldsChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber);
begin
  if Assigned(FOnHoldsChangedValue) then
    OnHoldsChangedValue(Element, Representation, Expression, Subscriber)
  else 
    DefaultHoldsChangedValue(Element, Representation, Expression, VariableList, Subscriber)
end;

procedure TBoldRendererCom.ReleaseChangedValue(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber);
begin
  if Assigned(FOnReleaseChangedValue) then
    OnReleaseChangedValue(Element, Representation, Expression, Subscriber)
  else
    DefaultReleaseChangedValue(Element, Representation, Expression, VariableList, Subscriber)
end;

procedure TBoldRendererCom.DefaultStartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom);
{$IFNDEF BOLDCOMCLIENT}
var
  Obj: IBoldObject;
{$ENDIF}  
begin
  {$IFNDEF BOLDCOMCLIENT}
  if BoldGUIHandler.DraggedObjects.Count <> 0 then
    raise EBold.Create(SDraggedObjectsNotCleared);

  if DragMode = bdgSelection then
  begin
    if (Element is IBoldObject) then
      Obj := element as IBoldObject
    else if (Element is IBoldObjectReference) then
      Obj := (Element as IBoldObjectReference).BoldObject
    else
      obj := nil;

    if Assigned(obj) then
      BoldGUIHandler.DraggedObjects.Add(Obj);
  end;
  {$ENDIF}
end;

procedure TBoldRendererCom.DefaultEndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean);
begin
  {$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.DraggedObjects.Clear;
  {$ENDIF}
end;

function TBoldRendererCom.DefaultDragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  result := false;
  {$ELSE}
  Result := Assigned(Element) and Element.ObserverMayModify(Self) and
    (BoldGUIHandler.DraggedObjects.Count > 0) and
    BoldGUIHandler.DraggedObjectsAssignable(Element, DropMode);
  {$ENDIF}
end;

procedure TBoldRendererCom.DefaultDragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
{$IFNDEF BOLDCOMCLIENT}
var
  i: integer;
  offset,
  prevIndex: integer;
  BoldObject: IBoldObject;
  TheLink: IBoldObjectReference;
  TheList: IBoldObjectList;
begin
  if element is IBoldObjectReference then
  begin
    TheLink := Element as IBoldObjectReference;
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
  else if element is IBoldObjectList then
  begin
    TheList := Element as IBoldObjectList;
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

procedure TBoldRendererCom.StartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom);
begin
  if Assigned(FOnStartDrag) then
    OnStartDrag(Element, DragMode, RendererData)
  else
    DefaultStartDrag(Element, DragMode, RendererData);
end;

procedure TBoldRendererCom.EndDrag(DragMode: TBoldDragMode; InternalDrag: Boolean);
begin
  if Assigned(FOnEndDrag) then
    OnEndDrag(DragMode, InternalDrag)
  else
    DefaultEndDrag(DragMode, InternalDrag);
end;

function TBoldRendererCom.DragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean;
begin
  if Assigned(FOnDragOver) then
    Result := FOnDragOver(Element, DropMode, InternalDrag, RendererData, dropindex)
  else
    Result := DefaultDragOver(Element, DropMode, InternalDrag, RendererData, dropindex)
end;

procedure TBoldRendererCom.SubscribeToElement(Element: IBoldElement;  Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldComClientSubscriber; VariableList: IBoldExternalVariableList = nil);
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

procedure TBoldRendererCom.DragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
begin
  if Assigned(FOnDragDrop) then
    OnDragDrop(Element, DropMode, dropindex)
  else
    DefaultDragDrop(Element, DropMode, dropindex)
end;

procedure TBoldRendererCom.DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Aligment: TAlignment; Margins: TPoint);
begin
end;

function TBoldRendererCom.HasEventOverrides: boolean;
begin
  result := assigned(fOnSubscribe);
end;

{ TBoldFollowerCom }

constructor TBoldFollowerCom.Create(MatchObject: TObject; Controller: TBoldFollowerControllerCom);
begin
  inherited Create(MatchObject);
  Assert(assigned(Controller));
  fController := Controller;
  fSubscriber := TBoldFollowerSubscriberCom.Create(Self);
  fIndex := -1;
end;

constructor TBoldFollowerCom.CreateSubFollower(OwningFollower: TBoldFollowerCom;
    Controller: TBoldFollowerControllerCom;
    Element: IBoldElement);
begin
  inherited Create(OwningFollower.MatchObject);
  FOwningFollower := OwningFollower;
  PrioritizedQueuable := OwningFollower;
  Assert(assigned(Controller));
  fController := Controller;
  fSubscriber := TBoldFollowerSubscriberCom.Create(Self);
  fElement := Element;
  fIndex := -1;
  MarkSubscriptionOutOfDate;
end;

destructor TBoldFollowerCom.Destroy;
begin
  FreeAndNil(FRendererData);
  FreeAndNil(fSubscriber);
  inherited;
end;

function TBoldFollowerCom.ExistInOwner: Boolean;
var
  Follower: TBoldFollowerCom;
  aElement: IBoldElement;
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

function TBoldFollowerCom.CheckIfInHierarchy(aElement: IBoldElement; aController: TBoldFollowerControllerCom): Boolean;
var
  Follower: TBoldFollowerCom;
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

function TBoldFollowerCom.GetSubFollower(index: Integer): TBoldFollowerCom;
begin
  Result := RendererData.GetSubFollower(index);
end;

function TBoldFollowerCom.GetSubFollowerCount: Integer;
begin
  if Active then
    Result := RendererData.GetSubFollowerCount
  else
    result := 0
end;

function TBoldFollowerCom.GetCurrentIndex: Integer;
begin
  Result := RendererData.GetCurrentSubFollowerIndex;
end;

function TBoldFollowerCom.GetActive: Boolean;
begin
  Result := not (State in [bfsInactiveValidElement, bfsInactiveInvalidElement]);
end;

procedure TBoldFollowerCom.SetActive(Value: Boolean);
begin
  if (Value <> GetActive) then
  begin
    if Value then
    begin
      Assert(State <> bfsInactiveInvalidElement);
      SetState(bfsActivating);
      MakeUptodateAndSubscribe;
      MarkClean;
    end
    else
      if (State <> bfsDirty) then
        SetState(bfsInactiveValidElement);
  end;
end;

function TBoldFollowerCom.GetElementValid: Boolean;
begin
  Result := State <> bfsInactiveInValidElement;
end;

procedure TBoldFollowerCom.SetElementValid(Value: Boolean);
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

function TBoldFollowerCom.GetRendererData: TBoldFollowerDataCom;
begin
  if not Active then
    raise EBold.Create(SBoldInactiveFollowerNoRenderData);
  if not Assigned(FRendererData) then
    FRendererData := AssertedController.RendererDataClass.Create(Self);
  Result := FRendererData;
end;

function TBoldFollowerCom.MayChange: Boolean;
begin
  if State = bfsDirty then
    raise EBold.Create(SCannotChangeStateWithModifiedValue)
  else
    Result := True;
end;

procedure TBoldFollowerCom.DiscardChange;
begin
  if State = bfsDirty then
  begin
    AssertedController.ReleaseChangedValue(Self);
    MarkValueOutOfDate;
  end;
end;

procedure TBoldFollowerCom.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
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

procedure TBoldFollowerCom.MakeUptodateAndSubscribe;
begin
  if State in bfdNeedResubscribe then
  begin
    Subscriber.CancelAllSubscriptions;
    AssertedController.AddSmallSubscription(Subscriber, [beValueChanged, beDestroying], breControllerChanged);
  end;
  AssertedController.MakeUptodateAndSubscribe(Self, State in bfdNeedResubscribe);
end;

procedure TBoldFollowerCom.MakeClean;
begin
  AssertedController.MakeClean(Self);
  if State = bfsDirty then
    SetState(bfsSubscriptionOutOfDate);
end;

procedure TBoldFollowerCom.ControlledValueChanged(IsChanged: Boolean);
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

procedure TBoldFollowerCom.SetElement(theElement: IBoldElement);
begin


  if not assigned(theElement) or (theElement <> fElement) then
  begin
    fElement := theElement;
    ElementValid := true;
    MarkSubscriptionOutOfDate;
  end;
end;

procedure TBoldFollowerCom.MarkValueOutOfDate;
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

procedure TBoldFollowerCom.MarkSubscriptionOutOfDate;
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

procedure TBoldFollowerCom.MarkDirty;
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

procedure TBoldFollowerCom.MarkClean;
begin
  SetState(bfsCurrent);
end;

function TBoldFollowerCom.Displayable: Boolean;
begin
  Result := State in bfsDisplayable;
end;

procedure TBoldFollowerCom.SetState(Value: TBoldFollowerState);
begin
  {action when leaving state}
  case State of
    bfsValueOutOfDate, bfsSubscriptionOutOfDate: {bfsOutOfDate}
      RemoveFromDisplayList(false);
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

procedure TBoldFollowerCom.Display;
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
          TBoldFollowerCom(Followers[i]).MarkClean;
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
      else
      begin
        if assigned(Controller) then
          E.message := Format('%s' + BOLDCRLF + 'occured when displaying component %s', [E.message, Controller.GetNamePath]);
        raise;
      end;
    end;
  end;
end;

procedure TBoldFollowerCom.EnsureMulti;
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
    ;
  end
end;

procedure TBoldFollowerCom.EnsureDisplayable;
begin
  if not Displayable then
  begin
    Active := True;
    MakeUptodateAndSubscribe;
    MarkClean;
  end;
  RendererData.EnsureSubfollowersDisplayable;
end;

procedure TBoldFollowerCom.Apply;
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

{ TBoldPopupCom }
function TBoldPopupCom.GetMenu(CONTROL: TControl; Element: IBoldElement): TPopupMenu;
begin
  Result := nil;
  {$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.PopupElement := Element;
  BoldGUIHandler.PopupControl := CONTROL;
  if not Enable then
    Result := BoldPopupMenu;
  {$ENDIF}
end;

procedure TBoldPopupCom.Assign(Source: TPersistent);
begin
  if Source is TBoldPopupCom then
  begin
    Enable := TBoldPopupCom(Source).Enable;
    InsertNew := TBoldPopupCom(Source).InsertNew;
    Delete := TBoldPopupCom(Source).Delete;
    Move := TBoldPopupCom(Source).Move;
  end
  else
    inherited Assign(Source);
end;

constructor TBoldFollowerSubscriberCom.Create(follower: TBoldFollowerCom);
begin
  inherited create;
  fFollower := Follower;
end;

function TBoldFollowerSubscriberCom.GetContextString: string;
begin
    Result := Follower.AssertedController.Getnamepath
end;

procedure TBoldFollowerControllerCom._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breRendererRemoved then
    UntypedRenderer := nil;
  if RequestedEvent = breVariablesRemoved then
    fVariables := nil;
  Changed;
end;

procedure TBoldFollowerCom.SetCurrentIndex(index: integer);
begin
  if index <> CurrentIndex then
  begin
    RendererData.SetCurrentSubFollowerIndex(index);
    MarkValueOutOfDate;
  end;
end;

procedure TBoldFollowerDataCom.SetCurrentSubFollowerIndex(index: integer);
begin
end;

function TBoldFollowerControllerCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(OnGetContextType) then
    result := OnGetContextType
  else
    result := nil;
end;

procedure TBoldRendererCom.Changed;
begin
  SendEvent(Self, beValueChanged);
end;

procedure TBoldRendererCom.SetOnSubscribe(Value: TBoldSubscribeCom);
begin
  if @fOnSubscribe <> @Value then
  begin
    FOnSubscribe := Value;
    Changed;
  end;
end;

procedure TBoldFollowerDataCom.EnsureSubfollowersDisplayable;
begin

end;

procedure TBoldFollowerControllerCom.SetVariables(const Value: TBoldOclVariables);
begin
  if Value <> fVariables then
  begin
    fVariables := Value;
    Resubscribe;
    Changed;
  end;
end;

procedure TBoldFollowerControllerCom.Resubscribe;
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

function TBoldFollowerControllerCom.GetVariableList: IBoldExternalVariableList;
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

function TBoldFollowerControllerCom.GetVariableListAndSubscribe(Subscriber: TBoldComClientSubscriber): IBoldExternalVariableList;
begin
  result := GetVariableList;
  {$IFNDEF BOLDCOMCLIENT}
  if assigned(Subscriber) and assigned(Variables) then
    Variables.SubscribeToHandles(Subscriber, Expression);
  {$ENDIF}
end;

procedure TBoldFollowerCom.CollectMatching(Followers: TBoldObjectArray;
  MatchController: TBoldFollowerControllerCom);
begin
  if Assigned(fOwningFollower) then
    fOwningFollower.CollectMatching(Followers, MatchController)
  else
    CollectMatchingDownwards(Followers, MatchController);
end;

procedure TBoldFollowerCom.CollectMatchingDownwards(Followers: TBoldObjectArray;
  MatchController: TBoldFollowerControllerCom);
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

function TBoldFollowerControllerCom.GetSupportsMulti: Boolean;
begin
  Result := EffectiveRenderer.SupportsMulti;
end;

class procedure TBoldFollowerCom.MultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  I: integer;
  Controller: TBoldFollowerControllerCom;
begin
  Assert(Followers.Count > 0);
  Controller :=  TBoldFollowerCom(Followers[0]).AssertedController;
  for I := 0 to Followers.Count - 1 do
    if TBoldFollowerCom(Followers[i]).State in bfdNeedResubscribe then
    begin
      TBoldFollowerCom(Followers[i]).Subscriber.CancelAllSubscriptions;
      Controller.AddSmallSubscription(TBoldFollowerCom(Followers[i]).Subscriber, [beValueChanged, beDestroying], breControllerChanged);
    end;
  Controller.MultiMakeUptodateAndSubscribe(Followers);
end;

procedure TBoldFollowerControllerCom.MultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  I: integer;
begin
  if Assigned(BeforeMakeUptoDate) then
    for i := 0 to Followers.Count - 1  do
      BeforeMakeUptoDate(TBoldFollowerCom(Followers[I]));
  try
    DoMultiMakeUptodateAndSubscribe(Followers);
  finally
    if Assigned(AfterMakeUptoDate) then
      for i := 0 to Followers.Count - 1  do
      AfterMakeUptoDate(TBoldFollowerCom(Followers[I]));
  end;
end;

procedure TBoldFollowerControllerCom.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
begin
  raise EBoldInternal.Create('DoMultiMakeUptodateAndSubscribe: called when Multi not supported');
end;

procedure TBoldFollowerSubscriberCom.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Follower.Receive(Originator, OriginalEvent, RequestedEvent);
end;

function TBoldFollowerCom.GetAssertedController: TBoldFollowerControllerCom;
begin
  if not assigned(fController) then
    raise EBold.CreateFmt('%s.GetAssertedController: Controller not assigned', [classname]);
  result := fController;
end;

procedure TBoldFollowerControllerCom.SubscribeToElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber);
begin
  EffectiveRenderer.SubscribeToElement(Element, representation, Expression, Subscriber, VariableList);
end;

function TBoldFollowerControllerCom.HandleApplyException(E: Exception; Elem: IBoldElement; var Discard: Boolean): Boolean;
var
  ExceptionHandler: TBoldExceptionHandlerCom;
begin
  Discard := False;
  ExceptionHandler := TBoldExceptionHandlerCom.FindExceptionHandler(fOwningComponent);
  Result := assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleApplyException(E, fOwningComponent, Elem, Discard, Result);
end;

procedure TBoldFollowerControllerCom.CleanRendererData(RendererData: TBoldFollowerDataCom);
begin
end;

function TBoldFollowerControllerCom.HandleDisplayException(E: Exception;
  Elem: IBoldElement): Boolean;
var
  ExceptionHandler: TBoldExceptionHandlerCom;
begin
  ExceptionHandler := TBoldExceptionHandlerCom.FindExceptionHandler(fOwningComponent);
  Result := Assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleDisplayException(E, fOwningComponent, Elem, Result);
end;

procedure TBoldFollowerControllerCom.MultiMakeEnsure(Followers: TBoldObjectArray);
{$IFNDEF BOLDCOMCLIENT}
var
  BoldType: IBoldClassTypeInfo;
  ListType: TBoldListTypeInfo;
  ObjectList: IBoldObjectList;
  RealObjectList: IBoldObjectList;
  ie: TBoldIndirectElement;
  i: integer;
  follower: TBoldFollowerCom;
  procedure AddObject(Obj: IBoldObject);
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
  BoldGuard := TBoldGuard.Create(ie, RealObjectList, ObjectList);
  ObjectList := IBoldObjectList.Create;
  BoldType := nil;
  for i := 0 to Followers.Count - 1 do
  begin
    Follower := TBoldFollowerCom(Followers[i]);
    if Follower.element is IBoldObject then
    begin
      AddObject(Follower.element as IBoldObject);
    end;
  end;

  if assigned(BoldType) then
  begin
    ObjectList.EnsureObjects;
    if (ObjectList.Count > 1) and (Expression <> '') then
    begin
      ListType := BoldType.SystemTypeInfo.ListTypeInfoByElement[BoldType];
      RealObjectlist := TBoldMemberFactory.CreateMemberFromBoldType(ListType) as IBoldObjectList;
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

function TBoldFollowerControllerCom.GetSupportsMultiEnsure: Boolean;
begin
  result := false;
end;

procedure TBoldFollowerCom.AddToDisplayList;
begin
  if Assigned(Controller) then
    inherited AddToDisplayList;
end;

initialization
  DefaultRenderer := TBoldRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultRenderer);

end.
