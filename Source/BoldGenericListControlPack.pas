
{ Global compiler directives }
{$include bold.inc}
unit BoldGenericListControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldElements,
  BoldControlPack,
  BoldListControlPack;

type
  { forward declarations }
  TBoldGenericAsListRenderer = class;
  TBoldGenericListController = class;
  TBoldGenericListPart = class;
  TBoldGenericListParts = class;
  TBoldFollowerListWithOwnedList = class;

  {$IFDEF BOLD_BCB}
  TGetFollowerControllerByNameEvent = procedure (const Name: string; var FollowerController: TBoldFollowerController) of object;
  TGetFollowerControllerEvent = procedure (Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent; var FollowerController: TBoldFollowerController) of object;
  {$ENDIF}

  {$IFDEF BOLD_DELPHI}
  TGetFollowerControllerByNameEvent = function (const Name: string): TBoldFollowerController of object;
  TGetFollowerControllerEvent = function (Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent): TBoldFollowerController of object;
  {$ENDIF}

  {$IFNDEF BOLDCOMCLIENT}
  TGetElementEvent = procedure (Sender: TBoldGenericListPart; Element: TBoldElement; Subscriber: TBoldSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean) of object;
  {$ENDIF}    

  { TBoldFollowerListWithOwnedList }
  TBoldFollowerListWithOwnedList = class (TBoldFollowerList)
  private
     FOwnedElements: TBoldClientableList;
   public
     constructor Create(OwningFollower: TBoldFollower); override;
     destructor Destroy; override;
     procedure AddOwnedElement(Element: TBoldElement);
     procedure FreeAllOwnedElements;
  end;

  { TBoldGenericListPart }
  TBoldGenericListPart = class(TCollectionItem)
  private
    FElementExpression: TBoldExpression;
    FControllerExpression: TBoldExpression;
    FInterpretAsList: Boolean;
    {$IFNDEF BOLDCOMCLIENT}
    FOnGetElement: TGetElementEvent;
    {$ENDIF}
    FOnGetFollowerController: TGetFollowerControllerEvent;
    fEnabled: Boolean;
    fName: String;
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher;
    function GetContextType: TBoldElementTypeInfo;

    procedure SetElementExpression(const Value: TBoldExpression);
    procedure SetControllerExpression(const Value: TBoldExpression);
    procedure SetInterpretAsList(Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    property Publisher: TBoldPublisher read GetPublisher;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$IFDEF BOLDCOMCLIENT}
    function GetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; Resubscribe: Boolean): IBoldElement;
    {$ELSE}
    procedure DefaultGetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
    procedure GetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
    {$ENDIF}
    function DefaultGetFollowerController(Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent): TBoldFollowerController;
    function GetFollowerController(Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent): TBoldFollowerController;
    property ContextType: TBoldElementTypeInfo read GetContextType;
  published
    property ElementExpression: TBoldExpression read FElementExpression write SetElementExpression nodefault;
    property ControllerExpression: TBoldExpression read FControllerExpression write SetControllerExpression nodefault;
    property InterpretAsList: Boolean read FInterpretAsList write SetInterpretAsList;
    property Name: String read fName write fName;
    property Enabled: Boolean read fEnabled write SetEnabled default true;
    {$IFNDEF BOLDCOMCLIENT}
    property OnGetElement: TGetElementEvent read FOnGetElement write FOnGetElement;
    {$ENDIF}
    property OnGetFollowerController: TGetFollowerControllerEvent read FOnGetFollowerController write FOnGetFollowerController;
  end;

  { TBoldGenericListParts }
  TBoldGenericListParts = class(TCollection)
  private
    FOwner: TBoldGenericListController;
    function GetPart(Index: Integer): TBoldGenericListPart;
    procedure SetPart(Index: Integer; Value: TBoldGenericListPart);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aOwner: TBoldGenericListController);
    function Add: TBoldGenericListPart;
    property Items[Index: Integer]: TBoldGenericListPart read GetPart write SetPart; default;
  end;

  { TBoldGenericAsListRenderer }
  TBoldGenericAsListRenderer = class(TBoldAsFollowerListRenderer)
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
  public
    procedure MakeUptodate(Follower: TBoldFollower;
                           Parts: TBoldGenericListParts;
                           GetFollowerControllerByName: TGetFollowerControllerByNameEvent);
    class function DefaultRenderer: TBoldGenericAsListRenderer;
  end;

  { TBoldGenericListController }
  TBoldGenericListController = class(TBoldAsFollowerListController)
  private
    FParts: TBoldGenericListParts;
    FGetFollowerControllerByName: TGetFollowerControllerByNameEvent;
    procedure SetParts(Value: TBoldGenericListParts);
    function GetRenderer: TBoldGenericAsListRenderer;
    procedure SetRenderer(Value: TBoldGenericAsListRenderer);
  protected
    class function PrecreateFollowers: boolean; override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    function GetEffectiveGenericAsListRenderer: TBoldGenericAsListRenderer;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    procedure DoAssign(Source: TPersistent); override;
    property EffectiveGenericAsListRenderer: TBoldGenericAsListRenderer read GetEffectiveGenericAsListRenderer;
  public
    constructor Create(aOwningComponent: TComponent;
                       GetFollowerControllerByNameFunc: TGetFollowerControllerByNameEvent);
    destructor Destroy; override;
    function FindPartByName(const Name: string): TBoldGenericListPart;
    function CanHaveSubFollowers: Boolean;
  published
    property Parts: TBoldGenericListParts read FParts write SetParts;
    property Renderer: TBoldGenericAsListRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldContainers,
  BoldControlPackDefs,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldNodeControlPack;

var
  DefaultGenericAsListRenderer: TBoldGenericAsListRenderer;

{ TBoldFollowerListWithOwnedList }

constructor TBoldFollowerListWithOwnedList.Create(OwningFollower: TBoldFollower);
begin
  inherited Create(OwningFollower);
  FOwnedElements := TBoldClientableList.Create(0, [bcoDataOwner, bcoThreadSafe]);
end;

destructor TBoldFollowerListWithOwnedList.Destroy;
begin
   FreeAndNil(FOwnedElements);
   inherited;
end;

procedure TBoldFollowerListWithOwnedList.AddOwnedElement(Element: TBoldElement);
begin
  FOwnedElements.Add(Element);
end;

procedure TBoldFollowerListWithOwnedList.FreeAllOwnedElements;
begin
  fOwnedElements.Clear;
end;

{---TBoldGenericListPart---}
constructor TBoldGenericListPart.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FControllerExpression := 'oclType.asstring->union(oclType.allsupertypes.asString)->union(''<Default>'')';
  fEnabled := true;
end;

procedure TBoldGenericListPart.Assign(Source: TPersistent);
begin
  if Source is TBoldGenericListPart then
  begin
    ElementExpression := TBoldGenericListPart(Source).ElementExpression;
    ControllerExpression := TBoldGenericListPart(Source).ControllerExpression;
    InterpretAsList := TBoldGenericListPart(Source).InterpretAsList;
    {$IFNDEF BOLDCOMCLIENT}
    OnGetElement := TBoldGenericListPart(Source).OnGetElement;
    {$ENDIF}
    OnGetFollowerController := TBoldGenericListPart(Source).OnGetFollowerController;
  end
  else
    inherited Assign(Source);
end;

procedure TBoldGenericListPart.SetElementExpression(const Value: TBoldExpression);
begin
  if Value <> ElementExpression then
  begin
    FElementExpression := Value;
    Changed(False);
  end;
end;

procedure TBoldGenericListPart.SetControllerExpression(const Value: TBoldExpression);
begin
  if Value <> ControllerExpression then
  begin
    FControllerExpression := Value;
    Changed(False);
  end;
end;

procedure TBoldGenericListPart.SetInterpretAsList(Value: Boolean);
begin
  if Value<>InterpretAsList then
  begin
    FInterpretAsList := Value;
    Changed(False);
  end;
end;

{$IFDEF BOLDCOMCLIENT}
function TBoldGenericListPart.GetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; Resubscribe: Boolean): IBoldElement;
begin
  if Assigned(Element) then
    result := Element.EvaluateAndSubscribeToExpression(ElementExpression, subscriber.ClientId, subscriber.SubscriberId, resubscribe, false)
  else
    result := nil;
end;
{$ELSE}
procedure TBoldGenericListPart.DefaultGetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
begin
  if Assigned(Element) then
    Element.EvaluateAndSubscribeToExpression(ElementExpression, Subscriber, ResultElement, Resubscribe)
  else
    ResultElement.SetOwnedValue(nil);
end;

procedure TBoldGenericListPart.GetElement(Element: TBoldElement; Subscriber: TBoldSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
begin
  if Enabled then
  begin
    if Assigned(FOnGetElement) then
      FOnGetElement(Self, Element, Subscriber, ResultElement, Resubscribe)
    else
      DefaultGetElement(Element, Subscriber, ResultElement, Resubscribe);
  end
  else
    ResultElement.SetReferenceValue(nil);

  Publisher.AddSubscription(Subscriber, beListPartEnabledChanged, breResubscribe);
end;
{$ENDIF}

function TBoldGenericListPart.DefaultGetFollowerController(Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent): TBoldFollowerController;
var
{$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
{$ELSE}
  E: TBoldIndirectElement;
  ListHolder: TBoldIndirectElement;
{$ENDIF}
  List: TBoldList;

function LoopList(List: TBoldList): TBoldFollowerController;
var
  i: integer;
begin
  result := nil;
  for i := 0 to List.Count - 1 do
  begin
    {$IFDEF BOLD_BCB}
    GetFollowerControllerByname(List.Elements[i].AsString, Result);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    Result := GetFollowerControllerByname(List.Elements[i].AsString);
    {$ENDIF}
    if Assigned(Result) then
      break;
  end;
end;

begin
  Result := nil;
  {$IFDEF BOLDCOMCLIENT}
  e := Element.EvaluateAndSubscribeToExpression(ControllerExpression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
  if Assigned(E) then
  begin
    if e.GetAsList.QueryInterface(IBoldList, List) = S_OK then
      result := LoopList(List);
  end;
  {$ELSE}
  E := TBoldIndirectElement.Create;
  ListHolder := TBoldIndirectElement.Create;
  try
    Element.EvaluateAndSubscribeToExpression(ControllerExpression, Subscriber, E, False);
    if Assigned(E.Value) then
    begin
      E.Value.GetAsList(ListHolder);
      List := TBoldList(ListHolder.Value);
      result := LoopList(List);
    end;
  finally
    FreeAndNil(E);
    FreeAndNil(ListHolder);
  end;
  {$ENDIF}
  if not Assigned(Result) then
    {$IFDEF BOLD_BCB}
    GetFollowerControllerByName(DefaultName, Result);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    Result := GetFollowerControllerByName(DefaultName);
    {$ENDIF}
end;

function TBoldGenericListPart.GetFollowerController(Element: TBoldElement; Subscriber: TBoldSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEvent): TBoldFollowerController;
begin
  try
    Result := nil;
    if Assigned(FOnGetFollowerController) then
      {$IFDEF BOLD_BCB}
      FOnGetFollowerController(Element, Subscriber, GetFollowerControllerByName, Result);
      {$ENDIF}
      {$IFDEF BOLD_DELPHI}
      Result := FOnGetFollowerController(Element, Subscriber, GetFollowerControllerByName);
      {$ENDIF}
    if not Assigned(Result) then
      Result := DefaultGetFollowerController(Element, Subscriber, GetFollowerControllerByName);
  except
    on E: Exception do
    begin
      E.message := Format('%s' + BOLDCRLF + 'occured when getting controller for component %s', [E.message, GetNamePath]);
      raise;
    end;
  end;
end;

{---TBoldGenericListParts---}
constructor TBoldGenericListParts.Create(aOwner: TBoldGenericListController);
begin
  inherited Create(TBoldGenericListPart);
  FOwner := aOwner;
end;

function TBoldGenericListParts.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBoldGenericListParts.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

function TBoldGenericListParts.GetPart(Index: Integer): TBoldGenericListPart;
begin
  Result := TBoldGenericListPart(GetItem(Index));
end;

procedure TBoldGenericListParts.SetPart(Index: Integer; Value: TBoldGenericListPart);
begin
  SetItem(Index, Value);
end;

function TBoldGenericListParts.Add: TBoldGenericListPart;
begin
  Result := TBoldGenericListPart.Create(Self);
end;

{---TBoldGenericListController---}
function TBoldGenericListController.GetRenderer: TBoldGenericAsListRenderer;
begin
  Result := (UntypedRenderer as TBoldGenericAsListRenderer);
end;

class function TBoldGenericListController.PrecreateFollowers: boolean;
begin
  result := true;
end;

procedure TBoldGenericListController.SetRenderer(Value: TBoldGenericAsListRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldGenericListController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveGenericAsListRenderer;
end;

function TBoldGenericListController.GetEffectiveGenericAsListRenderer: TBoldGenericAsListRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldGenericAsListRenderer.DefaultRenderer;
end;

constructor TBoldGenericListController.Create(aOwningComponent: TComponent;
                                              GetFollowerControllerByNameFunc: TGetFollowerControllerByNameEvent);
begin
  inherited Create(aOwningComponent);
  FGetFollowerControllerByName := GetFollowerControllerByNameFunc;
  FParts := TBoldGenericListParts.Create(Self);
end;

destructor TBoldGenericListController.Destroy;
begin
  FreePublisher;
  FreeAndNil(FParts);
  inherited Destroy;
end;

function TBoldGenericListController.CanHaveSubFollowers: Boolean;
begin
  Result := Parts.Count>0
end;

procedure TBoldGenericListController.SetParts(Value: TBoldGenericListParts);
begin
  FParts.Assign(Value);
end;

procedure TBoldGenericListController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  EffectiveGenericAsListRenderer.MakeUptodate(Follower as TBoldFollower, Parts, FGetFollowerControllerByName);
end;

{---TBoldGenericAsListRenderer---}
procedure TBoldGenericAsListRenderer.MakeUptodate(Follower: TBoldFollower;
                                                  Parts: TBoldGenericListParts;
                                                  GetFollowerControllerByName: TGetFollowerControllerByNameEvent);
var
  Controller: TBoldGenericListController;
  PartIndex: Integer;
  Part: TBoldGenericListPart;
  Element: TBoldElement;
  DestList: TBoldFollowerListWithOwnedList;
  DestListIndex: Integer;
  FollowerController: TBoldFollowerController;
  SourceList: TBoldList;
  SourceIndex: Integer;

  procedure AddNode(Element: TBoldElement);
  begin
    FollowerController := Part.GetFollowerController(Element, Follower.Subscriber, GetFollowerControllerByName);
    if Assigned(FollowerController) then
    begin
      DestList.EnsuredFollower(Controller, DestListIndex, Element, FollowerController);
      Inc(DestListIndex);
    end;
  end;

  function RetrieveElementAndCheckIfOwned(part: TBoldGenericListPart; var element: TBoldElement): Boolean;
  {$IFDEF BOLDCOMCLIENT}
  begin
    element := part.GetElement(Follower.Element, Follower.Subscriber, False);
    result := assigned(element);
  end;
  {$ELSE}
  var
    ie: TBoldIndirectElement;
  begin
    ie := TBoldIndirectElement.Create;
    try
      Part.GetElement(Follower.Element, Follower.Subscriber, ie, False);
      result := ie.OwnsValue;
      if result then
        Element := ie.RelinquishValue
      else
        Element := ie.Value;
    finally
      ie.Free;
    end;
  end;
  {$ENDIF}

begin
  DestList := Follower.RendererData as TBoldFollowerListWithOwnedList;
  Controller := Follower.Controller as TBoldGenericListController;
  DestList.FreeAllOwnedElements;
  DestListIndex := 0;
  for PartIndex := 0 to Parts.Count - 1 do
  begin
    Part := Parts[PartIndex];
    if RetrieveElementAndCheckIfOwned(part, element) then
      DestList.AddOwnedElement(Element);
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT}
      Element.QueryInterface(IBoldList, SourceList);
      {$ELSE}
      if element is TBoldList then
        Sourcelist := TBoldList(element)
      else
        Sourcelist := nil;
      {$ENDIF}

      if Part.InterpretAsList and assigned(Sourcelist) then
      begin
        if BoldTestType(sourcelist, TBoldObjectList) then
          (SourceList as TBoldObjectList).EnsureRange(0, SourceList.Count - 1);
        for SourceIndex := 0 to Sourcelist.Count - 1 do
          AddNode(Sourcelist.Elements[SourceIndex]);
      end
      else
        AddNode(ELement);
    end;
  end;
  DestList.PurgeEnd(Controller, DestListIndex);
end;

class function TBoldGenericAsListRenderer.DefaultRenderer: TBoldGenericAsListRenderer;
begin
  Result := DefaultGenericAsListRenderer;
end;

function TBoldGenericAsListRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldFollowerListWithOwnedList;
end;

procedure TBoldGenericListController.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldGenericListController);
  inherited DoAssign(Source);
  Parts.Assign(TBoldGenericListController(Source).Parts);
end;

function TBoldGenericListPart.GetContextType: TBoldElementTypeInfo;
begin
  Result := (Collection as TBoldGenericListParts).fOwner.ContextType;
end;

procedure TBoldGenericListPart.SetEnabled(const Value: Boolean);
begin
  if value <> fEnabled then
  begin
    fEnabled := Value;
    Publisher.SendExtendedEvent(self, beListPartEnabledChanged, []);
  end;
end;

destructor TBoldGenericListPart.Destroy;
begin
  if assigned(fPublisher) then
    fPublisher.NotifySubscribersAndClearSubscriptions(self);
  FreeAndNil(fPublisher);
  inherited;
end;

function TBoldGenericListController.FindPartByName(const Name: string): TBoldGenericListPart;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Parts.Count - 1 do
    if SameText(Parts[i].Name, Name) then
    begin
      result := Parts[i];
      exit;
    end;
end;

function TBoldGenericListPart.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create(fPublisher);
  result := fPublisher;
end;

initialization
  DefaultGenericAsListRenderer := TBoldGenericAsListRenderer.Create(nil);

finalization
  FreeAndNil(DefaultGenericAsListRenderer);

end.
