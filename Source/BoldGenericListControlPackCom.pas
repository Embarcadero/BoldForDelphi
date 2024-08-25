
{ Global compiler directives }
{$include bold.inc}
unit BoldGenericListControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,

  // Bold
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldDefs,
  BoldListControlPackCom,
  BoldSubscription;

type
  { forward declarations }
  TBoldGenericAsListRendererCom = class;
  TBoldGenericListControllerCom = class;
  TBoldGenericListPartCom = class;
  TBoldGenericListPartsCom = class;
  TBoldFollowerListWithOwnedListCom = class;

  {$IFDEF BOLD_BCB}
  TGetFollowerControllerByNameEventCom = procedure (Name: string; var FollowerController: TBoldFollowerControllerCom) of object;
  TGetFollowerControllerEventCom = procedure (Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom; var FollowerController: TBoldFollowerControllerCom) of object;
  {$ENDIF}

  {$IFDEF BOLD_DELPHI}
  TGetFollowerControllerByNameEventCom = function (Name: string): TBoldFollowerControllerCom of object;
  TGetFollowerControllerEventCom = function (Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom): TBoldFollowerControllerCom of object;
  {$ENDIF}

  {$IFNDEF BOLDCOMCLIENT}
  TGetElementEvent = procedure (Sender: TBoldGenericListPartCom; Element: IBoldElement; Subscriber: TBoldComClientSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean) of object;
  {$ENDIF}    

  { TBoldFollowerListWithOwnedListCom }
  TBoldFollowerListWithOwnedListCom = class (TBoldFollowerListCom)
  private
     FOwnedElements: TBoldClientableListCom;
   public
     constructor Create(OwningFollower: TBoldFollowerCom); override;
     destructor Destroy; override;
     procedure AddOwnedElement(Element: IBoldElement);
     procedure FreeAllOwnedElements;
  end;

  { TBoldGenericListPartCom }
  TBoldGenericListPartCom = class(TCollectionItem)
  private
    FElementExpression: TBoldExpression;
    FControllerExpression: TBoldExpression;
    FInterpretAsList: Boolean;
    {$IFNDEF BOLDCOMCLIENT}
    FOnGetElement: TGetElementEvent;
    {$ENDIF}
    FOnGetFollowerController: TGetFollowerControllerEventCom;
    fEnabled: Boolean;
    fName: String;
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher;
    function GetContextType: IBoldElementTypeInfo;

    procedure SetElementExpression(Value: TBoldExpression);
    procedure SetControllerExpression(Value: TBoldExpression);
    procedure SetInterpretAsList(Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    property Publisher: TBoldPublisher read GetPublisher;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$IFDEF BOLDCOMCLIENT}
    function GetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; Resubscribe: Boolean): IBoldElement;
    {$ELSE}
    procedure DefaultGetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
    procedure GetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
    {$ENDIF}
    function DefaultGetFollowerController(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom): TBoldFollowerControllerCom;
    function GetFollowerController(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom): TBoldFollowerControllerCom;
    property ContextType: IBoldElementTypeInfo read GetContextType;
  published
    property ElementExpression: TBoldExpression read FElementExpression write SetElementExpression nodefault;
    property ControllerExpression: TBoldExpression read FControllerExpression write SetControllerExpression nodefault;
    property InterpretAsList: Boolean read FInterpretAsList write SetInterpretAsList;
    property Name: String read fName write fName;
    property Enabled: Boolean read fEnabled write SetEnabled default true;
    {$IFNDEF BOLDCOMCLIENT}
    property OnGetElement: TGetElementEvent read FOnGetElement write FOnGetElement;
    {$ENDIF}
    property OnGetFollowerController: TGetFollowerControllerEventCom read FOnGetFollowerController write FOnGetFollowerController;
  end;

  { TBoldGenericListPartsCom }
  TBoldGenericListPartsCom = class(TCollection)
  private
    FOwner: TBoldGenericListControllerCom;
    function GetPart(Index: Integer): TBoldGenericListPartCom;
    procedure SetPart(Index: Integer; Value: TBoldGenericListPartCom);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aOwner: TBoldGenericListControllerCom);
    function Add: TBoldGenericListPartCom;
    property Items[Index: Integer]: TBoldGenericListPartCom read GetPart write SetPart; default;
  end;

  { TBoldGenericAsListRendererCom }
  TBoldGenericAsListRendererCom = class(TBoldAsFollowerListRendererCom)
  protected
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
  public
    procedure MakeUptodate(Follower: TBoldFollowerCom;
                           Parts: TBoldGenericListPartsCom;
                           GetFollowerControllerByName: TGetFollowerControllerByNameEventCom);
    class function DefaultRenderer: TBoldGenericAsListRendererCom;
  end;

  { TBoldGenericListControllerCom }
  TBoldGenericListControllerCom = class(TBoldAsFollowerListControllerCom)
  private
    FParts: TBoldGenericListPartsCom;
    FGetFollowerControllerByName: TGetFollowerControllerByNameEventCom;
    procedure SetParts(Value: TBoldGenericListPartsCom);
    function GetRenderer: TBoldGenericAsListRendererCom;
    procedure SetRenderer(Value: TBoldGenericAsListRendererCom);
  protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    function GetEffectiveGenericAsListRenderer: TBoldGenericAsListRendererCom;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); override;
    procedure DoAssign(Source: TPersistent); override;
    property EffectiveGenericAsListRenderer: TBoldGenericAsListRendererCom read GetEffectiveGenericAsListRenderer;
  public
    constructor Create(aOwningComponent: TComponent;
                       GetFollowerControllerByNameFunc: TGetFollowerControllerByNameEventCom);
    destructor Destroy; override;
    function FindPartByName(Name: string): TBoldGenericListPartCom;
    function CanHaveSubFollowers: Boolean;
  published
    property Parts: TBoldGenericListPartsCom read FParts write SetParts;
    property Renderer: TBoldGenericAsListRendererCom read GetRenderer write SetRenderer;
  end;

implementation

uses
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  SysUtils,
  BoldContainers,
  BoldControlPackDefs;

var
  DefaultGenericAsListRenderer: TBoldGenericAsListRendererCom;

{ TBoldFollowerListWithOwnedListCom }

constructor TBoldFollowerListWithOwnedListCom.Create(OwningFollower: TBoldFollowerCom);
begin
  inherited Create(OwningFollower);
  FOwnedElements := TBoldClientableListCom.Create(0, [bcoDataOwner, bcoThreadSafe]);
end;

destructor TBoldFollowerListWithOwnedListCom.Destroy;
begin
   FreeAndNil(FOwnedElements);
   inherited;
end;

procedure TBoldFollowerListWithOwnedListCom.AddOwnedElement(Element: IBoldElement);
begin
  FOwnedElements.Add(Element);
end;

procedure TBoldFollowerListWithOwnedListCom.FreeAllOwnedElements;
begin
  fOwnedElements.Clear;
end;

{---TBoldGenericListPartCom---}
constructor TBoldGenericListPartCom.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FControllerExpression := 'oclType.asstring->union(oclType.allsupertypes.asString)->union(''<Default>'')';
  fEnabled := true;
end;

procedure TBoldGenericListPartCom.Assign(Source: TPersistent);
begin
  if Source is TBoldGenericListPartCom then
  begin
    ElementExpression := TBoldGenericListPartCom(Source).ElementExpression;
    ControllerExpression := TBoldGenericListPartCom(Source).ControllerExpression;
    InterpretAsList := TBoldGenericListPartCom(Source).InterpretAsList;
    {$IFNDEF BOLDCOMCLIENT}
    OnGetElement := TBoldGenericListPartCom(Source).OnGetElement;
    {$ENDIF}
    OnGetFollowerController := TBoldGenericListPartCom(Source).OnGetFollowerController;
  end
  else
    inherited Assign(Source);
end;

procedure TBoldGenericListPartCom.SetElementExpression(Value: TBoldExpression);
begin
  if Value <> ElementExpression then
  begin
    FElementExpression := Value;
    Changed(False);
  end;
end;

procedure TBoldGenericListPartCom.SetControllerExpression(Value: TBoldExpression);
begin
  if Value <> ControllerExpression then
  begin
    FControllerExpression := Value;
    Changed(False);
  end;
end;

procedure TBoldGenericListPartCom.SetInterpretAsList(Value: Boolean);
begin
  if Value<>InterpretAsList then
  begin
    FInterpretAsList := Value;
    Changed(False);
  end;
end;

{$IFDEF BOLDCOMCLIENT}
function TBoldGenericListPartCom.GetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; Resubscribe: Boolean): IBoldElement;
begin
  if Assigned(Element) then
    result := Element.EvaluateAndSubscribeToExpression(ElementExpression, subscriber.ClientId, subscriber.SubscriberId, resubscribe, false)
  else
    result := nil;
end;
{$ELSE}
procedure TBoldGenericListPartCom.DefaultGetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
begin
  if Assigned(Element) then
    Element.EvaluateAndSubscribeToExpression(ElementExpression, Subscriber, ResultElement, Resubscribe)
  else
    ResultElement.SetOwnedValue(nil);
end;

procedure TBoldGenericListPartCom.GetElement(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; ResultElement: TBoldIndirectElement; Resubscribe: Boolean);
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

function TBoldGenericListPartCom.DefaultGetFollowerController(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom): TBoldFollowerControllerCom;
var
{$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
{$ELSE}
  E: TBoldIndirectElement;
  ListHolder: TBoldIndirectElement;
{$ENDIF}
  List: IBoldList;

function LoopList(List: IBoldList): TBoldFollowerControllerCom;
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
      List := IBoldList(ListHolder.Value);
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

function TBoldGenericListPartCom.GetFollowerController(Element: IBoldElement; Subscriber: TBoldComClientSubscriber; GetFollowerControllerByName: TGetFollowerControllerByNameEventCom): TBoldFollowerControllerCom;
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

{---TBoldGenericListPartsCom---}
constructor TBoldGenericListPartsCom.Create(aOwner: TBoldGenericListControllerCom);
begin
  inherited Create(TBoldGenericListPartCom);
  FOwner := aOwner;
end;

function TBoldGenericListPartsCom.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBoldGenericListPartsCom.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

function TBoldGenericListPartsCom.GetPart(Index: Integer): TBoldGenericListPartCom;
begin
  Result := TBoldGenericListPartCom(GetItem(Index));
end;

procedure TBoldGenericListPartsCom.SetPart(Index: Integer; Value: TBoldGenericListPartCom);
begin
  SetItem(Index, Value);
end;

function TBoldGenericListPartsCom.Add: TBoldGenericListPartCom;
begin
  Result := TBoldGenericListPartCom.Create(Self);
end;

{---TBoldGenericListControllerCom---}
function TBoldGenericListControllerCom.GetRenderer: TBoldGenericAsListRendererCom;
begin
  Result := (UntypedRenderer as TBoldGenericAsListRendererCom);
end;

procedure TBoldGenericListControllerCom.SetRenderer(Value: TBoldGenericAsListRendererCom);
begin
  UntypedRenderer := Value;
end;

function TBoldGenericListControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveGenericAsListRenderer;
end;

function TBoldGenericListControllerCom.GetEffectiveGenericAsListRenderer: TBoldGenericAsListRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldGenericAsListRendererCom.DefaultRenderer;
end;

constructor TBoldGenericListControllerCom.Create(aOwningComponent: TComponent;
                                              GetFollowerControllerByNameFunc: TGetFollowerControllerByNameEventCom);
begin
  inherited Create(aOwningComponent);
  FGetFollowerControllerByName := GetFollowerControllerByNameFunc;
  FParts := TBoldGenericListPartsCom.Create(Self);
end;

destructor TBoldGenericListControllerCom.Destroy;
begin
  FreePublisher;
  FreeAndNil(FParts);
  inherited Destroy;
end;

function TBoldGenericListControllerCom.CanHaveSubFollowers: Boolean;
begin
  Result := Parts.Count>0
end;

procedure TBoldGenericListControllerCom.SetParts(Value: TBoldGenericListPartsCom);
begin
  FParts.Assign(Value);
end;

procedure TBoldGenericListControllerCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  EffectiveGenericAsListRenderer.MakeUptodate(Follower as TBoldFollowerCom, Parts, FGetFollowerControllerByName);
end;

{---TBoldGenericAsListRendererCom---}
procedure TBoldGenericAsListRendererCom.MakeUptodate(Follower: TBoldFollowerCom;
                                                  Parts: TBoldGenericListPartsCom;
                                                  GetFollowerControllerByName: TGetFollowerControllerByNameEventCom);
var
  Controller: TBoldGenericListControllerCom;
  PartIndex: Integer;
  Part: TBoldGenericListPartCom;
  Element: IBoldElement;
  DestList: TBoldFollowerListWithOwnedListCom;
  DestListIndex: Integer;
  FollowerController: TBoldFollowerControllerCom;
  SourceList: IBoldList;
  SourceIndex: Integer;

  procedure AddNode(Element: IBoldElement);
  begin
    FollowerController := Part.GetFollowerController(Element, Follower.Subscriber, GetFollowerControllerByName);
    if Assigned(FollowerController) then
    begin
      DestList.EnsureFollower(Controller, DestListIndex, Element, FollowerController);
      Inc(DestListIndex);
    end;
  end;

  function RetrieveElementAndCheckIfOwned(part: TBoldGenericListPartCom; var element: IBoldElement): Boolean;
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
      if ie.OwnsValue then
        Element := ie.RelinquishValue
      else
        Element := ie.Value;

      result := ie.OwnsValue;
    finally
      ie.Free;
    end;
  end;
  {$ENDIF}

begin
  DestList := Follower.RendererData as TBoldFollowerListWithOwnedListCom;
  Controller := Follower.Controller as TBoldGenericListControllerCom;
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
      if element is IBoldList then
        Sourcelist := IBoldList(element)
      else
        Sourcelist := nil;
      {$ENDIF}

      if Part.InterpretAsList and assigned(Sourcelist) then
      begin
        if BoldTestType(sourcelist, IBoldObjectList) then
          (SourceList as IBoldObjectList).EnsureRange(0, SourceList.Count - 1);
        for SourceIndex := 0 to Sourcelist.Count - 1 do
          AddNode(Sourcelist.Elements[SourceIndex]);
      end
      else
        AddNode(ELement);
    end;
  end;
  DestList.PurgeEnd(Controller, DestListIndex);
end;

class function TBoldGenericAsListRendererCom.DefaultRenderer: TBoldGenericAsListRendererCom;
begin
  Result := DefaultGenericAsListRenderer;
end;

function TBoldGenericAsListRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldFollowerListWithOwnedListCom;
end;

procedure TBoldGenericListControllerCom.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldGenericListControllerCom);
  inherited DoAssign(Source);
  Parts.Assign(TBoldGenericListControllerCom(Source).Parts);
end;

function TBoldGenericListPartCom.GetContextType: IBoldElementTypeInfo;
begin
  Result := (Collection as TBoldGenericListPartsCom).fOwner.ContextType;
end;

procedure TBoldGenericListPartCom.SetEnabled(const Value: Boolean);
begin
  if value <> fEnabled then
  begin
    fEnabled := Value;
    Publisher.SendExtendedEvent(self, beListPartEnabledChanged, []);
  end;
end;

destructor TBoldGenericListPartCom.Destroy;
begin
  if assigned(fPublisher) then
    fPublisher.NotifySubscribersAndClearSubscriptions(self);
  FreeAndNil(fPublisher);
  inherited;
end;

function TBoldGenericListControllerCom.FindPartByName(Name: string): TBoldGenericListPartCom;
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

function TBoldGenericListPartCom.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.create(fPublisher);
  result := fPublisher;
end;

initialization
  DefaultGenericAsListRenderer := TBoldGenericAsListRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultGenericAsListRenderer);

end.
