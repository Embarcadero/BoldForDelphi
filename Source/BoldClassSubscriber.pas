unit BoldClassSubscriber;

interface

uses
  Classes,
  BoldHandles,
  BoldSubscription,
  BoldSystemRT,
  BoldSystem;

type
  TBoldClassSubscriber = class;
  TBoldClassEventMapping = class;
  TBoldClassEventMappingCollection = class;
  TBoldEventMappingEnumerator = class;

  TBoldClassSubscriptionEvent = procedure (AClassList: TBoldObjectList; ASubscriber: TBoldSubscriber) of object;

  TBoldClassEventMapping = class(TCollectionItem)
  private
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fClassTypeName: string;
    fOnEvent: TBoldExtendedEventHandler;
    fOnSubscribe: TBoldClassSubscriptionEvent;
    procedure SetClassTypeName(const Value: string);
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
    function GetCollection: TBoldClassEventMappingCollection;
    procedure SetOnSubscribe(const Value: TBoldClassSubscriptionEvent);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const args: array of const);
    function GetBoldSystem: TBoldSystem;
  protected
    function GetDisplayName: string; override;
    property Collection: TBoldClassEventMappingCollection read GetCollection;
    procedure PlaceSubscriptions;
    property Subscriber: TBoldExtendedPassthroughSubscriber read fSubscriber;
    property BoldSystem: TBoldSystem read GetBoldSystem;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property StaticSystemTypeInfo: TBoldSystemTypeInfo read GetStaticSystemTypeInfo;
    procedure Assign(Source: TPersistent); override;
  published
    property ClassTypeName: string read fClassTypeName write SetClassTypeName;
    property OnEvent: TBoldExtendedEventHandler read fOnEvent write fOnEvent;
    property OnSubscribe: TBoldClassSubscriptionEvent read fOnSubscribe write SetOnSubscribe;
  end;

  TBoldClassEventMappingCollection = class(TCollection)
  private
    fBoldEventSubscriber: TBoldClassSubscriber;
    function GetItem(Index: Integer): TBoldClassEventMapping;
    procedure SetItem(Index: Integer; Value: TBoldClassEventMapping);
    function GetEventSubscriber: TBoldClassSubscriber;
  protected
    function GetOwner: TPersistent; override;
    property EventSubscriber: TBoldClassSubscriber read GetEventSubscriber;
  public
    constructor Create(AEventSubscriber: TBoldClassSubscriber);
    function Add: TBoldClassEventMapping;
    function GetEnumerator: TBoldEventMappingEnumerator;
    property Items[Index: Integer]: TBoldClassEventMapping read GetItem write SetItem; default;
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldClassSubscriber = class(TBoldSystemExtensionComponent)
  private
    fEnabled: boolean;
    fMappingCollection: TBoldClassEventMappingCollection;
    procedure SetEnabled(const Value: boolean);
    procedure SetEventMappingCollection(const Value: TBoldClassEventMappingCollection);
  protected
    procedure PlaceSubscriptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: boolean read fEnabled write SetEnabled;
    property EventMapping: TBoldClassEventMappingCollection read fMappingCollection write SetEventMappingCollection;
  end;

  TBoldEventMappingEnumerator = class
  private
    FIndex: Integer;
    FCollection: TBoldClassEventMappingCollection;
  public
    constructor Create(ACollection: TBoldClassEventMappingCollection);
    function GetCurrent: TBoldClassEventMapping; inline;
    function MoveNext: Boolean; inline;
    property Current: TBoldClassEventMapping read GetCurrent;
  end;

implementation

uses
  System.SysUtils;

{ TBoldClassSubscriber }

constructor TBoldClassSubscriber.Create(AOwner: TComponent);
begin
  inherited;
  FMappingCollection := TBoldClassEventMappingCollection.Create(Self);
  fEnabled:= true;
end;

destructor TBoldClassSubscriber.Destroy;
begin
  FMappingCollection.free;
  inherited;
end;

procedure TBoldClassSubscriber.PlaceSubscriptions;
var
  BoldEventMapping: TBoldClassEventMapping;
begin
  inherited;
  for BoldEventMapping in FMappingCollection do
    BoldEventMapping.PlaceSubscriptions;
end;

procedure TBoldClassSubscriber.SetEnabled(const Value: boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    PlaceSubscriptions;
  end;
end;

procedure TBoldClassSubscriber.SetEventMappingCollection(
  const Value: TBoldClassEventMappingCollection);
begin
  fMappingCollection.Assign(Value);
end;

{ TBoldClassEventMapping }

procedure TBoldClassEventMapping.Assign(Source: TPersistent);
begin
  if source is TBoldClassEventMapping then
  begin
    fOnEvent := (Source as TBoldClassEventMapping).OnEvent;
    fOnSubscribe := (Source as TBoldClassEventMapping).OnSubscribe;
  end
  else
    inherited;
end;

constructor TBoldClassEventMapping.Create(Collection: TCollection);
begin
  inherited;
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(_Receive);
end;

destructor TBoldClassEventMapping.Destroy;
begin
  fSubscriber.Free;
  inherited;
end;

function TBoldClassEventMapping.GetBoldSystem: TBoldSystem;
begin
  result := Collection.fBoldEventSubscriber.BoldSystem;
end;

function TBoldClassEventMapping.GetCollection: TBoldClassEventMappingCollection;
begin
  result := inherited Collection as TBoldClassEventMappingCollection;
end;

function TBoldClassEventMapping.GetDisplayName: string;
begin
  result := fClassTypeName;
end;

function TBoldClassEventMapping.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  result := nil;
  if Assigned(Collection.fBoldEventSubscriber.StaticSystemHandle) then
    result := Collection.fBoldEventSubscriber.StaticSystemHandle.StaticSystemTypeInfo;
  if Collection.fBoldEventSubscriber.StaticSystemHandle = nil then
    raise Exception.Create('StaticSystemHandle is nil');
  if not Assigned(result) then
    raise Exception.Create('StaticSystemTypeInfo is nil');
end;

procedure TBoldClassEventMapping.PlaceSubscriptions;
begin
  fSubscriber.CancelAllSubscriptions;
  if Assigned(BoldSystem) and Assigned(fOnSubscribe) then
    OnSubscribe(BoldSystem.ClassByExpressionName[fClassTypeName], Subscriber);
end;

procedure TBoldClassEventMapping.SetClassTypeName(const Value: string);
begin
  if fClassTypeName <> Value then
  begin
    fClassTypeName := Value;
    PlaceSubscriptions;
  end;
end;

procedure TBoldClassEventMapping.SetOnSubscribe(const Value: TBoldClassSubscriptionEvent);
begin
  fOnSubscribe := Value;
end;

procedure TBoldClassEventMapping._Receive(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const args: array of const);
begin
  if Assigned(OnEvent) then
    OnEvent(Originator, OriginalEvent, RequestedEvent, args);
end;

{ TBoldClassEventMappingCollection }

function TBoldClassEventMappingCollection.Add: TBoldClassEventMapping;
begin
  Result := TBoldClassEventMapping(inherited Add);
end;

constructor TBoldClassEventMappingCollection.Create(
  AEventSubscriber: TBoldClassSubscriber);
begin
  inherited Create(TBoldClassEventMapping);
  fBoldEventSubscriber := AEventSubscriber;
end;

function TBoldClassEventMappingCollection.GetEnumerator: TBoldEventMappingEnumerator;
begin
  Result := TBoldEventMappingEnumerator.Create(Self);
end;

function TBoldClassEventMappingCollection.GetEventSubscriber: TBoldClassSubscriber;
begin
  result := owner as TBoldClassSubscriber;
end;

function TBoldClassEventMappingCollection.GetItem(Index: Integer): TBoldClassEventMapping;
begin
  Result := TBoldClassEventMapping(inherited GetItem(Index));
end;

function TBoldClassEventMappingCollection.GetOwner: TPersistent;
begin
  result := fBoldEventSubscriber;
end;

procedure TBoldClassEventMappingCollection.SetItem(Index: Integer;
  Value: TBoldClassEventMapping);
begin
  inherited SetItem(Index, Value);
end;

{ TBoldEventMappingEnumerator }

constructor TBoldEventMappingEnumerator.Create(
  ACollection: TBoldClassEventMappingCollection);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function TBoldEventMappingEnumerator.GetCurrent: TBoldClassEventMapping;
begin
  Result := FCollection.Items[FIndex];
end;

function TBoldEventMappingEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FCollection.Count;
end;

end.
