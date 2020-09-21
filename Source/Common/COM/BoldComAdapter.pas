unit BoldComAdapter;

interface

uses
  ActiveX,
  BoldBase,
  BoldContainers,
  BoldSubscription,
  BoldComObj;

type
  { forward declarations }
  IBoldComAdapter = interface;
  TBoldComAdapterContext = class;
  TBoldComAdapter = class;
  TBoldComAdapterFactory = class;

  TBoldAdaptableObject = TObject;
  TBoldAdaptableClass = class of TObject;

  TBoldComAdapterClass = class of TBoldComAdapter;

  {--- IBoldComAdapter ---}
  IBoldComAdapter = interface
    ['{38CDB78D-50D9-4628-B415-699B54F0A5D1}']
    function GetAdaptee: TBoldAdaptableObject;
    function GetIsOwner: Boolean;
    procedure SetAdaptee(AdaptableObject: TBoldAdaptableObject; Owner: Boolean);
    property Adaptee: TBoldAdaptableObject read GetAdaptee;
    property IsOwner: Boolean read GetIsOwner;
  end;

  TBoldComAdapterContext = class(TBoldNonRefCountedObject)
  end;

  {--- TBoldComAdapter ---}
  TBoldComAdapter = class(TBoldAutoInterfacedObject, IBoldComAdapter)
  private
    FAdaptee: TBoldAdaptableObject;
    FIsOwner: Boolean;
    FSubscriber: TBoldPassthroughSubscriber;
  protected
    function GetEnsuredAdaptee: TBoldAdaptableObject;
    { IBoldComAdapter }
    function GetAdaptee: TBoldAdaptableObject;
    function GetIsOwner: Boolean;
    procedure SetAdaptee(Value: TBoldAdaptableObject; Owner: Boolean = False);
    procedure ReceiveEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); virtual;
    destructor Destroy; override;
    property Adaptee: TBoldAdaptableObject read GetAdaptee;
    property EnsuredAdaptee: TBoldAdaptableObject read GetEnsuredAdaptee;
    property IsOwner: Boolean read GetIsOwner;
  end;

  {--- TBoldComAdapterFactory ---}
  TBoldComAdapterFactory = class(TBoldNonRefCountedObject)
  private
    fAdapterClasses: TBoldPointerArray;
    fAdaptableClasses: TBoldPointerArray;
    function FindAdapterClass(AdaptableClass: TBoldAdaptableClass): TBoldComAdapterClass;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateAdapterForClass(AdaptableClass: TBoldAdaptableClass): TBoldComAdapter;
    function CreateAdapterForObject(AdaptableObject: TBoldAdaptableObject;
      Owner: Boolean = False): TBoldComAdapter;
    class function Instance: TBoldComAdapterFactory;
    procedure RegisterAdapterClass(AdapterClass: TBoldComAdapterClass;
      AdaptableClass: TBoldAdaptableClass);
  end;

procedure BoldComRegisterAdapter(AdapterClass: TBoldComAdapterClass;
  AdaptableClass: TBoldAdaptableClass);
procedure BoldComCreateAdapter(Adaptee: TBoldAdaptableObject;
  Owner: Boolean; const IID: TGUID; out Obj);
function BoldComInterfaceToObject(const Unk: IUnknown): TBoldAdaptableObject;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldIndexableList,
  BoldHashIndexes,
  BoldComUtils,
  BoldComConst;

type
  { TBoldAdapterCache }
  TBoldAdapterCache = class(TBoldUnorderedIndexableList)
  private
    function GetAdapterByAdaptee(Adaptee: TBoldAdaptableObject): TBoldComAdapter;
  public
    constructor create;
    property AdapterByAdaptee[Adaptee: TBoldAdaptableObject]: TBoldComAdapter read GetAdapterByAdaptee;
  end;

  {---TBoldObjectHashIndex---}
  TBoldAdapterCacheIndex = class(TBoldObjectHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; override;
  end;

var
  G_BoldComAdapterFactory: TBoldComAdapterFactory = nil;
  G_AdapterCache: TBoldAdapterCache = nil;
  IX_BoldAdapterCache: integer = -1;

function AdapterCache: TBoldAdapterCache;
begin
  if not assigned(G_AdapterCache) then
  begin
    G_AdapterCache := TBoldAdapterCache.Create;
    G_AdapterCache.OwnsEntries := false;
  end;
  result := G_AdapterCache;
end;


procedure BoldComRegisterAdapter(AdapterClass: TBoldComAdapterClass;
  AdaptableClass: TBoldAdaptableClass);
begin
  TBoldComAdapterFactory.Instance.RegisterAdapterClass(AdapterClass,AdaptableClass);
end;

procedure BoldComCreateAdapter(Adaptee: TBoldAdaptableObject;
  Owner: Boolean; const IID: TGUID; out Obj);
var
  Adapter: TBoldComAdapter;
  UnknownAdapter: IUnknown;
begin
  Pointer(Obj) := nil;
  if Assigned(Adaptee) then
  begin
    Adapter := AdapterCache.AdapterByAdaptee[Adaptee];
    if not assigned(Adapter) then
      Adapter := TBoldComAdapterFactory.Instance.CreateAdapterForObject(Adaptee,Owner);

    if not Assigned(Adapter) then
      raise EBoldCom.CreateFmt(sNoAdapterRegistered,[Adaptee.ClassName]);
    UnknownAdapter := Adapter; // AddRef
    if UnknownAdapter.QueryInterface(IID,Obj) <> 0 then
    begin
      UnknownAdapter := nil; // Release
      raise EBoldCom.CreateFmt(sUnsupportedInterface,[Adapter.ClassName]);
    end;
  end;
end;

function BoldComInterfaceToObject(const Unk: IUnknown): TBoldAdaptableObject;
begin
  Result := nil;
  if Assigned(Unk) then
  begin
    with Unk as IBoldComAdapter do
      Result := GetAdaptee;
  end;
end;

{-- TBoldComAdapter -----------------------------------------------------------}

constructor TBoldComAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  FSubscriber := TBoldPassthroughSubscriber.Create(ReceiveEvent);
  SetAdaptee(AdaptableObject,Owner);
  inherited Create(TypeLib, DispIntf);
end;

destructor TBoldComAdapter.Destroy;
begin
  if assigned(Adaptee) then
    AdapterCache.remove(self);
  FreeAndNil(FSubscriber);
  if FIsOwner and Assigned(FAdaptee) then
    FreeAndNil(FAdaptee)
  else
    fAdaptee := nil;
  inherited Destroy;
end;

function TBoldComAdapter.GetAdaptee: TBoldAdaptableObject;
begin
  Result := FAdaptee;
end;

function TBoldComAdapter.GetIsOwner: Boolean;
begin
  Result := FIsOwner;
end;

function TBoldComAdapter.GetEnsuredAdaptee: TBoldAdaptableObject;
begin
  Result := FAdaptee;
  if not Assigned(Result) then
    raise EBoldCom.CreateFmt(sNoAdaptee, [ClassName]);
end;

procedure TBoldComAdapter.ReceiveEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = FAdaptee) and (OriginalEvent = beDestroying) then
  begin
    AdapterCache.Remove(self);
    FAdaptee := nil;
  end;
end;

procedure TBoldComAdapter.SetAdaptee(Value: TBoldAdaptableObject; Owner: Boolean);
begin
  if FAdaptee <> Value then
  begin
    if assigned(fAdaptee) then
      AdapterCache.remove(self);
    FSubscriber.CancelAllSubscriptions;
    if FIsOwner and Assigned(FAdaptee) then
      FreeAndNil(FAdaptee);

    FAdaptee := Value;

    if FAdaptee is TBoldSubscribableObject then
      TBoldSubscribableObject(FAdaptee).AddSubscription(FSubscriber, beDestroying, beDestroying)
    else if FAdaptee is TBoldSubscribableComponent then
      TBoldSubscribableComponent(FAdaptee).AddSubscription(FSubscriber, beDestroying, beDestroying)
    else if FAdaptee is TBoldSubscribablePersistent then
      TBoldSubscribablePersistent(FAdaptee).AddSubscription(FSubscriber, beDestroying, beDestroying)
    else if assigned(fAdaptee) then
      raise EBold.CreateFmt(sCannotAdaptNonSubscribables, [className, value.ClassName]);

    if assigned(fAdaptee) then
      AdapterCache.Add(self);
  end;
  if FIsOwner <> Owner then
    FIsOwner := Owner;
end;

{-- TBoldComAdapterFactory ----------------------------------------------------}

constructor TBoldComAdapterFactory.Create;
begin
  inherited Create;
  fAdapterClasses := TBoldPointerArray.Create(64,[]);
  fAdaptableClasses := TBoldPointerArray.Create(64,[]);
end;

destructor TBoldComAdapterFactory.Destroy;
begin
  FreeAndNil(fAdapterClasses);
  FreeAndNil(fAdaptableClasses);
  inherited Destroy;
end;

function TBoldComAdapterFactory.CreateAdapterForClass(
  AdaptableClass: TBoldAdaptableClass): TBoldComAdapter;
var
  AdapterClass: TBoldComAdapterClass;
begin
  AdapterClass := FindAdapterClass(AdaptableClass);
  if Assigned(AdapterClass) then
    Result := AdapterClass.Create(nil,False,nil,IUnknown)
  else
    Result := nil;
end;

function TBoldComAdapterFactory.CreateAdapterForObject(AdaptableObject: TObject;
  Owner: Boolean): TBoldComAdapter;
var
  AdapterClass: TBoldComAdapterClass;
begin
  AdapterClass := FindAdapterClass(TBoldAdaptableClass(AdaptableObject.ClassType));
  if Assigned(AdapterClass) then
    Result := AdapterClass.Create(AdaptableObject,Owner,nil,IUnknown)
  else
    Result := nil;
end;

function TBoldComAdapterFactory.FindAdapterClass(
  AdaptableClass: TBoldAdaptableClass): TBoldComAdapterClass;
var
  I: Integer;
begin
  Result := nil;
  while (AdaptableClass <> nil) do
  begin
    I := fAdaptableClasses.IndexOf(AdaptableClass);
    if I <> -1 then
    begin
      Result := fAdapterClasses[I];
      Break;
    end
    else
      AdaptableClass := TBoldAdaptableClass(AdaptableClass.ClassParent);
  end;
end;

class function TBoldComAdapterFactory.Instance: TBoldComAdapterFactory;
begin
  if not Assigned(G_BoldComAdapterFactory) then
    G_BoldComAdapterFactory := TBoldComAdapterFactory.Create;
  Result := G_BoldComAdapterFactory;
end;

procedure TBoldComAdapterFactory.RegisterAdapterClass(AdapterClass: TBoldComAdapterClass;
  AdaptableClass: TBoldAdaptableClass);
begin
  fAdapterClasses.Add(AdapterClass);
  fAdaptableClasses.Add(AdaptableClass);
end;


{ TBoldAdapterCache }

constructor TBoldAdapterCache.create;
begin
  inherited Create;
  SetIndexVariable(IX_BoldAdapterCache, AddIndex(TBoldAdapterCacheIndex.Create));
end;

function TBoldAdapterCache.GetAdapterByAdaptee(Adaptee: TBoldAdaptableObject): TBoldComAdapter;
begin
  Result := TBoldComAdapter(TBoldAdapterCacheIndex(indexes[IX_BoldAdapterCache]).FindByObject(Adaptee));
end;

{ TBoldAdapterCacheIndex }

function TBoldAdapterCacheIndex.ItemASKeyObject(Item: TObject): TObject;
begin
  assert(item is TBoldComAdapter);
  result := (item as TBoldComAdapter).Adaptee;
end;

initialization // empty

finalization
  FreeAndNil(G_BoldComAdapterFactory);
  FreeAndNil(G_AdapterCache);

end.


