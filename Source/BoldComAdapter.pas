
{ Global compiler directives }
{$include bold.inc}
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
    fIsOwner: Boolean;
    fSubscriber: TBoldPassthroughSubscriber;
  protected
    fAdaptee: TBoldAdaptableObject;
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

procedure BoldComRegisterAdapter(AdapterClass: TBoldComAdapterClass; AdaptableClass: TBoldAdaptableClass);
{ril}procedure BoldComCreateAdapter(Adaptee: TBoldAdaptableObject; Owner: Boolean; const IID: TGUID; out Obj);
function BoldComInterfaceToObject(const Unk: IUnknown): TBoldAdaptableObject;

//PATCH
function GetDebugInfo: string;

implementation

uses
  SysUtils,
  Classes, //PATCH

  BoldCoreConsts,
  BoldDefs,
  BoldIndexableList,
  BoldHashIndexes,
  BoldComUtils;

type
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

//PATCH
function GetDebugInfo: string;
var
  vAdapterCache: TBoldAdapterCache;
  vTraverser: TBoldIndexableListTraverser;
  vAdapter: TBoldComAdapter;
  vNoAdapteeCnt: Integer;
  vAdapterRefCount: array [0..255] of Integer;
  I: Integer;
  vClasses: TStringList;
begin
  vAdapterCache := AdapterCache;
  vClasses := TStringList.Create;
  vClasses.Sorted := True;

  Result := 'AdapterCache.Count:'+IntToStr(vAdapterCache.Count);
  vTraverser := vAdapterCache.CreateTraverser;
  try
    vNoAdapteeCnt := 0;
    FillChar(vAdapterRefCount,SizeOf(vAdapterRefCount),0);
    while vTraverser.MoveNext do
    begin
      vAdapter := vTraverser.Item as TBoldComAdapter;
      if Assigned(vAdapter.fAdaptee) then
      begin
        I := vClasses.Add(vAdapter.fAdaptee.ClassName);
        vClasses.Objects[I] := TObject(Integer(vClasses.Objects[I])+1);
      end
      else
        Inc(vNoAdapteeCnt);

      Inc(vAdapterRefCount[vAdapter.RefCount]);

      //Annat intressant som kan m�tas och anv�ndas f�r klassning:
      //
      //vAdapter.RefCount
    end;
    Result := Result+' HasNoAdaptee:'+IntToStr(vNoAdapteeCnt)+#13#10;
    for I:=0 to vClasses.Count-1 do
        Result := Result+vClasses[I]+'='+IntToStr(Integer(vClasses.Objects[I]))+#13#10;
    Result := Result+' RefCount=AdapterCount:';
    for I:=Low(vAdapterRefCount) to High(vAdapterRefCount) do
      if vAdapterRefCount[I]<>0 then
        Result := Result+IntToStr(I)+'='+IntToStr(vAdapterRefCount[I])+', ';

  finally
    FreeAndNil(vTraverser);
    FreeAndNil(vClasses);
  end;
end;


procedure BoldComRegisterAdapter(AdapterClass: TBoldComAdapterClass;
  AdaptableClass: TBoldAdaptableClass);
begin
  TBoldComAdapterFactory.Instance.RegisterAdapterClass(AdapterClass,AdaptableClass);
end;

procedure BoldComCreateAdapter(Adaptee: TBoldAdaptableObject; Owner: Boolean; const IID: TGUID; out Obj);
{ril}
var
  Adapter: TBoldComAdapter;
{
  UnknownAdapter: IUnknown;
}
begin
  Pointer(Obj) := nil;
  if Assigned(Adaptee) then
  begin
    Adapter := AdapterCache.AdapterByAdaptee[Adaptee];
    if not assigned(Adapter) then
      Adapter := TBoldComAdapterFactory.Instance.CreateAdapterForObject(Adaptee,Owner);

    if not Assigned(Adapter) then
      raise EBoldCom.CreateFmt(sNoAdapterRegistered,[Adaptee.ClassName]);
{
    UnknownAdapter := Adapter;
    if UnknownAdapter.QueryInterface(IID,Obj) <> 0 then
}
    if Adapter.QueryInterface(IID,Obj) <> 0 then
{    begin
       UnknownAdapter := nil;
}
      raise EBoldCom.CreateFmt('%s: Unsupported interface',[Adapter.ClassName]);
{
    end;
}
  end;
end;

function BoldComInterfaceToObject(const Unk: IUnknown): TBoldAdaptableObject;
begin
  Result := nil;
  if Assigned(Unk) then
//    Result := (Unk as IBoldComAdapter).GetAdaptee;
    Result := (Unk as IBoldComAdapter).GetAdaptee;
end;

{-- TBoldComAdapter -----------------------------------------------------------}

constructor TBoldComAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  fSubscriber := TBoldPassthroughSubscriber.Create(ReceiveEvent);
  SetAdaptee(AdaptableObject,Owner);
  inherited Create(TypeLib, DispIntf);
end;

destructor TBoldComAdapter.Destroy;
begin
  if assigned(Adaptee) then
    AdapterCache.remove(self);
  FreeAndNil(fSubscriber);
  if fIsOwner and Assigned(fAdaptee) then
    FreeAndNil(fAdaptee)
  else
    fAdaptee := nil;
  inherited Destroy;
end;

function TBoldComAdapter.GetAdaptee: TBoldAdaptableObject;
begin
  Result := fAdaptee;
end;

function TBoldComAdapter.GetIsOwner: Boolean;
begin
  Result := fIsOwner;
end;

function TBoldComAdapter.GetEnsuredAdaptee: TBoldAdaptableObject;
begin
  Result := fAdaptee;
  if Result=nil then
    raise EBoldCom.CreateFmt('%s: No adaptee',[ClassName]);
end;

procedure TBoldComAdapter.ReceiveEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fAdaptee) and (OriginalEvent = beDestroying) then
  begin
    AdapterCache.Remove(self);
    fAdaptee := nil;
  end;
end;

procedure TBoldComAdapter.SetAdaptee(Value: TBoldAdaptableObject; Owner: Boolean);
begin
  if fAdaptee <> Value then
  begin
    if assigned(fAdaptee) then
      AdapterCache.remove(self);
    fSubscriber.CancelAllSubscriptions;
    if fIsOwner and Assigned(fAdaptee) then
      FreeAndNil(fAdaptee);

    fAdaptee := Value;
    
    if fAdaptee is TBoldSubscribableObject then
      TBoldSubscribableObject(fAdaptee).AddSubscription(fSubscriber, beDestroying, beDestroying)
    else if fAdaptee is TBoldSubscribableComponent then
      TBoldSubscribableComponent(fAdaptee).AddSubscription(fSubscriber, beDestroying, beDestroying)
    else if fAdaptee is TBoldSubscribablePersistent then
      TBoldSubscribablePersistent(fAdaptee).AddSubscription(fSubscriber, beDestroying, beDestroying)
    else if assigned(fAdaptee) then
      raise EBold.CreateFmt('%s.SetAdaptee: Can not adapt objects that are not subscribable (such as %s)', [className, value.ClassName]);

    if assigned(fAdaptee) then
      AdapterCache.Add(self);
  end;
  if fIsOwner <> Owner then
    fIsOwner := Owner;
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

function TBoldAdapterCacheIndex.ItemAsKeyObject(Item: TObject): TObject;
begin
  assert(item is TBoldComAdapter);
  result := (item as TBoldComAdapter).Adaptee;
end;

initialization

finalization
  FreeAndNil(G_BoldComAdapterFactory);
  FreeAndNil(G_AdapterCache);

end.
