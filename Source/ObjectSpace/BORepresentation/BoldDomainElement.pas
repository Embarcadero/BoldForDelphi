unit BoldDomainElement;

interface

uses
  BoldBase,
  BoldIndexableList,
  BoldSubscription,
  BoldElements;

type
  {forward declarations of all classes}
  TBoldDomainElementProxyMode = (
    bdepContents,  // actual contents of member
//    bDepObjectSpace, // the "fronside" inteferace, not used yet
    bdepPMOut,  // Interface to Persistence mapper
    bdepPMIn,  // Interface from Persistence mapper
    bdepUnDo,  //Interface To/From UnDo-handler
    bdepInternalInitialize,  //
    bdRemove // ...
    );
type
  TBoldDomainElement = class;
  TBoldDomainElement_Proxy = class;
  TBoldDomainElementClass = class of TBoldDomainElement;
  TBoldDomainElementCollection = class;
  TBoldDomainElementCollectionTraverser = class;

  { TBoldDomainElement }
  TBoldDomainElement = class(TBoldElement)
  private
    FOwningElement: TBoldDomainElement;
  protected
    function GetDisplayName: String; virtual; abstract;
    function GetBoldDirty: Boolean; virtual; abstract;
    function MayCommit: Boolean; virtual;
    procedure ReceiveEventFromOwned(Originator: TObject; OriginalEvent: TBoldEvent); virtual;
    function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure StateError(S: string); virtual;
  public
    constructor Create(OwningElement: TBoldDomainElement); virtual;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; virtual;
    function CanCommit: Boolean;
    procedure SendEvent(OriginalEvent: TBoldEvent); override;
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); override;
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
    property BoldDirty: Boolean read GetBoldDirty;
    property BoldPersistent: Boolean index befPersistent read GetElementFlag;
    property OwningElement: TBoldDomainElement read FOwningElement;
    property DisplayName: String read GetDisplayName;
  end;

  { TBoldDomainElement_Proxy }
  TBoldDomainElement_Proxy = class(TBoldRefCountedObject)
  private
    fMode:  TBoldDomainElementProxyMode;
    fProxedElement: TBoldDomainElement;
  protected
    procedure UnsupportedMode(Mode: TBoldDomainElementProxyMode; Func: string);
  public
    constructor Create(ProxedElement: TBoldDomainElement; Mode:  TBoldDomainElementProxyMode);
    property ProxedElement: TBoldDomainElement read fProxedElement;
    property Mode: TBoldDomainElementProxyMode read fMode;
  end;

  { TBoldDomainElementCollectionTraverser }
  TBoldDomainElementCollectionTraverser = class(TBoldIndexableListTraverser)
  private
    function GetItem: TBoldDomainElement;
  public
    property Item: TBoldDomainElement read GetItem;
  end;

  { TBoldDomainElementCollection }
  TBoldDomainElementCollection = class(TBoldUnorderedIndexableList)
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor create;
    procedure Add(item: TBoldDomainElement);
    function Includes(item: TBoldDomainElement): Boolean;
    function CreateTraverser: TBoldDomainElementCollectionTraverser;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldIndex,
  BoldHashIndexes;

var
  IX_DomainElementCollection: integer = -1;

type
  TBoldDomainElementCollectionIndex = class(TBoldObjectHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; override;
  end;

{ TBoldDomainElement }

procedure TBoldDomainElement.StateError(S: string);
var
  MyClassName: string;
  MyOwnerClassname: string;
begin
  if Assigned(Self) then
    MyClassName := ClassName
  else
    MyClassName := '<nil>'; // do not localize
  if Assigned(Self) and Assigned(OwningElement) then
    MyOwnerClassname := OwningElement.ClassName + '.'
  else
    MyOwnerClassname := '';
  raise EBoldInternal.CreateFmt('%s.%s StateError: %s', [MyOwnerClassName, MyClassName, s]);
end;

(* //FIXME: How do we change persistent-flag of objects!
procedure TBoldDomainElement.SetBoldPersistent(Value: Boolean);
begin
  if Value <> GetBoldPersistent then
  begin
    if Value then
    begin
      if not Assigned(BoldRtInfo) or (not BoldRtInfo.Persistent) then
        raise EBold.CreateFmt('%s: Can''t make element persistent', [ClassName]);
    end
    else {not Value}
    begin
//      if (not (PersistenceState in [bpsVirgin, bpsModifiedNew])) then
//        raise EBold.CreateFmt('%s: Can''t make element transient.' + BOLDCRLF + 'PersistenceState: %s, OperationState: %s',
//                              [ClassName,
//                              GetEnumName(TypeInfo(TBoldObjectPersistenceState), Ord(BoldObjectPersistenceState)),
//                              GetEnumName(TypeInfo(TBoldOperationState), Ord(OperationState))]);
    end;
    SetElementFlag(befPersistent, Value);
  end;
end;
*)

constructor TBoldDomainElement.Create(OwningElement: TBoldDomainElement);
begin
  inherited Create;
  FOwningElement := OwningElement;
end;

procedure TBoldDomainElement.SendEvent(OriginalEvent: TBoldEvent);
begin
  if Assigned(OwningElement) then
    OwningElement.ReceiveEventFromOwned(self, OriginalEvent);
  inherited; // SendEvent(Originator, OriginalEvent);
end;

function TBoldDomainElement.SendQuery(
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
  if Assigned(OwningElement) then
    result := OwningElement.ReceiveQueryFromOwned(self, OriginalEvent, Args, Subscriber);
  if result then
    result := inherited SendQuery(OriginalEvent, Args, Subscriber);
end;

procedure TBoldDomainElement.ReceiveEventFromOwned(Originator: TObject; OriginalEvent: TBoldEvent);
begin
  // Intentionally left blank
end;

function TBoldDomainElement.ReceiveQueryFromOwned(Originator: TObject;
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

procedure TBoldDomainElement.SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const);
begin
  if Assigned(OwningElement) then
    OwningElement.ReceiveEventFromOwned(self, OriginalEvent);
  inherited;
end;

function TBoldDomainElement.CanCommit: Boolean;
begin
  result := MayCommit and SendQuery(bqMayCommit, [], nil);
end;

function TBoldDomainElement.MayCommit: Boolean;
begin
  result := True;
end;

function TBoldDomainElement.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  result := false;
end;

{ TBoldDomainElement_Proxy }

constructor TBoldDomainElement_Proxy.Create(ProxedElement: TBoldDomainElement; Mode:  TBoldDomainElementProxyMode);
begin
  inherited create;
  fProxedElement := ProxedElement;
  fMode := Mode;
end;

procedure TBoldDomainElement_Proxy.UnsupportedMode(Mode: TBoldDomainElementProxyMode; Func: string);
begin
  raise EBoldInternal.CreateFmt('%s.%s: Unsupported Mode: %d', [ClassType.ClassName, Func, Ord(Mode)]);
end;

{ TBoldDomainElementCollection }

procedure TBoldDomainElementCollection.Add(item: TBoldDomainElement);
begin
  inherited Add(item);
end;

constructor TBoldDomainElementCollection.create;
begin
  inherited create;
  OwnsEntries := false;
  SetIndexVariable(IX_DomainElementCollection, AddIndex(TBoldDomainElementCollectionIndex.Create));
end;

function TBoldDomainElementCollection.CreateTraverser: TBoldDomainElementCollectionTraverser;
begin
  result := TBoldDomainElementCollectionTraverser(inherited CreateTraverser);
  Assert(Result is TBoldDomainElementCollectionTraverser);
end;

function TBoldDomainElementCollection.Includes(item: TBoldDomainElement): Boolean;
begin
  Assert((Indexes[IX_DomainElementCollection] is TBoldDomainElementCollectionIndex));
  result := assigned(TBoldDomainElementCollectionIndex(Indexes[IX_DomainElementCollection]).FindByObject(Item));
end;

function TBoldDomainElementCollectionIndex.ItemASKeyObject(Item: TObject): TObject;
begin
  result := item;
end;

function TBoldDomainElementCollection.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldDomainElementCollectionTraverser;
end;

{ TBoldDomainElementCollectionTraverser }

function TBoldDomainElementCollectionTraverser.GetItem: TBoldDomainElement;
begin
  Assert((inherited item) is TBoldDomainElement);
  result := TBoldDomainElement(inherited item);
end;

end.
