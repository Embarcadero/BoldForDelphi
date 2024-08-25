
{ Global compiler directives }
{$include bold.inc}
unit BoldDomainElement;

interface

uses
  BoldBase,
  BoldIndexableList,
  BoldSubscription,
  BoldElements,
  BoldHashIndexes
  ;

type
  {forward declarations of all classes}
  TBoldDomainElementProxyMode = (
    bdepContents,
    bdepPMOut,
    bdepPMIn,
    bdepUnDo,
    bdepInternalInitialize,
    bdRemove
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
    function GetDisplayName: String; override;
    function GetBoldDirty: Boolean; virtual; abstract;
    function MayCommit: Boolean; virtual;
    function GetBoldSystem: TBoldDomainElement; virtual;
    procedure ReceiveEventFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const); virtual;
    function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure StateError(S: string); virtual;
  public
    constructor CreateWithOwner(OwningElement: TBoldDomainElement); virtual;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; virtual;
    procedure Discard; virtual; abstract;
    procedure Invalidate; virtual;
    function CanCommit: Boolean;
    procedure SendEvent(OriginalEvent: TBoldEvent); override;
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); override;
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber; Originator: TObject = nil): Boolean; override;
    property BoldDirty: Boolean read GetBoldDirty;
    property BoldPersistent: Boolean index befPersistent read GetElementFlag;
    property OwningElement: TBoldDomainElement read FOwningElement;
    property BoldSystem: TBoldDomainElement read GetBoldSystem;
  end;

  { TBoldDomainElement_Proxy }
  TBoldDomainElement_Proxy = class(TBoldRefCountedObject)
  private
    fMode:  TBoldDomainElementProxyMode;
  protected
    procedure UnsupportedMode(Mode: TBoldDomainElementProxyMode; Func: string);
    procedure Retarget(Mode:  TBoldDomainElementProxyMode);
  public
    constructor Create(Mode:  TBoldDomainElementProxyMode);
    property Mode: TBoldDomainElementProxyMode read fMode;
  end;

  { TBoldDomainElementCollectionTraverser }
  TBoldDomainElementCollectionTraverser = class(TBoldIndexableListTraverser)
  private
    function GetItem: TBoldDomainElement;
  public
    property Item: TBoldDomainElement read GetItem;
    property Current: TBoldDomainElement read GetItem;
  end;

  { TBoldDomainElementCollection }
  TBoldDomainElementCollection = class(TBoldUnorderedIndexableList)
  private
    class var IX_DomainElementCollection: integer;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create;
    function GetEnumerator: TBoldDomainElementCollectionTraverser;
    procedure Add(item: TBoldDomainElement);
    function Includes(item: TBoldDomainElement): Boolean;
    function CreateTraverser: TBoldDomainElementCollectionTraverser;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldIndex
  ;

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
    MyClassName := '<nil>';
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

constructor TBoldDomainElement.CreateWithOwner(OwningElement: TBoldDomainElement);
begin
  FOwningElement := OwningElement;
end;

function TBoldDomainElement.GetBoldSystem: TBoldDomainElement;
begin
  result := nil;
  if Assigned(OwningElement) then
    result := OwningElement.BoldSystem
  else
    if self.ClassName = 'TBoldSystem' then
      result := self;
end;

function TBoldDomainElement.GetDisplayName: String;
begin
  result := Inherited GetDisplayName;
  if Assigned(OwningElement) then
    result := OwningElement.DisplayName + '.' + result;
end;

procedure TBoldDomainElement.SendEvent(OriginalEvent: TBoldEvent);
begin
  if Assigned(OwningElement) then
    OwningElement.ReceiveEventFromOwned(self, OriginalEvent, []);
  inherited;
end;

function TBoldDomainElement.SendQuery(
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber; Originator: TObject = nil): Boolean;
begin
  result := True;
  if Assigned(OwningElement) then
    result := OwningElement.ReceiveQueryFromOwned(Originator, OriginalEvent, Args, Subscriber);
  result := result and inherited SendQuery(OriginalEvent, Args, Subscriber, Originator);
end;

procedure TBoldDomainElement.ReceiveEventFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
begin
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
    OwningElement.ReceiveEventFromOwned(self, OriginalEvent, Args);
  inherited;
end;

function TBoldDomainElement.CanCommit: Boolean;
begin
  result := MayCommit;
{$IFNDEF BOLD_NO_QUERIES}
  result := result and SendQuery(bqMayCommit, [], nil);
{$ENDIF}
end;

function TBoldDomainElement.MayCommit: Boolean;
begin
  result := True;
end;

function TBoldDomainElement.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  result := false;
end;

procedure TBoldDomainElement.Invalidate;
begin
// do nothing
end;

{ TBoldDomainElement_Proxy }

constructor TBoldDomainElement_Proxy.Create(Mode:  TBoldDomainElementProxyMode);
begin
  inherited create;
  fMode := Mode;
end;

procedure TBoldDomainElement_Proxy.Retarget(Mode: TBoldDomainElementProxyMode);
begin
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

constructor TBoldDomainElementCollection.Create;
begin
  inherited create;
  OwnsEntries := false;
  SetIndexVariable(IX_DomainElementCollection, AddIndex(TBoldDomainElementCollectionIndex.Create));
end;

function TBoldDomainElementCollection.CreateTraverser: TBoldDomainElementCollectionTraverser;
begin
  result := inherited CreateTraverser as TBoldDomainElementCollectionTraverser;
end;

function TBoldDomainElementCollection.GetEnumerator: TBoldDomainElementCollectionTraverser;
begin
  result := CreateTraverser;
end;

function TBoldDomainElementCollection.Includes(item: TBoldDomainElement): Boolean;
begin
  result := assigned(TBoldObjectHashIndex(Indexes[IX_DomainElementCollection]).FindByObject(Item));
end;

function TBoldDomainElementCollectionIndex.ItemAsKeyObject(Item: TObject): TObject;
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
  result := TBoldDomainElement(inherited item);
  Assert(result is TBoldDomainElement);
end;

initialization
  TBoldDomainElementCollection.IX_DomainElementCollection := -1;

end.
