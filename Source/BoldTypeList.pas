{ Global compiler directives }
{$include bold.inc}
unit BoldTypeList;

interface

uses
  BoldBase,
  BoldSubscription,
  BoldSystem,
  BoldDomainElement,
  BoldElements,
  BoldDefs;

type
  { forward declarations }
  TBoldTypeListFactory = class;
  TBoldTypeList = class;

  { TBoldTypeList }
  TBoldTypeList = class(TBoldList)
  protected
    procedure AddElement(Element: TBoldElement); override;
    procedure AllocateData; override;
    procedure FreeData; override;
    function GetCount: Integer; override;
    function GetElement(index: Integer): TBoldElement; override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure Initialize; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function InternalAddNew: TBoldElement; override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure InternalClear; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
  public
    procedure Assign(Source: TBoldElement); override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure InsertNew(index: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
  end;

  { TBoldTypeListFactory }
  TBoldTypeListFactory = class(TBoldMemoryManagedObject)
  public
    class function CreateList(elementTypeInfo: TBoldElementTypeInfo): TBoldTypeList;
  end;

implementation

uses
  SysUtils,

  BoldMetaElementList,
  BoldSystemRT,
  BoldCoreConsts;

type
  { TBoldTypeListController }
  TBoldTypeListController = class(TBoldListController)
  private
    fList: TBoldElementTypeInfoList;
  protected
    function GetCount: Integer; override;
    function GetCanCreateNew: Boolean; override;
    function CreateNew: TBoldElement; override;
    property List: TBoldElementTypeInfoList read fList;
    function GetStreamName: string; override;
  public
    constructor Create(OwningList: TBoldList); override;
    destructor Destroy; override;
    procedure AddElement(Element: TBoldElement); override;
    function GetElement(index: Integer): TBoldElement; override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
  end;

{ TBoldTypeList }

procedure TBoldTypeList.AddElement(Element: TBoldElement);
begin
  if (DuplicateMode = bldmAllow) or (ListController.IndexOfElement(element) = -1) or DuplicateControl then
    ListController.AddElement(element);
end;

procedure TBoldTypeList.AllocateData;
begin
end;

procedure TBoldTypeList.Assign(Source: TBoldElement);
begin
  if source is TBoldTypeList then
    addlist(Source as TBoldTypeList)
  else
    inherited;
end;

procedure TBoldTypeList.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  inherited;
end;

procedure TBoldTypeList.FreeData;
begin
end;

function TBoldTypeList.GetCount: Integer;
begin
  result := ListController.Count;
end;

function TBoldTypeList.GetElement(index: Integer): TBoldElement;
begin
  result := ListController.GetElement(index);
end;

function TBoldTypeList.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  raise EBold.CreateFmt('%s.GetProxy: Not available in this class.', [classname])
end;

function TBoldTypeList.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  result := IntToStr(Count);
end;

function TBoldTypeList.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := ListController.IncludesElement(item);
end;

function TBoldTypeList.IndexOfElement(Item: TBoldElement): Integer;
begin
  result := ListController.IndexOfElement(item);
end;

procedure TBoldTypeList.Initialize;
begin
  ListController := TBoldTypeListController.Create(self);
  DuplicateMode := bldmAllow;
  inherited;
end;

procedure TBoldTypeList.InsertElement(index: Integer; Element: TBoldElement);
begin
  ListController.InsertElement(index, element);
end;

procedure TBoldTypeList.InsertNew(index: Integer);
begin
  raise EBold.CreateFmt(sCannotInsertTypes, [classname])
end;

function TBoldTypeList.InternalAddNew: TBoldElement;
begin
  raise EBold.CreateFmt(sCannotAddTypes, [classname])
end;

procedure TBoldTypeList.InternalClear;
var
  i: integer;
begin
  for I := Count - 1 downto 0 do
    RemoveByIndex(I);
end;

procedure TBoldTypeList.Move(CurIndex, NewIndex: Integer);
begin
  ListController.Move(CurIndex, NewIndex);
end;

(*
function TBoldTypeList.ProxyClass: TBoldMember_ProxyClass;
begin
  raise EBold.CreateFmt(sAbstractError_InterfaceNotSupported, [ClassName]);
end;
*)

procedure TBoldTypeList.RemoveByIndex(index: Integer);
begin
  ListController.RemoveByIndex(index);
end;

procedure TBoldTypeList.SetElement(index: Integer; Value: TBoldElement);
begin
  ListController.SetElement(index, value);
end;

{ TBoldTypeListController }

procedure TBoldTypeListController.AddElement(Element: TBoldElement);
begin
  if element is TBoldElementTypeInfo then
    list.Add(element as TBoldElementTypeInfo)
  else
    raise EBold.CreateFmt(sCannotAddElement, [element.ClassName]);
end;

constructor TBoldTypeListController.Create(OwningList: TBoldList);
begin
  fList := TBoldElementTypeInfoList.Create;
  fList.OwnsEntries := false;
end;

function TBoldTypeListController.CreateNew: TBoldElement;
begin
  raise EBold.Create(sCannotCreateNewInTypeLists);
end;

destructor TBoldTypeListController.Destroy;
begin
  fList.Free;
  inherited;
end;

function TBoldTypeListController.GetCanCreateNew: Boolean;
begin
  result := false;
end;

function TBoldTypeListController.GetCount: Integer;
begin
  result := List.Count;
end;

function TBoldTypeListController.GetElement(index: Integer): TBoldElement;
begin
  result := list[index];
end;

function TBoldTypeListController.GetStreamName: string;
begin
  result := '';
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'GetStreamName']);
end;

function TBoldTypeListController.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := list.IndexOf(item) <> -1;
end;

function TBoldTypeListController.IndexOfElement(Item: TBoldElement): Integer;
begin
  result := list.IndexOf(item);
end;

procedure TBoldTypeListController.InsertElement(index: Integer;  Element: TBoldElement);
begin
  list.Insert(index, element);
end;

procedure TBoldTypeListController.Move(CurrentIndex, NewIndex: Integer);
begin
  list.Move(CurrentIndex, NewIndex);
end;

procedure TBoldTypeListController.RemoveByIndex(index: Integer);
begin
  list.RemoveByIndex(index);
end;

procedure TBoldTypeListController.SetElement(index: Integer; Value: TBoldElement);
begin
  raise Ebold.Create(sCannotSetElementsInTypeLists);
end;

{ TBoldTypeListFactory }

class function TBoldTypeListFactory.CreateList(elementTypeInfo: TBoldElementTypeInfo): TBoldTypeList;
begin
  with elementTypeInfo.SystemTypeInfo as TBoldSystemTypeInfo do
    result := TBoldTypeList.CreateWithTypeInfo(ListTypeInfoByElement[TypeTypeInfo]);
end;

end.