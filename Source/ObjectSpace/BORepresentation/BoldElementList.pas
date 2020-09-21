unit BoldElementList;

interface

uses
  BoldElements,
  BoldSystem,
  BoldDomainElement;

type
  { forward declarations }
  TBoldElementList = class;
  TBoldElementListFactory = class;

  { TBoldElementList }
  TBoldElementList = class(TBoldList)
  protected
    procedure AddElement(Element: TBoldElement); override;
    procedure AllocateData; override;
    procedure FreeData; override;
    function GetCount: Integer; override;
    function GetElement(index: Integer): TBoldElement; override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function InternalAddNew: TBoldElement; override;
    function GetCanCreateNew: Boolean; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure Assign(Source: TBoldElement); override;
    procedure InsertNew(index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
  end;

  { TBoldElementListFactory }
  TBoldElementListFactory = class
  public
    class function CreateList(aSystem: TBoldSystem): TBoldElementList;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldSubscription,
  BoldSystemRT,
  BoldIndexableList,
  BoldCoreConsts;

type
  { TBoldIndexableElementList }
  TBoldIndexableElementList = class(TBoldIndexableList)
  end;

  { TBoldElementListController }
  TBoldElementListController = class(TBoldListController)
  private
    fList: TBoldIndexableElementList;
  protected
    function GetCount: Integer; override;
    function GetCanCreateNew: Boolean; override;
    function CreateNew: TBoldElement; override;
    property List: TBoldIndexableElementList read fList;
    function GetStreamName: string; override;
  public
    constructor Create(OwningList: TBoldList);
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

{ TBoldElementList }

procedure TBoldElementList.AddElement(Element: TBoldElement);
begin
  if (ListController.IndexOfElement(Element) = -1) or DuplicateControl then
  begin
    listcontroller.AddElement(Element);
    Changed(beItemAdded, [Element]);
  end;
end;

procedure TBoldElementList.AllocateData;
begin
  // do nothing
end;

procedure TBoldElementList.Assign(Source: TBoldElement);
begin
  if source is TBoldElementList then
    addlist(Source as TBoldElementList)
  else
    inherited;
end;

procedure TBoldElementList.FreeData;
begin
  // do nothing
end;

function TBoldElementList.GetCanCreateNew: Boolean;
begin
  result := false;
end;

function TBoldElementList.GetCount: Integer;
begin
  result := ListController.Count;
end;

function TBoldElementList.GetElement(index: Integer): TBoldElement;
begin
  result := ListController.GetElement(index);
end;

function TBoldElementList.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := ListController.IncludesElement(item);
end;

function TBoldElementList.IndexOfElement(Item: TBoldElement): Integer;
begin
  result := ListController.IndexOfElement(item);
end;

procedure TBoldElementList.InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo);
begin
  ListController := TBoldElementListController.Create(self);
  DuplicateMode := bldmAllow;
  inherited;
end;

procedure TBoldElementList.InsertElement(index: Integer;
  Element: TBoldElement);
begin
  ListController.InsertElement(index, element);
end;

procedure TBoldElementList.InsertNew(index: Integer);
begin
  raise EBold.CreateFmt(sOperationNotAllowed, [className, 'InsertNew']); // do not localize
end;

function TBoldElementList.InternalAddNew: TBoldElement;
begin
  raise EBold.CreateFmt(sOperationNotAllowed, [className, 'InternalAddNew']); // do not localize
end;

procedure TBoldElementList.Move(CurIndex, NewIndex: Integer);
begin
  ListController.Move(CurIndex, NewIndex);
end;

function TBoldElementList.ProxyClass: TBoldMember_ProxyClass;
begin
  raise EBold.CreateFmt(sOperationNotAllowed, [className, 'ProxyClass']); // do not localize
end;

procedure TBoldElementList.RemoveByIndex(index: Integer);
begin
  ListController.RemoveByIndex(index);
  Changed(beItemDeleted, []);
end;

procedure TBoldElementList.SetElement(index: Integer;
  Value: TBoldElement);
begin
  ListController.SetElement(index, value);
end;

{ TBoldElementListController }

procedure TBoldElementListController.AddElement(Element: TBoldElement);
begin
  list.Add(element)
end;

constructor TBoldElementListController.Create(OwningList: TBoldList);
begin
  fList := TBoldIndexableElementList.Create;
  fList.OwnsEntries := false;
end;

function TBoldElementListController.CreateNew: TBoldElement;
begin
  raise EBold.Create(sNotImplemented);
end;

destructor TBoldElementListController.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TBoldElementListController.GetCanCreateNew: Boolean;
begin
  result := false;
end;

function TBoldElementListController.GetCount: Integer;
begin
  result := List.Count;
end;

function TBoldElementListController.GetElement(index: Integer): TBoldElement;
begin
  result := list.Items[index] as TBoldElement;
end;

function TBoldElementListController.GetStreamName: string;
begin
  result := '';
  raise EBold.create(sNotImplemented);
end;

function TBoldElementListController.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := list.IndexOf(item) <> -1;
end;

function TBoldElementListController.IndexOfElement(Item: TBoldElement): Integer;
begin
  result := list.IndexOf(item);
end;

procedure TBoldElementListController.InsertElement(index: Integer; Element: TBoldElement);
begin
  list.Insert(index, element);
end;

procedure TBoldElementListController.Move(CurrentIndex, NewIndex: Integer);
begin
  list.Move(CurrentIndex, NewIndex);
end;

procedure TBoldElementListController.RemoveByIndex(index: Integer);
begin
  list.RemoveByIndex(index);
end;

procedure TBoldElementListController.SetElement(index: Integer; Value: TBoldElement);
begin
  Raise EBold.Create(sCannotSetElementsInTypeLists);
end;

{ TBoldElementListFactory }

class function TBoldElementListFactory.CreateList(aSystem: TBoldSystem): TBoldElementList;
begin
  result := TBoldElementList.CreateWithTypeInfo(aSystem.BoldSystemTypeInfo.ListTypeInfoByElement[nil]);
end;

end.


