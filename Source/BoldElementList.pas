{ Global compiler directives }
{$include bold.inc}
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
    procedure Initialize; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function InternalAddNew: TBoldElement; override;
    function GetCanCreateNew: Boolean; override;
    procedure InternalClear; override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;    
    //function ProxyClass: TBoldMember_ProxyClass; override;
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

  BoldCoreConsts,
  BoldDefs,
  BoldSubscription,
  BoldSystemRT,
  BoldIndexableList;

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
    constructor Create(OwningList: TBoldList);  override;
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
  if (DuplicateMode = bldmAllow) or (ListController.IndexOfElement(Element) = -1) or DuplicateControl then
  begin
    ListController.AddElement(Element);
    Changed(beItemAdded, [Element]);
  end;
end;

procedure TBoldElementList.AllocateData;
begin
end;

procedure TBoldElementList.Assign(Source: TBoldElement);
begin
  if source is TBoldElementList then
    addlist(Source as TBoldElementList)
  else
    inherited;
end;

procedure TBoldElementList.AssignCloneValue(AClone: TBoldMember);
begin
  if AClone is TBoldElementList then
    TBoldElementList(AClone).AddList(self)
  else
    inherited;
end;

procedure TBoldElementList.FreeData;
begin
end;

function TBoldElementList.GetCanCreateNew: Boolean;
begin
  result := false;
end;

function TBoldElementList.GetCount: Integer;
begin
  result := ListController.Count;
end;

procedure TBoldElementList.InternalClear;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    RemoveByIndex(I);
end;

function TBoldElementList.GetElement(index: Integer): TBoldElement;
begin
  result := ListController.GetElement(index);
end;

function TBoldElementList.GetProxy(
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  raise EBoldInternal.Create('TBoldElementList.GetProxy called');
end;

function TBoldElementList.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := ListController.IncludesElement(item);
end;

function TBoldElementList.IndexOfElement(Item: TBoldElement): Integer;
begin
  result := ListController.IndexOfElement(item);
end;

procedure TBoldElementList.Initialize;
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

(*
function TBoldElementList.ProxyClass: TBoldMember_ProxyClass;
begin
  raise EBold.CreateFmt(sOperationNotAllowed, [className, 'ProxyClass']); // do not localize
end;
*)

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
  inherited;
  fList := TBoldIndexableElementList.Create;
  fList.OwnsEntries := false;
end;

function TBoldElementListController.CreateNew: TBoldElement;
begin
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'CreateNew']);
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
  result := TBoldElement(list.Items[index]);
  Assert(result is TBoldElement, result.classname);
end;

function TBoldElementListController.GetStreamName: string;
begin
  result := '';
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'GetStreamName']);
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
  List.Items[index] := Value;
end;

{ TBoldElementListFactory }

class function TBoldElementListFactory.CreateList(aSystem: TBoldSystem): TBoldElementList;
begin
  result := TBoldElementList.CreateWithTypeInfo(aSystem.BoldSystemTypeInfo.ListTypeInfoByElement[nil]);
end;

end.