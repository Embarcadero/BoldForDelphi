unit BoldAbstractListHandle;

interface

uses
  BoldSystemRT,
  BoldSystem,
  BoldElements,
  BoldRootedHandles;

type
  { forward declarations }
  TBoldAbstractListHandle = class;

  { TBoldAbstractListHandle }
  TBoldAbstractListHandle = class(TBoldRootedHandle)
  private
    function GetCount: Integer;
    function GetCurrentBoldObject: TBoldObject;
    function GetListElementType: TBoldElementTypeInfo;
    function GetHasNext: Boolean;
    function GetHasPrior: Boolean;
    function GetListType: TBoldListTypeInfo;
    function GetStaticListType: TBoldListTypeInfo;
    function GetObjectList: TBoldObjectList;
    function GetMutableObjectList: TBoldObjectList;
  protected
    function GetCurrentElement: TBoldElement; virtual; abstract;
    function GetCurrentIndex: Integer; virtual; abstract;
    function GetList: TBoldList; virtual; abstract;
    procedure SetCurrentIndex(Value: Integer); virtual; abstract;
    function GetMutableList: TBoldList; virtual;
  public
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    procedure RemoveCurrentElement;
    property Count: Integer read GetCount;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: Integer read GetCurrentIndex write SetCurrentIndex;
    property List: TBoldList read GetList;
    property ObjectList: TBoldObjectList read GetObjectList;
    property ListElementType: TBoldElementTypeInfo read GetListElementType;
    property ListType: TBoldListTypeInfo read GetListType;
    property StaticListType: TBoldListTypeInfo read GetStaticListType;
    property HasNext: Boolean read GetHasNext;
    property HasPrior: Boolean read GetHasPrior;
    property MutableList: TBoldList read GetMutableList;
    property MutableObjectList: TBoldObjectList read GetMutableObjectList;
  end;

implementation

uses
  SysUtils,
  HandlesConst,
  BoldDefs;

{ TBoldAbstractListHandle }

function TBoldAbstractListHandle.GetCount: Integer;
begin
 if Assigned(List) then
    Result := List.Count
  else
    Result := 0;
end;

procedure TBoldAbstractListHandle.Prior;
begin
  if GetHasPrior then
    CurrentIndex := CurrentIndex - 1
 else
    raise EBold.CreateFmt(sNoPreviousElement, [ClassName]);
end;

procedure TBoldAbstractListHandle.Next;
begin
  if GetHasNext then
    CurrentIndex := CurrentIndex + 1
  else
    raise EBold.CreateFmt(sNoNextElement, [ClassName]);
end;

procedure TBoldAbstractListHandle.First;
begin
  if Count > 0 then
    CurrentIndex := 0;
end;

procedure TBoldAbstractListHandle.Last;
begin
  CurrentIndex := Count - 1;
end;

procedure TBoldAbstractListHandle.RemoveCurrentElement;
begin
 if CurrentIndex = -1 then
    raise EBold.CreateFmt(sNoCurrentElement, [ClassName])
 else
 begin
   if list.mutable then
     List.RemoveByIndex(CurrentIndex)
   else if assigned(MutableLIst) then
     mutableList.remove(CurrentElement)
   else
     raise EBold.CreateFmt(sCannotRemoveCurrentFromImmutable, [classname, name]);
 end;
end;

function TBoldAbstractListHandle.GetCurrentBoldObject: TBoldObject;
begin
  if CurrentElement is TBoldObject then
    Result := TBoldObject(CurrentElement)
  else if not Assigned(CurrentElement) then
    Result := nil
  else
    raise EBold.CreateFmt(sCurrentElementNotBoldObject, [ClassName]);
end;

function TBoldAbstractListHandle.GetListElementType: TBoldElementTypeInfo;
begin
  if Assigned(List) then
    Result := TBoldListTypeInfo(List.BoldType).ListElementTypeInfo
  else
    Result := StaticBoldType;
end;

function TBoldAbstractListHandle.GetHasNext: Boolean;
begin
  Result := (CurrentIndex + 1) < Count;
end;

function TBoldAbstractListHandle.GetHasPrior: Boolean;
begin
  Result := CurrentIndex > 0;
end;

function TBoldAbstractListHandle.GetListType: TBoldListTypeInfo;
begin
  if Assigned(List) then
    Result := TBoldListTypeInfo(List.BoldType)
  else
    result := StaticListType;
end;

function TBoldAbstractListHandle.GetStaticListType: TBoldListTypeInfo;
begin
  if assigned(StaticSystemTypeInfo) then
    result := StaticSystemTypeInfo.ListTypeInfoByElement[StaticBoldType]
  else
    result := nil;
end;

function TBoldAbstractListHandle.GetMutableList: TBoldList;
var
  BoldList: TBoldList;
begin
  BoldList := List;
  if Assigned(BoldList) and BoldList.Mutable then
    result := BoldList
  else
    result := nil;
end;

function TBoldAbstractListHandle.GetObjectList: TBoldObjectList;
begin
  if list is TBoldObjectList then
    result := list as TBoldObjectList
  else
    result := nil;
end;

function TBoldAbstractListHandle.GetMutableObjectList: TBoldObjectList;
begin
  if MutableList is TBoldObjectList then
    result := MutableList as TBoldObjectList
  else
    result := nil;
end;

end.
