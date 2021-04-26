
{ Global compiler directives }
{$include bold.inc}
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
    procedure SetCurrentBoldObject(const Value: TBoldObject);
    procedure SetCurrentElement(const Value: TBoldElement);
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
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject write SetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement write SetCurrentElement;
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
  BoldDefs;

{ TBoldAbstractListHandle }

function TBoldAbstractListHandle.GetCount: Integer;
var
  BoldList: TBoldList;
begin
  BoldList := List;
  if Assigned(BoldList) then
    Result := BoldList.Count
  else
    Result := 0;
end;

procedure TBoldAbstractListHandle.Prior;
begin
  if GetHasPrior then
    CurrentIndex := CurrentIndex - 1
 else
    raise EBold.CreateFmt('%s: No previous element', [ClassName]);
end;

procedure TBoldAbstractListHandle.Next;
begin
  if GetHasNext then
    CurrentIndex := CurrentIndex + 1
  else
    raise EBold.CreateFmt('%s: No next element', [ClassName]);
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
var
  BoldList: TBoldList;
begin
  if CurrentIndex = -1 then begin
    raise EBold.CreateFmt('%s.RemoveCurrentElement: No current element', [ClassName])
  end else begin
    BoldList := List;
    if Assigned(BoldList) and BoldList.Mutable then begin
      BoldList.RemoveByIndex(CurrentIndex);
    end else begin
      BoldList := MutableList;
      if Assigned(BoldList) then begin
        BoldList.Remove(CurrentElement);
      end else begin
        raise EBold.CreateFmt('%s: Can not remove current Element from an immutable list (in %s)', [classname, name]);
      end;
    end;;
  end;
end;

procedure TBoldAbstractListHandle.SetCurrentBoldObject(
  const Value: TBoldObject);
begin
  SetCurrentElement(Value);
end;

procedure TBoldAbstractListHandle.SetCurrentElement(const Value: TBoldElement);
begin
  CurrentIndex := List.IndexOf(Value);
end;

function TBoldAbstractListHandle.GetCurrentBoldObject: TBoldObject;
var
  aCurrentElement: TBoldElement;
begin
  aCurrentElement := CurrentElement;
  if aCurrentElement is TBoldObject then
    Result := TBoldObject(aCurrentElement)
  else if aCurrentElement = nil then
    Result := nil
  else
    raise EBold.CreateFmt('%s.CurrentBoldObject: Current element is not a TBoldObject', [ClassName]);
end;

function TBoldAbstractListHandle.GetListElementType: TBoldElementTypeInfo;
var
  BoldList: TBoldList;
begin
  BoldList := List;
  if Assigned(BoldList) then
    Result := TBoldListTypeInfo(BoldList.BoldType).ListElementTypeInfo
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
var
  BoldList: TBoldList;
begin
  BoldList := List;
  if Assigned(BoldList) then
    Result := TBoldListTypeInfo(BoldList.BoldType)
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
var
  BoldList: TBoldList;
begin
  BoldList := List;
  if BoldList is TBoldObjectList then
    result := TBoldObjectList(BoldList)
  else
    result := nil;
end;

function TBoldAbstractListHandle.GetMutableObjectList: TBoldObjectList;
var
  BoldList: TBoldList;
begin
  BoldList := MutableList;
  if BoldList is TBoldObjectList then
    result := TBoldObjectList(BoldList)
  else
    result := nil;
end;

initialization
  
end.
