
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractListHandleCom;

interface

uses
  BoldComObjectSpace_TLB,
  BoldRootedHandlesCom;

type
  { forward declarations }
  TBoldAbstractListHandleCom = class;

  {-- TBoldAbstractListHandleCom --}
  TBoldAbstractListHandleCom = class(TBoldRootedHandleCom)
  private
    function GetCount: Integer;
    function GetCurrentBoldObject: IBoldObject;
    function GetCurrentElement: IBoldElement;
    function GetCurrentIndex: Integer;
    function GetHasNext: Boolean;
    function GetHasPrior: Boolean;
    function GetList: IBoldList;
    function GetListElementType: IBoldElementTypeInfo;
    procedure SetCurrentIndex(Value: Integer);
  protected
    FCount: Integer;
    FCurrentBoldObject: IBoldObject;
    FCurrentIndex: Integer;
    FList: IBoldList;
    FListElementType: IBoldElementTypeInfo;
    function GetMutableList: IBoldList; virtual;
  public
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    procedure RemoveCurrentElement;
    property Count: Integer read GetCount;
    property CurrentBoldObject: IBoldObject read GetCurrentBoldObject;
    property CurrentElement: IBoldElement read GetCurrentElement;
    property CurrentIndex: Integer read GetCurrentIndex write SetCurrentIndex;
    property List: IBoldList read GetList;
    property ListElementType:IBoldElementTypeInfo read GetListElementType;
    property HasNext: Boolean read GetHasNext;
    property HasPrior: Boolean read GetHasPrior;
    Property MutableList: IBoldList read GetMutableList;
  end;

implementation

uses
  SysUtils,
  BoldDefs;

{-- TBoldAbstractListHandleCom ------------------------------------------------}

function TBoldAbstractListHandleCom.GetCount: Integer;
begin
  EnsureCurrent;
  Result := FCount;
end;

function TBoldAbstractListHandleCom.GetCurrentBoldObject: IBoldObject;
begin
  EnsureCurrent;
  Result := FCurrentBoldObject;
end;

function TBoldAbstractListHandleCom.GetCurrentElement: IBoldElement;
begin
  EnsureCurrent;
  Result := FValue;
end;

function TBoldAbstractListHandleCom.GetCurrentIndex: Integer;
begin
  EnsureCurrent;
  Result := FCurrentIndex;
end;

function TBoldAbstractListHandleCom.GetHasNext: Boolean;
begin
  Result := (CurrentIndex + 1) < Count;
end;

function TBoldAbstractListHandleCom.GetHasPrior: Boolean;
begin
  Result := CurrentIndex > 0;
end;

function TBoldAbstractListHandleCom.GetList: IBoldList;
begin
  EnsureCurrent;
  Result := FList;
end;

function TBoldAbstractListHandleCom.GetListElementType: IBoldElementTypeInfo;
begin
  EnsureCurrent;
  Result := FListElementType;
end;

procedure TBoldAbstractListHandleCom.First;
begin
  if Count > 0 then
    CurrentIndex := 0;
end;

procedure TBoldAbstractListHandleCom.Last;
begin
  CurrentIndex := Count - 1;
end;

procedure TBoldAbstractListHandleCom.Next;
begin
  if GetHasNext then
    CurrentIndex := CurrentIndex + 1
  else
    raise EBold.Create('No next element');
end;

procedure TBoldAbstractListHandleCom.Prior;
begin
  if GetHasPrior then
    CurrentIndex := CurrentIndex - 1
  else
    raise EBold.Create('No previous element');
end;

procedure TBoldAbstractListHandleCom.RemoveCurrentElement;
begin
  if CurrentIndex = -1 then
    raise EBold.Create('No current element')
  else
    if HasHandleOnServer then
      List.RemoveByIndex(CurrentIndex);
end;

procedure TBoldAbstractListHandleCom.SetCurrentIndex(Value: Integer);
begin
  if (Value <> CurrentIndex) then
  begin
    FCurrentIndex := Value;
    LocalValueChanged;
  end;
end;

function TBoldAbstractListHandleCom.GetMutableList: IBoldList;
begin
  if assigned(list) and list.Mutable then
    result := list
  else
    result := nil;
end;

end.
