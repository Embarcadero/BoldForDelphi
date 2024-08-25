{ Global compiler directives }
{$include bold.inc}
unit BoldListControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  BoldDefs,
  Classes,
  BoldElements,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldSubscription,
  BoldControlPack;

type
  {Forward declarations of all classes}
  TBoldAsFollowerListRenderer = class;
  TBoldAsFollowerListController = class;
  TBoldFollowerList = class;

  {---TBoldAsFollowerListController---}
  TBoldAsFollowerListController = class(TBoldFollowerController)
  private
    fOnBeforeInsertItem: TBoldSubFollowerEvent;
    fOnAfterInsertItem: TBoldSubFollowerEvent;
    fOnBeforeDeleteItem: TBoldSubFollowerEvent;
    fOnAfterDeleteItem: TBoldSubFollowerEvent;
    fOnReplaceItem: TBoldSubFollowerEvent;
  protected
    procedure DoBeforeInsertItem(index: Integer; OwningFollower: TBoldFollower);
    procedure DoAfterInsertItem(index: Integer; Follower: TBoldFollower);
    procedure DoBeforeDeleteItem(index: Integer; Follower: TBoldFollower);
    procedure DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure DoReplaceItem(index: Integer; Follower: TBoldFollower);
    procedure CleanRendererData(RendererData: TBoldRendererData); override;
    class function PrecreateFollowers: boolean; virtual;
  public
    constructor Create(aOwningComponent: TComponent);
    procedure SetActiveRange(Follower: TBoldFollower; FirstActive: Integer; LastActive: Integer; RangeBuffer: Integer = 1);
    procedure SelectAll(Follower: TBoldFollower; SetSelect: Boolean); {Maybe make available in renderer?}
    procedure SelectRange(Follower: TBoldFollower; index: Integer);
    procedure SetSelected(Follower: TBoldFollower; index: Integer; Value: Boolean);
    procedure ToggleSelected(Follower: TBoldFollower; index: Integer);
    procedure AddSelectedToList(Follower: TBoldFollower; BoldList: TBoldList);
    function GetSelected(Follower: TBoldFollower; index: Integer): Boolean;
    property OnBeforeInsertItem: TBoldSubFollowerEvent read fOnBeforeInsertItem write fOnBeforeInsertItem;
    property OnAfterInsertItem: TBoldSubFollowerEvent read fOnAfterInsertItem write fOnAfterInsertItem;
    property OnBeforeDeleteItem: TBoldSubFollowerEvent read fOnBeforeDeleteItem write fOnBeforeDeleteItem;
    property OnAfterDeleteItem: TBoldSubFollowerEvent read fOnAfterDeleteItem write fOnAfterDeleteItem;
    property OnReplaceitem: TBoldSubFollowerEvent read fOnReplaceItem write fOnReplaceItem;
  end;

  {--- TBoldAsFollowerListRenderer ---}
  TBoldAsFollowerListRenderer = class(TBoldRenderer)
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
    procedure SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: integer = 1); virtual;
  end;

  {---TBoldFollowerList---}
  TBoldFollowerList = class(TBoldRendererData)
  private
    FArray: TList;
    FFirstActive: Integer;
    FLastActive: Integer;
    FCurrentItem: Integer;
    FPrevSelected: Integer;
    function GetSelectedCount: Integer;
    procedure SetSelected(index: Integer; V: Boolean);
    function GetSelected(index: Integer): Boolean;
    function UnsafeGetSelected(index: Integer): Boolean;
    procedure Insert(ListFollowerController: TBoldAsFollowerListController; Index: Integer; Follower: TBoldFollower);
    procedure Delete(ListFollowerController: TBoldAsFollowerListController; index: Integer);
    procedure Replace(ListFollowerController: TBoldAsFollowerListController; index: Integer; Follower: TBoldFollower);
    function GetBoldList: TBoldList;
  protected
    function UnsafeGetSubFollower(Index: Integer): TBoldFollower;
    procedure EnsureSubfollowersDisplayable; override;
    function GetSubFollowerCount: Integer; override;
    function GetSubFollower(Index: Integer): TBoldFollower; override;
    function GetEnsuredSubFollower(Index: Integer): TBoldFollower; override;
    function GetCurrentSubFollowerIndex: Integer; override;
    procedure SetCurrentSubFollowerIndex(index: integer); override;
    function GetSubFollowerAssigned(Index: Integer): boolean; override;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    destructor Destroy; override;
    procedure SetCapacity(aNewCapacity: integer);
    procedure SetActiveRange(FirstActive, LastActive: Integer);
    procedure SelectAll(SetSelect: Boolean);
    procedure SelectRange(index: Integer);
    procedure AddSelectedToList(BoldList: TBoldList);
    procedure PurgeEnd(ListFollowerController: TBoldAsFollowerListController; PurgeCount: Integer);
    function EnsuredFollower(aListFollowerController: TBoldAsFollowerListController;
                             aIndex: Integer;
                             aElement: TBoldElement;
                             aController: TBoldFollowerController): TBoldFollower; { may disturb list after index}
    property Followers[index: Integer]: TBoldFollower read GetSubFollower; default;
    property Count: Integer read GetSubFollowerCount;
    property FirstActive: Integer read FFirstActive;
    property LastActive: Integer read FLastActive;
    property SelectedCount: Integer read GetSelectedCount;
    property Selected[index: Integer]: Boolean read GetSelected write SetSelected;
    property CurrentItem: Integer read FCurrentItem write FCurrentItem;
    property BoldList: TBoldList read GetBoldList;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldControllerListControlPack,
  BoldListListControlPack;

{---TBoldAsFollowerListController---}

constructor TBoldAsFollowerListController.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent);
end;

procedure TBoldAsFollowerListController.DoBeforeInsertItem(index: Integer; OwningFollower: TBoldFollower);
begin
  if Assigned(FOnBeforeInsertItem) then
    FOnBeforeInsertItem(index, OwningFollower);
end;

procedure TBoldAsFollowerListController.DoReplaceItem(index: Integer;
  Follower: TBoldFollower);
begin
  if Assigned(fOnReplaceItem) then
    fOnReplaceItem(index, Follower);
end;

procedure TBoldAsFollowerListController.DoAfterInsertItem(index: Integer; Follower: TBoldFollower);
begin
  if Assigned(FOnAfterInsertItem) then
    FOnAfterInsertItem(index, Follower);
end;

procedure TBoldAsFollowerListController.DoBeforeDeleteItem(index: Integer; Follower: TBoldFollower);
begin
  if Assigned(FOnBeforeDeleteItem) then
    FOnBeforeDeleteItem(index, Follower);
end;

procedure TBoldAsFollowerListController.DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
begin
  if Assigned(FOnAfterDeleteItem) then
    FOnAfterDeleteItem(index, OwningFollower);
end;

procedure TBoldAsFollowerListController.SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: Integer = 1);
begin
  if Assigned(EffectiveRenderer) then
    (EffectiveRenderer as TBoldAsFollowerListRenderer).SetActiveRange(Follower, FirstActive, LastActive, RangeBuffer);
end;

procedure TBoldAsFollowerListController.SelectAll(Follower: TBoldFollower; SetSelect: Boolean);
begin
  if SetSelect {and Follower.ElementValid and (Follower.Element is TBoldList)} then
     TBoldAbstractListAsFollowerListController(Follower.Controller).SetActiveRange(Follower, 0, MaxInt);
  (Follower.RendererData as TBoldFollowerList).SelectAll(SetSelect)
end;

procedure TBoldAsFollowerListController.SelectRange(Follower: TBoldFollower; index: Integer);
begin
  (Follower.RendererData as TBoldFollowerList).SelectRange(index)
end;

procedure TBoldAsFollowerListController.SetSelected(Follower: TBoldFollower; index: Integer; Value: Boolean);
begin
  (Follower.RendererData as TBoldFollowerList).Selected[index] := Value;
end;

procedure TBoldAsFollowerListController.ToggleSelected(Follower: TBoldFollower; index: Integer);
begin
  with (Follower.RendererData as TBoldFollowerList) do
    Selected[index] := not Selected[index];
end;

function TBoldAsFollowerListController.GetSelected(Follower: TBoldFollower; index: Integer): Boolean;
begin
  Result := (Follower.RendererData as TBoldFollowerList).Selected[index]
end;

{---TBoldAsFollowerListRenderer---}
function TBoldAsFollowerListRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldFollowerList;
end;

procedure TBoldAsFollowerListRenderer.SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: integer = 1);
begin
  (Follower.RendererData as TBoldFollowerList).SetActiveRange(FirstActive, LastActive);
end;

{---TBoldFollowerList---}
constructor TBoldFollowerList.Create(OwningFollower: TBoldFollower);
begin
  inherited Create(OwningFollower);
  FArray := TList.Create;
  FCurrentItem := -1;
  FLastActive := MaxInt;
end;

function TBoldFollowerList.UnsafeGetSubFollower(
  Index: Integer): TBoldFollower;
begin
  Result := TBoldFollower(FArray[index])
end;

destructor TBoldFollowerList.Destroy;
var
  I: Integer;
//  Follower: TBoldFollower;
//  lBoldTreeNode: TBoldTreeNode;
begin
  for I := Count - 1 downto 0 do
  begin
{    Follower := UnsafeGetSubFollower(I);
    if Assigned(Follower) and Assigned(Follower.ControlData) then
    begin
      lBoldTreeNode := Follower.ControlData as TBoldTreeNode;
      lBoldTreeNode.Follower := nil;
    end;
}
    UnsafeGetSubFollower(I).free;
  end;
  FreeAndNil(FArray);
  inherited;
end;

procedure TBoldFollowerList.SetActiveRange(FirstActive, LastActive: Integer);
var
  lIndex: Integer;
  lFollower: TBoldFollower;  
  lActive: boolean;
begin
  FFirstActive := FirstActive;
  FLastActive := LastActive;
  for lIndex := 0 to Count - 1 do
  begin
    lActive := (lIndex >= FirstActive) and (lIndex <= LastActive);
    if lActive then
      lFollower := GetSubFollower(lIndex)
    else
      lFollower := UnsafeGetSubFollower(lIndex);
    if Assigned(lFollower) then
      lFollower.Active := lActive;
  end;
end;

function TBoldFollowerList.GetSelectedCount: Integer;
var
  Row: Integer;
begin
  Result := 0;
  for Row := 0 to Count - 1 do
    if UnsafeGetSelected(Row) then
      Inc(Result);
end;

function TBoldFollowerList.GetSubFollowerCount: Integer;
begin
  Result := FArray.Count;
end;

function TBoldFollowerList.GetSubFollower(Index: Integer): TBoldFollower;
  procedure InternalRaise;
  begin
    Assert( Cardinal(Index) < Cardinal(fArray.Count), 'TBoldFollowerList.GetSubFollower index out of range.' + IntToStr(Index) + '/' + IntToStr(fArray.count));
  end;

begin
  if Cardinal(Index) >= Cardinal(fArray.Count) then
    InternalRaise;
  Result := TBoldFollower(FArray[index]);
  if not Assigned(Result) then
  begin
    if (OwningFollower.Controller is TBoldControllerList) then
    begin
      result := EnsuredFollower(
        OwningFollower.Controller as TBoldAsFollowerListController,
        Index,
        OwningFollower.element,
        TBoldControllerList(OwningFollower.Controller)[Index]);
      FArray[index] := result;
      Assert(Assigned(result));
    end;
  end;
end;

function TBoldFollowerList.GetEnsuredSubFollower(
  Index: Integer): TBoldFollower;
var
  BoldAsFollowerListController: TBoldAsFollowerListController;
begin
  result := GetSubFollower(Index);
  if not Assigned(result) then
  begin
    BoldAsFollowerListController := nil;
    if (OwningFollower.Controller is TBoldControllerList) then
      BoldAsFollowerListController := OwningFollower.Controller as TBoldControllerList
    else
    if (OwningFollower.Controller is TBoldListAsFollowerListController) then
      BoldAsFollowerListController := TBoldListAsFollowerListController(OwningFollower.Controller);

    if Assigned(BoldAsFollowerListController) then
    begin
//      Delete(OwningFollower.Controller as TBoldAsFollowerListController, Index);
      result := TBoldFollower.CreateSubFollower(OwningFollower, BoldAsFollowerListController, OwningFollower.element, true, Index);
      Replace(BoldAsFollowerListController, Index, result);
//      Insert(OwningFollower.Controller as TBoldAsFollowerListController, Index, result);
    end;
  end;
end;

function TBoldFollowerList.GetBoldList: TBoldList;
begin
  Assert(Assigned(OwningFollower));
  if OwningFollower.Element = nil then
    result := nil
  else
  begin
    Assert(OwningFollower.Element is TBoldList);
    result := OwningFollower.Element as TBoldList;
  end;
end;

function TBoldFollowerList.GetCurrentSubFollowerIndex: Integer;
begin
  Result := FCurrentItem;
end;

procedure TBoldFollowerList.SetSelected(index: Integer; V: Boolean);
begin
  if (index > -1) and
    (index < Count) and
    Assigned(Followers[index]) and (Followers[index].Selected <> V) then
  begin
    if V then
      FPrevSelected := index;
    Followers[index].Selected := V;
  end;
end;

function TBoldFollowerList.GetSubFollowerAssigned(Index: Integer): boolean;
begin
  if Index < Count then
    Result := Assigned(UnsafeGetSubFollower(index))
  else
    result := false;
end;

function TBoldFollowerList.GetSelected(index: Integer): Boolean;
begin
  Result := Assigned(Followers[index]) and Followers[index].Selected;
end;

procedure TBoldFollowerList.SelectAll(SetSelect: Boolean);
var
  Row: Integer;
  lFollower: TBoldFollower;
begin
  for Row := 0 to Count - 1 do
  begin
    if SetSelect then
      lFollower := GetEnsuredSubFollower(Row)
    else
      lFollower := UnsafeGetSubFollower(Row);
    Assert(not SetSelect or Assigned(lFollower));
    if Assigned(lFollower) then
      lFollower.Selected := SetSelect;
  end;
end;

procedure TBoldFollowerList.SelectRange(index: Integer);
var
  pSelect: Integer;
  Row: Integer;

begin
  SelectAll(False);
  pSelect := FPrevSelected;
  for Row :=  FPrevSelected to index do
    Selected[Row] := True;
  for Row := FPrevSelected downto index do
    Selected[Row] := True;
  FPrevSelected := pSelect;
end;

{
procedure TBoldFollowerList.UnselectPrev;
begin
  if FPrevSelected <> -1 then
    Selected[FPrevSelected] := False;
end;
}

procedure TBoldFollowerList.AddSelectedToList(BoldList: TBoldList);
var
  Row: Integer;
  lBoldFollower: TBoldFollower;
begin
  for Row := 0 to Count - 1 do
  begin
    lBoldFollower := UnsafeGetSubFollower(Row);
    if Assigned(lBoldFollower) and (lBoldFollower.Selected) and
       Assigned(lBoldFollower.Element) and
       BoldTestType(lBoldFollower.Element, TBoldObject) then
      BoldList.Add(lBoldFollower.Element);
  end;
end;

procedure TBoldFollowerList.Delete(ListFollowerController: TBoldAsFollowerListController; Index: Integer);
var
  I: Integer;
  lFollower: TBoldFollower;
begin
  lFollower := UnsafeGetSubFollower(index);
  begin
    ListFollowerController.DoBeforeDeleteItem(index, lFollower);
    lFollower.Free;
  end;
  FArray.Delete(index);
  for I := index to Count - 1 do
  begin
    lFollower := UnsafeGetSubFollower(I);
    if Assigned(lFollower) then
      lFollower.Index := I;
  end;
  ListFollowerController.DoAfterDeleteItem(Index, OwningFollower);
end;

procedure TBoldFollowerList.Insert(ListFollowerController: TBoldAsFollowerListController; Index: Integer; Follower: TBoldFollower);
var
  I: Integer;
  lFollower: TBoldFollower;
begin
  ListFollowerController.DoBeforeInsertItem(Index, OwningFollower);
  FArray.Insert(index, Follower);
  Assert(not Assigned(Follower) or (Follower.index = index));
  for I := index+1 to Count - 1 do
  begin
    lFollower := UnsafeGetSubFollower(I);
    if Assigned(lFollower) then
      lFollower.Index := I;
  end;
  ListFollowerController.DoAfterInsertItem(Index, Follower);
end;

procedure TBoldFollowerList.Replace(ListFollowerController: TBoldAsFollowerListController; Index: Integer; Follower: TBoldFollower);
begin
  ListFollowerController.DoReplaceItem(Index, Follower);
  FArray[Index] := Follower;
end;

procedure TBoldFollowerList.PurgeEnd(ListFollowerController: TBoldAsFollowerListController; PurgeCount: Integer);
var
  I: Integer;
begin
  if Count > PurgeCount then
  begin
    TBoldPublisher.StartNotify;
    try
      for I := Count - 1 downto PurgeCount do
        Delete(ListFollowerController, I);
    finally
      TBoldPublisher.EndNotify;
    end;
  end;
end;

function TBoldFollowerList.EnsuredFollower(aListFollowerController: TBoldAsFollowerListController;
                                           aIndex: Integer;
                                           aElement: TBoldElement;
                                           aController: TBoldFollowerController): TBoldFollower;
var
  lBoldFollower: TBoldFollower;
  lActive: boolean;
begin
  result := nil;
  lActive := (aIndex <= LastActive) and (aIndex >= FirstActive); // and not aController.DelayedActivate;
  if (not Assigned(aElement) or not lActive) and not (aListFollowerController.PrecreateFollowers) then
  begin
    if (aIndex >= Count) then
    begin
      Assert(aIndex = Count);
      Insert(aListFollowerController, aIndex, nil);
    end
    else
    begin
      result := TBoldFollower(FArray[aIndex]);
      if (not Assigned(result) and Assigned(aElement)) then
      begin
//        Delete(aListFollowerController, aIndex);
        lBoldFollower := TBoldFollower.CreateSubFollower(OwningFollower, aController, aElement, lActive, aIndex);
        Replace(aListFollowerController, aIndex, lBoldFollower);
//        Insert(aListFollowerController, aIndex, lBoldFollower);
        result := lBoldFollower;
      end
      else
      begin
        if (Assigned(result) and not Assigned(aElement) and ((result.Controller <> aController))) then
        begin
          Insert(aListFollowerController, aIndex, nil);
          result := nil;
        end;
      end;
    end;
    exit;
  end;
  if (aIndex < Count) then
  begin
    lBoldFollower := UnsafeGetSubFollower(aIndex);
    if Assigned(lBoldFollower) then
    begin
      if (lBoldFollower.Element = aElement) and (lBoldFollower.Controller = aController) then
      begin
        result := lBoldFollower;
        result.Active := lActive;
        exit;
      end
      else
      begin
        Assert(Assigned(aListFollowerController), lBoldFollower.className );
//        if (lBoldFollower.Controller = aController)
//        begin
//          lBoldFollower.SetElementAndMakeCurrent(AElement, lActive);
//          Replace(aListFollowerController, aIndex, lBoldFollower);
//          result := lBoldFollower;
//        end
//        else
        begin
          Delete(aListFollowerController, aIndex);
          lBoldFollower := TBoldFollower.CreateSubFollower(OwningFollower, aController, aElement, lActive, aIndex);
          Insert(aListFollowerController, aIndex, lBoldFollower);
          result := lBoldFollower;
        end;
      end;
    end
    else
    begin
//      Delete(aListFollowerController, aIndex);
      lBoldFollower := TBoldFollower.CreateSubFollower(OwningFollower, aController, aElement, lActive, aIndex);
      Replace(aListFollowerController, aIndex, lBoldFollower);
//      Insert(aListFollowerController, aIndex, lBoldFollower);
      result := lBoldFollower;
    end;
  end
  else
  begin
    lBoldFollower := TBoldFollower.CreateSubFollower(OwningFollower, aController, aElement, lActive, aIndex);
    Insert(aListFollowerController, aIndex, lBoldFollower);
    result := lBoldFollower;
  end;
end;

procedure TBoldFollowerList.SetCapacity(aNewCapacity: integer);
begin
  if aNewCapacity > FArray.Capacity then
    FArray.Capacity := aNewCapacity;
end;

procedure TBoldFollowerList.SetCurrentSubFollowerIndex(index: integer);
begin
  inherited;
  FCurrentItem := index;
end;

procedure TBoldFollowerList.EnsureSubfollowersDisplayable;
var
  lIndex: integer;
  lBoldFollower: TBoldFollower;
begin
  for lIndex := 0 to Count - 1 do
  begin
    lBoldFollower := UnsafeGetSubFollower(lIndex);
    if Assigned(lBoldFollower) and not lBoldFollower.Displayable and lBoldFollower.Active then
    begin
      lBoldFollower.EnsureDisplayable;
    end;
  end;
end;

procedure TBoldAsFollowerListController.CleanRendererData(RendererData: TBoldRendererData);
begin
  inherited;
  (RendererData as TBoldFollowerList).PurgeEnd(self, 0);
end;

procedure TBoldAsFollowerListController.AddSelectedToList(Follower: TBoldFollower; 
  BoldList: TBoldList);
begin
  (Follower.RendererData as TBoldFollowerList).AddSelectedToList(BoldList);
end;

function TBoldFollowerList.UnsafeGetSelected(index: Integer): Boolean;
begin
  Result := UnsafeGetSubFollower(index).Selected;
end;

class function TBoldAsFollowerListController.PrecreateFollowers: boolean;
begin
  result := false;
end;

end.
