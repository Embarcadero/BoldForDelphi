unit BoldListControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  BoldDefs,
  Classes,
  BoldElements,
  {$IFNDEF BOLDCOMCLIENT} // uses
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
    fOnAfterInsertItem: TBoldFollowerEvent;
    fOnBeforeDeleteItem: TBoldFollowerEvent;
    fOnAfterDeleteItem: TBoldSubFollowerEvent;
  protected
    procedure DoBeforeInsertItem(index: Integer; OwningFollower: TBoldFollower);
    procedure DoAfterInsertItem(Follower: TBoldFollower);
    procedure DoBeforeDeleteItem(Follower: TBoldFollower);
    procedure DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure CleanRendererData(RendererData: TBoldRendererData); override;
  public
    constructor Create(aOwningComponent: TComponent);
    procedure SetActiveRange(Follower: TBoldFollower; FirstActive: Integer; LastActive: Integer; RangeBuffer: Integer = 1);
    procedure SelectAll(Follower: TBoldFollower; SetSelect: Boolean); {Maybe make available in renderer?}
    procedure SelectRange(Follower: TBoldFollower; index: Integer);
    procedure SetSelected(Follower: TBoldFollower; index: Integer; Value: Boolean);
    procedure ToggleSelected(Follower: TBoldFollower; index: Integer);
    function GetSelected(Follower: TBoldFollower; index: Integer): Boolean;
    property OnBeforeInsertItem: TBoldSubFollowerEvent read fOnBeforeInsertItem write fOnBeforeInsertItem;
    property OnAfterInsertItem: TBoldFollowerEvent read fOnAfterInsertItem write fOnAfterInsertItem;
    property OnBeforeDeleteItem: TBoldFollowerEvent read fOnBeforeDeleteItem write fOnBeforeDeleteItem;
    property OnAfterDeleteItem: TBoldSubFollowerEvent read fOnAfterDeleteItem write fOnAfterDeleteItem;
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
//    procedure UnselectPrev;
    procedure Insert(ListFollowerController: TBoldAsFollowerListController; Index: Integer; Follower: TBoldFollower);
    procedure Delete(ListFollowerController: TBoldAsFollowerListController; index: Integer);
  protected
    procedure EnsureSubfollowersDisplayable; override;
    function GetSubFollowerCount: Integer; override;
    function GetSubFollower(Index: Integer): TBoldFollower; override;
    function GetCurrentSubFollowerIndex: Integer; override;
    procedure SetCurrentSubFollowerIndex(index: integer); override;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    destructor Destroy; override;
    procedure SetActiveRange(FirstActive, LastActive: Integer);
    procedure SelectAll(SetSelect: Boolean);
    procedure SelectRange(index: Integer);
    procedure AddSelectedToList(BoldList: TBoldList);
    procedure PurgeEnd(ListFollowerController: TBoldAsFollowerListController; PurgeCount: Integer);
    procedure EnsureFollower(ListFollowerController: TBoldAsFollowerListController;
                             Index: Integer;
                             Element: TBoldElement;
                             Controller: TBoldFollowerController); { may disturb list after index}
    property Followers[index: Integer]: TBoldFollower read GetSubFollower; default;
    property Count: Integer read GetSubFollowerCount;
    property FirstActive: Integer read FFirstActive;
    property LastActive: Integer read FLastActive;
    property SelectedCount: Integer read GetSelectedCount;
    property Selected[index: Integer]: Boolean read GetSelected write SetSelected;
    property CurrentItem: Integer read FCurrentItem write FCurrentItem;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

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

procedure TBoldAsFollowerListController.DoAfterInsertItem(Follower: TBoldFollower);
begin
  if Assigned(FOnAfterInsertItem) then
    FOnAfterInsertItem(Follower);
end;

procedure TBoldAsFollowerListController.DoBeforeDeleteItem(Follower: TBoldFollower);
begin
  if Assigned(FOnBeforeDeleteItem) then
    FOnBeforeDeleteItem(Follower);
end;

procedure TBoldAsFollowerListController.DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
begin
  if Assigned(FOnAfterDeleteItem) then
    FOnAfterDeleteItem(index, OwningFollower);
end;

procedure TBoldAsFollowerListController.SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: Integer = 1);
begin
  if Assigned(EffectiveRenderer) then  // may be nil after finalization
    (EffectiveRenderer as TBoldAsFollowerListRenderer).SetActiveRange(Follower, FirstActive, LastActive, RangeBuffer);
end;

procedure TBoldAsFollowerListController.SelectAll(Follower: TBoldFollower; SetSelect: Boolean);
begin
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
  (Follower.RendererData as TBoldFollowerList).Selected[index] := not (Follower.RendererData as TBoldFollowerList).Selected[index];
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
  FLastActive := High(FLastActive);
  FCurrentItem := -1;
end;

destructor TBoldFollowerList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Followers[I].Free;
  FreeAndNil(FArray);
  inherited;
end;

procedure TBoldFollowerList.SetActiveRange(FirstActive, LastActive: Integer);
var
  I: Integer;
begin
  FFirstActive := FirstActive;
  FLastActive := LastActive;
  for I := 0 to Count - 1 do
  begin
    Followers[I].Active := (I >= FirstActive) and (I <= LastActive);
  end;
end;


function TBoldFollowerList.GetSelectedCount: Integer;
var
  Row: Integer;
begin
  Result := 0;
  for Row := 0 to Count - 1 do
    if Selected[Row] then
      Inc(Result);
end;

function TBoldFollowerList.GetSubFollowerCount: Integer;
begin
  Result := FArray.Count;
end;

function TBoldFollowerList.GetSubFollower(Index: Integer): TBoldFollower;
begin
  if (index >= 0) and (index < fArray.Count) then
    Result := TBoldFollower(FArray[index])
  else
    result := nil;
end;

function TBoldFollowerList.GetCurrentSubFollowerIndex: Integer;
begin
  Result := FCurrentItem;
end;

procedure TBoldFollowerList.SetSelected(index: Integer; V: Boolean);
begin
  if (index > -1) and
    (index < Count) and
    (Followers[index].Selected <> V) then
  begin
    if V then
      FPrevSelected := index;
    Followers[index].Selected := V;
  end;
end;

function TBoldFollowerList.GetSelected(index: Integer): Boolean;
begin
  Result := Followers[index].Selected;
end;

procedure TBoldFollowerList.SelectAll(SetSelect: Boolean);
var
  Row: Integer;
begin
  for Row := 0 to Count - 1 do
    Followers[Row].Selected := SetSelect;
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
begin
  for Row := 0 to Count - 1 do
    if (Followers[Row].Selected) and
       Assigned(Followers[Row].Element) and
       BoldTestType(Followers[Row].Element, TBoldObject) then
      BoldList.Add(Followers[Row].Element);
end;

procedure TBoldFollowerList.Delete(ListFollowerController: TBoldAsFollowerListController; Index: Integer);
var
  I: Integer;
begin
  ListFollowerController.DoBeforeDeleteItem(Followers[index]);
  Followers[index].Free;
  FArray.Delete(index);
  for I := index to Count - 1 do
    Followers[I].Index := I;
  ListFollowerController.DoAfterDeleteItem(Index, OwningFollower);
end;

procedure TBoldFollowerList.Insert(ListFollowerController: TBoldAsFollowerListController; Index: Integer; Follower: TBoldFollower);
var
  I: Integer;
begin
  ListFollowerController.DoBeforeInsertItem(Index, OwningFollower);
  FArray.Insert(index, Follower);
  for I := index to Count - 1 do
    Followers[I].Index := I;
  ListFollowerController.DoAfterInsertItem(Followers[Index]);
end;

procedure TBoldFollowerList.PurgeEnd(ListFollowerController: TBoldAsFollowerListController; PurgeCount: Integer);
var
  I: Integer;
begin
  TBoldPublisher.StartNotify;
  try
    for I := Count - 1 downto PurgeCount do
      Delete(ListFollowerController, I);
  finally
    TBoldPublisher.EndNotify;
  end;
end;

procedure TBoldFollowerList.EnsureFollower(ListFollowerController: TBoldAsFollowerListController;
                                           Index: Integer;
                                           Element: TBoldElement;
                                           Controller: TBoldFollowerController);
begin
  if (element = nil) and (index < Count) and (Followers[index].Controller = Controller) then
    Followers[index].Element := Element;
  if (index < Count) and (Followers[index].Element = Element) and (Followers[index].Controller = Controller) then
    {all ok}
  else if (index + 1 < Count) and (Followers[index + 1].Element = Element) and (Followers[index + 1].Controller = Controller) then
    Delete(ListFollowerController, Index)
  else
    Insert(ListFollowerController, index, TBoldFollower.CreateSubFollower(OwningFollower, Controller, Element));
  Followers[index].Active := (index >= FirstActive) and (index <= LastActive);
end;

procedure TBoldFollowerList.SetCurrentSubFollowerIndex(index: integer);
begin
  inherited;
  FCurrentItem := index;
end;

procedure TBoldFollowerList.EnsureSubfollowersDisplayable;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    with Followers[I] do
      if Active then
        EnsureDisplayable;
end;

procedure TBoldAsFollowerListController.CleanRendererData(RendererData: TBoldRendererData);
begin
  inherited;
  (RendererData as TBoldFollowerList).PurgeEnd(self, 0);
end;

end.

