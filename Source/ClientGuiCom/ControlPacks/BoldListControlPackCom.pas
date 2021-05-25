
{ Global compiler directives }
{$include bold.inc}
unit BoldListControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  BoldComObjectSpace_TLB,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldControlPackCom;

type
  {Forward declarations of all classes}
  TBoldAsFollowerListRendererCom = class;
  TBoldAsFollowerListControllerCom = class;
  TBoldFollowerListCom = class;

  {---TBoldAsFollowerListControllerCom---}
  TBoldAsFollowerListControllerCom = class(TBoldFollowerControllerCom)
  private
    fOnBeforeInsertItem: TBoldSubFollowerEventCom;
    fOnAfterInsertItem: TBoldFollowerEventCom;
    fOnBeforeDeleteItem: TBoldFollowerEventCom;
    fOnAfterDeleteItem: TBoldSubFollowerEventCom;
  protected
    procedure DoBeforeInsertItem(index: Integer; OwningFollower: TBoldFollowerCom);
    procedure DoAfterInsertItem(Follower: TBoldFollowerCom);
    procedure DoBeforeDeleteItem(Follower: TBoldFollowerCom);
    procedure DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
    procedure CleanRendererData(RendererData: TBoldFollowerDataCom); override;
  public
    constructor Create(aOwningComponent: TComponent);
    procedure SetActiveRange(Follower: TBoldFollowerCom; FirstActive: Integer; LastActive: Integer; RangeBuffer: Integer = 1);
    procedure SelectAll(Follower: TBoldFollowerCom; SetSelect: Boolean); {Maybe make available in renderer?}
    procedure SelectRange(Follower: TBoldFollowerCom; index: Integer);
    procedure SetSelected(Follower: TBoldFollowerCom; index: Integer; Value: Boolean);
    procedure ToggleSelected(Follower: TBoldFollowerCom; index: Integer);
    function GetSelected(Follower: TBoldFollowerCom; index: Integer): Boolean;
    property OnBeforeInsertItem: TBoldSubFollowerEventCom read fOnBeforeInsertItem write fOnBeforeInsertItem;
    property OnAfterInsertItem: TBoldFollowerEventCom read fOnAfterInsertItem write fOnAfterInsertItem;
    property OnBeforeDeleteItem: TBoldFollowerEventCom read fOnBeforeDeleteItem write fOnBeforeDeleteItem;
    property OnAfterDeleteItem: TBoldSubFollowerEventCom read fOnAfterDeleteItem write fOnAfterDeleteItem;
  end;

  {--- TBoldAsFollowerListRendererCom ---}
  TBoldAsFollowerListRendererCom = class(TBoldRendererCom)
  protected
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    procedure SetActiveRange(Follower: TBoldFollowerCom; FirstActive, LastActive: Integer; RangeBuffer: integer = 1); virtual;
  end;

  {---TBoldFollowerListCom---}
  TBoldFollowerListCom = class(TBoldFollowerDataCom)
  private
    FArray: TList;
    FFirstActive: Integer;
    FLastActive: Integer;
    FCurrentItem: Integer;
    FPrevSelected: Integer;
    function GetSelectedCount: Integer;
    procedure SetSelected(index: Integer; V: Boolean);
    function GetSelected(index: Integer): Boolean;
    procedure Insert(ListFollowerController: TBoldAsFollowerListControllerCom; Index: Integer; Follower: TBoldFollowerCom);
    procedure Delete(ListFollowerController: TBoldAsFollowerListControllerCom; index: Integer);
  protected
    procedure EnsureSubfollowersDisplayable; override;
    function GetSubFollowerCount: Integer; override;
    function GetSubFollower(Index: Integer): TBoldFollowerCom; override;
    function GetCurrentSubFollowerIndex: Integer; override;
    procedure SetCurrentSubFollowerIndex(index: integer); override;
  public
    constructor Create(OwningFollower: TBoldFollowerCom); override;
    destructor Destroy; override;
    procedure SetActiveRange(FirstActive, LastActive: Integer);
    procedure SelectAll(SetSelect: Boolean);
    procedure SelectRange(index: Integer);
    procedure AddSelectedToList(BoldList: IBoldList);
    procedure PurgeEnd(ListFollowerController: TBoldAsFollowerListControllerCom; PurgeCount: Integer);
    procedure EnsureFollower(ListFollowerController: TBoldAsFollowerListControllerCom;
                             Index: Integer;
                             Element: IBoldElement;
                             Controller: TBoldFollowerControllerCom); { may disturb list after index}
    property Followers[index: Integer]: TBoldFollowerCom read GetSubFollower; default;
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
  BoldSubscription,
  BoldUtils;

{---TBoldAsFollowerListControllerCom---}

constructor TBoldAsFollowerListControllerCom.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent);
end;

procedure TBoldAsFollowerListControllerCom.DoBeforeInsertItem(index: Integer; OwningFollower: TBoldFollowerCom);
begin
  if Assigned(FOnBeforeInsertItem) then
    FOnBeforeInsertItem(index, OwningFollower);
end;

procedure TBoldAsFollowerListControllerCom.DoAfterInsertItem(Follower: TBoldFollowerCom);
begin
  if Assigned(FOnAfterInsertItem) then
    FOnAfterInsertItem(Follower);
end;

procedure TBoldAsFollowerListControllerCom.DoBeforeDeleteItem(Follower: TBoldFollowerCom);
begin
  if Assigned(FOnBeforeDeleteItem) then
    FOnBeforeDeleteItem(Follower);
end;

procedure TBoldAsFollowerListControllerCom.DoAfterDeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
begin
  if Assigned(FOnAfterDeleteItem) then
    FOnAfterDeleteItem(index, OwningFollower);
end;

procedure TBoldAsFollowerListControllerCom.SetActiveRange(Follower: TBoldFollowerCom; FirstActive, LastActive: Integer; RangeBuffer: Integer = 1);
begin
  if Assigned(EffectiveRenderer) then
    (EffectiveRenderer as TBoldAsFollowerListRendererCom).SetActiveRange(Follower, FirstActive, LastActive, RangeBuffer);
end;

procedure TBoldAsFollowerListControllerCom.SelectAll(Follower: TBoldFollowerCom; SetSelect: Boolean);
begin
  (Follower.RendererData as TBoldFollowerListCom).SelectAll(SetSelect)
end;

procedure TBoldAsFollowerListControllerCom.SelectRange(Follower: TBoldFollowerCom; index: Integer);
begin
  (Follower.RendererData as TBoldFollowerListCom).SelectRange(index)
end;

procedure TBoldAsFollowerListControllerCom.SetSelected(Follower: TBoldFollowerCom; index: Integer; Value: Boolean);
begin
  (Follower.RendererData as TBoldFollowerListCom).Selected[index] := Value;
end;

procedure TBoldAsFollowerListControllerCom.ToggleSelected(Follower: TBoldFollowerCom; index: Integer);
begin
  (Follower.RendererData as TBoldFollowerListCom).Selected[index] := not (Follower.RendererData as TBoldFollowerListCom).Selected[index];
end;

function TBoldAsFollowerListControllerCom.GetSelected(Follower: TBoldFollowerCom; index: Integer): Boolean;
begin
  Result := (Follower.RendererData as TBoldFollowerListCom).Selected[index]
end;

{---TBoldAsFollowerListRendererCom---}
function TBoldAsFollowerListRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldFollowerListCom;
end;

procedure TBoldAsFollowerListRendererCom.SetActiveRange(Follower: TBoldFollowerCom; FirstActive, LastActive: Integer; RangeBuffer: integer = 1);
begin
  (Follower.RendererData as TBoldFollowerListCom).SetActiveRange(FirstActive, LastActive);
end;

{---TBoldFollowerListCom---}
constructor TBoldFollowerListCom.Create(OwningFollower: TBoldFollowerCom);
begin
  inherited Create(OwningFollower);
  FArray := TList.Create;
  FLastActive := High(FLastActive);
  FCurrentItem := -1;
end;

destructor TBoldFollowerListCom.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Followers[I].Free;
  FreeAndNil(FArray);
  inherited;
end;

procedure TBoldFollowerListCom.SetActiveRange(FirstActive, LastActive: Integer);
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


function TBoldFollowerListCom.GetSelectedCount: Integer;
var
  Row: Integer;
begin
  Result := 0;
  for Row := 0 to Count - 1 do
    if Selected[Row] then
      Inc(Result);
end;

function TBoldFollowerListCom.GetSubFollowerCount: Integer;
begin
  Result := FArray.Count;
end;

function TBoldFollowerListCom.GetSubFollower(Index: Integer): TBoldFollowerCom;
begin
  if (index >= 0) and (index < fArray.Count) then
    Result := TBoldFollowerCom(FArray[index])
  else
    result := nil;
end;

function TBoldFollowerListCom.GetCurrentSubFollowerIndex: Integer;
begin
  Result := FCurrentItem;
end;

procedure TBoldFollowerListCom.SetSelected(index: Integer; V: Boolean);
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

function TBoldFollowerListCom.GetSelected(index: Integer): Boolean;
begin
  Result := Followers[index].Selected;
end;

procedure TBoldFollowerListCom.SelectAll(SetSelect: Boolean);
var
  Row: Integer;
begin
  for Row := 0 to Count - 1 do
    Followers[Row].Selected := SetSelect;
end;

procedure TBoldFollowerListCom.SelectRange(index: Integer);
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
procedure TBoldFollowerListCom.UnselectPrev;
begin
  if FPrevSelected <> -1 then
    Selected[FPrevSelected] := False;
end;
}

procedure TBoldFollowerListCom.AddSelectedToList(BoldList: IBoldList);
var
  Row: Integer;
begin
  for Row := 0 to Count - 1 do
    if (Followers[Row].Selected) and
       Assigned(Followers[Row].Element) and
       BoldTestType(Followers[Row].Element, IBoldObject) then
      BoldList.Add(Followers[Row].Element);
end;

procedure TBoldFollowerListCom.Delete(ListFollowerController: TBoldAsFollowerListControllerCom; Index: Integer);
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

procedure TBoldFollowerListCom.Insert(ListFollowerController: TBoldAsFollowerListControllerCom; Index: Integer; Follower: TBoldFollowerCom);
var
  I: Integer;
begin
  ListFollowerController.DoBeforeInsertItem(Index, OwningFollower);
  FArray.Insert(index, Follower);
  for I := index to Count - 1 do
    Followers[I].Index := I;
  ListFollowerController.DoAfterInsertItem(Followers[Index]);
end;

procedure TBoldFollowerListCom.PurgeEnd(ListFollowerController: TBoldAsFollowerListControllerCom; PurgeCount: Integer);
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

procedure TBoldFollowerListCom.EnsureFollower(ListFollowerController: TBoldAsFollowerListControllerCom;
                                           Index: Integer;
                                           Element: IBoldElement;
                                           Controller: TBoldFollowerControllerCom);
begin
  if (element = nil) and (index < Count) and (Followers[index].Controller = Controller) then
    Followers[index].Element := Element;
  if (index < Count) and (Followers[index].Element = Element) and (Followers[index].Controller = Controller) then
    {all ok}
  else if (index + 1 < Count) and (Followers[index + 1].Element = Element) and (Followers[index + 1].Controller = Controller) then
    Delete(ListFollowerController, Index)
  else
    Insert(ListFollowerController, index, TBoldFollowerCom.CreateSubFollower(OwningFollower, Controller, Element));
  Followers[index].Active := (index >= FirstActive) and (index <= LastActive);
end;

procedure TBoldFollowerListCom.SetCurrentSubFollowerIndex(index: integer);
begin
  inherited;
  FCurrentItem := index;
end;

procedure TBoldFollowerListCom.EnsureSubfollowersDisplayable;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    with Followers[I] do
      if Active then
        EnsureDisplayable;
end;

procedure TBoldAsFollowerListControllerCom.CleanRendererData(RendererData: TBoldFollowerDataCom);
begin
  inherited;
  (RendererData as TBoldFollowerListCom).PurgeEnd(self, 0);
end;

end.
