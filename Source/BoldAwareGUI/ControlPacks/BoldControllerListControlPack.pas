
{ Global compiler directives }
{$include bold.inc}
unit BoldControllerListControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldControlPackDefs,  
  BoldElements,
  BoldControlPack,
  BoldListControlPack;

type
  TBoldControllerListAsFollowerListRenderer = class;
  TBoldControllerList = class;

  {---TBoldControllerList---}
  TBoldControllerList = class(TBoldAsFollowerListController)
  private
    FList: TList;
    function GetCount: Integer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetSubController(index: Integer): TBoldFollowerController; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  protected
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    function GetEffectiveDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRenderer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    class function PrecreateFollowers: boolean; override;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    procedure Add(BoldFollowerController: TBoldFollowerController); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure Delete(index: Integer); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure Remove(BoldFollowerController: TBoldFollowerController); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure Move(CurIndex, ToIndex: Integer); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function IndexOf(BoldFollowerController: TBoldFollowerController): integer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    property Count: Integer read GetCount;
    property Items[index: Integer]: TBoldFollowerController read GetSubController; default;
  published
    property DragMode default bdgSelection;
    property DropMode default bdpInsert;
  end;

  {---TBoldControllerListAsFollowerListRenderer---}
  TBoldControllerListAsFollowerListRenderer = class(TBoldAsFollowerListRenderer)
  public
    class function DefaultRenderer: TBoldControllerListAsFollowerListRenderer;
    procedure MakeUptodate(Follower: TBoldFollower; Element: TBoldElement);
  end;

implementation

uses
  SysUtils,
  BoldRev;

var
  DefaultDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRenderer;

{---TBoldControllerList---}
constructor TBoldControllerList.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent);
  FList := TList.Create;
  DragMode := bdgSelection;
  DropMode := bdpInsert;
end;

function TBoldControllerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBoldControllerList.GetSubController(index: Integer): TBoldFollowerController;
begin
  Result := TBoldSingleFollowerController(FList[index]);
end;

destructor TBoldControllerList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FreeAndNil(FList);
  inherited;
end;

procedure TBoldControllerList.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  (EffectiveRenderer as TBoldControllerListAsFollowerListRenderer).MakeUptodate(Follower, Follower.Element);
  if Subscribe and Assigned(Follower.Element) then
  {$IFDEF BOLDCOMCLIENT}
    Follower.Element.SubscribeToExpression('', Follower.Subscriber.ClientId, Follower.Subscriber.SubscriberId, False, true);
  {$ELSE}
    Follower.Element.SubscribeToExpression('', Follower.Subscriber, False); //       Follower.Element.DefaultSubscribe(Follower.Subscriber);
  {$ENDIF}
end;

procedure TBoldControllerList.Add(BoldFollowerController: TBoldFollowerController);
begin
  FList.Add(BoldFollowerController);
  Changed;
end;

procedure TBoldControllerList.Remove(BoldFollowerController: TBoldFollowerController);
begin
  FList.Remove(BoldFollowerController);
  Changed;
end;

procedure TBoldControllerList.Delete(index: Integer);
begin
  Items[index].Free;
  FList.Delete(index);
  Changed;
end;

procedure TBoldControllerList.Move(CurIndex, ToIndex: Integer);
begin
  FList.Move(CurIndex, ToIndex);
  Changed;
end;

function TBoldControllerList.GetEffectiveDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRenderer;
begin
  Result := TBoldControllerListAsFollowerListRenderer.DefaultRenderer;
end;

function TBoldControllerList.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := GetEffectiveDisplayPropertyListRenderer;
end;

function TBoldControllerList.IndexOf(
  BoldFollowerController: TBoldFollowerController): integer;
begin
  result := fList.IndexOf(BoldFollowerController);
end;

class function TBoldControllerList.PrecreateFollowers: boolean;
begin
  result := false;
end;

{---TBoldControllerListAsFollowerListRenderer---}

class function TBoldControllerListAsFollowerListRenderer.DefaultRenderer: TBoldControllerListAsFollowerListRenderer;
begin
  Result := DefaultDisplayPropertyListRenderer;
end;

procedure TBoldControllerListAsFollowerListRenderer.MakeUptodate(Follower: TBoldFollower; Element: TBoldElement);
var
  SourceList: TBoldControllerList;
  SourceIndex: Integer;
  DestList: TBoldFollowerList;
  lPrecreateFollowers: boolean;
begin
  DestList := Follower.RendererData as TBoldFollowerList;
  SourceList := Follower.Controller as TBoldControllerList;
  lPrecreateFollowers := SourceList.PrecreateFollowers;
  DestList.SetCapacity(SourceList.Count);
  for sourceIndex := 0 to SourceList.Count-1 do
    if lPrecreateFollowers then
      DestList.EnsuredFollower(SourceList, SourceIndex, Element, SourceList[SourceIndex])
    else
      DestList.EnsuredFollower(SourceList, SourceIndex, nil, SourceList[SourceIndex]);
  DestList.PurgeEnd(SourceList, SourceList.Count);
end;

initialization
  DefaultDisplayPropertyListRenderer := TBoldControllerListAsFollowerListRenderer.Create(nil);

finalization
  FreeAndNil(DefaultDisplayPropertyListRenderer);

end.
