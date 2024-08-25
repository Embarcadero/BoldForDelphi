
{ Global compiler directives }
{$include bold.inc}
unit BoldControllerListControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  BoldControlPackDefs,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldListControlPackCom;

type
  TBoldControllerListAsFollowerListRendererCom = class;
  TBoldControllerListCom = class;

  {---TBoldControllerListCom---}
  TBoldControllerListCom = class(TBoldAsFollowerListControllerCom)
  private
    FList: TList;
    function GetCount: Integer;
    function GetSubController(index: Integer): TBoldFollowerControllerCom;
  protected
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); override;
    function GetEffectiveRenderer: TBoldRendererCom; override;
    function GetEffectiveDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRendererCom;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    procedure Add(BoldFollowerController: TBoldFollowerControllerCom);
    procedure Delete(index: Integer);
    procedure Remove(BoldFollowerController: TBoldFollowerControllerCom);
    procedure Move(CurIndex, ToIndex: Integer);
    property Count: Integer read GetCount;
    property Items[index: Integer]: TBoldFollowerControllerCom read GetSubController; default;
  published
    property DragMode default bdgSelection;
    property DropMode default bdpInsert;
  end;

  {---TBoldControllerListAsFollowerListRendererCom---}
  TBoldControllerListAsFollowerListRendererCom = class(TBoldAsFollowerListRendererCom)
  public
    class function DefaultRenderer: TBoldControllerListAsFollowerListRendererCom;
    procedure MakeUptodate(Follower: TBoldFollowerCom; Element: IBoldElement);
  end;

implementation

uses
  SysUtils;

var
  DefaultDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRendererCom;

{---TBoldControllerListCom---}
constructor TBoldControllerListCom.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent);
  FList := TList.Create;
  DragMode := bdgSelection;
  DropMode := bdpInsert;
end;

destructor TBoldControllerListCom.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FreeAndNil(FList);
  inherited;
end;

procedure TBoldControllerListCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  (EffectiveRenderer as TBoldControllerListAsFollowerListRendererCom).MakeUptodate(Follower, Follower.Element);
  if Subscribe and Assigned(Follower.Element) then
  {$IFDEF BOLDCOMCLIENT}
    Follower.Element.SubscribeToExpression('', Follower.Subscriber.ClientId, Follower.Subscriber.SubscriberId, False, true);
  {$ELSE}
    Follower.Element.SubscribeToExpression('', Follower.Subscriber, False);
  {$ENDIF}
end;

function TBoldControllerListCom.GetSubController(index: Integer): TBoldFollowerControllerCom;
begin
  Result := TBoldSingleFollowerControllerCom(FList[index]);
end;

procedure TBoldControllerListCom.Add(BoldFollowerController: TBoldFollowerControllerCom);
begin
  FList.Add(BoldFollowerController);
  Changed;
end;

procedure TBoldControllerListCom.Remove(BoldFollowerController: TBoldFollowerControllerCom);
begin
  FList.Remove(BoldFollowerController);
  Changed;
end;

procedure TBoldControllerListCom.Delete(index: Integer);
begin
  Items[index].Free;
  FList.Delete(index);
  Changed;
end;

procedure TBoldControllerListCom.Move(CurIndex, ToIndex: Integer);
begin
  FList.Move(CurIndex, ToIndex);
  Changed;
end;

function TBoldControllerListCom.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBoldControllerListCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := GetEffectiveDisplayPropertyListRenderer;
end;

function TBoldControllerListCom.GetEffectiveDisplayPropertyListRenderer: TBoldControllerListAsFollowerListRendererCom;
begin
  Result := TBoldControllerListAsFollowerListRendererCom.DefaultRenderer;
end;

{---TBoldControllerListAsFollowerListRendererCom---}

class function TBoldControllerListAsFollowerListRendererCom.DefaultRenderer: TBoldControllerListAsFollowerListRendererCom;
begin
  Result := DefaultDisplayPropertyListRenderer;
end;

procedure TBoldControllerListAsFollowerListRendererCom.MakeUptodate(Follower: TBoldFollowerCom; Element: IBoldElement);
var
  SourceList: TBoldControllerListCom;
  SourceIndex: Integer;
  DestList: TBoldFollowerListCom;
begin
  DestList := Follower.RendererData as TBoldFollowerListCom;
  SourceList := Follower.Controller as TBoldControllerListCom;
  for sourceIndex := 0 to SourceList.Count-1 do
    DestList.EnsureFollower(SourceList, SourceIndex, Element, SourceList[SourceIndex]);
  DestList.PurgeEnd(SourceList, SourceList.Count);
end;

initialization
  DefaultDisplayPropertyListRenderer := TBoldControllerListAsFollowerListRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultDisplayPropertyListRenderer);

end.
