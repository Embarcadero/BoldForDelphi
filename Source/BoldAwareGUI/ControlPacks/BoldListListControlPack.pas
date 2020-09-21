unit BoldListListControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldDefs,
  BoldElements,
  BoldControlPackDefs,
  BoldControlPack,
  BoldListControlPack;

type
  { forward declarations }
  TBoldAbstractListAsFollowerListController = class;
  TBoldListAsFollowerListRenderer = class;
  TBoldListAsFollowerListController = class;

  { TBoldListAsFollowerListController }
  TBoldAbstractListAsFollowerListController = class(TBoldAsFollowerListController)
  private
    fInternalDrag: Boolean;
    fDefaultDblClick: Boolean;
    fFollowerController: TBoldFollowerController;
    fNilElementMode: TBoldNilElementMode;
    function GetRenderer: TBoldListAsFollowerListRenderer;
    procedure SetRenderer(Value: TBoldListAsFollowerListRenderer);
    procedure SetNilElementMode(const Value: TBoldNilElementMode);
   protected
    function GetEffectiveRenderer: TBoldRenderer; override;
    function GetEffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRenderer;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    procedure DoAssign(Source: TPersistent); override;
    property EffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRenderer read GetEffectiveListAsFollowerListRenderer;
    property DefaultDblClick: Boolean read fDefaultDblClick write fDefaultDblClick default True;
    property DragMode default bdgSelection;
    property DropMode default bdpAppend;
    property InternalDrag: Boolean read fInternalDrag write fInternalDrag default True;
    property Renderer: TBoldListAsFollowerListRenderer read GetRenderer write SetRenderer;
    property NilElementMode: TBoldNilElementMode read fNilElementMode write SetNilElementMode;
  public
    constructor Create(aOwningComponent: TComponent; FollowerController: TBoldFollowerController);
//    procedure DragDrop(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer); override;
//    function DragOver(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer): Boolean; override;
    function GetListIndex(Follower: TBoldFollower): Integer;
    function ListIndexToIndex(Follower: TBoldFollower; ListIndex: Integer): integer;
    function ListIndex(index: integer): integer;
  end;

  { TBoldListAsFollowerListController }
  TBoldListAsFollowerListController = class(TBoldAbstractListAsFollowerListController)
  published
    property DefaultDblClick;
    property DragMode;
    property DropMode;
    property InternalDrag;
    property Renderer;
    property NilElementMode;
  end;

  { TBoldListAsFollowerListRenderer }
  TBoldListAsFollowerListRenderer = class(TBoldAsFollowerListRenderer)
  public
    procedure SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: integer = 1); override;
    class function DefaultRenderer: TBoldListAsFollowerListRenderer;
    procedure MakeUptodate(Follower: TBoldFollower; FollowerController: TBoldFollowerController);
    procedure DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData); override;
    function DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean; override;
    procedure DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer); override;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStrings,
  BoldUtils,
  {$IFNDEF BOLDCOMCLIENT} // uses
  BoldSystem,
  BoldGUI,
  {$ENDIF}
  BoldMath;

var
  DefaultListAsFollowerListRenderer: TBoldListAsFollowerListRenderer;

{---TBoldAbstractListAsFollowerListController---}
constructor TBoldAbstractListAsFollowerListController.Create(aOwningComponent: TComponent; FollowerController: TBoldFollowerController);
begin
  inherited Create(aOwningComponent);
  fFollowerController := FollowerController;
  DragMode := bdgSelection;
  DropMode := bdpAppend;
  fDefaultDblClick := True;
  fNilElementMode := neNone;
  fInternalDrag := True;
end;

procedure TBoldAbstractListAsFollowerListController.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldAbstractListAsFollowerListController);
  inherited DoAssign(Source);
  DefaultDblClick := TBoldAbstractListAsFollowerListController(Source).DefaultDblClick;
  InternalDrag := TBoldAbstractListAsFollowerListController(Source).InternalDrag;
  NilElementMode := TBoldAbstractListAsFollowerListController(Source).NilElementMode;
end;

{function TBoldAbstractListAsFollowerListController.DragOver(Follower: TBoldFollower; dropindex: Integer): Boolean;
var
  List: TBoldList;
begin
  if Assigned(Follower.Element) then
    List := TBoldList(Follower.Element)
  else
    List := nil;
  Result := EffectiveRenderer.DragOver(List, DropMode, InternalDrag, Follower.RendererData, dropindex);
end;
}

{procedure TBoldAbstractListAsFollowerListController.DragDrop(Follower: TBoldFollower; ReceivingElement: TBoldElement; dropindex: Integer);
var
  List: TBoldList;
begin
  if Assigned(Follower.Element) then
    List := TBoldList(Follower.Element)
  else
    List := nil;
  EffectiveRenderer.DragDrop(List, DropMode, dropindex);
end;
}

procedure TBoldAbstractListAsFollowerListController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  (EffectiveRenderer as TBoldListAsFollowerListRenderer).MakeUptodate(Follower, fFollowerController);
  {$IFDEF BOLDCOMCLIENT} // MakeUptodateAndSubscribe
  if Subscribe and Assigned(Follower.Element) then
    Follower.Element.SubscribeToExpression('', Follower.Subscriber.ClientId, Follower.Subscriber.SubscriberId, false, false);
  {$ELSE}
  if Subscribe and Assigned(Follower.Element) then
    Follower.Element.SubscribeToExpression('', Follower.Subscriber, False);
  {$ENDIF}
end;

function TBoldAbstractListAsFollowerListController.GetRenderer: TBoldListAsFollowerListRenderer;
begin
  Result := UntypedRenderer as TBoldListAsFollowerListRenderer;
end;

procedure TBoldAbstractListAsFollowerListController.SetRenderer(Value: TBoldListAsFollowerListRenderer);
begin
  UntypedRenderer := Value;
end;

procedure TBoldAbstractListAsFollowerListController.SetNilElementMode(const Value: TBoldNilElementMode);
begin
  if (fNilElementMode <> Value) then
  begin
    fNilElementMode := Value;
    Changed;
  end;
end;

function TBoldAbstractListAsFollowerListController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveListAsFollowerListRenderer;
end;

function TBoldAbstractListAsFollowerListController.GetEffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldListAsFollowerListRenderer.DefaultRenderer;
end;

function TBoldAbstractListAsFollowerListController.GetListIndex(Follower: TBoldFollower): Integer;
begin
  Result := Follower.CurrentIndex;
  if (NilElementMode = neInsertFirst) and (Result >= 0) then
    Dec(Result);
end;

{---TBoldListAsFollowerListRenderer---}

procedure TBoldListAsFollowerListRenderer.SetActiveRange(Follower: TBoldFollower; FirstActive, LastActive: Integer; RangeBuffer: integer = 1);
var
  i: integer;
  list: TBoldList;
  FirstToEnsure, LastToEnsure: integer;
  FirstFollowerActive, LastFollowerActive: integer;
  FollowerController: TBoldAbstractListAsFollowerListController;
begin
  if not assigned(follower.element) then
    exit;

  Assert(BoldTestType(Follower.Element, TBoldList));
  list :=  Follower.Element as TBoldList;
  FollowerController := (Follower.Controller as TBoldAbstractListAsFollowerListController);
  FirstToEnsure := FollowerController.ListIndex(FirstActive);
  LastToEnsure := FollowerController.ListIndex(LastActive);

  if RangeBuffer > 1 then
  begin
    FirstToEnsure := (FirstToEnsure DIV RangeBuffer) * rangeBuffer;
    LastToEnsure := ((LastToEnsure DIV RangeBuffer) + 1) * rangeBuffer;
  end;

  if FirstToEnsure < 0 then
    FirstToEnsure := 0;

  if FirstToEnsure >= List.Count then
    FirstToEnsure := List.Count - 1;

  if LastToEnsure >=  list.Count then
    LastToEnsure := List.Count - 1;

  list.EnsureRange(FirstToEnsure, LastToEnsure);

  FirstFollowerActive := FirstActive;
  LastFollowerActive := LastActive;

  if FirstFollowerActive >= Follower.SubFollowerCount then
    FirstFollowerActive := Follower.SubFollowerCount-1;

  if FirstFollowerActive >= List.Count then
    FirstFollowerActive := List.Count - 1;

  if FirstFollowerActive < 0 then
    FirstFollowerActive := 0;

  if LastFollowerActive >= Follower.SubFollowerCount then
    LastFollowerActive := Follower.SubFollowerCount - 1;
  if LastFollowerActive >=  list.Count then
    LastFollowerActive := List.Count - 1;

  for i := FirstFollowerActive to LastFollowerActive do
    with Follower.SubFollowers[i] do
     if not ElementValid then
       Element := List.Elements[FollowerController.ListIndex(i)];
  inherited SetActiveRange(Follower, FirstFollowerActive, LastFollowerActive);
end;

class function TBoldListAsFollowerListRenderer.DefaultRenderer: TBoldListAsFollowerListRenderer;
begin
  Result := DefaultListAsFollowerListRenderer;
end;

procedure TBoldListAsFollowerListRenderer.MakeUptodate(Follower: TBoldFollower; FollowerController: TBoldFollowerController);
var
  Controller: TBoldAbstractListAsFollowerListController;
  SourceList: TBoldList;
  {$IFDEF BOLDCOMCLIENT}
  SourceVariant: Variant;
  unk: IUnknown;
  ElementInterface: IBoldElement;
  {$ENDIF}
  SourceIndex: Integer;
  DestList: TBoldFollowerList;
  DestIndex: Integer;

  procedure AddElement(aElement: TBoldElement);
  begin
    DestList.EnsureFollower(Controller, DestIndex, aElement, FollowerController);
    Inc(DestIndex);
  end;

begin
  DestList := Follower.RendererData as TBoldFollowerList;
  Controller := Follower.Controller as TBoldAbstractListAsFollowerListController;
  DestIndex := 0;

  if (Controller.NilElementMode=neInsertFirst) then
    AddElement(nil);
  if Assigned(Follower.Element) then
  begin
    SourceList := Follower.Element as TBoldList;
  {$IFNDEF BOLDCOMCLIENT}
    SourceList.EnsureRange(Controller.ListIndex(DestList.FirstActive), Controller.ListIndex(DestList.LastActive));
  {$ELSE}
    SourceVariant := SourceList.GetRange(Controller.ListIndex(DestList.FirstActive), Controller.ListIndex(DestList.LastActive));
  {$ENDIF}
    for SourceIndex := 0 to SourceList.Count - 1 do
      if (DestIndex >= DestList.FirstActive) and (Destindex <= DestList.LastActive) then
  {$IFNDEF BOLDCOMCLIENT}
        AddElement(SourceList[SourceIndex])
  {$ELSE}
      begin
        unk :=  SourceVariant[SourceIndex];
        ElementInterface := unk as IBoldElement;
        AddElement(ElementInterface);
      end
  {$ENDIF}
      else
      begin
        AddElement(nil);
        DestList[DestIndex-1].ElementValid := false;
      end;
  end;
  if (Controller.NilElementMode=neAddLast) then
    AddElement(nil);
  DestList.PurgeEnd(Controller, DestIndex);
end;

procedure TBoldListAsFollowerListRenderer.DefaultStartDrag(Element: TBoldElement; DragMode: TBoldDragMode; RendererData: TBoldRendererData);
{$IFNDEF BOLDCOMCLIENT} // dragdrop - DefaultStartDrag
var
  FollowerList: TBoldFollowerList;
{$ENDIF}
begin
  {$IFNDEF BOLDCOMCLIENT} // dragdrop - DefaultStartDrag
  if (DragMode = bdgSelection) then
  begin
    if BoldGUIHandler.DraggedObjects.Count <> 0 then
      raise EBold.CreateFmt(sDraggedObjectsNotCleared, [ClassName]);

    Followerlist := RendererData as TBoldFollowerList;
    Followerlist.AddSelectedToList(BoldGUIHandler.DraggedObjects);
  end;
  {$ENDIF}
end;

function TBoldListAsFollowerListRenderer.DefaultDragOver(Element: TBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldRendererData; dropindex: Integer): Boolean;
begin
  {$IFNDEF BOLDCOMCLIENT} // dragdrop - DefaultDragOver
  Result := (BoldGUIHandler.DraggedObjects.Count > 0) and
    BoldGUIHandler.DraggedObjectsAssignable(Element, DropMode); // FIXME move here
  {$ELSE}
  result := false;
  {$ENDIF}
end;

procedure TBoldListAsFollowerListRenderer.DefaultDragDrop(Element: TBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
{$IFNDEF BOLDCOMCLIENT} // dragdrop - DefaultDragDrop
var
  prevIndex,
  Offset,
  I: Integer;
  ObjectList: TBoldObjectList;
begin
  ObjectList := TBoldObjectList(Element);

(*  if (NilElementMode=neInsertFirst) then
    Offset := 1
  else*) //FIXME
    Offset := 0;
  case DropMode of
    bdpInsert:
    begin
      if dropindex < 0 then
        dropindex := 0;
      if dropindex > ObjectList.Count - 1 then
        dropindex := ObjectList.Count - 1;

      for I := 0 to BoldGUIHandler.DraggedObjects.Count - 1 do
      begin
        prevIndex := ObjectList.IndexOf(BoldGUIHandler.DraggedObjects[I]);
        if prevIndex = -1 then
        begin
          if (dropindex + Offset >= 0) and (dropindex + offset < ObjectList.Count) then
            ObjectList.Insert(dropindex + Offset, BoldGUIHandler.DraggedObjects[I])
          else
            ObjectList.Add(BoldGUIHandler.DraggedObjects[I]);
          Inc(dropindex);
        end
        else
        begin
          ObjectList.Move(prevIndex, dropindex);
          if prevIndex > dropindex then
            Inc(dropindex)
        end;
      end
    end;

    bdpAppend:
      for I := 0 to BoldGUIHandler.DraggedObjects.Count - 1 do
        // Dupe checking by the ObjectList
        ObjectList.Add(BoldGUIHandler.DraggedObjects[I]);

    bdpReplace:
      raise EBoldFeatureNotImplementedYet.CreateFmt(sReplaceNotImplemented, [ClassName]);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

function TBoldAbstractListAsFollowerListController.ListIndexToIndex(Follower: TBoldFollower; ListIndex: Integer): integer;
begin
  if (NilElementMode = neInsertFirst) then
    Result := Listindex - 1
  else
    Result := Listindex;
end;

function TBoldAbstractListAsFollowerListController.ListIndex(index: integer): integer;
begin
  if index = MaxInt then
    result := index // otherwise the result will overflow to -maxint
  else if NilElementMode = neInsertFirst then
    Result := index + 1
  else
    Result := index;
end;

initialization
  DefaultListAsFollowerListRenderer := TBoldListAsFollowerListRenderer.Create(nil);

finalization
  FreeAndNil(DefaultListAsFollowerListRenderer);

end.

