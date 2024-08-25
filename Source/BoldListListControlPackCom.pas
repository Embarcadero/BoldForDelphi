
{ Global compiler directives }
{$include bold.inc}
unit BoldListListControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,

  // Bold
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldControlPackDefs,
  BoldListControlPackCom;

type
  { forward declarations }
  TBoldAbstractListAsFollowerListControllerCom = class;
  TBoldListAsFollowerListRendererCom = class;
  TBoldListAsFollowerListControllerCom = class;

  { TBoldListAsFollowerListControllerCom }
  TBoldAbstractListAsFollowerListControllerCom = class(TBoldAsFollowerListControllerCom)
  private
    fInternalDrag: Boolean;
    fDefaultDblClick: Boolean;
    fFollowerController: TBoldFollowerControllerCom;
    fNilElementMode: TBoldNilElementMode;
    function GetRenderer: TBoldListAsFollowerListRendererCom;
    procedure SetRenderer(Value: TBoldListAsFollowerListRendererCom);
    procedure SetNilElementMode(const Value: TBoldNilElementMode);
   protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    function GetEffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRendererCom;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); override;
    procedure DoAssign(Source: TPersistent); override;
    property EffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRendererCom read GetEffectiveListAsFollowerListRenderer;
    property DefaultDblClick: Boolean read fDefaultDblClick write fDefaultDblClick default True;
    property DragMode default bdgSelection;
    property DropMode default bdpAppend;
    property InternalDrag: Boolean read fInternalDrag write fInternalDrag default True;
    property Renderer: TBoldListAsFollowerListRendererCom read GetRenderer write SetRenderer;
    property NilElementMode: TBoldNilElementMode read fNilElementMode write SetNilElementMode;
  public
    constructor Create(aOwningComponent: TComponent; FollowerController: TBoldFollowerControllerCom);

    function GetListIndex(Follower: TBoldFollowerCom): Integer;
    function ListIndexToIndex(Follower: TBoldFollowerCom; ListIndex: Integer): integer;
    function ListIndex(index: integer): integer;
  end;

  { TBoldListAsFollowerListControllerCom }
  TBoldListAsFollowerListControllerCom = class(TBoldAbstractListAsFollowerListControllerCom)
  published
    property DefaultDblClick;
    property DragMode;
    property DropMode;
    property InternalDrag;
    property Renderer;
    property NilElementMode;
  end;

  { TBoldListAsFollowerListRendererCom }
  TBoldListAsFollowerListRendererCom = class(TBoldAsFollowerListRendererCom)
  public
    procedure SetActiveRange(Follower: TBoldFollowerCom; FirstActive, LastActive: Integer; RangeBuffer: integer = 1); override;
    class function DefaultRenderer: TBoldListAsFollowerListRendererCom;
    procedure MakeUptodate(Follower: TBoldFollowerCom; FollowerController: TBoldFollowerControllerCom);
    procedure DefaultStartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom); override;
    function DefaultDragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean; override;
    procedure DefaultDragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer); override;
  end;

implementation

uses
  SysUtils
  {$IFNDEF BOLDCOMCLIENT}
  ,BoldComObjectSpace_TLB,
  BoldGUI
  {$ENDIF};

var
  DefaultListAsFollowerListRenderer: TBoldListAsFollowerListRendererCom;

{---TBoldAbstractListAsFollowerListControllerCom---}
constructor TBoldAbstractListAsFollowerListControllerCom.Create(aOwningComponent: TComponent; FollowerController: TBoldFollowerControllerCom);
begin
  inherited Create(aOwningComponent);
  fFollowerController := FollowerController;
  DragMode := bdgSelection;
  DropMode := bdpAppend;
  fDefaultDblClick := True;
  fNilElementMode := neNone;
  fInternalDrag := True;
end;

procedure TBoldAbstractListAsFollowerListControllerCom.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldAbstractListAsFollowerListControllerCom);
  inherited DoAssign(Source);
  DefaultDblClick := TBoldAbstractListAsFollowerListControllerCom(Source).DefaultDblClick;
  InternalDrag := TBoldAbstractListAsFollowerListControllerCom(Source).InternalDrag;
  NilElementMode := TBoldAbstractListAsFollowerListControllerCom(Source).NilElementMode;
end;

{function TBoldAbstractListAsFollowerListControllerCom.DragOver(Follower: TBoldFollowerCom; dropindex: Integer): Boolean;
var
  List: IBoldList;
begin
  if Assigned(Follower.Element) then
    List := IBoldList(Follower.Element)
  else
    List := nil;
  Result := EffectiveRenderer.DragOver(List, DropMode, InternalDrag, Follower.RendererData, dropindex);
end;
}

{procedure TBoldAbstractListAsFollowerListControllerCom.DragDrop(Follower: TBoldFollowerCom; ReceivingElement: IBoldElement; dropindex: Integer);
var
  List: IBoldList;
begin
  if Assigned(Follower.Element) then
    List := IBoldList(Follower.Element)
  else
    List := nil;
  EffectiveRenderer.DragDrop(List, DropMode, dropindex);
end;
}

procedure TBoldAbstractListAsFollowerListControllerCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
begin
  inherited DoMakeUptodateAndSubscribe(Follower, Subscribe);
  (EffectiveRenderer as TBoldListAsFollowerListRendererCom).MakeUptodate(Follower, fFollowerController);
  {$IFDEF BOLDCOMCLIENT}
  if Subscribe and Assigned(Follower.Element) then
    Follower.Element.SubscribeToExpression('', Follower.Subscriber.ClientId, Follower.Subscriber.SubscriberId, false, false);
  {$ELSE}
  if Subscribe and Assigned(Follower.Element) then
    Follower.Element.SubscribeToExpression('', Follower.Subscriber, False);
  {$ENDIF}
end;

function TBoldAbstractListAsFollowerListControllerCom.GetRenderer: TBoldListAsFollowerListRendererCom;
begin
  Result := UntypedRenderer as TBoldListAsFollowerListRendererCom;
end;

procedure TBoldAbstractListAsFollowerListControllerCom.SetRenderer(Value: TBoldListAsFollowerListRendererCom);
begin
  UntypedRenderer := Value;
end;

procedure TBoldAbstractListAsFollowerListControllerCom.SetNilElementMode(const Value: TBoldNilElementMode);
begin
  if (fNilElementMode <> Value) then
  begin
    fNilElementMode := Value;
    Changed;
  end;
end;

function TBoldAbstractListAsFollowerListControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveListAsFollowerListRenderer;
end;

function TBoldAbstractListAsFollowerListControllerCom.GetEffectiveListAsFollowerListRenderer: TBoldListAsFollowerListRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldListAsFollowerListRendererCom.DefaultRenderer;
end;

function TBoldAbstractListAsFollowerListControllerCom.GetListIndex(Follower: TBoldFollowerCom): Integer;
begin
  Result := Follower.CurrentIndex;
  if (NilElementMode = neInsertFirst) and (Result >= 0) then
    Dec(Result);
end;

{---TBoldListAsFollowerListRendererCom---}

procedure TBoldListAsFollowerListRendererCom.SetActiveRange(Follower: TBoldFollowerCom; FirstActive, LastActive: Integer; RangeBuffer: integer = 1);
var
  i: integer;
  list: IBoldList;
  FirstToEnsure, LastToEnsure: integer;
  FirstFollowerActive, LastFollowerActive: integer;
  FollowerController: TBoldAbstractListAsFollowerListControllerCom;
begin
  if not assigned(follower.element) then
    exit;

  Assert(BoldTestType(Follower.Element, IBoldList));
  list :=  Follower.Element as IBoldList;
  FollowerController := (Follower.Controller as TBoldAbstractListAsFollowerListControllerCom);
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

class function TBoldListAsFollowerListRendererCom.DefaultRenderer: TBoldListAsFollowerListRendererCom;
begin
  Result := DefaultListAsFollowerListRenderer;
end;

procedure TBoldListAsFollowerListRendererCom.MakeUptodate(Follower: TBoldFollowerCom; FollowerController: TBoldFollowerControllerCom);
var
  Controller: TBoldAbstractListAsFollowerListControllerCom;
  SourceList: IBoldList;
  {$IFDEF BOLDCOMCLIENT}
  SourceVariant: Variant;
  unk: IUnknown;
  ElementInterface: IBoldElement;
  {$ENDIF}
  SourceIndex: Integer;
  DestList: TBoldFollowerListCom;
  DestIndex: Integer;

  procedure AddElement(aElement: IBoldElement);
  begin
    DestList.EnsureFollower(Controller, DestIndex, aElement, FollowerController);
    Inc(DestIndex);
  end;

begin
  DestList := Follower.RendererData as TBoldFollowerListCom;
  Controller := Follower.Controller as TBoldAbstractListAsFollowerListControllerCom;
  DestIndex := 0;

  if (Controller.NilElementMode=neInsertFirst) then
    AddElement(nil);
  if Assigned(Follower.Element) then
  begin
    SourceList := Follower.Element as IBoldList;
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

procedure TBoldListAsFollowerListRendererCom.DefaultStartDrag(Element: IBoldElement; DragMode: TBoldDragMode; RendererData: TBoldFollowerDataCom);
{$IFNDEF BOLDCOMCLIENT}
var
  FollowerList: TBoldFollowerListCom;
{$ENDIF}
begin
  {$IFNDEF BOLDCOMCLIENT}
  if (DragMode = bdgSelection) then
  begin
    if BoldGUIHandler.DraggedObjects.Count <> 0 then
      raise EBold.Create('TBoldListAsFollowerListRendererCom.DefaultStartDrag: TBoldGUIHandler.DraggedObjects not cleared');

    Followerlist := RendererData as TBoldFollowerListCom;
    Followerlist.AddSelectedToList(BoldGUIHandler.DraggedObjects);
  end;
  {$ENDIF}
end;

function TBoldListAsFollowerListRendererCom.DefaultDragOver(Element: IBoldElement; DropMode: TBoldDropMode; InternalDrag: Boolean; RendererData: TBoldFollowerDataCom; dropindex: Integer): Boolean;
begin
  {$IFNDEF BOLDCOMCLIENT}
  Result := (BoldGUIHandler.DraggedObjects.Count > 0) and
    BoldGUIHandler.DraggedObjectsAssignable(Element, DropMode);
  {$ELSE}
  result := false;
  {$ENDIF}
end;

procedure TBoldListAsFollowerListRendererCom.DefaultDragDrop(Element: IBoldElement; DropMode: TBoldDropMode; dropindex: Integer);
{$IFNDEF BOLDCOMCLIENT}
var
  prevIndex,
  Offset,
  I: Integer;
  ObjectList: IBoldObjectList;
begin
  ObjectList := IBoldObjectList(Element);

(*  if (NilElementMode=neInsertFirst) then
    Offset := 1
  else*)
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
        ObjectList.Add(BoldGUIHandler.DraggedObjects[I]);

    bdpReplace:
      raise EBoldFeatureNotImplementedYet.CreateFmt('%s.DefaultDragDrop: Replace not implemented yet', [ClassName]);
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

function TBoldAbstractListAsFollowerListControllerCom.ListIndexToIndex(Follower: TBoldFollowerCom; ListIndex: Integer): integer;
begin
  if (NilElementMode = neInsertFirst) then
    Result := Listindex - 1
  else
    Result := Listindex;
end;

function TBoldAbstractListAsFollowerListControllerCom.ListIndex(index: integer): integer;
begin
  if index = MaxInt then
    result := index
  else if NilElementMode = neInsertFirst then
    Result := index + 1
  else
    Result := index;
end;

initialization
  DefaultListAsFollowerListRenderer := TBoldListAsFollowerListRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultListAsFollowerListRenderer);
  
end.
