
{ Global compiler directives }
{$include bold.inc}
unit BoldDragDropTargetCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface
uses
  Classes,
  Controls,
  ExtCtrls,
  BoldEnvironmentVCL,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldReferenceHandleCom,
  BoldNodeControlPackCom;

type
  TBoldDropTargetCom = class(TImage, IBoldOCLComponentCom)
  private
    FIsDropTarget: Boolean;
    FIsDragSource: Boolean;
    FNodeSelectionExpression: String;
    fHandleFollower: TBoldElementHandleFollowerCom;
    FRepresentations: TBoldTreeFollowerControllerCom;
    FImageList: TImageList;
    FEmptyImageIndex: integer;
    procedure SetBoldHandle(const Value: TBoldReferenceHandleCom);
    procedure SetIsDragSource(const Value: Boolean);
    procedure SetIsDropTarget(const Value: Boolean);
    procedure SetNodeSelectionExpression(const Value: String);
    procedure SetRepresentations(Value: TBoldTreeFollowerControllerCom);
    function CanAcceptDraggedObject: Boolean;
    function GetDraggedObject: IBoldObject;
    function GetBoldHandle: TBoldReferenceHandleCom;
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure SetImageList(const Value: TImageList);
    function GetImageIndex: integer;
    function GetCurrentNodeDescription: TBoldNodeDescriptionCom;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetEmptyImageIndex(const Value: integer);
    function GetElement: IBoldElement;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
  protected
    property DraggedObject: IBoldObject read GetDraggedObject;
    property Element: IBoldElement read GetElement;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor create(owner: TComponent); override;
    destructor destroy; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target:TObject; X, Y: Integer); override;
    procedure MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
  published
    property IsDropTarget: Boolean read FIsDropTarget write SetIsDropTarget;
    property IsDragSource: Boolean read FIsDragSource write SetIsDragSource;
    property NodeSelectionExpression: String read FNodeSelectionExpression write SetNodeSelectionExpression;
    property Representations: TBoldTreeFollowerControllerCom read FRepresentations write SetRepresentations;
    property BoldHandle: TBoldReferenceHandleCom read GetBoldHandle write SetBoldHandle;
    property ImageList: TImageList read FImageList write SetImageList;
    property EmptyImageIndex: integer read FEmptyImageIndex write SetEmptyImageIndex;
  end;

implementation
uses
  Graphics,
  {!! DO NOT REMOVE !! BoldAttributes ,}
  {$IFNDEF BOLDCOMCLIENT}
  BoldGui,
  {$ENDIF}
  SysUtils;

{ TBoldDropTargetCom }

procedure TBoldDropTargetCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  imageIndex: integer;
begin
  if not (csdesigning in ComponentState) then
  begin
    if assigned(ImageList) then
    begin
      Picture.Assign(nil);
      ImageIndex := GetImageIndex;
      if (ImageIndex >= 0) and (ImageIndex <Imagelist.Count) then
        ImageList.GetBitmap(ImageIndex, Picture.Bitmap);
    end;
  end;
end;

function TBoldDropTargetCom.CanAcceptDraggedObject: Boolean;
begin
  result :=
    assigned(BoldHandle) and
    assigned(BoldHandle.StaticBoldType) and
    assigned(DraggedObject) and
    DraggedObject.BoldType.ConformsTo(BoldHandle.StaticBoldType);
end;

constructor TBoldDropTargetCom.create(owner: TComponent);
begin
  inherited;
  FRepresentations := TBoldTreeFollowerControllerCom.Create(self);
  FRepresentations.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.create(Owner, Representations);
  FRepresentations.AfterMakeUptoDate := AfterMakeUptoDate;
  FNodeSelectionExpression := 'oclType.asstring->union(oclType.allsupertypes.asString)->union(''<Default>'')';
  AfterMakeUptoDate(fHandleFollower.Follower);
end;

destructor TBoldDropTargetCom.destroy;
begin
  FreeAndNil(FHandleFollower);
  FreeAndNil(FRepresentations);
  inherited;
end;

procedure TBoldDropTargetCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
{$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.DraggedObjects.Clear;
{$ENDIF}
end;

procedure TBoldDropTargetCom.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
{$IFNDEF BOLDCOMCLIENT}
  if assigned(BoldHandle) and (BoldHandle.Value is IBoldObject) then
  begin
    BoldGUIHandler.DraggedObjects.Clear;
    BoldGUIHandler.DraggedObjects.Add(BoldHandle.Value as IBoldObject);
  end;
{$ENDIF}
end;

procedure TBoldDropTargetCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if CanAcceptDraggedObject then
    BoldHandle.Value := DraggedObject;
end;

procedure TBoldDropTargetCom.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := IsDropTarget and CanAcceptDraggedObject;
  if Accept and Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
  Accept := Accept and IsDropTarget and CanAcceptDraggedObject;
end;

function TBoldDropTargetCom.GetBoldHandle: TBoldReferenceHandleCom;
begin
  result := fHandleFollower.BoldHandle as TBoldReferenceHandleCom;
end;

function TBoldDropTargetCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldDropTargetCom.GetCurrentNodeDescription: TBoldNodeDescriptionCom;
var
  NodeName: String;
  list: IBoldList;
  i: integer;
  ie: TBoldIndirectElement;
begin
  result := nil;
  if assigned(Element) then
  begin
    ie := TBoldIndirectElement.create;
    try
      Element.EvaluateAndSubscribeToExpression(NodeSelectionExpression, fHandleFollower.Follower.Subscriber, ie);
      if ie.value is IBoldList then begin
        list := ie.value as IBoldList;
        for i := 0 to list.count-1 do begin
          NodeName := list[i].AsString;
          result := FRepresentations.NodeDescriptions.FindByName(NodeName);
          if assigned(result) then
            exit;
        end
      end
      else if assigned(ie.value) then
      begin
        NodeName := ie.value.AsString;
        result := FRepresentations.NodeDescriptions.FindByName(NodeName);
      end;
    finally
      ie.free;
    end;

    if not assigned(result) then
      result := FRepresentations.NodeDescriptions.FindByName(FRepresentations.DefaultNodeDescriptionName);
  end;
end;

function TBoldDropTargetCom.GetDraggedObject: IBoldObject;
begin
{$IFNDEF BOLDCOMCLIENT}
  if BoldGUIHandler.DraggedObjects.Count = 1 then
    result := BoldGUIHandler.DraggedObjects[0]
  else
    result := nil;
{$ELSE}
  result := nil;
{$ENDIF}
end;

function TBoldDropTargetCom.GetElement: IBoldElement;
begin
  result := fHandleFollower.Follower.Element;
end;

function TBoldDropTargetCom.GetExpression: String;
begin
  result := NodeSelectionExpression;
end;

function TBoldDropTargetCom.GetImageIndex: integer;
var
  NodeDescription: TBoldNodeDescriptionCom;
  ie: TBoldIndirectElement;
begin
  result := EmptyImageIndex;
  if assigned(Element) then
  begin
    NodeDescription := GetCurrentNodeDescription;
    if assigned(NodeDescription) then
    begin
      ie:= TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(NodeDescription.IconController.Expression, fHandleFollower.Follower.Subscriber, ie);
        if (ie.Value is TBAInteger) then
          result := (ie.Value as TBAInteger).AsInteger;
      finally
        ie.free;
      end;
    end;
  end;
end;

function TBoldDropTargetCom.GetVariableList: IBoldExternalVariableList;
begin
  result := Representations.VariableList;
end;

procedure TBoldDropTargetCom.MouseDown(BUTTON: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if IsDragSource then
    BeginDrag(true);
end;

procedure TBoldDropTargetCom.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = ImageList) and (Operation = opremove) then
    ImageList := nil;
end;

procedure TBoldDropTargetCom.SetBoldHandle(const Value: TBoldReferenceHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldDropTargetCom.SetEmptyImageIndex(const Value: integer);
begin
  FEmptyImageIndex := Value;
end;

procedure TBoldDropTargetCom.SetExpression(Expression: String);
begin
  NodeSelectionExpression := Expression;
end;

procedure TBoldDropTargetCom.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  if assigned(FImageList) then
    FImageList.FreeNotification(self);
end;

procedure TBoldDropTargetCom.SetIsDragSource(const Value: Boolean);
begin
  FIsDragSource := Value;
end;

procedure TBoldDropTargetCom.SetIsDropTarget(const Value: Boolean);
begin
  FIsDropTarget := Value;
end;

procedure TBoldDropTargetCom.SetNodeSelectionExpression(const Value: String);
begin
  FNodeSelectionExpression := Value;
end;

procedure TBoldDropTargetCom.SetRepresentations(Value: TBoldTreeFollowerControllerCom);
begin
  FRepresentations.Assign(Value);
end;

end.
