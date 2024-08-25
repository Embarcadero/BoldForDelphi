
{ Global compiler directives }
{$include bold.inc}
unit BoldDragDropTarget;

{$UNDEF BOLDCOMCLIENT}

interface
uses
  Classes,
  Controls,
  ExtCtrls,
  BoldEnvironmentVCL,
  BoldElements,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldControlpack,
  BoldElementHandleFollower,
  BoldReferenceHandle,
  BoldNodeControlPack,
  BoldDefs;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldDropTarget = class(TImage, IBoldOclComponent)
  private
    FIsDropTarget: Boolean;
    FIsDragSource: Boolean;
    FNodeSelectionExpression: String;
    fHandleFollower: TBoldElementHandleFollower;
    FRepresentations: TBoldTreeFollowerController;
    FImageList: TImageList;
    FEmptyImageIndex: integer;
    procedure SetBoldHandle(const Value: TBoldReferenceHandle);
    procedure SetIsDragSource(const Value: Boolean);
    procedure SetIsDropTarget(const Value: Boolean);
    procedure SetNodeSelectionExpression(const Value: String);
    procedure SetRepresentations(Value: TBoldTreeFollowerController);
    function CanAcceptDraggedObject: Boolean;
    function GetDraggedObject: TBoldObject;
    function GetBoldHandle: TBoldReferenceHandle;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure SetImageList(const Value: TImageList);
    function GetImageIndex: integer;
    function GetCurrentNodeDescription: TBoldNodeDescription;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetEmptyImageIndex(const Value: integer);
    function GetElement: TBoldElement;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
  protected
    property DraggedObject: TBoldObject read GetDraggedObject;
    property Element: TBoldElement read GetElement;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target:TObject; X, Y: Integer); override;
    procedure MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
  published
    property IsDropTarget: Boolean read FIsDropTarget write SetIsDropTarget;
    property IsDragSource: Boolean read FIsDragSource write SetIsDragSource;
    property NodeSelectionExpression: String read FNodeSelectionExpression write SetNodeSelectionExpression;
    property Representations: TBoldTreeFollowerController read FRepresentations write SetRepresentations;
    property BoldHandle: TBoldReferenceHandle read GetBoldHandle write SetBoldHandle;
    property ImageList: TImageList read FImageList write SetImageList;
    property EmptyImageIndex: integer read FEmptyImageIndex write SetEmptyImageIndex;
  end;

implementation
uses
  Graphics,
  SysUtils,
 
  {$IFNDEF BOLDCOMCLIENT}
  BoldGui,
  {$ENDIF}
  BoldAttributes;

{ TBoldDropTarget }

procedure TBoldDropTarget.AfterMakeUptoDate(Follower: TBoldFollower);
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

function TBoldDropTarget.CanAcceptDraggedObject: Boolean;
begin
  result :=
    assigned(BoldHandle) and
    assigned(BoldHandle.StaticBoldType) and
    assigned(DraggedObject) and
    DraggedObject.BoldType.ConformsTo(BoldHandle.StaticBoldType);
end;

constructor TBoldDropTarget.Create(owner: TComponent);
begin
  inherited;
  FRepresentations := TBoldTreeFollowerController.Create(self);
  FRepresentations.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.create(Owner, Representations);
  FRepresentations.AfterMakeUptoDate := AfterMakeUptoDate;
  FNodeSelectionExpression := 'oclType.asstring->union(oclType.allsupertypes.asString)->union(''<Default>'')';
  AfterMakeUptoDate(fHandleFollower.Follower);
end;

destructor TBoldDropTarget.Destroy;
begin
  FreeAndNil(FHandleFollower);
  FreeAndNil(FRepresentations);
  inherited;
end;

procedure TBoldDropTarget.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
{$IFNDEF BOLDCOMCLIENT}
  BoldGUIHandler.DraggedObjects.Clear;
{$ENDIF}
end;

procedure TBoldDropTarget.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
{$IFNDEF BOLDCOMCLIENT}
  if assigned(BoldHandle) and (BoldHandle.Value is TBoldObject) then
  begin
    BoldGUIHandler.DraggedObjects.Clear;
    BoldGUIHandler.DraggedObjects.Add(BoldHandle.Value as TBoldObject);
  end;
{$ENDIF}
end;

procedure TBoldDropTarget.DragDrop(Source: TObject; X, Y: Integer);
begin
  if CanAcceptDraggedObject then
    BoldHandle.Value := DraggedObject;
end;

procedure TBoldDropTarget.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := IsDropTarget and CanAcceptDraggedObject;
  if Accept and Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
  Accept := Accept and IsDropTarget and CanAcceptDraggedObject;
end;

function TBoldDropTarget.GetBoldHandle: TBoldReferenceHandle;
begin
  result := fHandleFollower.BoldHandle as TBoldReferenceHandle;
end;

function TBoldDropTarget.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldDropTarget.GetCurrentNodeDescription: TBoldNodeDescription;
var
  NodeName: String;
  list: TBoldList;
  i: integer;
  ie: TBoldIndirectElement;
begin
  result := nil;
  if assigned(Element) then
  begin
    ie := TBoldIndirectElement.create;
    try
      Element.EvaluateAndSubscribeToExpression(NodeSelectionExpression, fHandleFollower.Follower.Subscriber, ie);
      if ie.value is TBoldList then begin
        list := ie.value as TBoldList;
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

function TBoldDropTarget.GetDraggedObject: TBoldObject;
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

function TBoldDropTarget.GetElement: TBoldElement;
begin
  result := fHandleFollower.Follower.Element;
end;

function TBoldDropTarget.GetExpression: String;
begin
  result := NodeSelectionExpression;
end;

function TBoldDropTarget.GetImageIndex: integer;
var
  NodeDescription: TBoldNodeDescription;
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

function TBoldDropTarget.GetVariableList: TBoldExternalVariableList;
begin
  result := Representations.VariableList;
end;

procedure TBoldDropTarget.MouseDown(BUTTON: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if IsDragSource then
    BeginDrag(true);
end;

procedure TBoldDropTarget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = ImageList) and (Operation = opremove) then
    ImageList := nil;
end;

procedure TBoldDropTarget.SetBoldHandle(const Value: TBoldReferenceHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldDropTarget.SetEmptyImageIndex(const Value: integer);
begin
  FEmptyImageIndex := Value;
end;

procedure TBoldDropTarget.SetExpression(const Value: TBoldExpression);
begin
  NodeSelectionExpression := Value;
end;

procedure TBoldDropTarget.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  if assigned(FImageList) then
    FImageList.FreeNotification(self);
end;

procedure TBoldDropTarget.SetIsDragSource(const Value: Boolean);
begin
  FIsDragSource := Value;
end;

procedure TBoldDropTarget.SetIsDropTarget(const Value: Boolean);
begin
  FIsDropTarget := Value;
end;

procedure TBoldDropTarget.SetNodeSelectionExpression(const Value: String);
begin
  FNodeSelectionExpression := Value;
end;

procedure TBoldDropTarget.SetRepresentations(Value: TBoldTreeFollowerController);
begin
  FRepresentations.Assign(Value);
end;

end.
