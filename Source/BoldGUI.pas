
{ Global compiler directives }
{$include bold.inc}
unit BoldGUI;

interface

uses
  // VCL
  Classes,
  Controls,
  Forms,
  Menus,

  // Bold
  BoldBase,
  BoldControlPackDefs,
  BoldElements,
  BoldSystem;

type
  TBoldGUIHandler = class;

  {---TBoldGUIHandler---}
  TBoldGUIHandler = class(TBoldMemoryManagedObject)
  private
    FPopupElement: TBoldElement;
    FPopupControl: TControl;
    FDraggedObjects: TBoldObjectList;
    fActivateTargetFormOnDrop: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoPopUp(sender: TObject);
    function FindHostingForm(Component: TComponent): TForm;
    function TryToFocusHostingForm(Component: TComponent): Boolean;
    function DraggedObjectsAssignable(element: TBoldElement; dropMode: TBoldDropMode): Boolean;
    property DraggedObjects: TBoldObjectList read FDraggedObjects;
    property PopupElement: TBoldElement read FPopupElement write FPopupElement;
    property PopupControl: TControl read FPopupControl write FPopupControl;
    property ActivateTargetFormOnDrop: Boolean read fActivateTargetFormOnDrop write fActivateTargetFormOnDrop;
  end;

function BoldGUIHandler: TBoldGUIHandler;

var
  BoldPopupMenu: TPopupMenu;

implementation

uses
  SysUtils,
  BoldSystemRT;

var
  G_BoldGUIHandler: TBoldGUIHandler = nil;

function BoldGUIHandler: TBoldGUIHandler;
begin
  if not Assigned(G_BoldGUIHandler) then
    G_BoldGUIHandler := TBoldGUIHandler.Create;
  Result := G_BoldGUIHandler;
end;

{---TBoldGUIHandler---}
constructor TBoldGUIHandler.Create;
begin
  inherited;
  FDraggedObjects := TBoldObjectList.Create;
end;

destructor TBoldGUIHandler.Destroy;
begin
  FreeAndNil(FDraggedObjects);
  inherited;
end;

function TBoldGUIHandler.DraggedObjectsAssignable(element: TBoldElement; dropMode: TBoldDropMode): Boolean;
var
  DraggedIndex: integer;
  AcceptableBoldType: TBoldElementTypeInfo;
  member: TBoldmember;
begin
  Result := False;

  if (element is TBoldObjectReference) then
    AcceptableBoldType := element.BoldType
  else if (element is TBoldList) then
    AcceptableBoldType := (element.BoldType as TBoldListTypeInfo).ListElementTypeInfo
  else
    Exit;

  if not element.Mutable then
    Exit;

  if element is TBoldMember then
  begin
    member := element as TBoldmember;
    if not member.CanModify then
      Exit;
  end;

  for DraggedIndex := 0 to DraggedObjects.Count - 1 do
  begin
    if not draggedObjects[DraggedIndex].BoldType.ConformsTo(AcceptableBoldType) then
      Exit;

    if (element is TBoldList) and
       not (element as TBoldList).CanInsert(-1, draggedObjects[DraggedIndex], nil) then
      Exit;

    if (element is TBoldObjectReference) and
      not (element as TBoldObjectReference).CanSetLocator(DraggedObjects[DraggedIndex].BoldObjectLocator, nil) then
      Exit;
  end;

  Result := (element is TBoldObjectList) or
            ((element is TBoldObjectReference) and
             (DraggedObjects.Count <= 1));
end;

procedure TBoldGUIHandler.DoPopUp;
begin
end;

function TBoldGUIHandler.FindHostingForm(Component: TComponent): TForm;
var
  temp: TComponent;
  Control: TControl;
begin
  if Component is TForm then
    result := Component as TForm
  else
  begin
    result := nil;
    if Component is TControl then
    begin
      Control := Component as TControl;
      while assigned(Control) and not (Control is TForm) do
        Control := Control.Parent;
      if Control is TForm then
        result := Control as TForm;
    end;

    if not assigned(result) then
    begin
      temp := Component;
      while assigned(temp) and not (temp is TForm) do
        temp := temp.Owner;
      if temp is TForm then
        result := temp as TForm;
    end;
  end;
end;

function TBoldGUIHandler.TryToFocusHostingForm(Component: TComponent): Boolean;
var
  form: TForm;
begin
  result := false;
  Form := FindHostingForm(Component);
  if assigned(Form) and Form.Visible then
  begin
    Form.SetFocus;
    result := true;
  end;
end;

initialization
  BoldPopupMenu := TPopupMenu.Create(nil);
  BoldPopupMenu.OnPopup := BoldGUIHandler.DoPopUp;

finalization
  FreeAndNil(G_BoldGUIHandler);
  FreeAndNil(BoldPopupMenu);

end.
