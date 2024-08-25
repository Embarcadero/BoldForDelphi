{ Global compiler directives }
{$include bold.inc}
unit BoldDragObject;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  BoldHandle,
  BoldUMLModel;

type
  { Forward declarations }
  TControlClass = class of TControl;
  TBoldDTDragObject = class;
  TBoldHandleClass = class of TBoldHandle;
  TBoldControlProviderList = class;
  TBoldControlProvider = class;
  TBoldControlProviderForClass = class;
  TBoldControlProviderForOperation = class;
  TBoldControlProviderForAttribute = class;
  TBoldControlProviderForStringAttribute = class;
  TBoldControlProviderForMemoAttribute = class;
  TBoldControlProviderForImageAttribute = class;
  TBoldControlProviderForBooleanAttribute = class;
  TBoldControlProviderForAssociationEnd = class;
  TBoldControlProviderForAssociationEndSingle = class;
  TBoldControlProviderForAssociationEndMulti = class;

  { TBoldDTDragObject }
  TBoldDTDragObject = class(TDragControlObject)
  private
    fAccepted: boolean;
  protected
    function AllowDrop(Control: TControl): boolean;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
    procedure PerformDrop(Target: TWinControl; x, y: integer);
  end;

  { TBoldControlProviderList }
  TBoldControlProviderList = class(TList)
  private
    function GetControlProviders(
      const index: integer): TBoldControlProvider;
  public
    destructor Destroy; override;
    procedure Add(ControlProvider: TBoldControlProvider);
    property Items[const index: integer]: TBoldControlProvider read GetControlProviders;
    function MatchingProvider(UMLElement: TUMLModelElement): TBoldControlProvider;
  end;

  { TBoldControlProvider }
  TBoldControlProvider = class
  private
    fUMLElement: TUMLModelElement;
    fHandle: TBoldHandle;
    fControl: TControl;
    fParent: TWinControl;
    fOwnerForm: TForm;
    fPoint: TPoint;
    function GetEnsuredHandle: TBoldHandle;
  protected
    function GetAddLabel: boolean; virtual;
    function UniqueName(const BaseName: string): string;
    procedure CreateLabel;
    function PrefixedUMLName(const Prefix: string): string;
    procedure SetupComponent; virtual;
    function GetComponentName: string; virtual;
    function GetHandleClass: TBoldHandleClass; virtual;
    function GetControlClass: TControlClass; virtual; abstract;
    property ComponentName: string read GetComponentName;
    property AddLabel: boolean read GetAddLabel;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; virtual; abstract;
    procedure MakeComponent(UMLElement: TUMLModelElement; TargetArea: TWinControl; Point: TPoint);
    property HandleClass: TBoldHandleClass read GetHandleClass;
    property ControlClass: TControlClass read GetControlClass;
    property EnsuredHandle: TBoldHandle read GetEnsuredHandle;
  end;

  { TBoldControlProviderForClass }
  TBoldControlProviderForClass = class(TBoldControlProvider)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForOperation }
  TBoldControlProviderForOperation = class(TBoldControlProvider)
  protected
    function GetAddLabel: boolean; override;
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
    procedure SetupComponent; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForAttribute }
  TBoldControlProviderForAttribute = class(TBoldControlProvider)
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForStringAttribute }
  TBoldControlProviderForStringAttribute = class(TBoldControlProviderForAttribute)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForMemoAttribute }
  TBoldControlProviderForMemoAttribute = class(TBoldControlProviderForAttribute)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForImageAttribute }
  TBoldControlProviderForImageAttribute = class(TBoldControlProviderForAttribute)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForBooleanAttribute }
  TBoldControlProviderForBooleanAttribute = class(TBoldControlProviderForAttribute)
  protected
    function GetAddLabel: boolean; override;
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForAssociationEnd }
  TBoldControlProviderForAssociationEnd = class(TBoldControlProvider)
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForAssociationEndSingle }
  TBoldControlProviderForAssociationEndSingle = class(TBoldControlProviderForAssociationEnd)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

  { TBoldControlProviderForAssociationEndMulti }
  TBoldControlProviderForAssociationEndMulti = class(TBoldControlProviderForAssociationEnd)
  protected
    function GetComponentName: string; override;
    function GetControlClass: TControlClass; override;
  public
    function IsMatch(UMLElement: TUMLModelElement): boolean; override;
  end;

function ControlProviderList: TBoldControlProviderList;
function BoldControlProviderListAssigned: boolean;

implementation

uses
  StdCtrls,
  BoldGUI,
  BoldElements,
  SysUtils,
  BoldGrid,
  BoldEdit,
  BoldListBox,
  BoldMemo,
  BoldImage,
  BoldCheckBox;

type
  TExposedControl = class(TControl);

var
  G_BoldControlProviderList: TBoldControlProviderList = nil;

function IsOnFormWithDesigner(Target: TWinControl): Boolean;
begin
  while Assigned(Target) and not (Target is TCustomForm) do
    Target := Target.Parent;
  Result := Assigned(Target) and Assigned(TCustomForm(Target).Designer);
end;

function GetControlAccepter(Target: TWinControl): TWinControl;
begin
  while assigned(Target) and (not (csAcceptsControls in Target.ControlStyle)) do
    Target := Target.Parent;
  Result := Target;  
end;


function BoldControlProviderListAssigned: boolean;
begin
  result := Assigned(G_BoldControlProviderList);
end;

function ControlProviderList: TBoldControlProviderList;
begin
  if not Assigned(G_BoldControlProviderList) then
    G_BoldControlProviderList := TBoldControlProviderList.Create;
  Result := G_BoldControlProviderList;
end;

{ TBoldDTDragObject }

function TBoldDTDragObject.AllowDrop(Control: TControl): boolean;
var
  UMLElement: TUMLModelElement;
  Target: TWinControl;
begin
  if Control is TWinControl then
    Target := TWinControl(Control)
  else
    Target := nil;
  Result := Assigned(GetControlAccepter(Target)) and
    IsOnFormWithDesigner(Target) and
    (BoldGUIHandler.DraggedObjects.Count = 1) and
    (BoldGUIHandler.DraggedObjects[0] is TUMLModelElement);
  if Result then
    begin
      UMLElement := BoldGUIHandler.DraggedObjects[0] as TUMLModelElement;
      Result := ControlProviderList.MatchingProvider(UMLElement) <> nil;
    end;
  fAccepted := Result;
end;

procedure TBoldDTDragObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
  if (not accepted) and (Target is TWinControl) and AllowDrop(TWinControl(Target)) then
    PerformDrop(TWinControl(Target), x, y);
  inherited;
end;

function TBoldDTDragObject.GetDragCursor(Accepted: Boolean; X,
  Y: Integer): TCursor;
var
  p: TPoint;
begin
  GetCursorPos(p);
  if Accepted then
    Result := inherited GetDragCursor(Accepted, X, Y)
   else if AllowDrop(FindDragTarget(p, True)) then
    Result := TExposedControl(Control).DragCursor
  else
    Result := crNoDrop;
end;


procedure TBoldDTDragObject.PerformDrop(Target: TWinControl; x, y: integer);
var
  p: TPoint;
  UMLElement: TUMLModelElement;
  ControlProvider: TBoldControlProvider;
begin
  Target := GetControlAccepter(Target);

  if IsOnFormWithDesigner(Target) and
     (BoldGUIHandler.DraggedObjects.Count > 0) and
     (BoldGUIHandler.DraggedObjects[0] is TUMLModelElement) then
  begin
    GetCursorPos(p);
    p := Target.ScreenToClient(p);
    UMLElement := BoldGUIHandler.DraggedObjects[0] as TUMLModelElement;
    ControlProvider := ControlProviderList.MatchingProvider(UMLElement);
    if Assigned(ControlProvider) then
      ControlProvider.MakeComponent(UMLElement, Target, p);
  end;
end;

{ TBoldControlProvider }

procedure TBoldControlProvider.CreateLabel;
var
  aLabel: TLabel;
begin
  if not Assigned(fControl) then Exit;
  aLabel := TLabel.Create(fControl.Owner);

  aLabel.Parent := fControl.Parent;
  aLabel.Top := fPoint.y - aLabel.Height;
  aLabel.Left := fPoint.x;
  aLabel.Caption := fUMLElement.Name;

  aLabel.Name := UniqueName('lbl' + fUMLElement.ExpandedExpressionName);
  if fControl is TWinControl then
    aLabel.FocusControl := fControl as TWinControl;
end;

function TBoldControlProvider.GetAddLabel: boolean;
begin
  Result := True;
end;

function TBoldControlProvider.GetComponentName: string;
begin
  Result := UniqueName(Copy(ControlClass.ClassName, 2, 255));
end;

function TBoldControlProvider.GetEnsuredHandle: TBoldHandle;
begin
  if not Assigned(fHandle) then
    fHandle := HandleClass.Create(fOwnerForm);
  Result := fHandle;
end;

function TBoldControlProvider.GetHandleClass: TBoldHandleClass;
begin



  Result := nil;
end;

procedure TBoldControlProvider.MakeComponent(UMLElement: TUMLModelElement;
  TargetArea: TWinControl; Point: TPoint);
begin
  fUMLElement := UMLElement;
  fParent := TargetArea;
  if TargetArea is TForm then
    fOwnerForm := TargetArea as TForm
  else
    fOwnerForm := TargetArea.Owner as TForm;
  fPoint := Point;
  SetupComponent;
  if AddLabel then
    CreateLabel;
end;

function TBoldControlProvider.PrefixedUMLName(const Prefix: string): string;
begin
  Result := UniqueName(Prefix + fUMLElement.ExpandedExpressionName);
end;

procedure TBoldControlProvider.SetupComponent;
var
  anIComponent: IBoldOCLComponent;
begin
  try
    fControl := ControlClass.Create(fOwnerForm);
    fControl.Parent := fParent;
    fControl.Top := fPoint.y;
    fControl.Left := fPoint.x;
    fControl.Name := ComponentName;

    if fControl.GetInterface(IBoldOCLComponent, anIComponent) then
      anIComponent.SetExpression(fUMLElement.ExpandedExpressionName);
  except
    fControl.Free;
    fControl := nil;
  end;
end;

function TBoldControlProvider.UniqueName(const BaseName: string): string;
begin
  Result := fOwnerForm.Designer.UniqueName(BaseName);
end;

{ TBoldControlProviderList }

procedure TBoldControlProviderList.Add(ControlProvider: TBoldControlProvider);
begin
  inherited Add(ControlProvider);
end;

destructor TBoldControlProviderList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TObject(Items[i]).Free;
  inherited;
end;

function TBoldControlProviderList.GetControlProviders(
  const index: integer): TBoldControlProvider;
begin
  Result := TBoldControlProvider(inherited Items[Index]);
end;

function TBoldControlProviderList.MatchingProvider(
  UMLElement: TUMLModelElement): TBoldControlProvider;
var
  i: integer;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
    if Items[i].IsMatch(UMLElement) then
    begin
      Result := Items[i];
      Break;
    end;
end;

{ TBoldControlProviderForClass }

function TBoldControlProviderForClass.GetComponentName: string;
begin
  Result := PrefixedUMLName('grd');
end;

function TBoldControlProviderForClass.GetControlClass: TControlClass;
begin
  Result := TBoldGrid;
end;

function TBoldControlProviderForClass.IsMatch(
  UMLElement: TUMLModelElement): boolean;
begin
  Result := UMLElement is TUMLClass;
end;

{ TBoldControlProviderForAttribute }

function TBoldControlProviderForAttribute.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := UMLElement is TUMLAttribute;
end;

{ TBoldControlProviderForStringAttribute }

function TBoldControlProviderForStringAttribute.GetComponentName: string;
begin
  Result := PrefixedUMLName('txt');
end;

function TBoldControlProviderForStringAttribute.GetControlClass: TControlClass;
begin
  Result := TBoldEdit;
end;

function TBoldControlProviderForStringAttribute.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := inherited IsMatch(UMLElement);
end;

{ TBoldControlProviderForAssociationEnd }

function TBoldControlProviderForAssociationEnd.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := UMLElement is TUMLAssociationEnd;
end;

{ TBoldControlProviderForAssociationEndMulti }

function TBoldControlProviderForAssociationEndMulti.GetComponentName: string;
begin
  Result := PrefixedUMLName('lbx');
end;

function TBoldControlProviderForAssociationEndMulti.GetControlClass: TControlClass;
begin
  Result := TBoldListBox;
end;

function TBoldControlProviderForAssociationEndMulti.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := Inherited IsMatch(UMLElement) and
            (UMLElement as TUMLAssociationEnd).Multi;
end;

{ TBoldControlProviderForAssociationEndSingle }

function TBoldControlProviderForAssociationEndSingle.GetComponentName: string;
begin
  Result := PrefixedUMLName('txt');
end;

function TBoldControlProviderForAssociationEndSingle.GetControlClass: TControlClass;
begin
  Result := TBoldEdit;
end;

function TBoldControlProviderForAssociationEndSingle.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := Inherited IsMatch(UMLElement) and
            not (UMLElement as TUMLAssociationEnd).Multi;
end;

{ TBoldControlProviderForMemoAttribute }

function TBoldControlProviderForMemoAttribute.GetComponentName: string;
begin
  Result := PrefixedUMLName('mmo');
end;

function TBoldControlProviderForMemoAttribute.GetControlClass: TControlClass;
begin
  Result := TBoldMemo;
end;

function TBoldControlProviderForMemoAttribute.IsMatch(
  UMLElement: TUMLModelElement): boolean;
begin
  Result := inherited IsMatch(UMLElement) and
           (AnsiCompareStr((UMLElement as TUMLAttribute).typeName, 'Blob') = 0);
end;

{ TBoldControlProviderForImageAttribute }

function TBoldControlProviderForImageAttribute.GetComponentName: string;
begin
  Result := PrefixedUMLName('img');
end;

function TBoldControlProviderForImageAttribute.GetControlClass: TControlClass;
begin
  Result := TBoldImage;
end;

function TBoldControlProviderForImageAttribute.IsMatch(
  UMLElement: TUMLModelElement): boolean;
begin
  Result := inherited IsMatch(UMLElement) and
           ((AnsiCompareStr((UMLElement as TUMLAttribute).typeName, 'BlobImageBMP') = 0) or
            (AnsiCompareStr((UMLElement as TUMLAttribute).typeName, 'BlobImageJPEG') = 0));
end;

{ TBoldControlProviderForBooleanAttribute }

function TBoldControlProviderForBooleanAttribute.GetAddLabel: boolean;
begin
  Result := False;
end;

function TBoldControlProviderForBooleanAttribute.GetComponentName: string;
begin
  Result := PrefixedUMLName('cbx');
end;

function TBoldControlProviderForBooleanAttribute.GetControlClass: TControlClass;
begin
  Result := TBoldCheckBox;
end;

function TBoldControlProviderForBooleanAttribute.IsMatch(
  UMLElement: TUMLModelElement): boolean;
begin
  Result := inherited IsMatch(UMLElement) and
           (AnsiCompareStr((UMLElement as TUMLAttribute).typeName, 'Boolean') = 0);
end;

{ TBoldControlProviderForOperation }

function TBoldControlProviderForOperation.GetAddLabel: boolean;
begin
  Result := False;
end;

function TBoldControlProviderForOperation.GetComponentName: string;
begin
  Result := PrefixedUMLName('btn');
end;

function TBoldControlProviderForOperation.GetControlClass: TControlClass;
begin
  Result := TButton;
end;

function TBoldControlProviderForOperation.IsMatch(UMLElement: TUMLModelElement): boolean;
begin
  Result := UMLElement is TUMLOperation;
end;

procedure TBoldControlProviderForOperation.SetupComponent;
begin
  inherited;
  (fControl as TButton).Caption := fUMLElement.Name; 
end;

initialization
  ControlProviderList.Add(TBoldControlProviderForClass.Create);
  ControlProviderList.Add(TBoldControlProviderForOperation.Create);
  ControlProviderList.Add(TBoldControlProviderForAssociationEndMulti.Create);
  ControlProviderList.Add(TBoldControlProviderForAssociationEndSingle.Create);
  ControlProviderList.Add(TBoldControlProviderForStringAttribute.Create);
  ControlProviderList.Add(TBoldControlProviderForMemoAttribute.Create);
  ControlProviderList.Add(TBoldControlProviderForImageAttribute.Create);
  ControlProviderList.Add(TBoldControlProviderForBooleanAttribute.Create);

finalization
  FreeAndNil(G_BoldControlProviderList);

end.
