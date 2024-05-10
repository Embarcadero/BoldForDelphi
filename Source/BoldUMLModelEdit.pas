
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEdit;

interface

uses
  Classes,
  BoldSubscription,
  BoldModel,
  BoldUMLModelEditForm,
  BoldUMLModelEditPlugIn;

type
  { forward declarations }
  TModelEdit = class;

  { TModelEdit }
  TModelEdit = class(TComponent)
  private
    fEditForms: TList;
    fPlugInList: TList;
    class procedure LoadFormsettingsFromRegistry(aForm: TBoldModelEditFrm);
    property EditForms: TList read FEditForms;
    function CreateFormForBoldModel(BoldModel: TBoldModel): TBoldModelEditFrm;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure SaveFormsettingsToRegistry(aForm: TBoldModelEditFrm);
    property PlugInList: TList read FPlugInList;
    procedure ShowEditFormForBoldModel(BoldModel: TBoldModel);
    function FindFormForBoldModel(BoldModel: TBoldModel): TBoldModelEditFrm;
  end;

function UMLModelEditor: TModelEdit;
function UMLModelEditorAssigned: Boolean;

implementation

uses
  SysUtils,
  BoldRegistry,
  Controls,
  BoldUMLModelDataModule,  
  BoldGuard;

var G_ModelEditor: TModelEdit = nil;

function UMLModelEditor: TModelEdit;
begin
  if not assigned(G_ModelEditor) then
    G_ModelEditor := TModelEdit.Create;
  result := G_ModelEditor;
end;

function UMLModelEditorAssigned: Boolean;
begin
  Result := Assigned(G_ModelEditor);
end;

constructor TModelEdit.Create;
begin
  inherited Create(nil);
  FEditForms := TList.Create;
  FPlugInList := TList.Create;
end;

destructor TModelEdit.Destroy;
begin
  while EditForms.Count > 0 do
    TBoldModelEditFrm(EditForms.Items[EditForms.Count - 1]).Free;
  FreeAndNil(FEditForms);
  FreeAndNil(FPlugInList);
  inherited;
end;

function TModelEdit.FindFormForBoldModel(BoldModel: TBoldModel): TBoldModelEditFrm;
var
  i: Integer;
  aForm: TBoldModelEditFrm;
begin
  for i := 0 to EditForms.Count - 1 do
  begin
    aForm := TObject((EditForms.Items[i])) as TBoldModelEditFrm;
    if aForm.ModelHandle = BoldModel then
    begin
      result := aForm;
      exit;
    end;
  end;
  result := nil;
end;

function TModelEdit.CreateFormForBoldModel(BoldModel: TBoldModel): TBoldModelEditFrm;
var
  aForm: TBoldModelEditFrm;
begin
  EnsureModelEditDataModule;
  aForm := TBoldModelEditFrm.Create(nil);
  Aform.FreeNotification(Self);
  EditForms.Add(aForm);
  aForm.ModelHandle := BoldModel;
  aForm.PlugInList := PlugInList;
  LoadFormsettingsFromRegistry(aForm);
  result := aForm;
end;

class procedure TModelEdit.SaveFormsettingsToRegistry(aForm: TBoldModelEditFrm);
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(BoldRegistry);
  BoldRegistry := TBoldRegistry.Create;
  try
    with BoldRegistry do
    begin
      OpenKey('\UMLModel');
      WriteInteger('Width', aForm.Width);
      WriteInteger('Height', aForm.Height);
      WriteInteger('Left', aForm.Left);
      WriteInteger('Top', aForm.Top);
      WriteInteger('TreeViewWidth', aForm.BoldTreeView1.Width);
      WriteInteger('ModelHeight', aForm.sbModel.Height);
      WriteInteger('ClassHeight', aForm.sbClass.Height);
      WriteInteger('OperationHeight', aForm.sbOperation.Height);
      WriteInteger('AssociationHeight', aForm.sbAssociation.Height);
      WriteInteger('AssociationEndHeight', aForm.sbAssociationEnd.Height);
    end;
  except
  end;
end;

class procedure TModelEdit.LoadFormsettingsFromRegistry(aForm: TBoldModelEditFrm);
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(BoldRegistry);
  try
    BoldRegistry := TBoldRegistry.Create;
    with BoldRegistry do
    begin
      OpenKey('\UMLModel');
      aForm.Width := ReadInteger('Width', 800);
      aForm.Height := ReadInteger('Height', 600);
      aForm.Left := ReadInteger('Left', 0);
      aForm.Top := ReadInteger('Top', 0);

      aForm.BoldTreeView1.Width := ReadInteger('TreeViewWidth', aForm.BoldTreeView1.Width);
      aForm.sbModel.Height := ReadInteger('ModelHeight', aForm.sbModel.Height);
      aForm.sbClass.Height := ReadInteger('ClassHeight', aForm.sbClass.Height);
      aForm.sbOperation.Height := ReadInteger('OperationHeight', aForm.sbOperation.Height);
      aForm.sbAssociation.Height := ReadInteger('AssociationHeight', aForm.sbAssociation.Height);
      aForm.sbAssociationEnd.Height := ReadInteger('AssociationEndHeight', aForm.sbAssociationEnd.Height);
    end;
  except
  end;
end;

procedure TModelEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent is TBoldModelEditFrm) and (Operation = opRemove) then
    EditForms.Delete(EditForms.IndexOf(AComponent));
end;

procedure TModelEdit.ShowEditFormForBoldModel(BoldModel: TBoldModel);
var
  aForm: TBoldModelEditFrm;
begin
  aForm := FindFormForBoldModel(BoldModel);
  if aForm = nil then
    aForm := CreateFormForBoldModel(BoldModel);
  aForm.Show;
end;

initialization

finalization
  FreeAndNil(G_ModelEditor);
end.
