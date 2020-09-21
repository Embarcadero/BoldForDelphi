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
  BoldRev,
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
    TBoldModelEditFrm(EditForms.Items[EditForms.Count - 1]).Free;  // Will remove from list through notification
  FreeAndNil(FEditForms);
  // The plugins are freed in the finalization part of BoldUMLPlugins.pas.
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
      OpenKey('\UMLModel'); // do not localize
      WriteInteger('Width', aForm.Width); // do not localize
      WriteInteger('Height', aForm.Height); // do not localize
      WriteInteger('Left', aForm.Left); // do not localize
      WriteInteger('Top', aForm.Top); // do not localize
      WriteInteger('TreeViewWidth', aForm.BoldTreeView1.Width); // do not localize
      WriteInteger('ModelHeight', aForm.sbModel.Height); // do not localize
      WriteInteger('ClassHeight', aForm.sbClass.Height); // do not localize
      WriteInteger('OperationHeight', aForm.sbOperation.Height); // do not localize
      WriteInteger('AssociationHeight', aForm.sbAssociation.Height); // do not localize
      WriteInteger('AssociationEndHeight', aForm.sbAssociationEnd.Height); // do not localize
    end;
  except
    // Silently discard exceptions
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
      OpenKey('\UMLModel'); // do not localize
      aForm.Width := ReadInteger('Width', 800); // do not localize
      aForm.Height := ReadInteger('Height', 600); // do not localize
      aForm.Left := ReadInteger('Left', 0); // do not localize
      aForm.Top := ReadInteger('Top', 0); // do not localize

      aForm.BoldTreeView1.Width := ReadInteger('TreeViewWidth', aForm.BoldTreeView1.Width); // do not localize
      aForm.sbModel.Height := ReadInteger('ModelHeight', aForm.sbModel.Height); // do not localize
      aForm.sbClass.Height := ReadInteger('ClassHeight', aForm.sbClass.Height); // do not localize
      aForm.sbOperation.Height := ReadInteger('OperationHeight', aForm.sbOperation.Height); // do not localize
      aForm.sbAssociation.Height := ReadInteger('AssociationHeight', aForm.sbAssociation.Height); // do not localize
      aForm.sbAssociationEnd.Height := ReadInteger('AssociationEndHeight', aForm.sbAssociationEnd.Height); // do not localize
    end;
  except
    //Silently discard exception
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
