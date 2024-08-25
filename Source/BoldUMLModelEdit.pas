{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEdit;

interface

uses
  Classes,
  BoldSubscription,
  BoldModel,
  BoldUMLModelEditPlugIn,
  Vcl.Forms;

type
  { forward declarations }
  TModelEdit = class;

  IBoldModelEditForm = interface(IUnknown)
  ['{D2275734-8CDE-4B32-9D68-6C3EE4B34824}']
    procedure SetModelHandle(const Value: TBoldModel);
    function GetCurrentModelHandle: TBoldModel;
    property ModelHandle: TBoldModel read GetCurrentModelHandle write SetModelHandle;
    procedure SaveFormsettingsToRegistry;
    procedure LoadFormsettingsFromRegistry;
    function GetPlugInList: TList;
    procedure SetPluginList(const Value: TList);
    procedure LoadModelFromFile;
    procedure SetLoadedFrom(const Value: string);
    property PlugInList: TList read GetPlugInList write SetPluginList;
    property LoadedFrom: string write SetLoadedFrom;
  end;

  { TModelEdit }
  TModelEdit = class(TComponent)
  private
    fEditForms: TList;
    fPlugInList: TList;
    property EditForms: TList read FEditForms;
    function CreateFormForBoldModel(BoldModel: TBoldModel): TForm;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property PlugInList: TList read FPlugInList;
    procedure ShowEditFormForBoldModel(BoldModel: TBoldModel);
    function FindFormForBoldModel(BoldModel: TBoldModel): TForm;
    function EnsureFormForBoldModel(BoldModel: TBoldModel): TForm;
  end;

function UMLModelEditor: TModelEdit;
function UMLModelEditorAssigned: Boolean;

var
  BoldModelEditFrmClass: TFormClass = nil; //TBoldModelEditFrm;

implementation

uses
  SysUtils,
  BoldRegistry,
  Controls,
  System.Types,
  BoldUMLModelDataModule,
  BoldUMLModelEditorHandlesDataModule,
  BoldUMLModelEditForm,
  BoldGuard;

var G_ModelEditor: TModelEdit = nil;

function UMLModelEditor: TModelEdit;
begin
  if not assigned(G_ModelEditor) then
  begin
    G_ModelEditor := TModelEdit.Create;
    if not Assigned(BoldModelEditFrmClass) then
      BoldModelEditFrmClass := TBoldModelEditFrm;
  end;
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
    TForm(EditForms.Items[EditForms.Count - 1]).Free;
  FreeAndNil(FEditForms);
  FreeAndNil(FPlugInList);
  inherited;
end;

function TModelEdit.FindFormForBoldModel(BoldModel: TBoldModel): TForm;
var
  i: Integer;
  aForm: TForm;
begin
  for i := 0 to EditForms.Count - 1 do
  begin
    aForm := TObject((EditForms.Items[i])) as TForm;
    if (aForm as IBoldModelEditForm).ModelHandle = BoldModel then
    begin
      result := aForm;
      exit;
    end;
  end;
  result := nil;
end;

function TModelEdit.CreateFormForBoldModel(BoldModel: TBoldModel): TForm;
var
  aForm: TForm;
begin
  if not Assigned(BoldModelEditFrmClass) then
    raise Exception.Create('BoldModelEditFrmClass not set, add BoldUMLModelEdit to uses.');
  if not Assigned(dmBoldUMLModelEditorHandles) then
    Application.CreateForm(TdmBoldUMLModelEditorHandles, dmBoldUMLModelEditorHandles);
  Application.CreateForm(BoldModelEditFrmClass, aForm);
  Aform.FreeNotification(Self);
  EditForms.Add(aForm);
  (aForm as IBoldModelEditForm).ModelHandle := BoldModel;
  (aForm as IBoldModelEditForm).PlugInList := PlugInList;
  (aForm as IBoldModelEditForm).LoadFormsettingsFromRegistry;
  result := aForm;
end;

procedure TModelEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    EditForms.Remove(AComponent);
end;

procedure TModelEdit.ShowEditFormForBoldModel(BoldModel: TBoldModel);
var
  aForm: TForm;
begin
  aForm := FindFormForBoldModel(BoldModel);
  if aForm = nil then
    aForm := CreateFormForBoldModel(BoldModel);
  aForm.Show;
end;

function TModelEdit.EnsureFormForBoldModel(
  BoldModel: TBoldModel): TForm;
begin
  result := FindFormForBoldModel(BoldModel);
  if result = nil then
    result := CreateFormForBoldModel(BoldModel);
end;

initialization

finalization
  FreeAndNil(G_ModelEditor);
end.