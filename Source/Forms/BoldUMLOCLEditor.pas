
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLOCLEditor;

interface
uses
  BoldHandles,
  BoldOclPropEditor,
  BoldUMLModel,
  BoldModel;

type
  { forward declarations }
  TBoldUMLOclEditor = class;

  { TBoldUMLOclEditor }
  TBoldUMLOclEditor = class
  private
    fSystemTypeInfohandle: TBoldSystemTypeInfoHandle;
  public
    constructor Create;
    destructor Destroy; override;
    function EditOcl(BoldModel: TBoldModel; Context: TUMLModelElement; OldExpression: String): String;
    procedure DiscardRuntimeModel;
  end;

function BoldUMLOclEditor_: TBoldUMLOclEditor;

implementation

uses
  SysUtils,
  Controls,
  BoldDefs,  
  BoldTypeNameDictionary;

var
  G_UMLOCLEditor: TBoldUMLOclEditor = nil;

function BoldUMLOclEditor_: TBoldUMLOclEditor;
begin
  if not assigned(G_UMLOclEditor) then
    G_UMLOCLEditor := TBoldUMLOclEditor.Create;
  result := G_UMLOCLEditor;
end;

{ TBoldUMLOclEditor }

constructor TBoldUMLOclEditor.Create;
begin
  inherited;
  fSystemTypeInfohandle := TBoldSystemTypeInfoHandle.Create(nil);
  fSystemTypeInfoHandle.UseGeneratedCode := false;
end;

destructor TBoldUMLOclEditor.Destroy;
begin
  FreeAndNil(fSystemTypeInfohandle);
  inherited;
end;

procedure TBoldUMLOclEditor.DiscardRuntimeModel;
begin

end;

function TBoldUMLOclEditor.EditOcl(BoldModel: TBoldModel; Context: TUMLModelElement; OldExpression: String): String;
var
  EditForm: TBoldOclPropEditForm;
  Mapping: TBoldTypeNameMapping;
begin
  fSystemTypeInfohandle.BoldModel := BoldModel;

  EditForm := TBoldOclPropEditForm.Create(nil);
  if context is TUMLClass then
    EditForm.Context := fSystemTypeInfohandle.StaticSystemTypeInfo.ElementTypeInfoByExpressionName[Context.ExpandedExpressionName]
  else if Context is TUMLAttribute then
  begin
    Mapping := BoldModel.TypeNameDictionary.MappingForModelName[(Context as TUMLAttribute).typeName];
    if assigned(Mapping) then
      EditForm.Context := fSystemTypeInfohandle.StaticSystemTypeInfo.ElementTypeInfoByExpressionName[mapping.ExpressionName];
  end
  else
    raise EBold.CreateFmt('Unsupported context type for OCL-expression: %s', [Context.ClassName]);

  EditForm.OclExpr := OldExpression;
  if Editform.ShowModal = mrOK then
    result := editForm.OclExpr
  else
    result := OldExpression;
  EditForm.Release;
end;


initialization

finalization
  FreeAndNil(G_UMLOCLEditor);
end.
