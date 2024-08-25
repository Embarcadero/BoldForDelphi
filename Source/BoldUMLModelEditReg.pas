
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEditReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  Forms,
  DesignEditors,
  DesignIntf,
  Classes,
  BoldCursorGuard,
  BoldSmallLogFrame,
  BoldLogHandlerForm,
  BoldUMLModel,
  BoldDefsDT,
  BoldUMLModelEdit,
  BoldModel,
  BoldUMLModelEditPlugIn;

type
  { forward declarations }
  TUMLModelComponentEditor = class;
  TUMLModelPropertyEditor = class;


  TUMLModelPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  {TUMLModelComponentEditor - Component editor for the BoldModel component}

  { TUMLModelComponentEditor }
  TUMLModelComponentEditor = class(TComponentEditor, IUnknown, IUMLModelPlugInContext)
  private
    fIsExecutingPlugin: Boolean;
    function GetIsExecutingPlugin: boolean;
    procedure SetIsExecutingPlugin(Value: boolean);
  protected
    function QueryInterface(const IId: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function GetCurrentModelHandle: TBoldModel;
    function GetCurrentElement: TUMLModelElement;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure ExecuteVerb(index: Integer); override;
    function GetVerb(index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{TUMLModelComponentEditor}
procedure TUMLModelComponentEditor.ExecuteVerb(index: Integer);
var
  CursorGuard: IBoldCursorGuard;
  Progressform: TForm;
begin
  case index of
    0: begin
      CursorGuard := TBoldCursorGuard.Create;
      Progressform := TBoldLogFrame.CreateSmallLogForm('Opening Model...');
      try
        UMLModelEditor.ShowEditFormForBoldModel(Component as TBoldModel);
      finally
        Progressform.release;
      end;
    end;
  else
    (TObject(UMLModelEditor.PlugInList[index - 1]) as TUMLPlugIn).GuardedExecute(self);
  end;
end;

function TUMLModelComponentEditor.GetVerb(index: Integer): string;
begin
  if index = 0 then
    Result := BOLD_OPENUMLEDITOR
  else
    Result := (TObject(UMLModelEditor.PlugInList[index - 1]) as TUMLPlugIn).MenuItemName;
end;

function TUMLModelComponentEditor.GetVerbCount: Integer;
begin
  Result := 1 + UMLModelEditor.PlugInList.Count;
end;

function TUMLModelComponentEditor.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TUMLModelComponentEditor._AddRef: Integer;
begin
  result := -1;
end;

function TUMLModelComponentEditor._Release: Integer;
begin
  result := -1;
end;

function TUMLModelComponentEditor.GetCurrentModelHandle: TBoldModel;
begin
  result := Component as TBoldModel;
end;

function TUMLModelComponentEditor.GetCurrentElement: TUMLModelElement;
begin
  result := GetCurrentModelHandle.EnsuredUMLModel;
end;

procedure Register;
begin
  RegisterComponentEditor(TBoldModel, TUMLModelComponentEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TBoldModel, 'EditableModel', TUMLModelPropertyEditor);
end;

constructor TUMLModelComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited;
  fIsExecutingPlugin := false;
end;

function TUMLModelComponentEditor.GetIsExecutingPlugin: boolean;
begin
  result := fIsExecutingPlugIn;
end;

procedure TUMLModelComponentEditor.SetIsExecutingPlugin(Value: boolean);
begin
  fIsExecutingPlugIn := value;
end;

{ TUMLModelPropertyEditor }

procedure TUMLModelPropertyEditor.Edit;
var
  CursorGuard: IBoldCursorGuard;
  Progressform: TForm;
begin
  CursorGuard := TBoldCursorGuard.Create;
  Progressform := TBoldLogFrame.CreateSmallLogForm('Opening Model...');
  try
    UMLModelEditor.ShowEditFormForBoldModel(GetComponent(0) as TBoldModel);
  finally
    Progressform.release;
  end;
end;

function TUMLModelPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly] - [paMultiSelect]; 
end;

function TUMLModelPropertyEditor.GetValue: string;
begin
  Result := '(Open model editor)';
end;

end.
