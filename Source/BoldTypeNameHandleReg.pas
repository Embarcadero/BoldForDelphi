
{ Global compiler directives }
{$include bold.inc}
unit BoldTypeNameHandleReg;

interface

uses
  DesignEditors,
  DesignIntf,
  BoldAbstractPropertyEditors;

type
  { forward declarations }
  TBoldTypeNameEditor = class;
  TBoldTypeNamePropEditor = class;

  { TBoldTypeNameEditor }
  TBoldTypeNameEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(index: Integer); override;
    function GetVerb(index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBoldTypeNamePropEditor }
  TBoldTypeNamePropEditor = class(TBoldPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: String; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses
  SysUtils,
  Controls,

  BoldCoreConsts,
  BoldTypeNameDictionary,
  BoldTypeNameHandle,
  BoldTypeNameEditor,
  BoldGuard;

procedure Register;
begin
  RegisterComponentEditor(TBoldTypeNameHandle, TBoldTypeNameEditor);
  RegisterPropertyEditor(TypeInfo(TBoldTypeNameDictionary), TBoldTypeNameHandle, 'Dictionary', TBoldTypeNamePropEditor);
end;

{ TTBoldModelEditor }

procedure TBoldTypeNameEditor.ExecuteVerb(index: Integer);
var
  EditorForm: TBoldTypeNameEditorForm;
begin
  with Component as TBoldTypeNameHandle do
  begin
    EditorForm := TBoldTypeNameEditorForm.Create(nil);
    EditorForm.LoadFromDictionary(Dictionary);

    if EditorForm.ShowModal = mrOK then
    begin
      EditorForm.SaveToDictionary(Dictionary);
      Designer.Modified;
    end;

    EditorForm.Free;
  end;
end;

function TBoldTypeNameEditor.GetVerb(index: Integer): string;
begin
  result := sEditTypeNames;
end;

function TBoldTypeNameEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBoldTypeNamePropEditor }

procedure TBoldTypeNamePropEditor.Edit;
var
  EditorForm: TBoldTypeNameEditorForm;
begin
  with GetComponent(0) as TBoldTypeNameHandle do
  begin
    EditorForm := TBoldTypeNameEditorForm.Create(nil);
    EditorForm.LoadFromDictionary(Dictionary);

    if EditorForm.ShowModal = mrOK then
    begin
      EditorForm.SaveToDictionary(Dictionary);
      Designer.Modified;
    end;

    EditorForm.Free;
  end;
end;

function TBoldTypeNamePropEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TBoldTypeNamePropEditor.GetValue: String;
begin
  result := 'TBoldTypeNameDictionary';
end;

end.
