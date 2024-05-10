
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceHandleSQLPropEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  BoldMeta,
  BoldDefs,
  Dialogs, StdCtrls, CheckLst;

type
  TBoldExternalPersistenceHandleSQLPropEditorForm = class(TForm)
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    procedure Initialize(MoldModel: TMoldModel; ClassesToHandle: TStrings);
  end;

implementation

{$R *.dfm}


{ TBoldExternalPersistenceHandleSQLPropEditorForm }

procedure TBoldExternalPersistenceHandleSQLPropEditorForm.Initialize(
  MoldModel: TMoldModel; ClassesToHandle: TStrings);

  function IsExternal(MoldClass: TMoldClass): Boolean;
  begin
    Result := MoldClass.Storage in [bsPartiallyExternal, bsExternal];
  end;

var
  i: integer;
begin
  for i := 0 to MoldModel.Classes.Count-1 do
    if IsExternal(MoldModel.Classes[i]) then
      CheckListBox1.Items.Add(MoldModel.Classes[i].ExpandedExpressionName);
  for i := 0 to ClassesToHandle.Count-1 do
    if CheckListBox1.Items.IndexOf(ClassesToHandle[i]) <> -1 then
      CheckListBox1.Checked[CheckListBox1.Items.IndexOf(ClassesToHandle[i])] := True;
end;

end.
