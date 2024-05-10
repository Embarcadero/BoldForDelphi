
{ Global compiler directives }
{$include bold.inc}
unit BoldReg;

interface

procedure Register;

implementation

uses
  DesignIntf,
  Classes,
  BoldPropertyEditors,
  BoldTemplateExpander,
  BoldIDEConsts;

{$R *.res}

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_MISC, [TBoldTemplateHolder]);
    RegisterComponentEditor(TBoldTemplateHolder, TBoldStringListEditor);
end;

end.

