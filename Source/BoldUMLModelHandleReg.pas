
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelHandleReg;

interface

procedure Register;

implementation

{$R BoldUMLModelHandleReg.res}

uses
  SysUtils,
  Classes,
  BoldIDEConsts,
  BoldModel;


procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_HANDLES, [TBoldModel]);
end;

procedure RegisterEditors;
begin
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
