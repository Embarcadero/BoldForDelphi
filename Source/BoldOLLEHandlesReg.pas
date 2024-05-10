
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEHandlesReg;

interface

procedure Register;

implementation

{$R BoldOLLEHandles.res}

uses
  SysUtils,
  BoldUtils,
  Classes,
  DesignIntf,
  DesignEditors,
  BoldIDEConsts,
  BoldIDESupport,
  BoldOlleHandles,
  BoldOlleHandlesComponentEditor;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OLLE, [TBoldOLLEHandle]);
end;

procedure RegisterComponentEditors;
begin
  RegisterComponentEditor(TBoldOLLEHandle, TBoldOLLEHandleComponentEditor);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterComponentEditors;
end;

end.
