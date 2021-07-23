
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalObjectSpaceEventHandlerReg;

interface

procedure Register;

implementation

{$R *.res}

uses
  SysUtils,
  BoldUtils,
  BoldGuard,
  Classes,
  DesignIntf,
  BoldHandles,
  BoldExternalObjectSpaceEventHandler,
  BoldAbstractPropertyEditors,
  BoldIDEConsts;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldExternalObjectSpaceEventHandler]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractSystemHandle), TBoldExternalObjectSpaceEventHandler, 'BoldSystemHandle', TBoldComponentPropertyIndicateMissing); // do not localize
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.

