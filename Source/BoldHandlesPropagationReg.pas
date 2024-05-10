
{ Global compiler directives }
{$include bold.inc}
unit BoldHandlesPropagationReg;

interface

procedure Register;

implementation

{$R BoldHandlesPropagationReg.res}

uses
  SysUtils,
  BoldUtils,
  Classes,
  DesignIntf,
  BoldIDEConsts,
  BoldGuard,
  BoldAbstractPropertyEditors,
  BoldListenerHandle,
  BoldIDAdderHandle,
  BoldSnooperHandle,
  BoldAbstractDequeuer;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldListenerHandle]);
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldSnooperHandle]);
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldIDAdderHandle]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractDequeuer), TBoldListenerHandle, 'Dequeuer', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldListenerHandle), TBoldIDAdderHandle, 'BoldListener', TBoldComponentPropertyIndicateMissing);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
end;

end.
