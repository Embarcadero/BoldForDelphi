unit BoldConcurrencyControlReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldLockManagerAdminHandleCom,
  BoldLockManagerHandleCom,
  BoldIDEConsts,
  Classes;

{$R *.res}


procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [
    TBoldLockManagerAdminHandleCom,
    TBoldLockManagerHandleCom
  ]);
end;


procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
