unit BoldLockingReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  Classes,
  BoldDefs,
  BoldIDEConsts,
  BoldLockingHandles;

{.$R *.res}

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldLockingHandle]);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
