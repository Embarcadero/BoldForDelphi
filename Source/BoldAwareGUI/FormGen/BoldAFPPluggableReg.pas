unit BoldAFPPluggableReg;

interface

procedure Register;

implementation

{$R BoldAFPPluggableReg.res}

uses
  Classes,
  BoldAFPPluggable,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldPlaceableAFP]);
end;

end.
