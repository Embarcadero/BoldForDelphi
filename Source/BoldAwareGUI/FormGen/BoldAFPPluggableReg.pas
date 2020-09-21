unit BoldAFPPluggableReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldAFPPluggable,
  BoldIDEConsts;

{$R *.res}

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldPlaceableAFP]);
end;

end.
