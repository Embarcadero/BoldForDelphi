
{ Global compiler directives }
{$include bold.inc}
unit BoldModelReg;

interface

uses
  DesignIntf,
  Classes;

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldGuard,
  BoldTypeNameHandle,
  BoldAbstractModel,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldTypeNameHandle]);
end;

end.
