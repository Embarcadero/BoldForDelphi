
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentAllowBothUseVCL;

interface

implementation

uses
  BoldEnvironmentVCL,
  BoldEnvironmentCLX,
  BoldEnvironment;

initialization
  BoldInternalAllowBothUseVCL := true;
end.
