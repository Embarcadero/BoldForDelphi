
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentAllowBothUseVCL;

interface

implementation

uses
  BoldEnvironmentVCL,
  BoldEnvironmentCLX,
  BoldEnvironment,
  BoldRev;

initialization
  BoldInternalAllowBothUseVCL := true;
end.
