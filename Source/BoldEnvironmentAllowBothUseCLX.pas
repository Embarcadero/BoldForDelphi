
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentAllowBothUseCLX;

interface

implementation

uses
  BoldEnvironmentVCL,
  BoldEnvironmentCLX,
  BoldEnvironment;

initialization
  BoldInternalAllowBothUseCLX := true;
end.
