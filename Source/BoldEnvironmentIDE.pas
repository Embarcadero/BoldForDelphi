
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentIDE;

interface

implementation

uses
  {$IFDEF BOLD_IDEVCL}
  BoldEnvironmentVCL,
  {$ENDIF}
  {$IFDEF BOLD_IDECLX}
  BoldEnvironmentCLX,
  {$ENDIF}
  BoldEnvironment;

initialization
  BoldInternalRunningInIDE := true;
end.
