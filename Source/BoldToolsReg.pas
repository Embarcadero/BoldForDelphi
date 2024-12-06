{ Global compiler directives }
{$include bold.inc}
unit BoldToolsReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldIdeConsts,
  BoldSystemCopy,
  BoldSystemMerger;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldSystemCopy, TBoldSystemMerger]);
end;

end.
