
{ Global compiler directives }
{$include bold.inc}
unit BoldCheckListBoxReg;

interface

procedure Register;
  
implementation

uses
  Classes,
  BoldIDEConsts,
  BoldCheckListBox,
  BoldRev;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_CONTROLS, [TBoldCheckListBox]);
end;

end.
