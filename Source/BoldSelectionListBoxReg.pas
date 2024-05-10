
{ Global compiler directives }
{$include bold.inc}
unit BoldSelectionListBoxReg;

interface

procedure Register;
  
implementation

uses
  Classes,
  BoldIDEConsts,
  BoldSelectionListBox;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_CONTROLS, [TBoldSelectionListBox]);
end;


end.
