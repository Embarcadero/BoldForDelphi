
{ Global compiler directives }
{$include bold.inc}
unit BoldUDPBroadcasterReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldIDEConsts,
  BoldUDPModificationBroadCaster;

{$R BoldUDPBroadCaster.res}

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldUDPModificationBroadcaster]);
end;

end.
