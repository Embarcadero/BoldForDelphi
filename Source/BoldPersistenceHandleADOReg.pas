
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleADOReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleADO.res}

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterADO,
  BoldPersistenceHandleADO,
  BoldIDEConsts;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleADO]);
    RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterADO]);
end;

end.
