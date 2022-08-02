{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleUniDACReg;

interface

procedure Register;

implementation

//{$R BoldPersistenceHandleUniDAC.res}

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterUniDAC,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterUniDAC]);
end;

end.
