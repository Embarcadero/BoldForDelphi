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
//  BoldPersistenceHandleUniDAC,
  BoldIDEConsts;

procedure Register;
begin
	{$WARNINGS OFF}
//  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleUniDAC]);
  {$WARNINGS ON}
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterUniDAC]);
end;

end.
