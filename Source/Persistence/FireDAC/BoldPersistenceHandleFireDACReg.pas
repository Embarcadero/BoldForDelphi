{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleFireDACReg;

interface

procedure Register;

implementation

//{$R BoldPersistenceHandleFireDAC.res}

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterFireDAC,
//  BoldPersistenceHandleFireDAC,
  BoldIDEConsts;

procedure Register;
begin
	{$WARNINGS OFF}
//  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleFireDAC]);
  {$WARNINGS ON}
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterFireDAC]);
end;

end.
