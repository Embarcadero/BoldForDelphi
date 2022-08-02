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
  BoldIDEConsts;

procedure Register;
begin

  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterFireDAC]);
end;

end.
