{ Global compiler directives }
unit BoldPersistenceHandleFireDACReg;

{$include bold.inc}

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
