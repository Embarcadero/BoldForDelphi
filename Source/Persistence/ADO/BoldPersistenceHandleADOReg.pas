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
	{$WARNINGS OFF}
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleADO]);
  {$WARNINGS ON}
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterADO]);
end;

end.
