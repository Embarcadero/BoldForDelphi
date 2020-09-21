unit BoldHTTPClientPersistenceHandleReg;

interface

procedure Register;

implementation

{$R *.res}

uses
  BoldIDEConsts,
  BoldHTTPClientPersistenceHandle,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldHTTPClientPersistenceHandle]);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
