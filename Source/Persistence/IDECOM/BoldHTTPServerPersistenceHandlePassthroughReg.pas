unit BoldHTTPServerPersistenceHandlePassthroughReg;

interface

procedure Register;

implementation

{$R *.res}

uses
  BoldIDEConsts,
  BoldHTTPServerPersistenceHandlePassthrough,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldHTTPServerPersistenceHandlePassthrough]);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
