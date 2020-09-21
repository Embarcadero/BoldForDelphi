unit BoldComElementHandleReg;

interface

procedure Register;

implementation

{$R BoldComElementHandleReg.Res}

uses
  Classes,
  BoldComServerElementHandles;

procedure Register;
begin
  RegisterComponents('Bold COM', [TBoldComServerElementHandle]); // do not localize
end;

end.
