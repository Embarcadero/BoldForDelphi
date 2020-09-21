unit BoldComServ;

interface

uses
  ComObj;

function Bold_TheComServer: TComServerObject;

implementation

uses
  ComServ;

function Bold_TheComServer: TComServerObject;
begin
  result := ComServer;
end;

end.
