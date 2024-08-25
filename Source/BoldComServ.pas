
{ Global compiler directives }
{$include bold.inc}
unit BoldComServ;

interface

uses
  ComObj;

function Bold_TheComServer: TComServerObject;

implementation

uses
  SysUtils,
  BoldUtils,
  ComServ;

function Bold_TheComServer: TComServerObject;
begin
  result := ComServer;
end;

end.
