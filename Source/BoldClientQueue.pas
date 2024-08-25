
{ Global compiler directives }
{$include bold.inc}
unit BoldClientQueue;

interface

uses
  Variants,
  Classes,
  BoldGuard,
  BoldThreadSafeQueue,
  BoldDefs;

type
  TBoldClientQueue = class;

  TBoldClientQueue = class(TBoldThreadSafeStringQueue)
  private
    fBoldClientID: TBoldClientID;
  public
    function AsVarArray: variant;
    property BoldClientID: TBoldClientID read fBoldClientID write fBoldClientID;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldClientQueue }

function TBoldClientQueue.AsVarArray: variant;
var
  i: integer;
begin
  Lock;
  try
    if UnsafeIsEmpty then
      Result := UnAssigned
    else
    begin
      Result := VarArrayCreate([0, Count - 1], varOleStr);
      for i := 0 to Count-1 do
        Result[i] := Dequeue;
    end;
  finally
    Unlock;
  end;
end;

end.
