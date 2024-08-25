
{ Global compiler directives }
{$include bold.inc}
unit BoldMath;

interface

function Floor(X: Extended): Integer;
function MaxIntValue(const Data: array of Integer): Integer;
function MaxValue(const Data: array of Double): Double;
function MinValue(const Data: array of Double): Double;
function MinIntValue(const Data: array of Integer): Integer;

implementation

function Floor(X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

function MaxIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I];
end;

function MaxValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I];
end;


function MinValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

function MinIntValue(const Data: array of Integer): Integer;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

end.
