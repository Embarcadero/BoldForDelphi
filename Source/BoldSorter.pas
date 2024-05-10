
{ Global compiler directives }
{$include bold.inc}
unit BoldSorter;

interface

type

  TBoldSortCompare = function (Index1, Index2: Integer): Integer of object;
  TBoldSortExchange = procedure(Index1, Index2: integer) of object;

procedure BoldSort(FirstIndex, LastIndex: Integer; Compare:TBoldSortCompare ; Exchange : TBoldSortExchange);

implementation


procedure BoldSort(FirstIndex, LastIndex: Integer; Compare:TBoldSortCompare ; Exchange : TBoldSortExchange);
var
  I, J, P: Integer;
begin
  repeat
    I := FirstIndex;
    J := LastIndex;
    P := (FirstIndex + LastIndex) shr 1;
    repeat
      while Compare(I, P) < 0 do Inc(I);
      while Compare(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if FirstIndex < J then BoldSort(FirstIndex, J, Compare, Exchange);
    FirstIndex := I;
  until I >= LastIndex;
end;



end.
