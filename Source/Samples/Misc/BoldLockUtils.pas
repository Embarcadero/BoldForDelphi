
{ Global compiler directives }
{$include bold.inc}
unit BoldLockUtils;

interface

uses
  Classes,
  Grids,
  BoldMath,
  BoldObjectRetriever,
  BoldSystem,
  BoldIndex,
  BoldLockHolder;

type
  TBoldLockListSortKey = (llskLock, llskType, llskClass, llskObject, llskNoSort);

  TBoldLockUtils = class
  public
    class function GetObjectFromLock(Lock: TBoldLock; System: TBoldSystem): TBoldObject;
    class procedure GetLockInfo(System: TBoldSystem; LockHolder: TBoldAbstractLockHolder; LockInfo: TStringList);
    class procedure LockInfoToStringGrid(StringGrid: TStringGrid; System: TBoldSystem; LockHolder: TBoldAbstractLockHolder);
  end;

var
  BoldLockListSortKey: TBoldLockListSortKey = llskNoSort;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldLockUtils }

function LockListSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1: TStringList;
  s2: TStringList;
begin
  s1 := TStringList.Create;
  s2 := TStringList.Create;
  s1.CommaText := List[Index1];
  s2.CommaText := List[Index2];
  case BoldLockListSortKey of
    llskLock: result := AnsiCompareText(s1[0], s2[0]);
    llskType: result := AnsiCompareText(s1[1], s2[1]);
    llskClass: result := AnsiCompareText(s1[2], s2[2]);
    llskObject: result := AnsiCompareText(s1[3], s2[3]);
    else result := 0;
  end;
  s1.Free;
  s2.Free;
end;

class procedure TBoldLockUtils.GetLockInfo(System: TBoldSystem; LockHolder: TBoldAbstractLockHolder; LockInfo: TStringList);
  procedure AddLockInfo(Lock: TBoldLock; Exclusive: Boolean);
  var
    obj: TBoldObject;
    res: TStringList;
  begin
    res := TStringList.Create;
    res.Add(Lock.Name);
    Obj := GetObjectFromLock(Lock, System);
    if Exclusive then
      res.Add('Exclusive')
    else
      res.Add('Shared');
    if assigned(Obj) then
    begin
      Res.Add(Obj.BoldClassTypeInfo.ExpressionName);
      res.Add(Obj.AsString);
    end;
    LockInfo.Add(Res.CommaText);
    Res.Free;
  end;

  procedure TraverseList(List: TBoldLockList; Exclusive: Boolean);
  var
    Traverser: TBoldIndexTraverser;
  begin
    Traverser := List.CreateTraverser;
    while Traverser.MoveNext do
      AddLockInfo(Traverser.item as TBoldLock, Exclusive);
    Traverser.Free;
  end;

begin
  TraverseList(LockHolder.HeldExclusive, true);
  TraverseList(LockHolder.HeldShared, false);
  LockInfo.CustomSort(LockListSort);
end;

class function TBoldLockUtils.GetObjectFromLock(Lock: TBoldLock; System: TBoldSystem): TBoldObject;
var
  Id: string;
begin
  result := nil;
  if pos('.', Lock.Name) <> 0 then
  begin
    id := copy(Lock.Name, 1, pos('.', Lock.Name)-1);
    if StrToIntDef(id, -1) <> -1 then
      result := BoldRetrieveObjectByIdString(System, Id);
  end;
end;

class procedure TBoldLockUtils.LockInfoToStringGrid(StringGrid: TStringGrid; System: TBoldSystem; LockHolder: TBoldAbstractLockHolder);
  procedure InitCol(Col, MinWidth: integer; Caption: String);
  begin
    if StringGrid.ColWidths[Col] < MinWidth then
      StringGrid.ColWidths[Col] := MinWidth;
    StringGrid.Cells[Col, 0] := Caption;
  end;

var
  LockInfo: TStringList;
  i: integer;
begin
  LockInfo := TStringList.Create;
  TBoldLockUtils.GetLockInfo(System, LockHolder, LockInfo);
  StringGrid.RowCount := MaxIntValue([LockInfo.Count+1, 2]);
  StringGrid.FixedRows := 1;
  StringGrid.ColCount := 4;
  InitCol(0, 100, 'Lock');
  InitCol(1,  60, 'Type');
  InitCol(2, 120, 'Class');
  InitCol(3, 200, 'Object');
  StringGrid.Rows[1].Text := '';

  for i := 0 to LockInfo.Count-1 do
    StringGrid.Rows[i+1].CommaText := LockInfo[i];
  LockInfo.Free;
end;

end.
