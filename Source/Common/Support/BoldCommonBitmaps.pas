
{ Global compiler directives }
{$include bold.inc}
unit BoldCommonBitmaps;

interface

uses
  Graphics;

var
  bmpBoldGridSelected: TBitmap = nil;
  bmpBoldGridCurrent: TBitmap = nil;
  bmpBoldGridConstraint_true: TBitmap = nil;
  bmpBoldGridConstraint_false: TBitmap = nil;

  bmpBoldNavigatorDelete: TBitMap = nil;
  bmpBoldNavigatorFirst: TBitMap = nil;
  bmpBoldNavigatorLast: TBitMap = nil;
  bmpBoldNavigatorInsert: TBitMap = nil;
  bmpBoldNavigatorNext: TBitMap = nil;
  bmpBoldNavigatorPrior: TBitMap = nil;
  bmpBoldNavigatorMoveUp: TBitMap = nil;
  bmpBoldNavigatorMoveDown: TBitMap = nil;

  bmpBOLDEDIT_ELLIPSIS: TBitMap = nil;

implementation

{$R *.res}

uses
  SysUtils;

function NewBitMap(Name: String; Transparent: Boolean): TBitMap;
begin
  result := TBitMap.Create;
  result.LoadFromResourceName(HInstance, Name);
  result.Transparent := Transparent;
end;

procedure LoadBitmapsFromResources;
begin
  bmpBoldGridSelected  := NewBitMap('BoldGrid_SELECTED', True); // do not localize
  bmpBoldGridCurrent  := NewBitMap('BoldGrid_CURRENT', True); // do not localize
  bmpBoldGridConstraint_true  := NewBitMap('BoldGrid_Constraint_True', False); // do not localize
  bmpBoldGridConstraint_false  := NewBitMap('BoldGrid_Constraint_False', False); // do not localize

  bmpBoldNavigatorDelete := NewBitMap('BOLDNAV_DELETE', true); // do not localize
  bmpBoldNavigatorFirst := NewBitMap('BOLDNAV_FIRST', true); // do not localize
  bmpBoldNavigatorLast := NewBitMap('BOLDNAV_LAST', true); // do not localize
  bmpBoldNavigatorInsert := NewBitMap('BOLDNAV_INSERT', true); // do not localize
  bmpBoldNavigatorNext := NewBitMap('BOLDNAV_NEXT', true); // do not localize
  bmpBoldNavigatorPrior := NewBitMap('BOLDNAV_PRIOR', true); // do not localize
  bmpBoldNavigatorMoveUp := NewBitMap('BOLDNAV_MOVEUP', true); // do not localize
  bmpBoldNavigatorMoveDown := NewBitMap('BOLDNAV_MOVEDOWN', true); // do not localize

  bmpBOLDEDIT_ELLIPSIS := NewBitMap('BoldEdit_Ellipsis', true); // do not localize
end;

procedure FreeAllBitMaps;
begin
  FreeAndNil(bmpBoldGridSelected);
  FreeAndNil(bmpBoldGridCurrent);
  FreeAndNil(bmpBoldGridConstraint_true);
  FreeAndNil(bmpBoldGridConstraint_false);

  FreeAndNil(bmpBoldNavigatorDelete);
  FreeAndNil(bmpBoldNavigatorFirst);
  FreeAndNil(bmpBoldNavigatorLast);
  FreeAndNil(bmpBoldNavigatorInsert);
  FreeAndNil(bmpBoldNavigatorNext);
  FreeAndNil(bmpBoldNavigatorPrior);
  FreeAndNil(bmpBoldNavigatorMoveUp);
  FreeAndNil(bmpBoldNavigatorMoveDown);

  FreeAndNil(bmpBOLDEDIT_ELLIPSIS);
end;

initialization
  LoadBitmapsFromResources;

finalization
  FreeAllBitmaps;

end.


