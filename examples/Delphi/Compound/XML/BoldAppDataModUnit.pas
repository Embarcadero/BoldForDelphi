unit BoldAppDataModUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldHandles, BoldSubscription, BoldSystemHandle, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldModel, BoldTypeNameHandle, Budgetclasses, BoldElements,
  BoldSortedHandle, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, BoldAbstractModel, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TdmMain = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
    blhAllBudget: TBoldListHandle;
    blhAllColumn: TBoldListHandle;
    bcColumn: TBoldComparer;
    blhAllRow: TBoldListHandle;
    bcRow: TBoldComparer;
    blhBudgetCells: TBoldListHandle;
    bcCells: TBoldComparer;
    blhAllCell: TBoldListHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    function bcColumnCompare(Item1, Item2: TBoldElement): Integer;
    function bcRowCompare(Item1, Item2: TBoldElement): Integer;
    function bcCellsCompare(Item1, Item2: TBoldElement): Integer;
    procedure IBDatabase1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    function getBudgetByName(const BudgetName: string): TBudget;
    function GetElementById(const Id: string): TBoldElement;
    function GetIdForElement(Element: TBoldElement): string;
  end;

var
  dmMain: TdmMain;

implementation
uses
  BoldSystem,
  BoldID,
  BoldDefaultID,
  BoldUtils;
{$R *.DFM}

function TdmMain.getBudgetByName(const BudgetName: string): TBudget;
var
  i: integer;
begin
  Result := nil;
  blhAllBudget.First;
  for i:= 0 to blhAllbudget.Count - 1 do
    if ((blhAllBudget.List[i] as TBudget).aName = BudgetName) then
    begin
      Result := (blhAllBudget.List[i] as TBudget);
      Break;
    end
end;

function TdmMain.bcColumnCompare(Item1, Item2: TBoldElement): Integer;
var
  aCol1, aCol2: TCol;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aCol1 := Item1 as TCol;
    aCol2 := Item2 as TCol;
    if (ACol1.aNumber = aCol2.aNumber) then
      Result := 0
    else if (ACol1.aNumber > aCol2.aNumber) then
      Result := 1
    else
      Result := -1;
  end;
end;

function TdmMain.bcRowCompare(Item1, Item2: TBoldElement): Integer;
var
  aRow1, aRow2: TRow;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aRow1 := Item1 as TRow;
    aRow2 := Item2 as TRow;
    if (ARow1.aNumber = aRow2.aNumber) then
      Result := 0
    else if (ARow1.aNumber > aRow2.aNumber) then
      Result := 1
    else
      Result := -1;
  end;
end;

function TdmMain.bcCellsCompare(Item1, Item2: TBoldElement): Integer;
var
  acell1, acell2: TACell;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aCell1 := Item1 as TACell;
    aCell2 := Item2 as TACell;
    if (aCell1.Row.aNumber < aCell2.Row.aNumber) then
      Result := -1
    else if (aCell1.Row.aNumber > aCell2.Row.aNumber) then
      Result := 1
    else if (aCell1.Col.aNumber < aCell2.Col.aNumber) then
      Result := -1
    else if (aCell1.Col.aNumber > aCell2.Col.aNumber) then
      Result := 1;
  end;
end;

function TdmMain.GetElementById(const Id: string): TBoldElement;
var
  ObjectId: string;
  AttrIndex: integer;
  p: integer;
  defaultId: TBoldDefaultID;
  obj: TBoldObject;
begin
  p:= Pos(':', Id);
  if (p = 0) then
  begin
    Result := nil;
    exit;
  end;
  ObjectId := Copy(id, 1, p-1);
  AttrIndex := StrToInt(Copy(id, p+1, length(id) - p));
  defaultID := TBoldDefaultId.CreateWithClassID(0, false);
  defaultID := TBoldDefaultId.CreateWithClassID(0, false);
  try
    defaultID.AsInteger := StrToInt(ObjectId);
    obj := BoldSystemHandle1.System.EnsuredLocatorByID[defaultId].EnsuredBoldObject;
    Result := obj.BoldMembers[AttrIndex];
  finally
    FreeAndNIl(defaultID);
  end;
end;

function TdmMain.GetIdForElement(Element: TBoldElement): string;
var
  attr: TBoldAttribute;
  obj: TBoldObject;
begin
  if (Element is TBoldAttribute) then
  begin
    attr := Element as TBoldAttribute;
    Result := Format('%s:%d', [attr.OwningObject.BoldObjectLocator.BoldObjectID.AsString, attr.BoldAttributeRTInfo.Index]);
  end
  else if (Element is TBoldObject) then
  begin
    obj := Element as TBoldObject;
    Result := Format('%s', [obj.BoldObjectLocator.BoldObjectID.AsString]);
  end
end;

procedure TdmMain.IBDatabase1BeforeConnect(Sender: TObject);
begin
  IBDatabase1.DatabaseName := 'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(IBDatabase1.DatabaseName);
end;

end.
