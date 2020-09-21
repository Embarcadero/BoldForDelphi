unit DMCore;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  DB,
  stdctrls,
  Graphics,
  BoldSystem,
  BoldSystemRT,
  BoldElements,
  BoldAbstractModel,
  BoldModel,
  BoldSubscription,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldStringControlPack,
  BoldTypeNameHandle,
  BoldBDEInterfaces,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TDMSystem = class(TDataModule)
    BoldModel1: TBoldModel;
    IsRichFilter: TBoldFilter;
    SystemHandle: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    NameComparer: TBoldComparer;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure IsRichFilterSubscribe(element: TBoldElement;
      subscriber: TBoldSubscriber);
    function NameComparerCompare(Item1, Item2: TBoldElement): Integer;
    procedure NameComparerSubscribe(Element: TBoldElement;
      Subscriber: TBoldSubscriber);
    function IsRichFilterFilter(Element: TBoldElement): Boolean;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMSystem: TDMSystem;

implementation

uses BuildingsAndOwners;

{$R *.DFM}

function TDMSystem.IsRichFilterFilter(Element: TBoldElement): Boolean;
begin
  result := false;
  if Element is TPerson then
    result := (Element as TPerson).Assets > 10000;
end;

procedure TDMSystem.IsRichFilterSubscribe(element: TBoldElement;
  subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

function TDMSystem.NameComparerCompare(Item1,
  Item2: TBoldElement): Integer;
begin
  Result := 0;
  if (item1 is TPerson) and (item2 is TPerson) then
    result := AnsiCompareText(
      (item1 as TPerson).LastName,
      (item2 as TPerson).Lastname);
end;

procedure TDMSystem.NameComparerSubscribe(Element: TBoldElement;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('lastName', Subscriber, False);
end;


procedure TDMSystem.DataModuleCreate(Sender: TObject);
begin
  IBDatabase1.DatabaseName := ExtractFilePath(ParamStr(0)) + IBDatabase1.DatabaseName;

end;

end.
