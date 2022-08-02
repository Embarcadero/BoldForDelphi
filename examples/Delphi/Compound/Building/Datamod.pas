unit datamod;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  stdctrls,
  Graphics,
  BoldSystem,
  BoldSystemRT,
  BoldAbstractModel,
  BoldModel,
  BoldSubscription,
  BoldElements,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldTypeNameHandle,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldStringControlPack,
  BoldActions,
  BoldreferenceHandle,
  ActnList,
  BoldHandleAction, BoldAFPPluggable, BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  BoldDatabaseAdapterFireDAC, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.DApt, BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM, BoldPersistenceHandlePassthrough,
  BoldPersistenceHandlePTWithModel, BoldSnooperHandle, BoldAbstractDequeuer,
  BoldExternalObjectSpaceEventHandler, BoldConstraintValidator, FireDAC.Phys.PG,
  FireDAC.Phys.PGDef;

type
  TDataModule1 = class(TDataModule)
    BoldModel1: TBoldModel;
    IsRichRenderer: TBoldAsCheckBoxStateRenderer;
    IsRichFilter: TBoldFilter;
    NameComparer: TBoldComparer;
    NegativeRedRenderer: TBoldAsStringRenderer;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterPostgres: TBoldDatabaseAdapterFireDAC;
    FDConnectionSQLServer: TFDConnection;
    BoldConstraintValidatorOnModify: TBoldConstraintValidator;
    BoldConstraintValidatorOnUpdate: TBoldConstraintValidator;
    FDConnectionPostgres: TFDConnection;
    BoldDatabaseAdapterSQLServer: TBoldDatabaseAdapterFireDAC;
    function NameComparerCompare(item1, item2: TBoldElement): Integer;
    procedure NameComparerSubscribe(boldElement: TBoldElement; subscriber: TBoldSubscriber);
    function IsRichFilterFilter(element: TBoldElement): Boolean;
    procedure IsRichFilterSubscribe(element: TBoldElement; subscriber: TBoldSubscriber);
    procedure BoldPlaceableAFP1GetFormClass(Element: TBoldElement; var Result: TFormClass);
    procedure BoldPlaceableAFP1RetrieveHandle(Form: TForm;
      var Result: TBoldReferenceHandle);
    function IsRichRendererGetAsCheckBoxState(
      aFollower: TBoldFollower): TCheckBoxState;
    procedure IsRichRendererSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererSetFont(aFollower: TBoldFollower;
      AFont: TFont);
    function IsRichRendererMayModify(aFollower: TBoldFollower): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

uses
  BuildingClasses, PersonAutoFormUnit;

{$R *.DFM}


function TDataModule1.NameComparerCompare(item1, item2: TBoldElement): Integer;
begin
  Result := AnsiCompareText((Item1 as TPerson).LastName, (Item2 as TPerson).LastName);
  if Result = 0 then
    Result := AnsiCompareText((Item1 as TPerson).FirstName, (Item2 as TPerson).FirstName);
end;

procedure TDataModule1.NameComparerSubscribe(boldElement: TBoldElement; subscriber: TBoldSubscriber);
begin
  boldElement.SubscribeToExpression('firstName', Subscriber, false);
  boldElement.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichFilterFilter(element: TBoldElement): Boolean;
begin
  Result := False;
  if Element is TPerson then
    Result := TPerson(element).Assets > 10000;
end;

procedure TDataModule1.NegativeRedRendererSetFont(aFollower: TBoldFollower;
  AFont: TFont);
begin
  if aFollower.Element is TPerson then
  begin
    if TPerson(aFollower.Element).Assets < 0 then
      aFont.Color :=  clRed
    else
      aFont.Color := clBlue;
  end;
end;

procedure TDataModule1.IsRichFilterSubscribe(element: TBoldElement; subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

function TDataModule1.IsRichRendererGetAsCheckBoxState(
  aFollower: TBoldFollower): TCheckBoxState;
begin
  result := cbGrayed;
  if aFollower.element is TPerson then
  begin
    if TPerson(aFollower.element).Assets > 10000 then
       Result := cbChecked
    else
       Result := cbUnChecked
  end;
end;

function TDataModule1.IsRichRendererMayModify(
  aFollower: TBoldFollower): Boolean;
begin
  result := false;
end;

procedure TDataModule1.IsRichRendererSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.BoldPlaceableAFP1GetFormClass(Element: TBoldElement; var Result: TFormClass);
begin
  if Element is TPerson then
    result := TPersonAutoForm
  else
    result := nil;
end;

procedure TDataModule1.BoldPlaceableAFP1RetrieveHandle(Form: TForm; var Result: TBoldReferenceHandle);
begin
  if form is TPersonAutoForm then
  begin
    result := TPersonAutoForm(form).brhPerson;
    TPersonAutoForm(form).BoldFormSaver1.SystemHandle := BoldSystemHandle1;
  end
  else
    result := nil;
end;

end.
