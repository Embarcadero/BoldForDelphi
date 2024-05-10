
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractExternalPersistenceHandle;

interface

uses
  Classes,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldPersistenceHandlePTWithModel,
  BoldAbstractExternalPersistenceController;

type
  TBoldAbstractExternalPersistenceHandle = class(TBoldPersistenceHandlePassthroughWithModel)
  private
    FOnStartUpdates: TNotifyEvent;
    FOnEndUpdates: TNotifyEvent;
    fOnFailUpdates: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeActivate: TNotifyEvent;
    FUpdateBoldDatabaseFirst: boolean;
    function GetPersistenceController: TBoldAbstractExternalPersistenceController;
  protected
    procedure SetActive(Value: Boolean); override;
    property OnStartUpdates: TNotifyEvent read FOnStartUpdates  write FOnStartUpdates;
    property OnEndUpdates: TNotifyEvent read FOnEndUpdates write FOnEndUpdates;
    property OnFailUpdates: TNotifyEvent read FOnFailUpdates write FOnFailUpdates;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeActivate: TNotifyEvent read FOnDeActivate write FOnDeActivate;
  public
    function KeyForObject(ObjectContents: IBoldObjectContents): IBoldValue;
    function KeyStringForObject(ObjectContents: IBoldObjectContents): String;
    function KeyIntForObject(ObjectContents: IBoldObjectContents): Integer;
    function ValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): IBoldValue;
    function StringValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): String;
    function DateValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): TDateTime;
    function CurrencyValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): Currency;
    function IntValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): Integer;
    function GetReferredObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string; ValueSpace: IBoldValueSpace): IBoldObjectContents;
    property PersistenceController: TBoldAbstractExternalPersistenceController read GetPersistenceController;
    property UpdateBoldDatabaseFirst: boolean read FUpdateBoldDatabaseFirst write FUpdateBoldDatabaseFirst default false;
  end;

implementation

uses
  BoldCoreConsts,
  BoldDefs;

{ TBoldAbstractExternalPersistenceHandle }

function TBoldAbstractExternalPersistenceHandle.CurrencyValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): Currency;
var
  Value: IBoldValue;
  CurrValue: IBoldCurrencyContent;
begin
  Value := ValueForObject(ObjectContents, MemberExpressionName);
  if Value.QueryInterface(IBoldCurrencyContent, Currvalue) = S_OK then
    result := Currvalue.asCurrency
  else
    raise EBold.createFmt(sValueNotCurrency, [classname, MemberExpressionName]);
end;

function TBoldAbstractExternalPersistenceHandle.DateValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): TDateTime;
var
  Value: IBoldValue;
  Datevalue: IBoldDateContent;
begin
  Value := ValueForObject(ObjectContents, MemberExpressionName);
  if Value.QueryInterface(IBoldDateContent, Datevalue) = S_OK then
    result := Datevalue.asDate
  else
    raise EBold.createFmt(sValueNotDate, [classname, MemberExpressionName]);
end;

function TBoldAbstractExternalPersistenceHandle.GetPersistenceController: TBoldAbstractExternalPersistenceController;
begin
  result := (inherited PersistenceController) as TBoldAbstractExternalPersistenceController;
end;

function TBoldAbstractExternalPersistenceHandle.GetReferredObject(
  ObjectContents: IBoldObjectContents; MemberExpressionName: string;
  ValueSpace: IBoldValueSpace): IBoldObjectContents;
var
  Value: IBoldValue;
  IdRef: IBoldObjectIdRef;
begin
  Value := ValueForObject(ObjectContents, MemberExpressionName);
  if Value.QueryInterface(IBoldObjectIdRef, IdRef) = S_OK then
  begin
    if assigned(IdRef.Id) then
      result := ValueSpace.ObjectContentsByObjectId[IdRef.Id]
    else
      result := nil;
  end;
end;

function TBoldAbstractExternalPersistenceHandle.IntValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): Integer;
var
  Value: IBoldValue;
  Intvalue: IBoldIntegerContent;
begin
  Value := ValueForObject(ObjectContents, MemberExpressionName);
  if Value.QueryInterface(IBoldIntegerContent, Intvalue) = S_OK then
    result := Intvalue.asInteger
  else
    raise EBold.createFmt(sValueNotInteger, [classname, MemberExpressionName]);
end;

function TBoldAbstractExternalPersistenceHandle.KeyForObject(ObjectContents: IBoldObjectContents): IBoldValue;
begin
  result := PersistenceController.KeyForObject(ObjectContents);
end;

function TBoldAbstractExternalPersistenceHandle.KeyIntForObject(
  ObjectContents: IBoldObjectContents): Integer;
var
  Value: IBoldValue;
  Intvalue: IBoldIntegerContent;
begin
  Value := KeyForObject(ObjectContents);
  if Value.QueryInterface(IBoldIntegerContent, Intvalue) = S_OK then
    result := Intvalue.asInteger
  else
    raise EBold.createFmt(sKeyNotInteger, [classname]);
end;

function TBoldAbstractExternalPersistenceHandle.KeyStringForObject(
  ObjectContents: IBoldObjectContents): String;
var
  Value: IBoldValue;
  StrValue: IBoldStringContent;
begin
  Value := KeyForObject(ObjectContents);
  if Value.QueryInterface(IBoldStringContent, Strvalue) = S_OK then
    result := (Value as IBoldStringContent).asString
  else
    raise EBold.createFmt(sKeyNotString, [classname]);
end;

function TBoldAbstractExternalPersistenceHandle.StringValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): String;
var
  Value: IBoldValue;
  Strvalue: IBoldStringContent;
begin
  result := '';
  Value := ValueForObject(ObjectContents, MemberExpressionName);
  if Value.QueryInterface(IBoldStringContent, StrValue) = S_OK then
    result := StrValue.asString
  else
    raise EBold.createFmt(sValueNotString, [classname, MemberExpressionName]);
end;

function TBoldAbstractExternalPersistenceHandle.ValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): IBoldValue;
begin
  result := PersistenceController.ValueForObject(objectContents, MemberExpressionName);
end;

procedure TBoldAbstractExternalPersistenceHandle.SetActive(Value: Boolean);
begin
  if Value then
  begin
    if assigned(OnActivate) then
      OnActivate(self);
    inherited SetActive(Value);
  end
  else
  begin
    inherited SetActive(Value);
    if assigned(OnDeActivate) then
      OnDeActivate(self);
  end;
end;

end.
