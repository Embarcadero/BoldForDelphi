
{ Global compiler directives }
{$include bold.inc}
unit BoldPMWideString;

interface

uses
  { RTL / VCL }
  DB,

  { Bold }
  BoldMeta,
  BoldId,
  BoldValueSpaceInterfaces,
  BoldDbInterfaces,
  BoldPMappers,
  BoldTypeNameDictionary,
  BoldPMappersDefault;

type
  { The following class implements the persistence mapper for the WideString }
  { type.  Allowing Bold to pass field values and parameters between the }
  { persistence store and the value space }
  TBoldPMWideString = class(TBoldSingleColumnMember)
  private
    fColumnSize: integer;
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

{$J+}
const
  SNationalTextSQLType = 'national character varying(%d)';

implementation

uses
  Variants,
  SysUtils,
  BoldDefs,
  BoldValueInterfaces,
  BoldPMapperLists,
  { Bold Unicode Support }
  BoldWideStringInterface;

{******************************************************************************}
{* TBoldPMUnicodeString                                                       *}
{******************************************************************************}


constructor TBoldPMWideString.CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fColumnSize := (Moldmember as TMoldAttribute).Length;
  inherited;
end;

class function TBoldPMWideString.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_WideString) = 0;
end;

function TBoldPMWideString.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aWideString: IBoldWideStringContent;
begin
  EnsureFirstColumn(ColumnIndex);

  if GetValue(ObjectContent).QueryInterface(IBoldWideStringContent, aWideString) = S_OK then
  begin
    if not CheckEitherNull(Field, aWideString, Result) then
    begin
      if TVarData(Field.Value).VType = varOleStr then
        Result := WideString(Field.Value) = aWideString.asWideString
      else
        Result := Field.AsString = (GetValue(ObjectContent) as IBoldStringContent).asString;
    end;
  end;
end;

function TBoldPMWideString.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftWideString;
end;

function TBoldPMWideString.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := Format(SNationalTextSQLType, [GetColumnSize(ColumnIndex)]);
end;

procedure TBoldPMWideString.ValueFromField(
  OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
var
  aWideString: IBoldWideStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aWideString := GetEnsuredValue(ObjectContent) as IBoldWideStringContent;
  if Field.IsNull then
    aWideString.SetContentToNull
  else
  begin
    if Field.Field is TWideStringField then
      aWideString.AsWideString := (Field.Field as TWideStringField).Value
    else
      aWideString.AsWideString := VarAsType(Field.Value, varOleStr);
  end;
end;

procedure TBoldPMWideString.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aWideString: IBoldWideStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aWideString := GetEnsuredValue(ObjectContent) as IBoldWideStringContent;
  if aWideString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.asVariant := aWideString.asWideString
end;

function TBoldPMWideString.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  Result := fColumnSize;
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMWideString, alConcrete);

finalization
  if BoldMemberPersistenceMappersAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMWideString);
end.
