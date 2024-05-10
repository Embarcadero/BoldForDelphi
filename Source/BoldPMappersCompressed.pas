{ Global compiler directives }
{$include bold.inc}
unit BoldPMappersCompressed;

interface

uses
  BoldMemberTypeDictionary,
  BoldDefs,
  BoldPMappersAttributeDefault,
  BoldId,
  BoldValueSpaceInterfaces,
  BoldDBInterfaces;

type
  TBoldPMCompressedString = class(TBoldPMString)
  public
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  TBoldPMCompressedBlob = class(TBoldPMBlob)
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

implementation

// TODO: Check for Unicode. Strings probably should be changed to AnsiString, look at parent implementation

uses
  SysUtils,
  BoldValueInterfaces,
  BoldPMapperLists,
  BoldDefaultStreamNames,
  ZLibEx;

{ TBoldPMCompressedString }

procedure TBoldPMCompressedString.ValueFromField(OwningObjectId: TBoldObjectId;
  const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; const Field: IBoldField;
  ColumnIndex: Integer);
var
  s: TBoldAnsiString;
const
  cHeader = 'xœ';
  cMinCompressedLength = 8;
begin
  if Field.IsNull then
    inherited;
  EnsureFirstColumn(ColumnIndex);
  s := Field.AsBlob;
  if (s <> '') and (length(s) >= cMinCompressedLength) then
  try
    if copy(s,1,length(cHeader)) = cHeader then
    s := ZLibEx.ZDecompressStr(s);
  except
    on e:EZDecompressionError do;
    // we ignore Decompress errors, this can happen when reading existing uncompressed data
    else raise;
  end;
  (GetEnsuredValue(ObjectContent) as IBoldStringContent).AsString := string(s);
end;

procedure TBoldPMCompressedString.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  vValue: IBoldNullableValue;
  s: string;
begin
  EnsureFirstColumn(ColumnIndex);
  vValue := GetEnsuredValue(ObjectContent) as IBoldNullableValue;
  if vValue.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin
    Param.DataType := ColumnBDEFieldType[ColumnIndex];
    s :=  (vValue as IBoldStringContent).AsString;
// commented out compression when writing to db, needed for Postgres
//    if s <> '' then
//      s := ZLibEx.ZCompressStr(s);
    Param.AsString := s;
  end;
end;

{ TBoldPMCompressedBlob }

procedure TBoldPMCompressedBlob.ValueFromField(OwningObjectId: TBoldObjectId;
  const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; const Field: IBoldField;
  ColumnIndex: Integer);
var
  s: TBoldAnsiString;
begin
  if Field.IsNull then
    inherited;
  EnsureFirstColumn(ColumnIndex);
  s := Field.AsBlob;
  if s <> '' then
  try
    s := ZLibEx.ZDecompressStr(s);
  except
    on EZDecompressionError do
    // we ignore Decompress errors, this can happen when reading existing uncompressed data
    else
    raise;
  end;
  (GetEnsuredValue(ObjectContent) as IBoldBlobContent).AsBlob := s;
end;

procedure TBoldPMCompressedBlob.ValueToParam(const ObjectContent: IBoldObjectContents;
  const Param: IBoldParameter; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList);
var
  vValue: IBoldNullableValue;
  s: TBoldAnsiString;
begin
  EnsureFirstColumn(ColumnIndex);
  vValue := GetEnsuredValue(ObjectContent) as IBoldNullableValue;
  if vValue.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin
    Param.DataType := ColumnBDEFieldType[ColumnIndex];
    s := (vValue as IBoldBlobContent).AsBlob;
    if s <> '' then
      s := ZLibEx.ZCompressStr(s);
    Param.AsString := string(s);
  end;
end;


initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMCompressedString, alConcrete);
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMCompressedBlob, alConcrete);

finalization
  if BoldMemberPersistenceMappersAssigned and BoldMemberTypesAssigned then
  begin
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMCompressedString);
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMCompressedBlob);
  end;

end.
