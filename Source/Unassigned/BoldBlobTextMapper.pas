unit BoldBlobTextMapper;

interface

uses
  Classes,
  DB,
  BoldDefs,
  BoldDBInterfaces,
  BoldId,
  BoldPMapperLists,
  BoldPMappers,
  BoldPMappersAttributeDefault,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces;

// To use this mapper, add the following to your TypeNameDictionary:
// ModelName = 'Text'
// ExpressionName = 'Text'
// DelphiName = 'TBABlob'
// ContentsName = 'Blob'
// MapperName = 'TBoldTextBlobMapper'
// Accessor = 'AsString'
// NativeType = 'String'
// UnitName = 'BoldAttributes'
// ComType = 'WideString'
// IDLType = 'BSTR'


type
  TBoldBlobTextMapper = class(TBoldPMBlob)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
  public
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldBlobTextMapper }

function TBoldBlobTextMapper.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := ftMemo;
end;

function TBoldBlobTextMapper.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := 'TEXT';
end;

procedure TBoldBlobTextMapper.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.DataType := GetColumnBDEFieldType(ColumnIndex);
  inherited;
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldBlobTextMapper, alConcrete);
finalization
  if BoldMemberPersistenceMappersAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldBlobTextMapper);
end.
