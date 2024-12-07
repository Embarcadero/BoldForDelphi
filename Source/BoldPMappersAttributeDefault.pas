{ Global compiler directives }
{$include bold.inc}
unit BoldPMappersAttributeDefault;

interface

uses
  DB,
  BoldDBInterfaces,
  BoldMeta,
  BoldPMappers,
  BoldPMappersDefault,
  BoldId,
  BoldTypeNameDictionary,
  BoldValueSpaceInterfaces;

const
  NULLDATETIMEREPRESENTATION = 0.1;

type
  {forward declarations}
  TBoldPMString = class;
  TBoldPMInteger = class;
  TBoldPMSmallInt = class;
  TBoldPMByte = class;
  TBoldPMWord = class;
  TBoldPMShortInt = class;
  TBoldPMLogic = class;
  TBoldPMCurrency = class;
  TBoldPMDateTime = class;
  TBoldPMDate = class;
  TBoldPMTime = class;
  TBoldPMGuid = class;

  {TBoldPMString}
  TBoldPMString = class(TBoldSingleColumnMember)
  private
    fColumnSize: integer;
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMAnsiString}
  TBoldPMAnsiString = class(TBoldPMString)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
        ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList:
        TBoldIdTranslationList): Boolean; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  public
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent:
        IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList:
        TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param:
        IBoldParameter; ColumnIndex: Integer; TranslationList:
        TBoldIdTranslationList); override;
  end;

  {TBoldPMUnicodeString}
  TBoldPMUnicodeString = class(TBoldPMString)
  protected
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
        ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList:
        TBoldIdTranslationList): Boolean; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  public
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent:
        IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList:
        TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param:
        IBoldParameter; ColumnIndex: Integer; TranslationList:
        TBoldIdTranslationList); override;
  end;

  TBoldPMStringBDECodePageBug = class(TBoldPMString)
  public
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMText}
  TBoldPMText = class(TBoldSingleColumnMember)
  private
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMUnicodeText}
  TBoldPMUnicodeText = class(TBoldPMText)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  end;

  {TBoldPMInteger}
  TBoldPMInteger = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMSmallInt}
  TBoldPMSmallInt = class(TBoldPMInteger)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  {TBoldPMByte}
  TBoldPMByte = class(TBoldPMSmallInt)
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  {TBoldPMShortInt}
  TBoldPMShortInt = class(TBoldPMSmallInt)
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  {TBoldPMWord}
  TBoldPMWord = class(TBoldPMInteger)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  {TBoldPMLogic}
  TBoldPMLogic = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMFloat}
  TBoldPMFloat = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMCurrency}
  TBoldPMCurrency = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMBlob}
  TBoldPMBlob = class(TBoldSingleColumnMember)
  private
    function StoreAsString(ColumnIndex: Integer): Boolean;
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMBlob}
  TBoldPMMemoBlob = class(TBoldPMBlob)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  end;

  TBoldPMNonEmptyBlob = class(TBoldPMBlob)
  protected
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  TBoldPMLiveBlob = class(TBoldPMBlob)
  protected
    function GetRequiresLiveQuery: Boolean; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
  end;

  TBoldPMTypedBlob = class(TBoldPMBlob)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnCount: Integer; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function GetInitialColumnName(ColumnIndex: Integer): string; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  TBoldPMLiveTypedBlob = class(TBoldPMTypedBlob)
  protected
    function GetRequiresLiveQuery: Boolean; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
  end;


  {TBoldPMDateTime}
  TBoldPMDateTime = class(TBoldSingleColumnMember)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMDate}
  TBoldPMDate = class(TBoldPMDateTime)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  {TBoldPMTime}
  TBoldPMTime = class(TBoldPMDateTime)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  TBoldPMGuid = class(TBoldPMString)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  end;

implementation

uses
  SysUtils,
  Variants,

  BoldCoreConsts,
  BoldDefs,
  BoldUtils,
  BoldValueInterfaces,
  BoldPMapperLists,
  BoldDefaultStreamNames;

var
  TwoSeconds: TDateTime;


{---TBoldPMString---}
constructor TBoldPMString.CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fColumnSize := (Moldmember as TMoldAttribute).Length;
  inherited;
  if (DefaultDbValue <> '') and
     (DefaultDbValue[1] <> '''') and
     (DefaultDbValue[1] <> '"') then
    DefaultDbValue := '''' + DefaultDbValue + '''';
  if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
  begin
    if (MoldMember as TMoldAttribute).AllowNull then
      raise EBold.CreateFmt(sAttributeMustNotAllowNullIfEmptyStringsStoreAsNull, [
        MoldClass.Name, MoldMember.Name]);
    fAllowNull := True;
  end;
end;

function TBoldPMString.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.GetColumnTypeForString(GetColumnSize(ColumnIndex));
end;

class function TBoldPMString.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_String) = 0;
end;

function TBoldPMString.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  if SystemPersistenceMapper.SQLDataBaseConfig.TreatStringFieldAsUnicode then
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.IsSQLServerEngine then
    begin
      // Changed from ftWideString to ftWideMemo as MSSQL truncates
      // string params to 8000
      Result := ftWideMemo;
    end else
    begin
      Result := ftWideString
    end;
  end else
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.IsSQLServerEngine then
    begin
      // Changed from ftString to ftMemo as MSSQL truncates string params to 8000
      Result := ftMemo;
    end else
    begin
      Result := ftString;
    end;
  end;
end;

function TBoldPMString.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  Result := fColumnSize;
end;

function TBoldPMString.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if aString.IsNull then
    result := null
  else
  begin
    result := aString.AsString;
  end;
end;

procedure TBoldPMString.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if Field.IsNull then
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
      aString.AsString := ''
    else
      aString.SetContentToNull
  end
  else
    aString.AsString := Field.AsString
end;

function TBoldPMString.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetValue(ObjectContent) as IBoldStringContent;
  result :=
    SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL and
    Field.IsNull and (aString.AsString = '');

  if not result then
  begin
    if not CheckEitherNull(field, aString, result) then
      result := Field.AsString = aString.AsString;
  end;
end;


procedure TBoldPMString.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
const
{$IFDEF BOLD_UNICODE}
  // check for 4000 chars, because since using unicode
  // each char requires twice as much space
  cnMaxMSSQLStringLength = 4000;
{$ELSE}
  cnMaxMSSQLStringLength = 8000;
{$ENDIF}
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if aString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin
  // the setting of Param DataType is a workaround for UniDAC MSSQL param trim to 8000 bug.
    if Length(aString.AsString) >= cnMaxMSSQLStringLength then
      Param.DataType := GetColumnBDEFieldType(0);
    Param.AsString := aString.AsString;
  end;
end;

{---TBoldPMInteger---}
function TBoldPMInteger.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;

class function TBoldPMInteger.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Integer) = 0

end;

function TBoldPMInteger.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftInteger;
end;

function TBoldPMInteger.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  anInteger: IBoldIntegerContent;
begin
  EnsureFirstColumn(ColumnIndex);
  anInteger := GetValue(ObjectContent) as IBoldIntegerContent;
  if not CheckEitherNull(field, anInteger, result) then
    result := Field.AsInteger = anInteger.AsInteger;
end;

function TBoldPMInteger.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  anInteger: IBoldIntegerContent;
begin
  EnsureFirstColumn(ColumnIndex);
  anInteger := GetEnsuredValue(ObjectContent) as IBoldIntegerContent;
  if anInteger.IsNull then
    result := null
  else
    Result := anInteger.AsInteger;
end;

procedure TBoldPMInteger.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  anInteger: IBoldIntegerContent;
begin
  EnsureFirstColumn(ColumnIndex);
  anInteger := GetEnsuredValue(ObjectContent) as IBoldIntegerContent;
  if Field.IsNull then
    anInteger.SetContentToNull
  else
    anInteger.AsInteger := Field.AsInteger;
end;

procedure TBoldPMInteger.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  anInteger: IBoldIntegerContent;
begin
  EnsureFirstColumn(ColumnIndex);
  anInteger := GetEnsuredValue(ObjectContent) as IBoldIntegerContent;
  if anInteger.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsInteger := anInteger.AsInteger;
end;

{---TBoldPMSmallInt---}
function TBoldPMSmallInt.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForSmallInt;
end;

Class function TBoldPMSmallInt.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Integer) = 0;
end;

function TBoldPMSmallInt.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftSmallInt;
end;

{---TBoldPMShortInt---}
class function TBoldPMShortInt.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Integer) = 0;
end;

{---TBoldPMWord---}
class function TBoldPMWord.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Integer) = 0;
end;

{---TBoldPMByte---}
class function TBoldPMByte.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Integer)= 0;
end;

function TBoldPMWord.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftWord;
end;

{---TBoldPMLogic---}
function TBoldPMLogic.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := 'VARCHAR(1)';
end;

class function TBoldPMLogic.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Boolean) = 0;
end;

function TBoldPMLogic.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftBoolean;
end;

function TBoldPMLogic.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBoolean: IBoldBooleanContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBoolean := GetValue(ObjectContent) as IBoldBooleanContent;
  if not CheckEitherNull(field, aBoolean, result) then
    result := Field.AsBoolean = aBoolean.AsBoolean;
end;

function TBoldPMLogic.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aBoolean: IBoldBooleanContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBoolean := GetEnsuredValue(ObjectContent) as IBoldBooleanContent;
  if aBoolean.IsNull then
    result := Null
  else
    result := aBoolean.asBoolean;
end;

procedure TBoldPMLogic.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aBoolean: IBoldBooleanContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBoolean := GetEnsuredValue(ObjectContent) as IBoldBooleanContent;
  if Field.IsNull then
    aBoolean.SetContentToNull
  else
    aBoolean.AsBoolean := Field.AsBoolean;
end;

procedure TBoldPMLogic.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aBoolean: IBoldBooleanContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBoolean := GetEnsuredValue(ObjectContent) as IBoldBooleanContent;
  if aBoolean.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsBoolean := aBoolean.asBoolean;
end;

{---TBoldPMFloat---}
function TBoldPMFloat.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForFloat;
end;

class function TBoldPMFloat.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Float) = 0;
end;

function TBoldPMFloat.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftFloat;
end;

function TBoldPMFloat.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aFloat: IBoldFloatContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aFloat := GetValue(ObjectContent) as IBoldFloatContent;
  if not CheckEitherNull(field, aFloat, result) then
    result := Field.AsFloat = aFloat.AsFloat;
end;

function TBoldPMFloat.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aFloat: IBoldFloatContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aFloat := GetEnsuredValue(ObjectContent) as IBoldFloatContent;
  if aFloat.IsNull then
    result := Null
  else
    result := aFloat.AsFloat;
end;

procedure TBoldPMFloat.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aFloat: IBoldFloatContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aFloat := GetEnsuredValue(ObjectContent) as IBoldFloatContent;
  if Field.IsNull then
    aFloat.SetContentToNull
  else
    aFloat.AsFloat := Field.AsFloat;
end;

procedure TBoldPMFloat.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aFloat: IBoldFloatContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aFloat := GetEnsuredValue(ObjectContent) as IBoldFloatContent;
  if aFloat.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsFloat := aFloat.AsFloat;
end;

{---TBoldPMCurrency---}
class function TBoldPMCurrency.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Currency) = 0;
end;

function TBoldPMCurrency.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftCurrency;
end;

function TBoldPMCurrency.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aCurrency: IBoldCurrencyContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aCurrency := GetValue(ObjectContent) as IBoldCurrencyContent;
  if not CheckEitherNull(field, aCurrency, result) then
    result := Field.AsCurrency = aCurrency.AsCurrency;
end;


function TBoldPMCurrency.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aCurrency: IBoldCurrencyContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aCurrency := GetEnsuredValue(ObjectContent) as IBoldCurrencyContent;
  if aCurrency.IsNull then
    result := null
  else
    result := aCurrency.AsCurrency;
end;

procedure TBoldPMCurrency.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aCurrency: IBoldCurrencyContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aCurrency := GetEnsuredValue(ObjectContent) as IBoldCurrencyContent;
  if Field.IsNull then
    aCurrency.SetContentToNull
  else
    aCurrency.AsCurrency := Field.AsCurrency;
end;

procedure TBoldPMCurrency.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aCurrency: IBoldCurrencyContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aCurrency := GetEnsuredValue(ObjectContent) as IBoldCurrencyContent;
  if aCurrency.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsCurrency := aCurrency.AsCurrency;
end;

{---TBoldPMBlob---}
function TBoldPMBlob.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForBlob;
end;

function TBoldPMBlob.StoreAsString(ColumnIndex: Integer): Boolean;
begin
   Result := GetColumnBDEFieldType(ColumnIndex) = ftstring;
end;

class function TBoldPMBlob.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Blob) = 0;
end;

function TBoldPMBlob.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.FieldTypeForBlob;
end;

function TBoldPMBlob.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetValue(ObjectContent) as IBoldBlobContent;
  if not CheckEitherNull(field, aBlob, result) then
  begin
    if StoreAsString(ColumnIndex) then
      result := aBlob.AsString = Field.AsString
    else
      result := aBlob.AsBlob = Field.AsBlob;
  end;
end;

procedure TBoldPMBlob.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aBlob: IBoldBlobContent;
  aBlobStreamContent: IBoldBlobStreamContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  aBlobStreamContent := aBlob as IBoldBlobStreamContent;
  aBlobStreamContent.BeginSupressEvents;
  try
    if Field.IsNull then
      aBlob.SetContentToNull
    else
    begin
      if StoreAsString(ColumnIndex) then
        aBlob.AsBlob :=  Field.AsAnsiString
      else
        aBlob.AsBlob :=  Field.AsBlob;
    end;
  finally
    aBlobStreamContent.EndSupressEvents;
  end;
end;

procedure TBoldPMBlob.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if aBlob.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin
    if StoreAsString(ColumnIndex) then
      Param.asString := aBlob.asString
    else
      Param.AsBlob :=  aBlob.asBlob;
  end;
end;

function TBoldPMBlob.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if aBlob.IsNull then
    result := Null
  else
  begin
    result := aBlob.asBlob;
  end;
end;

function TBoldPMTypedBlob.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  if ColumnIndex = 1 then
    result := ftString
  else
    result := inherited GetColumnBDEFieldType(ColumnIndex);
end;

function TBoldPMTypedBlob.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  if ColumnIndex = 1 then
    result := format(SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForString, [63])
  else
    result := inherited GetColumnTypeAsSQL(ColumnIndex);
end;

function TBoldPMTypedBlob.GetColumnCount: Integer;
begin
  result := 2;
end;

function TBoldPMTypedBlob.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  if ColumnIndex = 1 then
    Result := 63
  else
    Result := inherited GetColumnSize(ColumnIndex);
end;

function TBoldPMTypedBlob.GetInitialColumnName(ColumnIndex: Integer): string;
begin
  if ColumnIndex = 1 then
    Result := InitialColumnRootName + '_Content'
  else
    Result := inherited GetInitialColumnName(ColumnIndex);
end;

class function TBoldPMTypedBlob.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_TypedBlob) = 0;
end;

function TBoldPMTypedBlob.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aTypedBlob: IBoldTypedBlob;
begin
  if ColumnIndex = 1 then
  begin
    aTypedBlob := GetValue(ObjectContent) as IBoldTypedBlob;
    if not CheckEitherNull(field, aTypedBlob, result) then
      result := Field.Value = aTypedBlob.ContentTypeContent
  end
  else
    result := inherited CompareField(ObjectContent, Field, ColumnIndex, ValueSpace, TranslationList);
end;

function TBoldPMTypedBlob.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aTypedBlob: IBoldTypedBlob;
begin
  if ColumnIndex = 1 then
  begin
    aTypedBlob := GetEnsuredValue(ObjectContent) as IBoldTypedBlob;
    if aTypedBlob.IsNull then
      result := null
    else
      result := aTypedBlob.ContentTypeContent;
  end
  else
    result := Inherited ValueAsVariant(ObjectContent, ColumnIndex, TranslationList);
end;

procedure TBoldPMTypedBlob.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aTypedBlob: IBoldTypedBlob;
begin
  if ColumnIndex = 1 then
  begin
    aTypedBlob := GetEnsuredValue(ObjectContent) as IBoldTypedBlob;
    if field.IsNull then
      aTypedBlob.SetContentToNull
    else
      aTypedBlob.ContentTypeContent := Field.AsString
  end
  else
    Inherited;
end;

procedure TBoldPMTypedBlob.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aTypedBlob: IBoldTypedBlob;
begin
  if ColumnIndex = 1 then
  begin
    aTypedBlob := GetEnsuredValue(ObjectContent) as IBoldTypedBlob;
    if aTypedBlob.isNull then
      SetParamToNullWithDataType(Param, GetColumnBDEFieldType(ColumnIndex))
    else
      Param.AsString := aTypedBlob.ContentTypeContent
  end
  else
    Inherited;
end;

{---TBoldPMDateTime---}
function TBoldPMDateTime.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForDateTime;
end;

class function TBoldPMDateTime.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_DateTime) = 0;
end;

function TBoldPMDateTime.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftDateTime; // workaround for DBX is to use ftDate, DBX does not properly support DateTime
end;

function TBoldPMDateTime.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetValue(ObjectContent) as IBoldDateTimeContent;
  if Field.IsNull {$IFDEF ConvertZeroDateToDateNil} or (Field.AsDateTime = 0) or(abs(Field.AsDateTime - NULLDATETIMEREPRESENTATION) < TwoSeconds) {$ENDIF} then
    result := (aDateTime as IBoldNullableValue).isNull
  else if (aDateTime as IBoldNullableValue).IsNull then
    result := false
  else
    result := abs(Field.AsDateTime - aDateTime.AsDateTime) < TwoSeconds;
end;

function TBoldPMDateTime.DefaultDefaultDbValue: String;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.DefaultValueForDateTime;
end;

function TBoldPMDateTime.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetEnsuredValue(ObjectContent) as IBoldDateTimeContent;
  if aDateTime.IsNull then
{$IFDEF Attracs}
    result := NULLDATETIMEREPRESENTATION // TODO: was this workaround for DBX or business reasons ?
{$ELSE}
    result := null
{$ENDIF}
  else
    result := aDateTime.AsDateTime;
end;

procedure TBoldPMDateTime.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetEnsuredValue(ObjectContent) as IBoldDateTimeContent;
  if Field.IsNull {$IFDEF ConvertZeroDateToDateNil} or (Field.AsDateTime = 0) or (abs(Field.AsDateTime - NULLDATETIMEREPRESENTATION) < TwoSeconds) {$ENDIF} then
    aDateTime.SetContentToNull
  else
  begin
{$IFDEF NoNegativeDates}
    if (Field.Value < 0) then
      raise EBold.Create(Format('Negative date in Object %s column %s', [OwningObjectId.AsString, Field.FieldName]));
{$ENDIF}
    aDateTime.AsDateTime := Field.Value;
  end;
end;

procedure TBoldPMDateTime.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetEnsuredValue(ObjectContent) as IBoldDateTimeContent;
  if aDateTime.IsNull then
{$IFDEF Attracs}
    Param.AsDateTime := NULLDATETIMEREPRESENTATION // TODO: was this workaround for DBX or business reasons ?
{$ELSE}
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
{$ENDIF}
  else
    Param.AsDateTime := aDateTime.AsDateTime;
end;

{--TBoldPMDate---}
class function TBoldPMDate.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Date) = 0;
end;

function TBoldPMDate.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftDate;
end;

function TBoldPMDate.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aDate: IBoldDateContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDate := GetValue(ObjectContent) as IBoldDateContent;
  if not CheckEitherNull(field, aDate, result) then
    result := trunc(Field.AsDate) = trunc(aDate.AsDate);
end;

function TBoldPMDate.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForDate;
end;

function TBoldPMDate.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aDate: IBoldDateContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDate := GetEnsuredValue(ObjectContent) as IBoldDateContent;
  if aDate.IsNull then
    result := null
  else
    result := aDate.AsDate;
end;

procedure TBoldPMDate.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aDate: IBoldDateContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDate := GetEnsuredValue(ObjectContent) as IBoldDateContent;
  if Field.IsNull then
    aDate.SetContentToNull
  else
    aDate.AsDate := Field.Value;
end;

procedure TBoldPMDate.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aDate: IBoldDateContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDate := GetEnsuredValue(ObjectContent) as IBoldDateContent;
  if aDate.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsDate := aDate.AsDate;
end;

{---TBoldPMTime---}
class function TBoldPMTime.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Time) = 0;
end;

function TBoldPMTime.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftTime;
end;

function TBoldPMTime.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aTime: IBoldTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aTime := GetValue(ObjectContent) as IBoldTimeContent;
  result := true;
  if not CheckEitherNull(field, aTime, result) then
    result := abs(Field.AsTime - aTime.AsTime) < TwoSeconds;
end;

function TBoldPMTime.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForTime;
end;

function TBoldPMTime.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
var
  aTime: IBoldTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aTime := GetEnsuredValue(ObjectContent) as IBoldTimeContent;
  if aTime.IsNull then
    result := null
  else
    result := aTime.AsTime;
end;

procedure TBoldPMTime.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aTime: IBoldTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aTime := GetEnsuredValue(ObjectContent) as IBoldTimeContent;
  if Field.IsNull then
    aTime.SetContentToNull
  else
    aTime.AsTime := Field.Value;
end;

procedure TBoldPMTime.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aTime: IBoldTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aTime := GetEnsuredValue(ObjectContent) as IBoldTimeContent;
  if aTime.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsTime := aTime.AsTime;
end;

{ TBoldPMMemoBlob }

function TBoldPMMemoBlob.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := ftMemo;
end;

{ TBoldPMNonEmptyBlob }

const
  InternalEmptyString = '<## Internal Empty ##>';

function TBoldPMNonEmptyBlob.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBlobContent: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlobContent := GetValue(ObjectContent) as IBoldBlobContent;
  if not CheckEitherNull(field, aBlobContent, result) then
    result := Field.Value = aBlobContent.AsBlob;
end;

procedure TBoldPMNonEmptyBlob.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
var
  aBlobContent: IBoldBlobContent;
begin
  inherited;
  aBlobContent := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if not aBlobContent.IsNull and (aBlobContent.AsBlob = InternalEmptyString) then
    aBlobContent.AsBlob := '';
end;

procedure TBoldPMNonEmptyBlob.ValueToParam(const ObjectContent: IBoldObjectContents;
  const Param: IBoldParameter; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList);
var
  EmptyBlobHandled: Boolean;
  aBlobContent: IBoldBlobContent;
begin
  EmptyBlobHandled := false;
  aBlobContent := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if not aBlobContent.IsNull and (aBlobContent.AsBlob = '') then
  begin
    aBlobContent.AsBlob := InternalEmptyString;
    EmptyBlobHandled := true;
  end;
  inherited;
  if EmptyBlobHandled then
    aBlobContent.AsBlob := '';
end;

{ TBoldPMLiveBlob }

constructor TBoldPMLiveBlob.CreateFromMold(Moldmember: TMoldMember;
  MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  fCustomFetch := true;
end;

function TBoldPMLiveBlob.GetRequiresLiveQuery: Boolean;
begin
  result := true;
end;

{ TBoldPMLiveTypedBlob }

constructor TBoldPMLiveTypedBlob.CreateFromMold(Moldmember: TMoldMember;
  MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  fCustomFetch := true;
end;

function TBoldPMLiveTypedBlob.GetRequiresLiveQuery: Boolean;
begin
  result := true;
end;


function TBoldPMString.DefaultDefaultDbValue: String;
begin
  result:='';
  if not AllowNull and (SystemPersistenceMapper.SQLDataBaseConfig.SupportsStringDefaultValues) then
    result := '''''';
end;

function TBoldPMInteger.DefaultDefaultDbValue: String;
begin
  if AllowNull then
    result := ''
  else
    result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMFloat.DefaultDefaultDbValue: String;
begin
  if AllowNull then
    result := ''
  else
    result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMCurrency.DefaultDefaultDbValue: String;
begin
  if AllowNull then
    result := ''
  else
    result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMCurrency.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForCurrency;
end;

{ TBoldPMAnsiString }

function TBoldPMAnsiString.CompareField(const ObjectContent: IBoldObjectContents;
    const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
    TranslationList: TBoldIdTranslationList): Boolean;
var
  aString: IBoldAnsiStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetValue(ObjectContent) as IBoldAnsiStringContent;
  result :=
    SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL and
    Field.IsNull and (aString.asAnsiString = '');

  if not result then
  begin
    if not CheckEitherNull(field, aString, result) then
      result := Field.AsAnsiString = aString.asAnsiString;
  end;
end;

function TBoldPMAnsiString.GetColumnBDEFieldType(
  ColumnIndex: Integer): TFieldType;
begin
  Result := ftString;
end;

function TBoldPMAnsiString.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.GetColumnTypeForAnsiString(GetColumnSize(ColumnIndex));
end;

procedure TBoldPMAnsiString.ValueFromField(OwningObjectId: TBoldObjectId;
    const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace;
    TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex:
    Integer);
var
  aString: IBoldAnsiStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldAnsiStringContent;
  if Field.IsNull then
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
      aString.AsString := ''
    else
      aString.SetContentToNull
  end
  else
    aString.asAnsiString := Field.AsAnsiString
end;

procedure TBoldPMAnsiString.ValueToParam(const ObjectContent: IBoldObjectContents;
    const Param: IBoldParameter; ColumnIndex: Integer; TranslationList:
    TBoldIdTranslationList);
var
  aString: IBoldAnsiStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldAnsiStringContent;
  if aString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsAnsiString := aString.asAnsiString
end;

{ TBoldPMUnicodeString }

function TBoldPMUnicodeString.CompareField(const ObjectContent: IBoldObjectContents;
    const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
    TranslationList: TBoldIdTranslationList): Boolean;
var
  aString: IBoldUnicodeStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetValue(ObjectContent) as IBoldUnicodeStringContent;
  result :=
    SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL and
    Field.IsNull and (aString.asUnicodeString = '');

  if not result then
  begin
    if not CheckEitherNull(field, aString, result) then
      result := Field.AsWideString = aString.asUnicodeString;
  end;
end;

function TBoldPMUnicodeString.GetColumnBDEFieldType(ColumnIndex: Integer):
    TFieldType;
begin
  Result := ftWideString;
end;

function TBoldPMUnicodeString.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.GetColumnTypeForUnicodeString(GetColumnSize(ColumnIndex));
end;

procedure TBoldPMUnicodeString.ValueFromField(OwningObjectId: TBoldObjectId;
    const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace;
    TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex:
    Integer);
var
  aString: IBoldUnicodeStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldUnicodeStringContent;
  if Field.IsNull then
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
      aString.AsString := ''
    else
      aString.SetContentToNull
  end
  else
    aString.asUnicodeString := Field.AsWideString
end;

procedure TBoldPMUnicodeString.ValueToParam(const ObjectContent: IBoldObjectContents;
    const Param: IBoldParameter; ColumnIndex: Integer; TranslationList:
    TBoldIdTranslationList);
var
  aString: IBoldUnicodeStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldUnicodeStringContent;
  if aString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsWideString := aString.asUnicodeString
end;

{ TBoldPMStringBDECodePageBug }

procedure TBoldPMStringBDECodePageBug.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if aString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin

    Param.AsString := copy(aString.AsString, 1, maxint);
  end;
end;

{---TBoldPMText---}
constructor TBoldPMText.CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  if (DefaultDbValue <> '') and
     (DefaultDbValue[1] <> '''') and
     (DefaultDbValue[1] <> '"') then
    DefaultDbValue := '''' + DefaultDbValue + '''';
  if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
  begin
    if (MoldMember as TMoldAttribute).AllowNull then
      raise EBold.CreateFmt('String attribute must not allow NULL in the model if persistencemapper stores empty strings as NULL (%s.%s)', [
        MoldClass.Name, MoldMember.Name]);
    fAllowNull := True;
  end;
end;

function TBoldPMText.DefaultDefaultDbValue: String;
begin
  if SystemPersistenceMapper.SQLDataBaseConfig.SupportsStringDefaultValues then
    result := ''''''
  else
    result := '';
end;

function TBoldPMText.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForText;
end;

class function TBoldPMText.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_String) = 0;
end;

function TBoldPMText.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftMemo;
end;

procedure TBoldPMText.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if Field.IsNull then
  begin
    if SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL then
      aString.AsString := ''
    else
      aString.SetContentToNull
  end
  else
    aString.AsString := Field.AsString
end;

function TBoldPMText.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetValue(ObjectContent) as IBoldStringContent;
  result :=
    SystemPersistenceMapper.SQLDataBaseConfig.StoreEmptyStringsAsNULL and
    Field.IsNull and (aString.AsString = '');

  if not result then
  begin
    if not CheckEitherNull(field, aString, result) then
      result := Field.AsString = aString.AsString;
  end;
end;

procedure TBoldPMText.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aString: IBoldStringContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aString := GetEnsuredValue(ObjectContent) as IBoldStringContent;
  if aString.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
    Param.AsString := aString.AsString
end;

{ TBoldPMUnicodeText }

function TBoldPMUnicodeText.GetColumnBDEFieldType(ColumnIndex: Integer):
    TFieldType;
begin
  Result := ftWideMemo;
end;

function TBoldPMUnicodeText.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForUnicodeText;
end;

{ TBoldPMGuid }

function TBoldPMGuid.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := ftGuid;
end;

function TBoldPMGuid.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForGUID;
end;

initialization
  TwoSeconds := EncodeTime(0, 0, 2, 0);

  with BoldMemberPersistenceMappers do
  begin
    AddDescriptor(TBoldSingleColumnMember, alAbstract);
    AddDescriptor(TBoldPMString, alConcrete);
    AddDescriptor(TBoldPMAnsiString, alConcrete);
    AddDescriptor(TBoldPMUnicodeString, alConcrete);
    AddDescriptor(TBoldPMStringBDECodePageBug, alConcrete);
    AddDescriptor(TBoldPMText, alConcrete);
    AddDescriptor(TBoldPMUnicodeText, alConcrete);
    AddDescriptor(TBoldPMInteger, alConcrete);
    AddDescriptor(TBoldPMSmallInt, alConcrete);
    AddDescriptor(TBoldPMByte, alConcrete);
    AddDescriptor(TBoldPMShortInt, alConcrete);
    AddDescriptor(TBoldPMWord, alConcrete);
    AddDescriptor(TBoldPMFloat, alConcrete);
    AddDescriptor(TBoldPMCurrency, alConcrete);
    AddDescriptor(TBoldPMBlob, alConcrete);
    AddDescriptor(TBoldPMNonEmptyBlob, alConcrete);
    AddDescriptor(TBoldPMMemoBlob, alConcrete);
    AddDescriptor(TBoldPMTypedBlob, alConcrete);
    AddDescriptor(TBoldPMLiveBlob, alConcrete);
    AddDescriptor(TBoldPMLiveTypedBlob, alConcrete);
    AddDescriptor(TBoldPMDateTime, alConcrete);
    AddDescriptor(TBoldPMDate, alConcrete);
    AddDescriptor(TBoldPMTime, alConcrete);
    AddDescriptor(TBoldPMLogic, alConcrete);
    AddDescriptor(TBoldPMGuid, alConcrete);
  end;
  {end - initialization}

finalization
  if BoldMemberPersistenceMappersAssigned then
    with BoldMemberPersistenceMappers do
    begin
    RemoveDescriptorByClass(TBoldSingleColumnMember);
    RemoveDescriptorByClass(TBoldPMString);
    RemoveDescriptorByClass(TBoldPMAnsiString);
    RemoveDescriptorByClass(TBoldPMUnicodeString);
    RemoveDescriptorByClass(TBoldPMStringBDECodePageBug);
    RemoveDescriptorByClass(TBoldPMText);
    RemoveDescriptorByClass(TBoldPMUnicodeText);
    RemoveDescriptorByClass(TBoldPMInteger);
    RemoveDescriptorByClass(TBoldPMSmallInt);
    RemoveDescriptorByClass(TBoldPMByte);
    RemoveDescriptorByClass(TBoldPMShortInt);
    RemoveDescriptorByClass(TBoldPMWord);
    RemoveDescriptorByClass(TBoldPMFloat);
    RemoveDescriptorByClass(TBoldPMCurrency);
    RemoveDescriptorByClass(TBoldPMBlob);
    RemoveDescriptorByClass(TBoldPMMemoBlob);
    RemoveDescriptorByClass(TBoldPMNonEmptyBlob);
    RemoveDescriptorByClass(TBoldPMTypedBlob);
    RemoveDescriptorByClass(TBoldPMLiveBlob);
    RemoveDescriptorByClass(TBoldPMLiveTypedBlob);
    RemoveDescriptorByClass(TBoldPMDateTime);
    RemoveDescriptorByClass(TBoldPMDate);
    RemoveDescriptorByClass(TBoldPMTime);
    RemoveDescriptorByClass(TBoldPMLogic);
    RemoveDescriptorByClass(TBoldPMGuid);
    end;
  {END finalization}

end.
