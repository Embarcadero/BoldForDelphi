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

  {TBoldPMString}
  TBoldPMString = class(TBoldSingleColumnMember)
  private
    fColumnSize: integer;
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  TBoldPMStringBDECodePageBug = class(TBoldPMString)
  public
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;


  {TBoldPMInteger}
  TBoldPMInteger = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
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
  TBoldPMByte = class(TBoldPMSmallInt) // No TFieldType value for byte
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  {TBoldPMShortInt}
  TBoldPMShortInt = class(TBoldPMSmallInt) // No TFieldType value for ShortInt
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
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMFloat}
  TBoldPMFloat = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMCurrency}
  TBoldPMCurrency = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMBlob}
  TBoldPMBlob = class(TBoldSingleColumnMember)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMBlob}
  TBoldPMMemoBlob = class(TBoldPMBlob)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
  end;

  TBoldPMNonEmptyBlob = class(TBoldPMBlob)
  protected
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
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
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
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
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMDate}
  TBoldPMDate = class(TBoldPMDateTime)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

  {TBoldPMTime}
  TBoldPMTime = class(TBoldPMDateTime)
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldValueInterfaces,
  BoldPMapperLists,
  BoldDefaultStreamNames,
  BoldPMappersSQL,
  BoldPMConsts;

var
  TwoSeconds: TDateTime; // initialized in initizlization-section


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
  Result := Format(SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForString, [GetColumnSize(ColumnIndex)]);
end;

class function TBoldPMString.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_String) = 0;
end;

function TBoldPMString.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftString;
end;

function TBoldPMString.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  Result := fColumnSize;
end;

procedure TBoldPMString.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

function TBoldPMString.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
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


procedure TBoldPMString.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

function TBoldPMInteger.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  anInteger: IBoldIntegerContent;
begin
  EnsureFirstColumn(ColumnIndex);
  anInteger := GetValue(ObjectContent) as IBoldIntegerContent;
  if not CheckEitherNull(field, anInteger, result) then
    result := Field.AsInteger = anInteger.AsInteger;
end;

procedure TBoldPMInteger.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMInteger.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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
  Result := 'VARCHAR(1)'; // do not localize
end;

class function TBoldPMLogic.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Boolean) = 0;
end;

function TBoldPMLogic.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := ftBoolean;
end;

function TBoldPMLogic.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBoolean: IBoldBooleanContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBoolean := GetValue(ObjectContent) as IBoldBooleanContent;
  if not CheckEitherNull(field, aBoolean, result) then
    result := Field.AsBoolean = aBoolean.AsBoolean;
end;

procedure TBoldPMLogic.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMLogic.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

function TBoldPMFloat.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aFloat: IBoldFloatContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aFloat := GetValue(ObjectContent) as IBoldFloatContent;
  if not CheckEitherNull(field, aFloat, result) then
    result := Field.AsFloat = aFloat.AsFloat;
end;

procedure TBoldPMFloat.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMFloat.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

function TBoldPMCurrency.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aCurrency: IBoldCurrencyContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aCurrency := GetValue(ObjectContent) as IBoldCurrencyContent;
  if not CheckEitherNull(field, aCurrency, result) then
    result := Field.AsCurrency = aCurrency.AsCurrency;
end;


procedure TBoldPMCurrency.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMCurrency.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

class function TBoldPMBlob.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_Blob) = 0;
end;

function TBoldPMBlob.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.FieldTypeForBlob;
end;

function TBoldPMBlob.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetValue(ObjectContent) as IBoldBlobContent;
  if not CheckEitherNull(field, aBlob, result) then
    result := Field.AsBlob = aBlob.AsBlob;
end;

procedure TBoldPMBlob.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if Field.IsNull then
    aBlob.SetContentToNull
  else
    aBlob.AsBlob := Field.AsBlob;
end;

procedure TBoldPMBlob.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aBlob: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlob := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if aBlob.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
  else
  begin
    Param.DataType := ColumnBDEFieldType[ColumnIndex];
//marco    Param.AsBlob := aBlob.AsBlob;
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
    Result := InitialColumnRootName + '_Content' // do not localize
  else
    Result := inherited GetInitialColumnName(ColumnIndex);
end;

class function TBoldPMTypedBlob.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_TypedBlob) = 0;
end;

function TBoldPMTypedBlob.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
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

procedure TBoldPMTypedBlob.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMTypedBlob.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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
  Result := ftDateTime;
end;

function TBoldPMDateTime.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetValue(ObjectContent) as IBoldDateTimeContent;
  if not CheckEitherNull(field, aDateTime, result) then
    result := abs(Field.AsDateTime - aDateTime.AsDateTime) < TwoSeconds;
end;

procedure TBoldPMDateTime.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetEnsuredValue(ObjectContent) as IBoldDateTimeContent;
  if Field.IsNull then
    aDateTime.SetContentToNull
  else
    aDateTime.AsDateTime := Field.Value;
end;

procedure TBoldPMDateTime.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  aDateTime: IBoldDateTimeContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aDateTime := GetEnsuredValue(ObjectContent) as IBoldDateTimeContent;
  if aDateTime.IsNull then
    SetParamToNullWithDataType(Param, GetColumnBDEFieldType(0))
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

function TBoldPMDate.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
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

procedure TBoldPMDate.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMDate.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

function TBoldPMTime.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
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


procedure TBoldPMTime.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
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

procedure TBoldPMTime.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
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

function TBoldPMNonEmptyBlob.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
var
  aBlobContent: IBoldBlobContent;
begin
  EnsureFirstColumn(ColumnIndex);
  aBlobContent := GetValue(ObjectContent) as IBoldBlobContent;
  if not CheckEitherNull(field, aBlobContent, result) then
    result := Field.Value = aBlobContent.AsBlob;
end;

procedure TBoldPMNonEmptyBlob.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents;
  ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  Field: IBoldField; ColumnIndex: Integer);
var
  aBlobContent: IBoldBlobContent;
begin
  inherited;
  aBlobContent := GetEnsuredValue(ObjectContent) as IBoldBlobContent;
  if not aBlobContent.IsNull and (aBlobContent.AsBlob = InternalEmptyString) then
    aBlobContent.AsBlob := '';
end;

procedure TBoldPMNonEmptyBlob.ValueToParam(ObjectContent: IBoldObjectContents;
  Param: IBoldParameter; ColumnIndex: Integer;
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
  if SystemPersistenceMapper.SQLDataBaseConfig.SupportsStringDefaultValues then
    result := ''''''
  else
    result := '';
end;

function TBoldPMInteger.DefaultDefaultDbValue: String;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMFloat.DefaultDefaultDbValue: String;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMCurrency.DefaultDefaultDbValue: String;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0');
end;

function TBoldPMCurrency.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForCurrency;
end;

{ TBoldPMStringBDECodePageBug }

procedure TBoldPMStringBDECodePageBug.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
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
    // make a copy of the string so that the param can
    // mess with it alone
    Param.AsString := copy(aString.AsString, 1, maxint);
  end;
end;

initialization

  TwoSeconds := EncodeTime(0, 0, 2, 0);

  with BoldMemberPersistenceMappers do
  begin
    AddDescriptor(TBoldSingleColumnMember, alAbstract);
    AddDescriptor(TBoldPMString, alConcrete);
    AddDescriptor(TBoldPMStringBDECodePageBug, alConcrete);
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
  end;
  {end - initialization}

finalization
  if BoldMemberPersistenceMappersAssigned then
    with BoldMemberPersistenceMappers do
    begin
    RemoveDescriptorByClass(TBoldSingleColumnMember);
    RemoveDescriptorByClass(TBoldPMString);
    RemoveDescriptorByClass(TBoldPMStringBDECodePageBug);
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
    end;
  {END finalization}

end.

