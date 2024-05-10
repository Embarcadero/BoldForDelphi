
{ Global compiler directives }
{$include bold.inc}
unit BoldTypeNameDictionary;

interface

uses
  Classes;

type
  TBoldTypeNameDictionary = class;
  TBoldTypeNameDictionaryClass = class of TBoldTypeNameDictionary;

  TBoldTypeNameMapping = class(TCollectionItem)
  private
    FExpressionName: String;
    FAccessor: String;
    FDelphiName: String;
    FModelName: string;
    FNativeType: String;
    FMapperName: String;
    fContentsName: String;
    fBoldUnitName: String;
    fComType: String;
    fIDLType: String;
    fValueInterface: String;
    fValueInterfaceAccessor: String;
    fValueInterfaceNativeType: String;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetExpandedAccessor: String;
    function GetExpandedDelphiName: String;
    function GetExpandedMapperName: String;
    function GetExpandedNativeType: String;
    function GetExpandedContentsName: String;
    function GetExpandedComType: String;
    procedure ReadStreamName(Reader: TReader);
  protected
    property AsString: string read GetAsString write SetAsString;
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property ExpandedDelphiName: String read GetExpandedDelphiName;
    property ExpandedContentsName: String read GetExpandedContentsName;
    property ExpandedMapperName: String read GetExpandedMapperName;
    property ExpandedAccessor: String read GetExpandedAccessor;
    property ExpandedNativeType: String read GetExpandedNativeType;
    property ExpandedComType: String read GetExpandedComType;
  published
    property ModelName: string read FModelName write fModelName;
    property ExpressionName: String read FExpressionName write fExpressionName;
    property DelphiName: String read FDelphiName write fDelphiName;
    property ContentsName: String read FContentsName write FContentsName;
    property MapperName: String read FMapperName write fMapperName;
    property Accessor: String read FAccessor write fAccessor;
    property NativeType: String read FNativeType write fNativeType;
    property BoldUnitName: String read fBoldUnitName write fBoldUnitName;
    {$WARNINGS OFF}
    property UnitName: String read fBoldUnitName write fBoldUnitName stored false;
    {$WARNINGS ON}
    property ComType: String read fComType write fComType;
    property IDLType: String read fIDLType write fIDLType;
    property ValueInterface: String read fValueInterface write fValueInterface;
    property ValueInterfaceAccessor: String read fValueInterfaceAccessor write fValueInterfaceAccessor;
    property ValueInterfaceNativeType: String read fValueInterfaceNativeType write fValueInterfaceNativeType;
  end;

  TBoldTypeNameDictionary = class(TCollection)
  private
    fOwner: TComponent;
    function GetMapping(Index: integer): TBoldTypeNameMapping;
    function GetMappingForModelName(ModelName:String): TBoldTypeNameMapping;
    function GetExactMappingForModelName(ModelName:String): TBoldTypeNameMapping;
    procedure SaveToStringList(StrList: TStringList);
    procedure LoadFromStringList(StrList: TStringList);
  protected
   function GetOwner: TPersistent; override;
  public
    constructor create(Owner: TComponent);
    property Mapping[Index: integer]: TBoldTypeNameMapping read GetMapping;
    property MappingForModelName[ModelName:String]: TBoldTypeNameMapping read GetMappingForModelName;
    property ExactMappingForModelName[ModelName:String]: TBoldTypeNameMapping read GetExactMappingForModelName;
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    procedure AddDefaultMappings;
    function AddMapping: TBoldTypeNameMapping;
  end;

var
  TBoldCurrentTypeNameDictionaryClass: TBoldTypeNameDictionaryClass = TBoldTypeNameDictionary;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldNameExpander,
  BoldTaggedValueSupport;

const
  DefaultMappings: array[0..32, 0..12] of String = (
  (DEFAULTNAMELITERAL,'String',     'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldStringContent', 'AsString', 'String'),
  ('String',        'String',       'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldStringContent', 'AsString', 'String'),
  ('AnsiString',    'String',       'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldStringContent', 'AsString', 'String'),
  ('UnicodeString', 'UnicodeString','TBA<Name>',   'String',   'TBoldPM<Name>',   'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldUnicodeStringContent', 'AsUnicodeString', 'String'),
  ('Text',          'Text',         'TBA<Name>',   'String',   'TBoldPM<Name>',   'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldAnsiStringContent',    'AsAnsiString',    'AnsiString'),
  ('UnicodeText',   'UnicodeText',  'TBA<Name>',   'String',   'TBoldPM<Name>',   'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldUnicodeStringContent', 'AsUnicodeString', 'String'),
  ('<Numeric>',     'Numeric',      'TBA<Name>',   '',         '',                '',           '',          'BoldAttributes',   '',           '',         '',                   '', ''),
  ('Integer',       'Integer',      'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Int',           'Integer',      'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Word',          'Word',         'TBA<Name>',   'Integer',  'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('SmallInt',      'SmallInt',     'TBA<Name>',   'Integer',  'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Byte',          'Byte',         'TBA<Name>',   'Integer',  'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('ShortInt',      'ShortInt',     'TBA<Name>',   'Integer',  'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Float',         'Float',        'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   'double',    'BoldAttributes',   'Double',     'DOUBLE',   'IBoldFloatContent',    'AsFloat', 'double'),
  ('Double',        'Float',        'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   'double',    'BoldAttributes',   'Double',     'DOUBLE',  'IBoldFloatContent',     'AsFloat', 'double'),
  ('Currency',      'Currency',     'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   '<Name>',    'BoldAttributes',   'Currency',   'CURRENCY', 'IBoldCurrencyContent', 'AsCurrency', 'Currency'),
  ('<Moment>',      'Moment',       'TBA<Name>',   '',         '',                '',           '',          'BoldAttributes',   '',           '',         '',                     '', ''),
  ('DateTime',      'DateTime',     'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   'T<Name>',   'BoldAttributes',   'TDateTime',  'DATE',     'IBoldDateTimeContent', 'AsDateTime', 'TDateTime'),
  ('TimeStamp',     'DateTime',     'TBADateTime', '<Name>',   'TBoldPM<Name>',   'As<Name>',   'T<Name>',   'BoldAttributes',   'TDateTime',  'DATE',     'IBoldDateTimeContent', 'AsDateTime', 'TDateTime'),
  ('Date',          'Date',         'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   'T<Name>',   'BoldAttributes',   'TDateTime',  'DATE',     'IBoldDateContent', 'AsDate', 'TDateTime'),
  ('Time',          'Time',         'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'As<Name>',   'T<Name>',   'BoldAttributes',   'TDateTime',  'DATE',     'IBoldTimeContent', 'AsTime', 'TDateTime'),
  ('<ValueSet>',    'ValueSet',     'TBA<Name>',   'Integer',  'TBoldPMInteger',  'AsInteger',  'integer',   'BoldAttributes',   'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Boolean',       'Boolean',      'TBA<Name>',   'Integer',  'TBoldPMInteger',  'As<Name>',   '<Name>',    'BoldAttributes',   'WordBool',   'VARIANT_BOOL', 'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Bool',          'Boolean',      'TBA<Name>',   'Integer',  'TBoldPMInteger',  'As<Name>',   '<Name>',    'BoldAttributes',   'WordBool',   'VARIANT_BOOL', 'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Constraint',    'Constraint',   'TBA<Name>',   'Integer',  'TBoldPMInteger',  'AsBoolean',  'Boolean',   'BoldAttributes',   'WordBool',   'VARIANT_BOOL', 'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Blob',          'Blob',         'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldBlobContent',    'AsBlob', 'String'),
  ('TypedBlob',     'TypedBlob',    'TBA<Name>',   '<Name>',   'TBoldPM<Name>',   'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldTypedBlob',      'AsBlob', 'String'),
  ('BlobImageBMP',  'BlobImageBMP', 'TBA<Name>',   'Blob',     'TBoldPMBlob',     'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldBlobContent',    'AsBlob', 'String'),
  ('BlobImageJPEG', 'BlobImageJPEG','TBA<Name>',   'Blob',     'TBoldPMBlob',     'AsString',   'String',    'BoldAttributes',   'WideString', 'BSTR',     'IBoldBlobContent',    'AsBlob', 'String'),
  ('MLString',      'MLString',     'TBA<Name>',   'Blob',     'TBoldPMBlob',     'AsString',   'String',    'BoldMLAttributes', 'WideString', 'BSTR',     'IBoldBlobContent',    'AsBlob', 'String'),
  ('MLSubString',   'MLSubString',  'TBA<Name>',   'String',   'TBoldPMString',   'AsString',   'String',    'BoldMLAttributes', 'WideString', 'BSTR',     'IBoldStringContent',  'AsString', 'String'),
  ('MLValueSet',    'MLValueSet',   'TBA<Name>',   'Integer',  'TBoldPMInteger',  'AsInteger',  'integer',   'BoldMLAttributes', 'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'),
  ('Language',      'Language',     'TBA<Name>',   'Integer',  'TBoldPMInteger',  'AsInteger',  'integer',   'BoldMLAttributes', 'Integer',    'LONG',     'IBoldIntegerContent', 'AsInteger', 'integer'));

{ TBoldTypeNameDictionary }

procedure TBoldTypeNameDictionary.AddDefaultMappings;
var
  i: integer;
begin
  for i := 0 to High(DefaultMappings) do
  begin
    with Add as TBoldTypeNameMapping do
    begin
      ModelName       := DefaultMappings[i, 0];
      ExpressionName  := DefaultMappings[i, 1];
      DelphiName      := DefaultMappings[i, 2];
      ContentsName    := DefaultMappings[i, 3];
      MapperName      := DefaultMappings[i, 4];
      Accessor        := DefaultMappings[i, 5];
      NativeType      := DefaultMappings[i, 6];
      BoldUnitName    := DefaultMappings[i, 7];
      ComType         := DefaultMappings[i, 8];
      IDLType         := DefaultMappings[i, 9];
      ValueInterface  := DefaultMappings[i, 10];
      ValueInterfaceAccessor  := DefaultMappings[i, 11];
      ValueInterfaceNativeType  := DefaultMappings[i, 12];
    end;
  end;
end;

function TBoldTypeNameDictionary.AddMapping: TBoldTypeNameMapping;
begin
  Add;
  result := Mapping[Count-1];
end;

constructor TBoldTypeNameDictionary.create(Owner: TComponent);
begin
  inherited create(TBoldTypeNameMapping);
  fOwner := Owner;
end;

function TBoldTypeNameDictionary.GetMapping(
  Index: integer): TBoldTypeNameMapping;
begin
  result := GetItem(index) as TBoldTypeNameMapping;
end;

function TBoldTypeNameDictionary.GetExactMappingForModelName(ModelName: string): TBoldTypeNameMapping;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if SameText(Mapping[i].ModelName, ModelName)then
    begin
      Result := Mapping[i];
      Exit;
    end;
end;

function TBoldTypeNameDictionary.GetMappingForModelName(
  ModelName: String): TBoldTypeNameMapping;
begin
  Result := ExactMappingForModelName[ModelName];
  if not Assigned(Result) then
    Result := ExactMappingForModelName[DEFAULTNAME];
end;

function TBoldTypeNameDictionary.GetOwner: TPersistent;
begin
  result := fOwner;
end;

procedure TBoldTypeNameDictionary.LoadFromFile(FileName: String);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  StrList.LoadFromFile(FileName);
  LoadFromStringList(StrList);
  StrList.Free;
end;

procedure TBoldTypeNameDictionary.LoadFromStringList(StrList: TStringList);
var
  i: integer;
begin
  for i := 0 to StrList.Count-1 do
    if trim(StrList[i]) <> '' then
    begin
      Add;
      Mapping[Count-1].AsString := Trim(StrList[i]);
    end;
end;

procedure TBoldTypeNameDictionary.SaveToFile(FileName: String);
var
  StrList: TStringLIst;
begin
  StrList := TStringList.Create;
  SaveToStringList(StrList);
  StrList.SaveToFile(FileName);
  StrList.Free;
end;

procedure TBoldTypeNameDictionary.SaveToStringList(StrList: TStringList);
var
  i: integer;
begin
  for i := 0 to Count-1 do
    StrList.Add(Mapping[i].AsString);
end;

{ TBoldTypeNameMapping }

function TBoldTypeNameMapping.GetAsString: string;
begin
  Result := Format('ModelName=%s,ExpressionName=%s,DelphiName=%s,ContentsName=%s,MapperName=%s,AccessorName=%s,NativeType=%s,UnitName=%s,ComType=%s,IDLType=%s,ValueInterface=%s,ValueInterfaceAccessor=%s,ValueInterfaceNativeType=%s',
                    [ModelName,
                     ExpressionName,
                     DelphiName,
                     ContentsName,
                     MapperName,
                     Accessor,
                     NativeType,
                     BoldUnitName,
                     ComType,
                     IDLType,
                     ValueInterface,
                     ValueInterfaceAccessor,
                     ValueInterfaceNativetype]);
end;

function TBoldTypeNameMapping.GetExpandedAccessor: String;
begin
  result := BoldExpandName(Accessor, ExpressionName, xtExpression, -1, nccDefault);
end;

function TBoldTypeNameMapping.GetExpandedDelphiName: String;
begin
  result := BoldExpandName(DelphiName, ExpressionName, xtExpression, -1, nccDefault);
end;

function TBoldTypeNameMapping.GetExpandedMapperName: String;
begin
  result := BoldExpandName(MapperName, ExpressionName, xtExpression, -1, nccDefault);
end;

function TBoldTypeNameMapping.GetExpandedNativeType: String;
begin
  result := BoldExpandName(NativeType, ExpressionName, xtExpression, -1, nccDefault);
end;

function TBoldTypeNameMapping.GetExpandedContentsName: String;
begin
  result := BoldExpandName(ContentsName, ExpressionName, xtExpression, -1, nccDefault);
end;

procedure TBoldTypeNameMapping.SetAsString(const Value: string);
var
  vTmpList: TStringList;
begin
  vTmpList := TStringList.Create;
  try
    vTmpList.CommaText  := value;
    ModelName           := vTmpList.Values['ModelName'];
    ExpressionName      := vTmpList.Values['ExpressionName'];
    DelphiName          := vTmpList.Values['DelphiName'];

    if vTmpList.Values['StreamName'] <> '' then
      ContentsName      := vTmpList.Values['StreamName']
    else
      ContentsName      := vTmpList.Values['ContentsName'];

    MapperName      := vTmpList.Values['MapperName'];
    Accessor        := vTmpList.Values['AccessorName'];
    NativeType      := vTmpList.Values['NativeType'];
    BoldUnitName    := vTmpList.Values['UnitName'];
    ComType         := vTmpList.Values['ComType'];
    IDLType         := vTmpList.Values['IDLType'];
    ValueInterface  := vTmpList.Values['ValueInterface'];
    ValueInterfaceAccessor  := vTmpList.Values['ValueInterfaceAccessor'];
    ValueInterfaceNativeType  := vTmpList.Values['ValueInterfaceNativeType'];
  finally
    Free;
  end;
end;

procedure TBoldTypeNameMapping.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('StreamName', ReadStreamName, nil, False);
end;

procedure TBoldTypeNameMapping.ReadStreamName(Reader: TReader);
begin
  ContentsName := Reader.ReadString
end;

procedure TBoldTypeNameMapping.AssignTo(Dest: TPersistent);
begin
  if dest is TBoldTypeNameMapping then
    with dest as TBoldTypeNameMapping do begin
      ModelName := self.ModelName;
      ExpressionName := self.ExpressionName;
      DelphiName := self.DelphiName;
      ContentsName := self.ContentsName;
      MapperName := self.MapperName;
      Accessor := self.Accessor;
      NativeType := self.NativeType;
      BoldUnitName := self.BoldUnitName;
      ComType := self.ComType;
      IDLType := self.IDLType;
      ValueInterface := self.ValueInterface;
      ValueInterfaceAccessor := self.ValueInterfaceAccessor;
      ValueInterfaceNativeType := self.ValueInterfaceNativeType;
    end
  else
   inherited;
end;


function TBoldTypeNameMapping.GetExpandedComType: String;
begin
  result := BoldExpandName(ComType, ExpressionName, xtExpression, -1, nccDefault);
end;


end.
