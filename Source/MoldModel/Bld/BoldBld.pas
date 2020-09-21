unit BoldBld;

interface

uses
  Classes,
  BoldDefs,
  BoldMeta;

 {Read and write models in .bld format}

type
  {---Forward declaration of classes---}
  TMoldBLDRW = class;

  {---Exceptions---}
  EBoldBLDParseError = class(EBold);

  {---TMoldBLDRW---}
  TMoldBLDRW = class
    class function ModelFromFile(const filename: string): TMoldModel; // returns dynamically allocated model, with parts.
    class procedure ModelToFile(Model: TMoldModel; const filename: string);
    class procedure ModelToStrings(Model: TMoldModel; s: TSTrings);
    class function StringsToModel(s: TStrings): TMoldModel; // returns dynamically allocated model, with parts.
  end;


function BooleanToMultiplicityString(Value: Boolean): String;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldBase,
  BoldTaggedValueSupport,
  BoldDefaultTaggedValues,
  BoldUMLTaggedValues,
  BoldUMLTypes,
  BoldMoldConsts;

const
  LINKEXTENSION: string = '.bld';
  LINKDESC: string = 'Bold Proprietary Format';
  CURRENTVERSION = 19;
  SPACE = #32;
  TAB = #9;

type
{---forward declarations---}
  TWriter = class;
  TReader = class;
  TElement = class;
  TModel = class;
  TPClass = class;

  TElementClass = class of TElement;

  TElementClassRecord = record
    ElementClass: TElementClass;
    MoldElementClass: TMoldElementClass;
    Name: string;
  end;

  {---TWriter---}
  TWriter = class(TBoldMemoryManagedObject)
  private
    modelAsStrings: TStrings;
    line: integer;
    indent: integer;
    procedure DoIndent;
    procedure PutStartBlock(const keyword: string);
    procedure PutEndBlock;
    procedure Put(const s: string);
    procedure PutQuotedString(const s: string);
    procedure Putinteger(i: Integer);
    procedure PutBoolean(b: Boolean);
    procedure PutClassReference(aClass: TMoldClass);
  public
    constructor Create(s: TStrings);
  end;

  BLDTokenKind = (LPAR, RPAR, KEYWORD, QSTRING, INTVALUE, ENDOFFILE);
  {---TToken---}
  TToken = class(TBoldMemoryManagedObject)
  public
    Kind: BLDTokenKind;
    StringValue: string;
    IntegerValue: integer
  end;

  {---TReader---}
  TReader = class(TBoldMemoryManagedObject)
  private
    modelAsStrings: TSTrings;
    FFormatVersion: integer;
    fLineNumber: integer;
    fLine: string;
    fLineLength: integer;
    fEOS: Boolean;
    Position: Integer;
    CurrentToken: TToken;
    CurrentCharacter: Char;
    procedure GetToken; // Get a token
    procedure Skip; // skip to next non-whitespace character
    procedure GetCharacter; // Get next character
    procedure GetCharacterNotEof; // Get next character, don't allow EOF
    function NextCharacter: Char; // Get next non-whitespace character, and return in
    procedure EatStartBlock(const keyw: string); // Eat start block
    function TryKeyword(const keyw: string): Boolean; // eat keyword if matching
    function GetKeyword: string;
    procedure Eat(t: BLDTokenKind);
    procedure EatKeyword(const keyw: string);
    function GetQuotedString: string;
    function Getinteger: integer;
    function GetBoolean: Boolean;
    function GetClassReference: TMoldClass;
    property EOS: Boolean read fEOS;
    property LineNumber: integer read fLineNumber;
    property Line: string read fLine;
  public
    CurrentModel: TMoldModel;
    property FormatVersion: integer read FFormatVersion;
    constructor Create(s: TStrings);
    destructor Destroy; override;
  end;

  {---TElement---}
  TElement = class(TBoldMemoryManagedObject)
  public
    class procedure Write(element: TMoldElement; w: TWriter); virtual;
    class procedure Read(element: TMoldElement; r: TReader); virtual;
    class function CreateAndRead(parent: TMOldElement; r: TReader): TMoldElement;
  end;

  {---TModel---}
  TModel = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TElementList---}
  TElementList = class(TBoldMemoryManagedObject)
    class procedure Write(list: TMoldElementList; w: TWriter);
    class procedure Read(parent: TMoldElement; r: TReader);  // classes link themselves into the right list
  end;

  {---TPClass---}
  TPClass = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TAttribute---}
  TAttribute = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TMethod---}
  TMethod = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TRole---}
  TRole = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TQualifier---}
  TQualifier = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

  {---TAssociation---}
  TAssociation = class(TElement)
    class procedure Write(element: TMoldElement; w: TWriter); override;
    class procedure Read(element: TMoldElement; r: TReader); override;
  end;

function TypeTableByMoldElement(moldElement: TMoldElement): TElementClassRecord;  forward;
function TypeTableByName(Name: string): TElementClassRecord;   forward;

{---TMoldBLDRW---}
class procedure TMoldBLDRW.ModelToStrings(model: TMoldModel; s: TStrings);
var
  theWriter: TWriter;
begin
  if not Assigned(model) then
    Exit;
  theWriter := TWriter.Create(s);
  try
    theWriter.Put(Format('VERSION %d', [CURRENTVERSION])); // do not localize
    Tmodel.Write(model, theWriter);
  finally
    theWriter.Free;
  end;
end;

class procedure TMoldBLDRW.ModelToFile(Model: TMoldModel; const Filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    ModelToStrings(model, sl);
    sl.SaveToFile(filename);
  finally
    sl.Free
  end;
end;

class function TMoldBLDRW.StringsToModel(s:TStrings): TMoldModel; // returns dynamically allocated model, with parts.
var
  reader: TReader;
begin
  Result := nil;
  if s.Count = 0 then
      Result := TMoldModel.Create(nil, 'New_Model') // do not localize
  else
  begin
    reader := TReader.Create(s);
    try
      try
        reader.CurrentToken := TToken.Create;
        reader.GetCharacter;
        reader.GetToken;
        // Get version information (if any)
        if reader.TryKeyword('VERSION') then // do not localize
          reader.FFormatVersion := reader.GetInteger
        else
           reader.FFormatVersion := 1;
{        if reader.fFormatVersion = 15 then
          // version 15 was only used in early betas of Bold2... no customer should have that version
          // both version -14 and 16+ are still supported
          raise EBoldBLDParseError.Create('Version 15 is no longer supported by the BLD-reader. Please reimport your model from your OOAD-tool');
}
        Result := TElement.CreateAndRead(nil, reader) as TMoldModel;

//        if reader.fFormatVersion < 11 then
//          Result.RootClass.ClassID := Result.FreeClassID;

      except
        on e: EBoldBLDParseError do
          raise EBoldBLDParseError.CreateFmt(sErrorOnPos, [e.Message, reader.LineNumber, reader.Position]);
      end;
      finally
        Reader.Free;
    end;
  end;
end;

class function TMoldBLDRW.ModelFromFile(const Filename: string): TMoldModel;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    Result := StringsToModel(sl);
  finally
    sl.Free;
  end;
end;

{---TWriter---}
constructor TWriter.Create(s: TStrings);
begin
  inherited Create;
  modelAsStrings := s;
  s.Clear;
  s.Add('');
end;

procedure TWriter.DoIndent;
var
  i: integer;
begin
  modelAsStrings.Add('');
  inc(line);
  for i:= 0 to indent - 1 do
    Put(TAB);
end;


procedure TWriter.PutStartBlock(const keyword: string);
begin
  doIndent;
  Put('(' +  keyword);
  inc(indent);
end;

procedure TWriter.PutEndBlock;
begin
  dec(indent);
  doIndent;
  put(')');
end;

procedure TWriter.Put(const s: string);
begin
  ModelAsStrings[line] := ModelAsStrings[line] + s;
end;

procedure TWriter.PutQuotedString(const s: string);
var
  i: integer;
begin
  doIndent;
  Put('"');
  for i := 1 to Length(s) do
    if (s[i] = '"') or (s[i] = '\') then
      Put('\' + s[i])
    else if s[i] = BOLDCR then
       Put('\' + 'c')
    else if s[i] = BOLDLF then
      begin
       Put('\' + 'l');
  //     DontIndent;
      end
    else
      Put(s[i]);
  Put('"');
end;

procedure TWriter.Putinteger(i: Integer);
begin
  doIndent;
  Put(IntToStr(i));
end;

procedure TWriter.PutBoolean(b: Boolean);
begin
  doIndent;
  if b then
    Put('TRUE') // do not localize
  else
    Put('FALSE'); // do not localize
end;

Procedure TWriter.PutClassReference(aClass: TMoldClass);
begin
  if Assigned(aClass) then
    PutQuotedString(aClass.Name)
  else
    PutQuotedString('<NONE>'); // do not localize
end;

{---TReader---}
constructor TReader.Create(s: TStrings);
begin
  inherited Create;
  ModelAsStrings := s;
  fEOS := ModelAsStrings.Count = 0;
  Position := 1;
end;

destructor TReader.Destroy;
begin
  CurrentToken.Free;
  inherited;
end;

procedure TReader.Skip;
begin
  while (not EOS) and (CurrentCharacter in [SPACE, TAB]) do
    GetCharacter;
end;

procedure TReader.GetCharacter;

  procedure NextLine;
  begin
    if LineNumber >=  modelAsStrings.Count then
      fEOS := True
    else
    begin
      fLine := modelAsStrings[LineNumber];
      fLineLength := Length(Line);
      Inc(fLineNumber);
    end;
    Position := 1;
    CurrentCharacter := ' '; // Space for newline
  end;

begin
  if EOS then
    CurrentCharacter := ' ' // Space for newline
  else
  if (Position > fLineLength) then
    NextLine
  else
  begin
    CurrentCharacter := Line[Position];
    INC(Position);
  end;
 //  write(output, currentcharacter);  // DEBUG only (or you'll get an IO error 103)
end;

procedure TReader.GetCharacterNotEof;
begin
  if EOS then
    raise EBoldBLDParseError.Create(sUnexpectedEOF)
  else
    GetCharacter;
end;

function TReader.NextCharacter: Char;
begin
  GetCharacter;
  Result := CurrentCharacter;
end;

var
  TokenBuffer: string = '      ';
  TokenBufferLength: integer = 6; // TokenBuffer MUST consist of this many spaces to start with

procedure TReader.GetToken;
var
  i: integer;
  LineWithToken: String;
  StartIndex, FoundLength: integer;
begin
  Skip;
  with CurrentToken do
    if EOS then
      Kind := ENDOFFILE
    else
    case CurrentCharacter of
      '(':
      begin
        Kind := LPAR;
        GetCharacter;
      end;

      ')':
      begin
        Kind := RPAR;
        GetCharacter;
      end;

      '0'..'9':
      begin
        Kind := INTVALUE;
        IntegerValue := ord(CurrentCharacter) - ord('0');
        while Nextcharacter in ['0'..'9'] do
          IntegerValue := IntegerValue * 10 +  ord(CurrentCharacter) - ord('0')
      end;

      'A'..'Z','a'..'z':
      begin
        Kind := KEYWORD;
        LineWithToken := fLine;
        StartIndex := Position;
        FoundLength := 1;
        TokenBuffer[FoundLength] := CurrentCharacter;
        while NextCharacter in ['A'..'Z','a'..'z'] do
          Inc(FoundLength);
        StringValue := Copy(LineWithToken, StartIndex-1, FoundLength);
      end;

      '"':
      begin
        Kind := QSTRING;
        GetCharacterNotEof;
        i := 0;
        while CurrentCharacter <> '"' do
        begin
          if CurrentCharacter = '\' then
          begin
            GetCharacterNotEof;
            if CurrentCharacter = 'c' then
              CurrentCharacter := BOLDCR
            else if CurrentCharacter = 'l' then
              CurrentCharacter := BOLDLF;
          end;
          inc(i);
          if i > tokenBufferLength then
          begin
            TokenBuffer := TokenBuffer + StringOfChar(' ', TokenBufferLength);
            TokenBufferLength := TokenBufferLength * 2;
          end;
          TokenBuffer[i] := CurrentCharacter;
          GetCharacterNotEof;
        end;
        GetCharacter;

        StringValue := Copy(TokenBuffer, 1, i);
      end;
      else
        raise EBoldBLDParseError.CreateFmt(sBadCharacter, [IntToStr(ord(CurrentCharacter))])
  end;
end;

procedure TReader.Eat(t: BLDTokenKind);
begin
  if CurrentToken.Kind = t then
    GetToken
  else
    raise EBoldBLDParseError.Create(sSyntaxError);
end;

procedure TReader.EatKeyword(const keyw: string);
begin
  if not TryKeyword(keyw) then
    raise EBoldBLDParseError.CreateFmt(sAKeywordExpected, [keyw]);
end;

procedure TReader.EatStartBlock(const keyw: string);
begin
  Eat(LPAR);
  EatKeyword(keyw);
end;

function TReader.TryKeyword(const keyw: string): Boolean;
begin
  if (CurrentToken.Kind = KEYWORD) and (CurrentToken.StringValue = keyw) then
  begin
    GetToken;
    Result := True;
  end
  else
    Result := False;
end;

function TReader.GetQuotedString: string;
begin
  Result := CurrentToken.StringValue;
  if CurrentToken.Kind = QSTRING then
    GetToken
  else
    raise EBoldBLDParseError.Create(sQuotedStringExpected);
end;

function TReader.GetKeyword: string;
begin
  Result := CurrentToken.StringValue;
  if CurrentToken.Kind = KEYWORD then
    GetToken
  else
    raise EBoldBLDParseError.Create(sKeyWordTokenExpected);
end;

function TReader.GetInteger: integer;
begin
  Result := CurrentToken.IntegerValue;
  if CurrentToken.Kind = INTVALUE then
    GetToken
  else
    raise EBoldBLDParseError.Create(sIntegerExpected);
end;

function TReader.GetBoolean: Boolean;
begin
  Result := False;
  if (CurrentToken.Kind = KEYWORD) and (CurrentToken.StringValue = 'TRUE') then // do not localize
    Result := True
  else
  if (CurrentToken.Kind <> KEYWORD) or (CurrentToken.StringValue <> 'FALSE') then // do not localize
    raise EBoldBLDParseError.Create(sBooleanExpected);
  GetToken;
end;

function TReader.GetClassReference: TMoldClass;
var
  name: string;
begin
  name := GetQuotedString;
  if name = '<NONE>' then // do not localize
    Result := nil
  else
    Result := CurrentModel.GetClassByName(name);
end;

{---TModel---}
class procedure TModel.Write(element: TMoldElement; w: TWriter);
var
  model: TMoldModel;
begin
  model := element as TMoldModel;
  w.PutStartBlock('Model'); // do not localize
  w.PutQuotedString(Model.Name);
  { version 11 }
  w.PutQuotedString(Model.RootClass.Name);
  { version 16 }
  w.PutQuotedString(Model.Stereotype);
  w.PutQuotedString(Model.Constraints.CommaText);
  { version 17 }
  w.PutQuotedString(Model.NonDefaultTaggedValuesCommaText);

  w.PutStartBlock('Classes'); // do not localize
  TElementList.Write(Model.Classes, w);
  w.PutEndBlock;
  w.PutStartBlock('Associations'); // do not localize
  TElementList.Write(Model.Associations, w);
  w.PutEndBlock;
  w.PutEndBlock;
end;

class Procedure TModel.Read(element: TMoldElement; r: TReader);
var
  model: TMoldModel;
begin
  model := element as TMoldModel;
  inherited read(element, r);
  if r.FormatVersion >= 6 then
  begin
    if r.formatVersion < 19 then model.BoldTVByName[TAG_INTERFACEUSES] := r.GetQuotedString;
    if r.formatVersion < 19 then model.BoldTVByName[TAG_IMPLEMENTATIONUSES] := r.GetQuotedString;
  end;

  if r.FormatVersion >= 11 then
    model.RootClass.Name := r.GetQuotedString;

  if r.FormatVersion >= 12 then
    if r.formatVersion < 19 then Model.BoldTVByName[TAG_PMAPPERNAME] := r.GetQuotedString;

  if r.FormatVersion >= 13 then
    if r.formatVersion < 19 then r.GetQuotedString; // legacy, was Model.DatabaseName

  if r.FormatVersion >= 15 then
  begin
    if r.formatVersion < 19 then Model.BoldTVByName[TAG_USEXFILES] := BooleanToString(r.GetBoolean);
    if r.formatVersion < 19 then Model.BoldTVByName[TAG_USETIMESTAMP] := BooleanToString(r.GetBoolean);
    if r.formatVersion < 19 then Model.BoldTVByName[TAG_USEGLOBALID] := BooleanToString(r.GetBoolean);
    if r.formatVersion < 19 then Model.BoldTVByName[TAG_USEREADONLY] := BooleanToString(r.GetBoolean);
  end;

  if r.FormatVersion >= 16 then
  begin
    Model.Stereotype := r.GetQuotedString;
    Model.Constraints.CommaText := r.GetQuotedString;
  end;

  if r.FormatVersion >= 17 then
  begin
    Model.NonDefaultTaggedValuesCommaText := r.GetQuotedString;
  end;

  {get classes}
  r.EatStartBlock('Classes'); // do not localize
  r.CurrentModel := model;
  TElementList.Read(model, r);
  r.Eat(RPAR);

  r.EatStartBlock('Associations'); // do not localize
  TElementList.Read(model, r);
  r.Eat(RPAR);
  r.Eat(RPAR);

  r.Eat(EndOfFile);
end;

{---TElement---}
class procedure TElement.Write(element: TMoldElement; w: TWriter);
begin
  w.PutQuotedString(element.Name);
end;

class procedure TElement.Read(element: TMoldElement; r: TReader);
begin
  // Do nothing
end;

class function TElement.CreateAndRead(parent: TMoldElement; r: TReader): TMoldElement;
var
  typeinfo: TElementClassRecord;
  Name: string;
begin
  r.Eat(LPAR);
  typeInfo := TypeTableByName(r.GetKeyword);
  Name := r.GetQuotedString;
  if typeInfo.MoldElementClass = TMoldClass then
    Result := (parent as TMoldModel).GetClassByName(Name)
  else
    Result := typeInfo.moldElementClass.Create(parent, Name);
  typeInfo.elementClass.read(Result, r);
end;

{---TElementList---}
class procedure TElementList.Write(list: TMoldElementList;w: TWriter);
var
  i: integer;
begin
  for i := 0 to list.Count-1 do
    TElementClass(TypeTableByMoldElement(list[i]).elementClass).write(list[i], w);
end;

class procedure TElementList.Read(parent: TMoldElement; r: TReader);
begin
  while r.CurrentToken.Kind = LPAR do
    TElement.CreateAndRead(parent, r);
end;

{---TPClass---}
class procedure TPClass.Write(element: TMoldElement; w: TWriter);
begin
  with element as TMoldClass do
  begin
    w.PutStartBlock('Class'); // do not localize
    inherited write(element, w);
    w.PutClassReference(SuperClass);

    { version 3 }
    w.PutBoolean(Persistent);
    w.PutBoolean(IsAbstract);
    { version 16 }
    w.PutQuotedString(Stereotype);
    w.PutQuotedString(Constraints.CommaText);
    { version 17 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);

    w.PutStartBlock('Attributes'); // do not localize
    TElementList.Write(Attributes, w);
    w.PutEndBlock;
    w.PutStartBlock('Methods'); // do not localize
    TElementList.Write(Methods, w);
    w.PutEndBlock;
    w.PutEndBlock;
  end;
end;

class procedure TPClass.Read(element: TMoldElement; r: TReader);
begin
  with element as TMoldClass do
  begin
    // note special handling for classes    inherited read(element, r);
    if r.formatVersion < 19 then BoldTVByName[TAG_TABLENAME] := r.GetQuotedString;
    if r.formatVersion < 19 then BoldTVByName[TAG_TABLEMAPPING] := TBoldTaggedValueSupport.TableMappingToString(TTableMapping(r.GetInteger));
    if r.formatVersion < 19 then r.GetInteger; // used to be classid, read for backward comaptibility
    SuperClass := r.GetClassReference;
    // for older versions superclass may be nil but should be RootClass
    if (not Assigned(SuperClass)) and
       (Element <> Model.RootClass) then
      SuperClass := Model.RootClass;

    if r.FormatVersion >= 3 then
    begin
      persistent := r.GetBoolean;
      IsAbstract := r.GetBoolean;
      if r.formatVersion < 19 then BoldTVByName[TAG_IMPORTED] := BooleanToString(r.GetBoolean);
      if r.formatVersion < 19 then BoldTVByName[TAG_DELPHINAME] := r.GetQuotedString;
       if r.FormatVersion >= 7 then
         if r.formatVersion < 19 then BoldTVByName[TAG_EXPRESSIONNAME] := r.GetQuotedString;
    end
    else
    begin
       BoldTVByName[TAG_DELPHINAME] := TableName;
    end;

    if r.FormatVersion >= 5 then
      if r.formatVersion < 19 then BoldTVByName[TAG_INCFILENAME] := r.GetQuotedString;

    if r.FormatVersion >= 12 then
      if r.formatVersion < 19 then BoldTVByName[TAG_PMAPPERNAME] := r.GetQuotedString;

    if r.FormatVersion >= 16 then
    begin
      Stereotype := r.GetQuotedString;
      Constraints.CommaText := r.GetQuotedString;
    end;

    if r.FormatVersion >= 17 then
    begin
      NonDefaultTaggedValuesCommaText := r.GetQuotedString;
    end;

    r.EatStartBlock('Attributes'); // do not localize
    TElementList.Read(element, r);
    r.Eat(RPAR);

    r.EatStartBlock('Methods'); // do not localize
    TElementList.Read(element, r);
    r.Eat(RPAR);
    r.Eat(RPAR);
  end;
end;

{---TAttribute---}
class procedure TAttribute.Write(element: TMoldElement; w: TWriter);
begin
  with Element as TMoldAttribute do
  begin
    w.PutStartBlock('Attribute'); // do not localize
    inherited write(element, w);
    w.PutQuotedString(BoldType);
    { version 3 }
    w.PutBoolean(Derived);
    { version 16 }
    w.PutQuotedString(Stereotype);
    w.PutQuotedString(Constraints.CommaText);
    w.PutInteger(Ord(Visibility));
    w.PutQuotedString(InitialValue);
    { version 17 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);
    { version 18 }
    w.PutEndBlock;
  end;
end;

class procedure TAttribute.Read(element: TMoldElement; r: TReader);
begin
  with Element as TMoldAttribute do
  begin
    inherited read(element, r);
    if r.formatVersion < 19 then BoldTVByName[TAG_COLUMNNAME] := r.GetQuotedString;
    BoldType := r.GetQuotedString;

    if r.FormatVersion >= 3 then
    begin
      if r.formatVersion < 19 then BoldTVByName[TAG_ALLOWNULL] := BooleanToString(r.GetBoolean);
      if r.formatVersion < 19 then
      begin
        if r.GetBoolean then
          StdTVByName[TAG_PERSISTENCE] := TV_PERSISTENCE_PERSISTENT
        else
          StdTVByName[TAG_PERSISTENCE] := TV_PERSISTENCE_TRANSIENT;
      end;
      Derived := r.GetBoolean;
      if r.FormatVersion >= 8 then
      	if r.formatVersion < 19 then BoldTVByName[TAG_DELAYEDFETCH] := BooleanToString(r.GetBoolean);
      if r.formatVersion < 19 then BoldTVByName[TAG_Length] := IntToStr(r.GetInteger);
      if r.formatVersion < 19 then BoldTVByName[TAG_DELPHINAME] := r.GetQuotedString;
      if r.FormatVersion >= 10 then
        if r.formatVersion < 19 then BoldTVByName[TAG_PMAPPERNAME] := r.GetQuotedString;

      if r.FormatVersion = 15 then
      begin
        r.GetBoolean;
        r.GetBoolean;
        r.GetBoolean;
        r.GetInteger;
      end;

      if r.FormatVersion >= 16 then
      begin
        if r.formatVersion < 19 then BoldTVByName[TAG_ATTRIBUTEKIND] := TBoldTaggedValueSupport.AttributeKindToString(TBoldAttributeKind(r.GetInteger));
        Stereotype := r.GetQuotedString;
        Constraints.CommaText := r.GetQuotedString;
        Visibility := TVisibilityKind(r.GetInteger);
        if r.formatVersion < 19 then {DelphiField := }r.GetQuotedString; // removed in version 18
        if r.formatVersion < 19 then BoldTVByName[TAG_DPREAD] := TBoldTaggedValueSupport.DelphiPropertyAccessKindToString(TDelphiPropertyAccessKind(r.GetInteger));
        if r.formatVersion < 19 then BoldTVByName[TAG_DPWRITE] := TBoldTaggedValueSupport.DelphiPropertyAccessKindToString(TDelphiPropertyAccessKind(r.GetInteger));
        if r.formatVersion < 19 then {DerivationOCL := }r.GetQuotedString;
        InitialValue := r.GetQuotedString;
      end
      else
        Visibility := vkPublic;


      if r.FormatVersion >= 17 then
      begin
        NonDefaultTaggedValuesCommaText := r.GetQuotedString;
      end;

      if r.FormatVersion >= 18 then
        if r.formatVersion < 19 then BoldTVByName[TAG_DELPHIFIELD] := BooleanToString(r.GetBoolean);
    end
    else
    begin
      // FIXME: Jan, ta ställning till vad de andra ska bli /fredrik 96-11-04
      BoldTVByName[TAG_DELPHINAME] := ColumnName;
    end;
    r.Eat(RPAR);
  end;
end;

{---TMethod---}
class procedure TMethod.Write(element: TMoldElement; w: TWriter);
begin
  with element as TMoldMethod do
  begin
    w.PutStartBlock('Method'); // do not localize
    inherited write(element, w);
    w.PutQuotedString(Signature);
    w.PutBoolean(IsClassMethod);
    { version 3 }
    w.PutQuotedString(ReturnType);
    { version 16 }
    w.PutQuotedString(Stereotype);
    w.PutInteger(Ord(Visibility));
    w.PutQuotedString(Constraints.CommaText);
    { version 17 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);

    w.PutEndBlock;
  end;
end;

class procedure TMethod.Read(element: TMoldElement; r: TReader);
begin
  with element as TMoldMethod do
  begin
    inherited read(element, r);
    Signature := r.GetQuotedString;
    if r.formatVersion < 19 then r.GetBoolean; {Ignore, used to be isfuncion}
    IsClassMethod := r.GetBoolean;

    if r.FormatVersion >= 3 then
    begin
      ReturnType  := r.GetQuotedString;
      if r.formatVersion < 19 then BoldTVByName[TAG_DELPHIOPERATIONKIND] := TBoldTaggedValueSupport.DelphiFunctionTypeToString(TDelphiFunctionType(r.GetInteger));
      if r.formatVersion < 19 then BoldTVByName[TAG_DELPHINAME]  := r.GetQuotedString;
    end;

    if r.FormatVersion >= 16 then
    begin
      Stereotype := r.GetQuotedString;
      Visibility := TVisibilityKind(r.GetInteger);
      Constraints.CommaText := r.GetQuotedString;
    end
    else
      Visibility := vkPublic;

    if r.FormatVersion >= 17 then
    begin
      NonDefaultTaggedValuesCommaText := r.GetQuotedString;
    end;

    if r.FormatVersion >= 18 then
      if r.formatVersion < 19 then BoldTVByName[TAG_OVERRIDEINALLSUBCLASSES] := BooleanToString(r.GetBoolean);
    r.Eat(RPAR);
  end;
end;

{---TRole---}
class procedure TRole.Write(element: TMoldElement; w: TWriter);
begin
  with element as TMoldRole do
  begin
    w.PutStartBlock('Role'); // do not localize
    inherited write(element, w);
    w.PutBoolean(Navigable);
    { version 2 }
    w.PutBoolean(Ordered);
    w.PutClassReference(MoldClass);
    { version 16 }
    w.PutQuotedString(Stereotype);
    w.PutQuotedString(Multiplicity);
    w.PutQuotedString(Constraints.CommaText);
    w.PutInteger(Ord(Aggregation));
    w.PutInteger(Ord(Visibility));
    w.PutInteger(Ord(Changeability));
    { version 17 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);
    { version 13}
    w.PutStartBlock('Qualifiers'); // do not localize
    TElementList.Write(Qualifiers, w);
    w.PutEndBlock;
    w.PutEndBlock;
  end;
end;

class procedure TRole.Read(element: TMoldElement;r: TReader);
begin
  with element as TMoldRole do
  begin
    inherited read(element, r);
    if r.formatVersion < 19 then BoldTVByName[TAG_COLUMNNAME] := r.GetQuotedString;
    if r.FormatVersion < 16 then
      Multiplicity := BooleanToMultiplicityString(r.GetBoolean);
    Navigable   := r.GetBoolean;

    if r.FormatVersion >= 2 then
      Ordered := r.GetBoolean
    else
      Ordered := Multi and Navigable;

    MoldClass := r.GetClassReference;

    if r.FormatVersion >= 3 then
    begin
      if r.formatVersion < 19 then BoldTVByName[TAG_DELPHINAME] := r.GetQuotedString
    end
    else
      BoldTVByName[TAG_DELPHINAME] := ColumnName;

    if r.FormatVersion >= 4 then
      if r.formatVersion < 19 then r.GetBoolean; // legacy, was Mandatory

    if r.formatVersion < 19 then BoldTVByName[TAG_EMBED] := BooleanToString((r.FormatVersion < 9) or   { Embed introduced in v9, default True }
             r.GetBoolean);

    if r.FormatVersion >= 12 then
      if r.formatVersion < 19 then BoldTVByName[TAG_PMAPPERNAME] := r.GetQuotedString;

    if r.FormatVersion >= 16 then
    begin
      Stereotype := r.GetQuotedString;
      Multiplicity := r.GetQuotedString;
      Constraints.CommaText := r.GetQuotedString;
      Aggregation := TAggregationKind(r.GetInteger);
      Visibility := TVisibilityKind(r.GetInteger);
      Changeability := TChangeableKind(r.GetInteger);
    end
    else
      Visibility := vkPublic;

    if r.FormatVersion >= 17 then
    begin
      NonDefaultTaggedValuesCommaText := r.GetQuotedString;
    end;

    if r.FormatVersion >= 14 then
    begin
      r.EatStartBlock('Qualifiers'); // do not localize
      TElementList.Read(element, r);
      r.Eat(RPAR);
    end;

    r.Eat(RPAR);
  end;
end;

{---TQualifier---}
class procedure TQualifier.Write(element: TMoldElement; w: TWriter);
begin
  with Element as TMoldQualifier do
  begin
    w.PutStartBlock('Qualifier'); // do not localize
    inherited write(element, w);
    w.PutQuotedString(BoldType);
    { version 19 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);
    w.PutEndBlock;
  end;
end;

class procedure TQualifier.Read(element: TMoldElement; r: TReader);
begin
  with Element as TMoldQualifier do
  begin
    inherited read(element, r);
    BoldType := r.GetQuotedString;
    if r.formatVersion < 19 then BoldTVByName[TAG_DELPHINAME] := r.GetQuotedString;

    if r.FormatVersion >= 19 then
      NonDefaultTaggedValuesCommaText := r.GetQuotedString;

    r.Eat(RPAR);
  end;
end;

{---TAssociation---}
class procedure TAssociation.Write(element: TMoldElement;w: TWriter);
begin
  with element as TMoldAssociation do
  begin
    w.PutStartBlock('Association'); // do not localize
    inherited write(element, w);
    w.PutClassReference(LinkClass);
    { version 16 }
    w.PutQuotedString(Stereotype);
    w.PutQuotedString(Constraints.CommaText);
    { version 17 }
    w.PutQuotedString(NonDefaultTaggedValuesCommaText);
    { version 19 }
    w.PutBoolean(Derived);

    w.PutStartBlock('Roles'); // do not localize
    TElementList.Write(Roles, w);
    w.PutEndBlock;
    w.PutEndBlock;
  end;
end;

class procedure TAssociation.Read(element: TMoldElement;r: TReader);
begin
  with element as TMoldAssociation do
  begin
    inherited read(element, r);
    if r.FormatVersion >= 2 then
    begin
      LinkClass := r.GetClassReference;
    end;
    if r.FormatVersion >= 16 then
    begin
      Stereotype := r.GetQuotedString;
      Constraints.CommaText := r.GetQuotedString;
    end;
    // support for older versions that did not store "Persistent" for associations
    StdTVByName[TAG_PERSISTENCE] := TV_PERSISTENCE_PERSISTENT;
    if r.FormatVersion >= 17 then
    begin
      NonDefaultTaggedValuesCommaText := r.GetQuotedString;
    end;
    // Get aRole list

    if r.FormatVersion >= 19 then
      Derived := r.GetBoolean;

    r.EatStartBlock('Roles'); // do not localize
    TElementList.Read(element, r);
    r.Eat(RPAR);
    r.Eat(RPAR);
  end;
end;

type
  typetableIndex = 0..6;
var
  TypeTable: array [typetableIndex] of TElementClassRecord =
    (
     (elementClass: TModel;       moldElementClass: TMoldModel;       name: 'Model'), // do not localize
     (elementClass: TPClass;      moldElementClass: TMoldClass;       name: 'Class'), // do not localize
     (elementClass: TAttribute;   moldElementClass: TMoldAttribute;   name: 'Attribute'), // do not localize
     (elementClass: TMethod;      moldElementClass: TMoldMethod;      name: 'Method'), // do not localize
     (elementClass: TAssociation; moldElementClass: TMoldAssociation; name: 'Association'), // do not localize
     (elementClass: TRole;        moldElementClass: TMoldRole;        name: 'Role'), // do not localize
     (elementClass: TQualifier;   moldElementClass: TMoldQualifier;   name: 'Qualifier') // do not localize
   );

function TypeTableByMoldElement(moldElement: TMoldElement): TElementClassRecord;
var
  i: integer;
begin
  for i := Low(Typetable) to High(typetable) do
    if moldElement is typetable[i].moldElementClass then
    begin
      Result := typetable[i];
      Exit;
    end;
  raise EBoldInternal.Create('TypeTableByMoldElement: moldElementClass not found in typetable'); // do not localize
end;

function TypeTableByName(name: string): TElementClassRecord;
var
  i: integer;
begin
  for i := Low(Typetable) to High(typetable) do
    if typetable[i].name = name then
    begin
      Result := typetable[i];
      Exit;
    end;
  raise EBoldInternal.CreateFmt('TypeTableByName: %s not found in typetable', [name]); // do not localize
end;

function BooleanToMultiplicityString(Value: Boolean): String;
begin
  if Value then
    Result := '*'
  else
    Result := '1';
end;

end.
