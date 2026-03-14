unit BoldXMLStreaming;

interface

uses
  MSXML_TLB,
  BoldBase,
  BoldStreams,
  BoldIndexableList,
  classes;

const
  BoldSOAPNullAttributeName = 'xsi:null';
  BoldSOAPTypeAttributeName = 'xsi:type';

type
  { forward declarations }
  TBoldXMLStreamerRegistry = class;
  TBoldXMLStreamManager = class;
  TBoldXMLStreamer = class;
  TBoldXMLStreamerList = class;
  TBoldXMLObjectStreamer = class;
  TBoldXMLInterfaceStreamer = class;
  TBoldXMLNode = class;
  TBoldXMLStreamStateManager = class;

  { TBoldXMLStreamStateManager }
  TBoldXMLStreamStateManager = class(TBoldMemoryManagedObject)
  private
    fStateObjectList: TStringList;
    function GetEmpty: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObjectByName(const Name: String; Obj: TObject);
    procedure RemoveObjectByName(const Name: String);
    function GetObjectByName(const Name: String): TObject;
    property Empty: Boolean read GetEmpty;
  end;

  { TBoldXMLNode }
  TBoldXMLNode = class(TBoldMemoryManagedObject)
  private
    fNode: IXMLDOMElement;
    fManager: TBoldXMLStreamManager;
    fStateManager: TBoldXMLStreamStateManager;
    procedure EnsureType(const DynamicStreamName, StaticStreamName: string);
    function GetType(const StaticStreamName: string): string;
    function GetDocument: IXMLDOMDocument;
    property Document: IXMLDOMDocument read GetDocument;
    function GetAccessor: string;
    function GetIsNull: Boolean;
  public
    constructor Create(Manager: TBoldXMLStreamManager; Node: IXMLDomElement; StreamStateManager: TBoldXMLStreamStateManager);
    function GetSubNode(const Accessor: string): TBoldXMLNode;
    function IsEmpty: Boolean;
    function NewSubNode(const Accessor: string): TBoldXMLNode;
    function MakeNodeForElement(Element: IXMLDOMElement): TBoldXMLNode;
    function ReadBoolean: Boolean;
    procedure ReadInterface(const StreamName: string; Item: IBoldStreamable);
    function ReadInteger: Integer;
    function ReadObject(const StreamName: string): TObject;
    function ReadString: string;
    function ReadFloat: Double;
    function ReadCurrency: Currency;
    function ReadSubNodeBoolean(const Accessor: string): Boolean;
    function ReadSubNodeInteger(const Accessor: string): Integer;
    function ReadSubNodeObject(const Accessor: string; const StreamName: string): TObject;
    function ReadSubNodeString(const Accessor: string): String; //The caller of this function should take care of freeing the result object
    function ReadSubNodeFloat(const Accessor: string): Double;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteInteger(Value: Integer);
    procedure WriteInterface(const StaticStreamName: string; Item: IBoldStreamable);
    procedure WriteObject(const StaticStreamName: string; Obj: TBoldInterfacedObject);
    procedure WriteString(const Value: string);
    procedure WriteFloat(value: Double);
    procedure WriteCurrency(value: Currency);
    procedure WriteSubNodeBoolean(const Accessor: string; Value: Boolean);
    procedure WriteSubNodeInteger(const Accessor: string; Value: Integer);
    procedure WriteSubNodeObject(const Accessor: string; const StaticStreamName: string; Obj: TBoldInterfacedObject);
    procedure WriteSubNodeString(const Accessor: string; const Value: String);
    procedure WriteSubNodeFloat(const Accessor: string; Value: Double);

    function ReadTime: TDateTime;
    function ReadSubNodeTime(const Accessor: string): TDateTime;
    procedure WriteTime(Value: TDateTime);
    procedure WriteSubNodeTime(const Accessor: string; Value: TDateTime);

    function ReadDate: TDateTime;
    function ReadSubNodeDate(const Accessor: string): TDateTime;
    procedure WriteDate(Value: TDateTime);
    procedure WriteSubNodeDate(const Accessor: string; Value: TDateTime);

    function ReadDateTime: TDateTime;
    function ReadSubNodeDateTime(const Accessor: string): TDateTime;
    procedure WriteDateTime(Value: TDateTime);
    procedure WriteSubNodeDateTime(const Accessor: string; Value: TDateTime);

    function ReadData: string;
    function ReadSubNodeData(const Accessor: string): string;
    procedure WriteData(Value: string);
    procedure WriteSubNodeData(const Accessor: string; const Value: string);

    procedure AddStateObject(const Name: string; StateObject: TObject);
    procedure RemoveStateObject(const Name: string);
    function GetStateObject(const Name: String): TObject;
    property Manager: TBoldXMLStreamManager read fManager;
    property XMLDomElement: IXMLDOMElement read fNode;
    property Accessor: string read GetAccessor;
    property IsNull: Boolean read GetIsNull;
    procedure SetToNull;
  end;

  { TBoldXMLStreamerRegistry }
  TBoldXMLStreamerRegistry = class(TBoldMemoryManagedObject)
  private
    fStreamers: TBoldXMLStreamerList;
    fParentRegistry: TBoldXMLStreamerRegistry;
    function GetStreamer(const Name: string): TBoldXMLStreamer;
    function GetObjectStreamer(const Name: string): TBoldXMLObjectStreamer;
    function GetInterfaceStreamer(const Name: string): TBoldXMLInterfaceStreamer;
  public
    constructor Create(Parent: TBoldXMLStreamerRegistry);
    destructor Destroy; override;
    procedure RegisterStreamer(Streamer: TBoldXMLStreamer);
    class function MainStreamerRegistry: TBoldXMLStreamerRegistry;
  end;

  { TBoldXMLStreamManager }
  TBoldXMLStreamManager = class(TBoldMemoryManagedObject)
  private
    fRegistry: TBoldXMLStreamerRegistry;
  public
    constructor Create(Registry: TBoldXMLStreamerRegistry);
    function GetRootNode(Document: TDomDocument; const Accessor: string): TBoldXMLNode;
    function NewRootNode(Document: TDomDocument; const Accessor: string): TBoldXMLNode;
    function GetSOAP(Document: TDomDocument): TBoldXMLNode;
    function NewSOAP(Document: TDomDocument): TBoldXMLNode;
    property Registry: TBoldXMLStreamerRegistry read fRegistry;
  end;

  { TBoldXMLStreamer }
  TBoldXMLStreamer = class(TBoldMemoryManagedObject)
  protected
    function GetStreamName: string; virtual; abstract;
  public
    property StreamName: string read GetStreamName;
  end;

  { TBoldXMLStreamerList }
  TBoldXMLStreamerList = class(TBoldIndexableList)
  private
    function GetStreamerByName(const Name: string): TBoldXMLStreamer;
  public
    constructor Create;
    property StreamerByName[const Name: string]: TBoldXMLStreamer read GetStreamerByName;
  end;

  { TBoldXMLObjectStreamer }
  TBoldXMLObjectStreamer = class(TBoldXMLStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); virtual;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); virtual;
    function CreateObject: TObject; virtual; abstract;
  end;

  { TBoldXMLInterfaceStreamer }
  TBoldXMLInterfaceStreamer = class(TBoldXMLStreamer)
  public
    procedure WriteInterface(Item: IBoldStreamable; Node: TBoldXMLNode); virtual;
    procedure ReadInterface(Item: IBoldStreamable; Node: TBoldXMLNode); virtual;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldHashIndexes,
  BoldBase64,
  BoldCommonConst;

const
  BoldNodeName_Year = 'Year';
  BoldNodeName_Month = 'Month';
  BoldNodeName_Day = 'Day';
  BoldNodeName_Hour = 'Hour';
  BoldNodeName_Min = 'Min';
  BoldNodeName_Sec = 'Sec';
  BoldNodeName_MSec = 'MSec';

type
  TBoldXMLStreamerIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

var
  G_MainRegistry: TBoldXMLStreamerRegistry = nil;
  IX_XMLStreamerName: integer = -1;

  oldThousandSeparator: char = '*';
  oldDecimalSeparator: char = '#';
  FloatSettingsPushed: Boolean = false;

procedure PushFloatSettings;
begin
  if FloatSettingsPushed then
    raise EBold.Create(sCannotNestPushFloat);
  FloatSettingsPushed := true;
  oldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  oldThousandSeparator := FormatSettings.ThousandSeparator;
  FormatSettings.ThousandSeparator := ',';
end;

procedure PopFloatSettings;
begin
  if not FloatSettingsPushed then
    raise EBold.Create(sPushNestMismatch);
  FormatSettings.DecimalSeparator := oldDecimalSeparator;
  FormatSettings.ThousandSeparator := oldThousandSeparator;
  oldDecimalSeparator := '*';
  oldThousandSeparator := '#';
  FloatSettingsPushed := false;
end;

{ TBoldXMLStreamerManager }

constructor TBoldXMLStreamerRegistry.Create(Parent: TBoldXMLStreamerRegistry);
begin
  fStreamers := TBoldXMLStreamerList.Create;
  fParentRegistry := Parent;
end;

destructor TBoldXMLStreamerRegistry.Destroy;
begin
  FreeAndNil(fStreamers);
  inherited;
end;

function TBoldXMLStreamerRegistry.GetInterfaceStreamer(const Name: string): TBoldXMLInterfaceStreamer;
begin
  result := GetStreamer(Name) as TBoldXMLInterfaceStreamer;
end;

function TBoldXMLStreamerRegistry.GetObjectStreamer(const Name: string): TBoldXMLObjectStreamer;
begin
  result := GetStreamer(Name) as TBoldXMLObjectStreamer;
end;

function TBoldXMLStreamerRegistry.GetStreamer(const Name: string): TBoldXMLStreamer;
begin
  result := fStreamers.StreamerByName[Name];
  if not assigned(result) and assigned(fParentRegistry) then
    result := fParentRegistry.GetStreamer(Name);

  if not assigned(result) then
    raise EBoldInternal.CreateFmt(sStreamerNotFound, [classname, name]);
end;

class function TBoldXMLStreamerRegistry.MainStreamerRegistry: TBoldXMLStreamerRegistry;
begin
  if not assigned(G_MainRegistry) then
    G_MainRegistry := TBoldXMLStreamerRegistry.Create(nil);
  result := G_MainRegistry;
end;

procedure TBoldXMLStreamerRegistry.RegisterStreamer(Streamer: TBoldXMLStreamer);
begin
  fStreamers.Add(Streamer);
end;

{ TBoldXMLObjectStreamer }

procedure TBoldXMLObjectStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLObjectStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
begin
end;

{ TBoldXMLNode }

procedure TBoldXMLNode.WriteObject(const StaticStreamName: string; Obj: TBoldInterfacedObject);
var
  DynamicStreamName: string;
begin
  if assigned(Obj) then
  begin
    DynamicStreamName := (Obj as IBoldStreamable).StreamName;
    EnsureType(DynamicStreamName, StaticStreamName);
    fManager.Registry.GetObjectStreamer(DynamicStreamName).WriteObject(Obj, self);
  end else
    SetToNull;
end;

function TBoldXMLNode.ReadObject(const StreamName: string): TObject;
var
  aStreamer: TBoldXMLObjectStreamer;
begin
  if IsNull then
    result := nil
  else
  begin
    aStreamer := fManager.Registry.GetObjectStreamer(GetType(StreamName));
    result := aStreamer.CreateObject;
    aStreamer.ReadObject(result, self);
  end;
end;

function TBoldXMLNode.GetSubNode(const Accessor: string): TBoldXMLNode;
var
  aList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  anElement: IXMLDOMElement;
begin
  // this sucks. We shouldn't have to iterate our selves, but using
  // GetElementsByTagName searches also lower levels than true child-nodes :-(
  result := nil;
  aList := fNode.childNodes;
  aNode := aList.nextNode;
  while assigned(aNode) do
  begin
    if aNode.nodeType = NODE_ELEMENT then
    begin
      anElement := aNode as IXMLDOMElement;
      if anElement.tagName = Accessor then
      begin
        result := TBoldXMLNode.Create(fManager, anElement, fStatemanager);
        exit;
      end;
    end;
    aNode := aList.nextNode;
  end;
end;

constructor TBoldXMLNode.Create(Manager: TBoldXMLStreamManager; Node: IXMLDomElement; StreamStateManager: TBoldXMLStreamStateManager);
begin
  fNode := Node;
  fManager := Manager;
  fStatemanager := StreamStateManager;
end;

function TBoldXMLNode.NewSubNode(const Accessor: string): TBoldXMLNode;
var
  aNode: IXMLDOMElement;
begin
  aNode := Document.createElement(Accessor);
  fNode.appendChild(aNode);
  result := TBoldXMLNode.Create(fManager, aNode, fStatemanager);
end;

procedure TBoldXMLNode.WriteString(const Value: string);
begin
  fNode.appendChild(Document.createTextNode(Value));
end;

function TBoldXMLNode.ReadString: string;
var
  aNode: IXMLDOMNode;
begin
  aNode := fNode.childNodes.nextNode;
  if assigned(aNode) and (aNode.nodeType = NODE_TEXT) then
    result := aNode.Text
  else
    result := '';
end;

function TBoldXMLNode.ReadBoolean: Boolean;
begin
  result := (ReadString = '1'); //do not localize
end;

procedure TBoldXMLNode.WriteBoolean(Value: Boolean);
begin
  if Value then
    WriteString('1') //do not localize
  else
    WriteString('0'); //do not localize
end;

function TBoldXMLNode.GetDocument: IXMLDOMDocument;
begin
  result := fNode.ownerDocument;
//  result := fManager.fDocument;
end;

function TBoldXMLNode.ReadInteger: Integer;
begin
  result := StrToIntDef(ReadString, 0);
end;

procedure TBoldXMLNode.WriteInteger(Value: Integer);
begin
  WriteString(IntToStr(Value));
end;

procedure TBoldXMLNode.WriteInterface(const StaticStreamName: string; Item: IBoldStreamable);
var
  DynamicStreamName: string;
begin
  DynamicStreamName := Item.StreamName;
  EnsureType(DynamicStreamName, StaticStreamName);
  fManager.Registry.GetInterfaceStreamer(DynamicStreamName).WriteInterface(Item, self);
end;

procedure TBoldXMLNode.EnsureType(const DynamicStreamName, StaticStreamName: string);
begin
  if DynamicStreamName <> StaticStreamName then
    fNode.setAttribute(BoldSOAPTypeAttributeName, DynamicStreamName);
end;

procedure TBoldXMLNode.ReadInterface(const StreamName: string; Item: IBoldStreamable);
var
  aStreamer: TBoldXMLInterfaceStreamer;
begin
  aStreamer := fManager.Registry.GetInterfaceStreamer(GetType(StreamName));
  aStreamer.ReadInterface(Item, self);
end;

function TBoldXMLNode.GetType(const StaticStreamName: string): string;
var
  anAttr: IXMLDOMAttribute;
begin
  anAttr := fNode.getAttributeNode(BoldSOAPTypeAttributeName);
  if assigned(anAttr) then
    result := anAttr.Value
  else
    result := StaticStreamName;
end;

function TBoldXMLNode.GetAccessor: string;
begin
  result := fNode.nodeName;
end;

function TBoldXMLNode.IsEmpty: Boolean;
begin
  result := not XMLDomElement.hasChildNodes;
end;

// The caller of this function should take care of freeing the returned object
function TBoldXMLNode.ReadSubNodeObject(const Accessor, StreamName: string): TObject;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadObject(StreamName);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeObject(const Accessor, StaticStreamName: string; Obj: TBoldInterfacedObject);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteObject(StaticStreamName, Obj);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeInteger(const Accessor: string; Value: Integer);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteInteger(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeBoolean(const Accessor: string; Value: Boolean);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteBoolean(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeString(const Accessor, Value: String);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteString(Value);
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadSubNodeInteger(const Accessor: String): Integer;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadInteger;
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadSubNodeBoolean(const Accessor: string): Boolean;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadBoolean;
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadSubNodeString(const Accessor: string): String;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadString;
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadFloat: Double;
begin
  try
    PushFloatSettings;
    try
      result := StrToFloat(fNode.text);
    except
      result := 0;
    end;
  finally
    PopFloatSettings;
  end;
end;

procedure TBoldXMLNode.WriteFloat(value: Double);
begin
  PushFloatSettings;
  WriteString(FloatToStr(Value));
  PopFloatSettings;
end;

function TBoldXMLNode.ReadSubNodeFloat(const Accessor: string): Double;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadFloat;
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeFloat(const Accessor: string; Value: Double);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteFloat(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.AddStateObject(const Name: string; StateObject: TObject);
begin
  if not assigned(fStateManager) then
    fStateManager := TBoldXMLStreamStateManager.Create;
  fStateManager.AddObjectByName(name, StateObject);
end;

function TBoldXMLNode.GetStateObject(const Name: String): TObject;
begin
  if assigned(fStateManager) then
    result := fStatemanager.GetObjectByName(Name)
  else
    result := nil;
end;

procedure TBoldXMLNode.RemoveStateObject(const Name: string);
begin
  if assigned(fStatemanager) then
  begin
    fStatemanager.RemoveObjectByName(name);
    if fStatemanager.Empty then
      FreeAndNil(fStateManager);
  end;
end;

function TBoldXMLNode.MakeNodeForElement(Element: IXMLDOMElement): TBoldXMLNode;
begin
  result := TBoldXMLNode.Create(Manager, Element, fStateManager);
end;

function TBoldXMLNode.ReadSubNodeDate(const Accessor: string): TDateTime;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadDate;
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadDate: TDateTime;
begin
  result := EncodeDate(ReadSubNodeInteger(BoldNodeName_Year),
                       ReadSubNodeInteger(BoldNodeName_Month),
                       ReadSubNodeInteger(BoldNodeName_Day));
end;

procedure TBoldXMLNode.WriteSubNodeDate(const Accessor: string; Value: TDateTime);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteDate(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteDate(Value: TDateTime);
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  WriteSubNodeInteger(BoldNodeName_Year, Year);
  WriteSubNodeInteger(BoldNodeName_Month, Month);
  WriteSubNodeInteger(BoldNodeName_Day, Day);
end;

function TBoldXMLNode.ReadSubNodeTime(const Accessor: string): TDateTime;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadTime;
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadTime: TDateTime;
begin
  result := EncodeTime(ReadSubNodeInteger(BoldNodeName_Hour),
                       ReadSubNodeInteger(BoldNodeName_Min),
                       ReadSubNodeInteger(BoldNodeName_Sec),
                       ReadSubNodeInteger(BoldNodeName_MSec));
end;

procedure TBoldXMLNode.WriteSubNodeTime(const Accessor: string; Value: TDateTime);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteTime(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteTime(Value: TDateTime);
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  WriteSubNodeInteger(BoldNodeName_Hour, Hour);
  WriteSubNodeInteger(BoldNodeName_Min, Min);
  WriteSubNodeInteger(BoldNodeName_Sec, Sec);
  WriteSubNodeInteger(BoldNodeName_MSec, MSec);
end;

function TBoldXMLNode.ReadDateTime: TDateTime;
begin
  result := ReadDate + ReadTime;
end;

function TBoldXMLNode.ReadSubNodeDateTime(const Accessor: string): TDateTime;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadDateTime;
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteDateTime(Value: TDateTime);
begin
  WriteDate(Value);
  WriteTime(Value);
end;

procedure TBoldXMLNode.WriteSubNodeDateTime(const Accessor: string;
  Value: TDateTime);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteDateTime(Value);
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteData(Value: string);

  function IncludesIllegalChar(Value: string): Boolean;
  var
    i: Integer;
  begin
    result := true;
    for i := 1 to Length(Value) do
      if not (Value[i] in [#9, BOLDLF, BOLDCR, #32..#255]) then
        exit;
    result := false;
  end;

var
  DataString: string;
  Encoder: TBase64;
begin
  if IncludesIllegalChar(Value) then
  begin
    Encoder := TBase64.Create;
    Encoder.EncodeData(Value, DataString);
    XMLDomElement.setAttribute('dt', 'binary.base64'); // do not localize
    Encoder.Free;
  end else
    DataString := Value;
  WriteString(DataString);
end;

function TBoldXMLNode.ReadData: string;
var
  anAttr: IXMLDOMAttribute;
  DataString: string;
  Decoder: TBase64;
begin
  DataString := ReadString;
  anAttr := fNode.getAttributeNode('dt'); // do not localize
  if assigned(anAttr) and (anAttr.Value = 'binary.base64') then // do not localize
  begin
    Decoder := TBase64.Create;
    Decoder.DecodeData(DataString, result);
    Decoder.Free;
  end else
    result := DataString;
end;

function TBoldXMLNode.GetIsNull: Boolean;
var
  anAttr: IXMLDOMAttribute;
begin
  anAttr := XMLDomElement.getAttributeNode(BoldSOAPNullAttributeName);
  result := assigned(anAttr) and (anAttr.Value = '1');
end;

procedure TBoldXMLNode.SetToNull;
begin
  XMLDomElement.setAttribute(BoldSOAPNullAttributeName, '1');
end;

function TBoldXMLNode.ReadSubNodeData(const Accessor: string): string;
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := GetSubNode(Accessor);
  try
    result := aSubNode.ReadData;
  finally
    aSubNode.Free;
  end;
end;

procedure TBoldXMLNode.WriteSubNodeData(const Accessor, Value: string);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := NewSubNode(Accessor);
  try
    aSubNode.WriteData(Value);
  finally
    aSubNode.Free;
  end;
end;

function TBoldXMLNode.ReadCurrency: Currency;
begin
  try
    PushFloatSettings;
    try
      result := StrToCurr(fNode.text);
    except
      result := 0;
    end;
  finally
    PopFloatSettings;
  end;
end;

procedure TBoldXMLNode.WriteCurrency(value: Currency);
begin
  PushFloatSettings;
  WriteString(CurrToStr(Value));
  PopFloatSettings;
end;

{ TBoldXMLInterfaceStreamer }

procedure TBoldXMLInterfaceStreamer.ReadInterface(Item: IBoldStreamable; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLInterfaceStreamer.WriteInterface(Item: IBoldStreamable; Node: TBoldXMLNode);
begin
end;

{ TBoldXMLStreamManager }

constructor TBoldXMLStreamManager.Create(Registry: TBoldXMLStreamerRegistry);
begin
  fRegistry := Registry;
end;

function TBoldXMLStreamManager.GetRootNode(Document: TDomDocument; const Accessor: string): TBoldXMLNode;
begin
  if not assigned(Document) then
    raise EBold.CreateFmt(sStreamerNotConnected, [classname, 'GetRootNode']); //do not localize
  if not assigned(Document.documentElement) then
    raise EBold.CreateFmt(sDocumentHasNoRootNode, [classname]);
  if (Accessor <> '') and (Document.documentElement.tagName <> Accessor) then
    raise EBold.CreateFmt(sWrongTagName,
                          [classname, Document.documentElement.tagName, Accessor]);

  result := TBoldXMLNode.Create(self, Document.documentElement, nil);
end;

function TBoldXMLStreamManager.GetSOAP(Document: TDomDocument): TBoldXMLNode;
var
  aNode: TBoldXMLNode;
begin
  aNode := GetRootNode(Document, 'SOAP-ENV:Envelope'); // do not localize
  result := aNode.GetSubNode('SOAP-ENV:Body'); // do not localize
  aNode.Free;
end;

function TBoldXMLStreamManager.NewRootNode(Document: TDomDocument; const Accessor: string): TBoldXMLNode;
begin
  if not assigned(Document) then
    raise EBold.CreateFmt(sStreamerNotConnected, [classname, 'NewRootNode']); // do not localize
  if assigned(Document.documentElement) then
    raise EBold.CreateFmt(sDocumentHasRootNode, [classname]);

  Document.documentElement := Document.createElement(Accessor);
  result := TBoldXMLNode.Create(self, Document.documentElement, nil);
  result.XMLDomElement.setAttribute('xmlns:xsi', 'http://www.w3.org/1999/XMLSchema-instance'); // do not localize
  result.XMLDomElement.setAttribute('xml:space', 'preserve'); // do not localize
end;

function TBoldXMLStreamManager.NewSOAP(Document: TDomDocument): TBoldXMLNode;
var
  aNode: TBoldXMLNode;
begin
  aNode := NewRootNode(Document, 'SOAP-ENV:Envelope'); // do not localize
  aNode.XMLDomElement.setAttribute('xmlns:SOAP-ENV', 'http://schemas.xmlsoap.org/soap/envelope/'); // do not localize
  aNode.XMLDomElement.setAttribute('SOAP-ENV:encodingStyle', 'http://schemas.xmlsoap.org/soap/encoding/'); // do not localize
  result := aNode.NewSubNode('SOAP-ENV:Body'); // do not localize
  aNode.Free;
end;

{ TBoldXMLStreamStateManager }

procedure TBoldXMLStreamStateManager.AddObjectByName(const Name: String; Obj: TObject);
begin
  assert(fStateObjectList.IndexOf(name) = -1, 'There is already a stateobject with the name '+name);
  fStateObjectList.AddObject(Name, Obj);
end;

constructor TBoldXMLStreamStateManager.create;
begin
  fStateObjectList := TStringList.Create;
  fStateObjectList.Sorted := true;
end;

destructor TBoldXMLStreamStateManager.Destroy;
begin
  freeAndNil(fStateObjectList);
  inherited;
end;

function TBoldXMLStreamStateManager.GetEmpty: Boolean;
begin
  result := fStateObjectList.Count = 0;
end;

function TBoldXMLStreamStateManager.GetObjectByName(const Name: String): TObject;
var
  pos: integer;
begin
  pos := fStateObjectList.IndexOf(Name);
  if pos <> -1 then
    result := fStateObjectList.Objects[Pos]
  else
    result := nil;
end;

procedure TBoldXMLStreamStateManager.RemoveObjectByName(const Name: String);
var
  pos: integer;
begin
  pos := fStateObjectList.IndexOf(Name);
  if pos <> -1 then
    fStateObjectList.Delete(Pos);
end;

{ TBoldXMLStreamerList }

constructor TBoldXMLStreamerList.Create;
begin
  inherited;
  SetIndexVariable(IX_XMLStreamerName, AddIndex(TBoldXMLStreamerIndex.Create));
end;

function TBoldXMLStreamerList.GetStreamerByName(const Name: string): TBoldXMLStreamer;
begin
  assert(Indexes[IX_XMLStreamerName] is TBoldXMLStreamerIndex, 'Wrong index type');
  result := TBoldStringHashIndex(Indexes[IX_XMLStreamerName]).FindByString(Name) as TBoldXMLStreamer;
end;

{ TBoldXMLStreamerIndex }

function TBoldXMLStreamerIndex.ItemASKeyString(Item: TObject): string;
begin
  assert(Item is TBoldXMLStreamer);
  result := TBoldXMLStreamer(Item).StreamName;
end;

initialization // empty

finalization
  FreeAndNil(G_MainRegistry);

end.
