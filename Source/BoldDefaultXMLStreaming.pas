{ Global compiler directives }
{$include bold.inc}
unit BoldDefaultXMLStreaming;

interface

uses
  BoldXMLStreaming,
  BoldStreams,
  BoldId,
  BoldDefs,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldIndexableList,
  BoldMeta;

type
  { forward declarations }
  TBoldDefaultXMLStreamerRegistry = class;
  TBoldXMLModelElementStreamer = class;
  TBoldXMLModelElementStreamerList = class;
  TBoldXMLMemberStreamer = class;
  TBoldXMLClassStreamer = class;
  TBoldDefaultXMLStreamManager = class;
  TBoldXMLValueStreamer = class;
  TBoldXMLNullableValueStreamer = class;
  TBoldXMLStringContentStreamer = class;
  TBoldXMLIntegerContentStreamer = class;
  TBoldXMLFloatContentStreamer = class;
  TBoldXMLCurrencyContentStreamer = class;
  TBoldXMLBooleanContentStreamer = class;
  TBoldXMLDateContentStreamer = class;
  TBoldXMLTimeContentStreamer = class;
  TBoldXMLDateTimeContentStreamer = class;
  TBoldXMLBlobContentStreamer = class;
  TBoldXMLTypedBlobStreamer = class;
  TBoldXMLRoleStreamer = class;
  TBoldXMLIdRefStreamer = class;
  TBoldXMLIdRefPairStreamer = class;
  TBoldXMLIdListRefStreamer = class;
  TBoldXMLIdListRefPairStreamer = class;

  { TBoldDefaultXMLStreamerRegistry }
  TBoldDefaultXMLStreamerRegistry = class(TBoldXMLStreamerRegistry)
  public
    class function MainStreamerRegistry: TBoldDefaultXMLStreamerRegistry;
  end;

  { TBoldXMLModelElementStreamer }
  TBoldXMLModelElementStreamer = class
  private
    fExpressionName: string;
  public
    property ExpressionName: string read fExpressionName;
  end;

  { TBoldXMLModelElementStreamerList }
  TBoldXMLModelElementStreamerList = class(TBoldIndexableList)
  private
    function GetStreamerByName(Name: string): TBoldXMLModelElementStreamer;
  public
    constructor Create;
    property StreamerByName[Name: string]: TBoldXMLModelElementStreamer read GetStreamerByName;
  end;

  { TBoldXMLMemberStreamer }
  TBoldXMLMemberStreamer = class(TBoldXMLModelElementStreamer)
  private
    fTypeStreamName: string;
    fIndex: Integer;
    fPersistent: Boolean;
    fOwner: TBoldXMLClassStreamer;
    fMemberId: TBoldMemberId;
  public
    constructor Create(MoldMember: TMoldMember; Owner: TBoldXMLClassStreamer);
    destructor Destroy; override;
    procedure WriteValue(Node: TBoldXMLNode; const Value: IBoldValue);
    procedure ReadValue(Node: TBoldXMLNode; const Value: IBoldValue);
    property TypeStreamName: string read fTypeStreamName;
    property Persistent: Boolean read fPersistent;
    property MemberId: TBoldMemberId read fMemberId;
    property Index: Integer read fIndex;
  end;

  { TBoldXMLClassStreamer }
  TBoldXMLClassStreamer = class(TBoldXMLModelElementStreamer)
  private
    fMemberStreamers: TBoldXMLModelElementStreamerList;
    fOwner: TBoldDefaultXMLStreamManager;
    function GetMemberStreamer(Index: Integer): TBoldXMLMemberStreamer;
    function GetMemberStreamerByName(const Name: string): TBoldXMLMemberStreamer;
  public
    constructor Create(MoldClass: TMoldClass; Owner: TBoldDefaultXMLStreamManager);
    destructor Destroy; override;
    procedure WriteObject(Node: TBoldXMLNode; const ObjectContents: IBoldObjectContents; ObjectId: TBoldObjectId; MemberIdList: TBoldMemberIdList);
    procedure ReadObject(Node: TBoldXMLNode; const ValueSpace: IBoldValueSpace);
    property MemberStreamers[Index: integer]: TBoldXMLMemberStreamer read GetMemberStreamer;
    property MemberStreamerByName[const Name: String]: TBoldXMLMemberStreamer read GetMemberStreamerByName;
  end;

  { TBoldDefaultXMLStreamManager }
  TBoldDefaultXMLStreamManager = class(TBoldXMLStreamManager)
  private
    fModel: TMoldModel;
    fClassStreamers: TBoldXMLModelElementStreamerList;
    fIgnorePersistenceState: Boolean;
    fPersistenceStatesToBeStreamed: TBoldValuePersistenceStateSet;
    fPersistenceStatesToOverwrite: TBoldValuePersistenceStateSet;
    function GetClassStreamer(TopSortedIndex: Integer): TBoldXMLClassStreamer;
    function GetClassStreamerByName(const Name: string): TBoldXMLClassStreamer;
  public
    constructor Create(Registry: TBoldXMLStreamerRegistry; Model: TMoldModel);
    destructor Destroy; override;
    procedure WriteValueSpace(const ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList; MemberIdList: TBoldMemberIdList; Node: TBoldXMLNode);
    procedure ReadValueSpace(const ValueSpace: IBoldValueSpace; Node: TBoldXMLNode);
    property Model: TMoldModel read fModel;
    property ClassStreamers[TopSortedIndex: Integer]: TBoldXMLClassStreamer read GetClassStreamer;
    property ClassStreamerByName[const Name: string]: TBoldXMLClassStreamer read GetClassStreamerByName;
    property IgnorePersistenceState: Boolean read fIgnorePersistenceState write fIgnorePersistenceState;
    property PersistenceStatesToOverwrite: TBoldValuePersistenceStateSet read fPersistenceStatesToOverwrite write fPersistenceStatesToOverwrite;
    property PersistenceStatesToBeStreamed: TBoldValuePersistenceStateSet read fPersistenceStatesToBeStreamed write fPersistenceStatesToBeStreamed;
  end;

  { TBoldXMLValueStreamer }
  TBoldXMLValueStreamer = class(TBoldXMLInterfaceStreamer)
  public
    procedure WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode); override;
    procedure ReadInterface(const Item: IBoldStreamable; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLNullableValueStreamer }
  TBoldXMLNullableValueStreamer = class(TBoldXMLValueStreamer)
  protected
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); virtual;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); virtual;
  public
    procedure WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode); override;
    procedure ReadInterface(const Item: IBoldStreamable; Node: TboldXMLNode); override;
  end;

  { TBoldXMLStringContentStreamer }
  TBoldXMLStringContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLIntegerContentStreamer }
  TBoldXMLIntegerContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLFloatContentStreamer }
  TBoldXMLFloatContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLCurrencyContentStreamer }
  TBoldXMLCurrencyContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLBooleanContentStreamer }
  TBoldXMLBooleanContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLDateContentStreamer }
  TBoldXMLDateContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLTimeContentStreamer }
  TBoldXMLTimeContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLDateTimeContentStreamer }
  TBoldXMLDateTimeContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLBlobContentStreamer }
  TBoldXMLBlobContentStreamer = class(TBoldXMLNullableValueStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLTypedBlobStreamer }
  TBoldXMLTypedBlobStreamer = class(TBoldXMLBlobContentStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLRoleStreamer }
  TBoldXMLRoleStreamer = class(TBoldXMLValueStreamer)
  protected
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); virtual;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); virtual;
  public
    procedure WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode); override;
    procedure ReadInterface(const Item: IBoldStreamable; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLIdRefStreamer }
  TBoldXMLIdRefStreamer = class(TBoldXMLRoleStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLIdRefPairStreamer }
  TBoldXMLIdRefPairStreamer = class(TBoldXMLRoleStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLIdListRefStreamer }
  TBoldXMLIdListRefStreamer = class(TBoldXMLRoleStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLIdListRefPairStreamer }
  TBoldXMLIdListRefPairStreamer = class(TBoldXMLRoleStreamer)
  protected
    function GetStreamName: string; override;
    procedure WriteContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
    procedure ReadContent(const Item: IBoldValue; Node: TBoldXMLNode); override;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldHashIndexes,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldDefaultStreamNames;

const
  BoldNodeName_persistencestate = 'persistencestate';
  BoldNodeName_existencestate = 'existencestate';
  BoldNodeName_timestamp = 'timestamp';
  BoldNodeName_globalid = 'globalid';
  BoldNodeName_members = 'members';
  BoldNodeName_content = 'content';
  BoldNodeName_ContentType = 'ContentType';
  BoldNodeName_id = 'id';
  BoldNodeName_id1 = 'id1';
  BoldNodeName_id2 = 'id2';
  BoldNodeName_idlist = 'idlist';
  BoldNodeName_idlist1 = 'idlist1';
  BoldNodeName_idlist2 = 'idlist2';
  BoldNodeName_OrderNo = 'OrderNo';

type
  TBoldExpressionNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

var
  G_MainRegistry: TBoldDefaultXMLStreamerRegistry = nil;
  IX_ExpressionNameIndex: integer = -1;

{ TBoldDefaultXMLStreamManager }

constructor TBoldDefaultXMLStreamManager.Create(Registry: TBoldXMLStreamerRegistry; Model: TMoldModel);
var
  i: Integer;
begin
  inherited Create(Registry);
  fModel := Model;
  fClassStreamers := TBoldXMLModelElementStreamerList.Create;
  fIgnorePersistenceState := True;
  fPersistenceStatesToBeStreamed := [bvpsModified];
  fPersistenceStatesToOverwrite := [bvpsInvalid];
  Model.EnsureTopSorted;
  for i := 0 to Model.Classes.Count - 1 do
    fClassStreamers.Add(TBoldXMLClassStreamer.Create(Model.Classes[i], self));
end;

destructor TBoldDefaultXMLStreamManager.Destroy;
begin
  FreeAndNil(fClassStreamers);
  inherited;
end;

function TBoldDefaultXMLStreamManager.GetClassStreamer(TopSortedIndex: Integer): TBoldXMLClassStreamer;
begin
  if TopSortedIndex >= fClassStreamers.Count then
    raise EBold.CreateFmt(sInvalidIndex, [classname]);

  result := TBoldXMLClassStreamer(fClassStreamers.Items[TopSortedIndex]);
end;

function TBoldDefaultXMLStreamManager.GetClassStreamerByName(const Name: string): TBoldXMLClassStreamer;
begin
  result := fClassStreamers.StreamerByName[Name] as TBoldXMLClassStreamer;

  if not assigned(result) then
    raise EBold.CreateFmt(sUnrecognizedClassName, [classname, 'GetClassStreamerByName', name]); // Do not localize
end;

procedure TBoldDefaultXMLStreamManager.ReadValueSpace(const ValueSpace: IBoldValueSpace; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
  {$IFDEF OXML}
  aNodeListEnumerator: TXMLChildNodeListEnumerator;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDomNodeList;
  aNode: IXMLDomNode;
  {$ENDIF}
begin
  if not assigned(ValueSpace) then begin
    exit;
  end;
  {$IFDEF OXML}
  aNodeListEnumerator := Node.XMLDomElement.ChildNodes.GetEnumerator;
  try
    while aNodeListEnumerator.MoveNext do
    begin
      aNode := aNodeListEnumerator.Current;
      aSubNode := TBoldXMLNode.Create(Node.Manager, aNode, nil);
      ClassStreamerByName[aSubNode.Accessor].ReadObject(aSubNode, ValueSpace);
      aSubNode.Free;
    end;
  finally
    aNodeListEnumerator.Free;
  end;
  {$ELSE}
  aNodeList := Node.XMLDomElement.childNodes;
  aNode := aNodeList.nextNode;
  while assigned(aNode) do begin
    aSubNode := TBoldXMLNode.Create(Node.Manager, aNode as IXMLDOMElement, nil);
    ClassStreamerByName[aSubNode.Accessor].ReadObject(aSubNode, ValueSpace);

    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
  {$ENDIF}
end;

procedure TBoldDefaultXMLStreamManager.WriteValueSpace(const ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList;
  MemberIdList: TBoldMemberIdList; Node: TBoldXMLNode);
var
  i: integer;
  anId: TBoldObjectId;
begin
  if not assigned(ValueSpace) then
    exit;
  for i := 0 to IdList.Count - 1 do
  begin
    anId := IdList[i];
    ClassStreamers[anId.TopSortedIndex].WriteObject(Node,
                                                    ValueSpace.ObjectContentsByObjectId[anId],
                                                    anId,
                                                    MemberIdList);
  end;
end;

{ TBoldXMLValueStreamer }

procedure TBoldXMLValueStreamer.ReadInterface(const Item: IBoldStreamable; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
begin
  inherited;
  if not (Node.Manager as TBoldDefaultXMLStreamManager).IgnorePersistenceState then
  begin
    SubNode := Node.GetSubNode(BoldNodeName_persistencestate);
    if Assigned(SubNode) then begin
      (Item as IBoldValue).BoldPersistenceState :=
          TBoldValuePersistenceState(SubNode.ReadInteger);
      SubNode.Free;
    end else begin
      // No node found -> set default BoldPersistenceState (bvpsCurrent)
      (Item as IBoldValue).BoldPersistenceState := bvpsCurrent;
    end;
  end;
end;

procedure TBoldXMLValueStreamer.WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  iPersistenceState: Integer;
begin
  inherited;
  iPersistenceState := Integer((Item as IBoldValue).BoldPersistenceState);
  // Only write BoldPersistenceState other than bvpsCurrent, to minimize XML size
  if iPersistenceState <> 0 then begin
    SubNode := Node.NewSubNode(BoldNodeName_persistencestate);
    SubNode.WriteInteger(iPersistenceState);
    SubNode.Free;
  end;
end;

{ TBoldXMLNullableValueStreamer }

procedure TBoldXMLNullableValueStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLNullableValueStreamer.ReadInterface(const Item: IBoldStreamable; Node: TboldXMLNode);
var
  ContentNode: TBoldXMLNode;
begin
  inherited;
  ContentNode := Node.GetSubNode(BoldNodeName_content);
  if ContentNode.IsNull then
    (Item as IBoldNullableValue).SetContentToNull
  else
    ReadContent(Item as IBoldValue, ContentNode);
  ContentNode.Free;
end;

procedure TBoldXMLNullableValueStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLNullableValueStreamer.WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode);
var
  ContentNode: TBoldXMLNode;
begin
  inherited;
  ContentNode := Node.NewSubNode(BoldNodeName_content);
  if not (Item as IBoldNullableValue).IsNull then
    WriteContent(Item as IBoldValue, ContentNode)
  else
    ContentNode.SetToNull;
  ContentNode.Free;
end;

{ TBoldXMLStringContentStreamer }

function TBoldXMLStringContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_String;
end;

procedure TBoldXMLStringContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  (Item as IBoldStringContent).asString := Node.ReadString;
end;

procedure TBoldXMLStringContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteString((Item as IBoldStringContent).asString);
end;

{ TBoldXMLIntegerContentStreamer }

function TBoldXMLIntegerContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Integer;
end;

procedure TBoldXMLIntegerContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  (Item as IBoldIntegerContent).asInteger := Node.ReadInteger;
end;

procedure TBoldXMLIntegerContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteInteger((Item as IBoldIntegerContent).asInteger);
end;

{ TBoldXMLRefStreamer }

procedure TBoldXMLRoleStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLRoleStreamer.ReadInterface(const Item: IBoldStreamable; Node: TBoldXMLNode);
var
  ContentNode: TBoldXMLNode;
begin
  inherited;
  ContentNode := Node.GetSubNode(BoldNodeName_content);
  ReadContent(Item as IBoldValue, ContentNode);
  ContentNode.Free;
end;

procedure TBoldXMLRoleStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
end;

procedure TBoldXMLRoleStreamer.WriteInterface(const Item: IBoldStreamable; Node: TBoldXMLNode);
var
  ContentNode: TBoldXMLNode;
begin
  inherited;
  ContentNode := Node.NewSubNode(BoldNodeName_content);
  WriteContent(Item as IBoldValue, ContentNode);
  ContentNode.Free;
end;

{ TBoldXMLIdRefStreamer }

function TBoldXMLIdRefStreamer.GetStreamName: string;
begin
  result := BoldContentName_ObjectIdRef;
end;

procedure TBoldXMLIdRefStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  anId: TBoldObjectId;
  anIdRef: IBoldObjectIdRef;
begin
  inherited;
  anIdRef := Item as IBoldObjectIdRef;
  anId := Node.ReadSubNodeObject(BoldNodeName_id , '') as TBoldObjectId;
  anIdRef.SetFromId(anId, true);
  SubNode := Node.GetSubNode(BoldNodeName_OrderNo);
  if assigned(SubNode) then
    anIdRef.OrderNo := SubNode.ReadInteger;
  SubNode.Free;
end;

procedure TBoldXMLIdRefStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  anIdRef: IBoldObjectIdRef;
begin
  inherited;
  anIdRef := Item as IBoldObjectIdRef;
{  SubNode := Node.NewSubNode(BoldNodeName_id);
  SubNode.WriteObject('', anIdRef.Id);
  SubNode.Free;}
  Node.WriteSubNodeObject(BoldNodeName_id, '', anIdRef.Id);
  Node.WriteSubNodeInteger(BoldNodeName_OrderNo, anIdRef.OrderNo);
end;

{ TBoldXMLIdRefPairStreamer }

function TBoldXMLIdRefPairStreamer.GetStreamName: string;
begin
  result := BoldContentName_ObjectIdRefPair;
end;

procedure TBoldXMLIdRefPairStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  Id1, Id2: TBoldObjectId;
begin
  inherited;
  SubNode := Node.GetSubNode(BoldNodeName_id1);
  Id1 := SubNode.ReadObject('') as TBoldObjectId;
  SubNode.Free;
  SubNode := Node.GetSubNode(BoldNodeName_id2);
  Id2 := SubNode.ReadObject('') as TBoldObjectId;
  SubNode.Free;
  (Item as IBoldObjectIdRefPair).SetFromIds(Id1, Id2);
  Id1.Free;
  Id2.Free;
end;

procedure TBoldXMLIdRefPairStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
begin
  inherited;
  SubNode := Node.NewSubNode(BoldNodeName_id1);
  SubNode.WriteObject('', (Item as IBoldObjectIdRefPair).Id1);
  SubNode.Free;
  SubNode := Node.NewSubNode(BoldNodeName_id2);
  SubNode.WriteObject('', (Item as IBoldObjectIdRefPair).Id2);
  SubNode.Free;
end;

{ TBoldXMLIdListRefStreamer }

function TBoldXMLIdListRefStreamer.GetStreamName: string;
begin
  result := BoldContentName_ObjectIdListRef;
end;

procedure TBoldXMLIdListRefStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
begin
  inherited;
  SubNode := Node.GetSubNode(BoldNodeName_idlist);
  anIdList := SubNode.ReadObject(BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  SubNode.Free;
  (Item as IBoldObjectIdListRef).SetFromIdList(anIdList);
  anIdList.Free;
end;

procedure TBoldXMLIdListRefStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  anIdList: TBoldobjectIdList;
  i: Integer;
  anIdListRef: IBoldObjectIdListRef;
begin
  inherited;
  anIdList := TBoldObjectIdList.Create;
  anIdListRef := Item as IBoldObjectIdListRef;
  SubNode := Node.NewSubNode(BoldNodeName_idlist);
  for i := 0 to anIdListRef.Count - 1 do
    anIdList.Add(anIdListRef.IdList[i]);
  SubNode.WriteObject(BOLDOBJECTIDLISTNAME, anIdList);
  SubNode.Free;
  anIdList.Free;
end;

{ TBoldXMLIdListRefPairStreamer }

function TBoldXMLIdListRefPairStreamer.GetStreamName: string;
begin
  result := BoldContentName_ObjectIdListRefPair;
end;

procedure TBoldXMLIdListRefPairStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  IdList1, IdList2: TBoldObjectIdList;
begin
  inherited;
  SubNode := Node.GetSubNode(BoldNodeName_idlist1);
  IdList1 := SubNode.ReadObject(BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  SubNode.Free;
  SubNode := Node.GetSubNode(BoldNodeName_idlist2);
  IdList2 := SubNode.ReadObject(BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  SubNode.Free;
  (Item as IBoldObjectIdListRefPair).SetFromIdLists(IdList1, IdList2);
  IdList1.Free;
  IdList2.Free;
end;

procedure TBoldXMLIdListRefPairStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
var
  SubNode: TBoldXMLNode;
  anIdList: TBoldobjectIdList;
  i: Integer;
  anIdListRefPair: IBoldObjectIdListRefPair;
begin
  inherited;
  anIdList := TBoldObjectIdList.Create;
  anIdListRefPair := Item as IBoldObjectIdListRefPair;

  SubNode := Node.NewSubNode(BoldNodeName_idlist1);
  for i := 0 to anIdListRefPair.Count - 1 do
    anIdList.Add(anIdListRefPair.IdList1[i]);
  SubNode.WriteObject(BOLDOBJECTIDLISTNAME, anIdList);
  SubNode.Free;

  SubNode := Node.NewSubNode(BoldNodeName_idlist2);
  anIdList.Clear;
  for i := 0 to anIdListRefPair.Count - 1 do
    anIdList.Add(anIdListRefPair.IdList2[i]);
  SubNode.WriteObject(BOLDOBJECTIDLISTNAME, anIdList);
  SubNode.Free;

  anIdList.Free;
end;

{ TBoldDefaultXMLStreamerRegistry }

class function TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry: TBoldDefaultXMLStreamerRegistry;
begin
  if not assigned(G_MainRegistry) then
    G_MainRegistry := TBoldDefaultXMLStreamerRegistry.Create(TBoldXMLStreamerRegistry.MainStreamerRegistry);
  result := G_MainRegistry;
end;

{ TBoldXMLClassStreamer }

constructor TBoldXMLClassStreamer.Create(MoldClass: TMoldClass; Owner: TBoldDefaultXMLStreamManager);
var
  i: Integer;
begin
  fOwner := Owner;
  fExpressionName := MoldClass.ExpandedExpressionName;
  fMemberStreamers := TBoldXMLModelElementStreamerList.Create;
  for i := 0 to MoldClass.AllBoldMembers.Count - 1 do
    fMemberStreamers.Add(TBoldXMLMemberStreamer.Create(MoldClass.AllBoldMembers[i], self)); 
end;

destructor TBoldXMLClassStreamer.Destroy;
begin
  FreeAndNil(fMemberStreamers);
  inherited;
end;

function TBoldXMLClassStreamer.GetMemberStreamer(Index: Integer): TBoldXMLMemberStreamer;
begin
  if Index < fMemberStreamers.Count then
    result := TBoldXMLMemberStreamer(fMemberStreamers.Items[Index])
  else
    result := nil;
end;

function TBoldXMLClassStreamer.GetMemberStreamerByName(const Name: string): TBoldXMLMemberStreamer;
begin
  result := fMemberStreamers.StreamerByName[Name] as TBoldXMLMemberStreamer;

  if not assigned(result) then
    raise EBold.CreateFmt(sUnrecognizedClassName, [classname, 'GetMemberStreamerByName', name]); // do not localize
end;

procedure TBoldXMLClassStreamer.ReadObject(Node: TBoldXMLNode; const ValueSpace: IBoldValueSpace);
var
  aSubNode: TBoldXMLNode;
  MembersNode: TBoldXMLNode;
  anId: TboldObjectId;
  ObjectContents: IBoldObjectContents;
  aMemberStreamer: TBoldXMLMemberStreamer;
  {$IFDEF OXML}
  aNodeEnumerator: TXMLChildNodeListEnumerator;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDomNodeList;
  aNode: IXMLDomNode;
  {$ENDIF}
begin
  aSubNode := Node.GetSubNode(BoldNodeName_id);
  anId := aSubNode.ReadObject('') as TBoldObjectId;
  aSubNode.Free;
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[anId];
  anId.Free;
  if not fOwner.IgnorePersistenceState then
  begin
    aSubNode := Node.GetSubNode(BoldNodeName_persistencestate);
    if Assigned(aSubNode) then begin
      ObjectContents.BoldPersistenceState := TBoldValuePersistenceState(aSubNode.ReadInteger);
      aSubNode.Free;
    end else begin
      // No node found -> set default BoldPersistenceState (bvpsCurrent)
      ObjectContents.BoldPersistenceState := bvpsCurrent;
    end;
  end;
  aSubNode := Node.GetSubNode(BoldNodeName_existencestate);
  if Assigned(aSubNode) then begin
    ObjectContents.BoldExistenceState := TBoldExistenceState(aSubNode.ReadInteger);
    aSubNode.Free;
  end else begin
    // No node found -> set default BoldExistenceState (besExisting)
    ObjectContents.BoldExistenceState := besExisting;
  end;


  aSubNode := Node.GetSubNode(BoldNodeName_timestamp);
  if assigned(aSubNode) then
  begin
    ObjectContents.TimeStamp := aSubNode.ReadInteger;
    aSubNode.Free;
  end;

  aSubNode := Node.GetSubNode(BoldNodeName_globalid);
  if assigned(aSubNode) then
  begin
    ObjectContents.GlobalId := aSubNode.ReadString;
    aSubNode.Free;
  end;

  MembersNode := Node.GetSubNode(BoldNodeName_members);
  {$IFDEF OXML}
  aNodeEnumerator := MembersNode.XMLDomElement.ChildNodes.GetEnumerator;
  try
    while aNodeEnumerator.MoveNext do begin
      aNode := aNodeEnumerator.Current;
      aSubNode := TBoldXMLNode.Create(Node.Manager, aNode, nil);
      aMemberStreamer := MemberStreamerByName[aSubNode.Accessor];
      ObjectContents.EnsureMember(aMemberStreamer.MemberId, aMemberStreamer.TypeStreamName);
      aMemberStreamer.ReadValue(aSubNode, ObjectContents.ValueByIndex[aMemberStreamer.Index]);
      aSubNode.Free;
    end;
  finally
    aNodeEnumerator.Free;
    MembersNode.Free;
  end;
  {$ELSE}
  aNodeList := MembersNode.XMLDomElement.childNodes;
  aNode := aNodeList.nextNode;
  while assigned(aNode) do
  begin
    aSubNode := TBoldXMLNode.Create(Node.Manager, aNode as IXMLDOMElement, nil);
    aMemberStreamer := MemberStreamerByName[aSubNode.Accessor];
    ObjectContents.EnsureMember(aMemberStreamer.MemberId, aMemberStreamer.TypeStreamName);
    aMemberStreamer.ReadValue(aSubNode, ObjectContents.ValueByIndex[aMemberStreamer.Index]);
    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
  MembersNode.Free;
  {$ENDIF}
end;

procedure TBoldXMLClassStreamer.WriteObject(Node: TBoldXMLNode;
  const ObjectContents: IBoldObjectContents; ObjectId: TBoldObjectId; MemberIdList: TBoldMemberIdList);
var
  i: Integer;
  ObjNode: TBoldXMLNode;
  MembersNode: TBoldXMLNode;
  aSubNode: TBoldXMLNode;
  iBoldPersistenceState,
  iBoldExistenceState: Integer;
begin
  if not assigned(ObjectContents) then
    exit;

  ObjNode := Node.NewSubNode(ExpressionName);

  aSubNode := ObjNode.NewSubNode(BoldNodeName_id);
  aSubNode.WriteObject('', ObjectId);
  aSubNode.Free;
  iBoldPersistenceState := Integer(ObjectContents.BoldPersistenceState);
  // Only write BoldPersistenceState other than bvpsCurrent, to minimize XML size
  if iBoldPersistenceState <> 0 then begin
    aSubNode := ObjNode.NewSubNode(BoldNodeName_persistencestate);
    aSubNode.WriteInteger(iBoldPersistenceState);
    aSubNode.Free;
  end;
  iBoldExistenceState := Integer(ObjectContents.BoldExistenceState);
  // Only write BoldExistenceState other than besExisting, to minimize XML size
  if iBoldExistenceState <> 1 then begin
    aSubNode := ObjNode.NewSubNode(BoldNodeName_existencestate);
    aSubNode.WriteInteger(iBoldExistenceState);
    aSubNode.Free;
  end;
  // Onyl write TimeStamp if it is set, to minimize XML size
  if ObjectContents.TimeStamp <> -1 then begin
    aSubNode := ObjNode.NewSubNode(BoldNodeName_timestamp);
    aSubNode.WriteInteger(ObjectContents.TimeStamp);
    aSubNode.Free;
  end;
  if ObjectContents.GlobalId <> '' then
    ObjNode.WriteSubNodeString(BoldNodeName_globalid, ObjectContents.GlobalId);

  MembersNode := ObjNode.NewSubNode(BoldNodeName_members);
  if assigned(MemberIdList) then
  begin
    for i := 0 to MemberIdList.Count - 1 do
      MemberStreamers[MemberIdList[i].MemberIndex].WriteValue(MembersNode,
                                    ObjectContents.ValueByMemberId[MemberIdList[i]]);
  end else
  begin
    for i := 0 to ObjectContents.MemberCount - 1 do
      MemberStreamers[i].WriteValue(MembersNode,
                                    ObjectContents.ValueByIndex[i]);
  end;
  MembersNode.Free;
  ObjNode.Free;
end;

{ TBoldXMLMemberStreamer }

constructor TBoldXMLMemberStreamer.Create(MoldMember: TMoldMember; Owner: TBoldXMLClassStreamer);
begin
  fOwner := Owner;
  fExpressionName := MoldMember.ExpandedExpressionName;
  fTypeStreamName := MoldMember.TypeStreamName;
  fPersistent := MoldMember.EffectivePersistent;
  fIndex := MoldMember.MoldClass.AllBoldMembers.IndexOf(MoldMember);
  fMemberId := TBoldMemberId.create(fIndex);
end;

destructor TBoldXMLMemberStreamer.Destroy;
begin
  FreeAndNil(fMemberId);
  inherited;
end;

procedure TBoldXMLMemberStreamer.ReadValue(Node: TBoldXMLNode;
  const Value: IBoldValue);
begin
  if assigned(self) and assigned(Value) then
    if (Value.BoldPersistenceState in fOwner.fOwner.PersistenceStatesToOverwrite) then
      Node.ReadInterface(TypeStreamName, Value as IBoldStreamable);
end;

procedure TBoldXMLMemberStreamer.WriteValue(Node: TBoldXMLNode; const Value: IBoldValue);
var
  aSubNode: TBoldXMLNode;
begin
  if assigned(self) and assigned(Value) then
    if Persistent and (Value.BoldPersistenceState in fOwner.fOwner.PersistenceStatesToBeStreamed) then
    begin
      aSubNode := Node.NewSubNode(ExpressionName);
      aSubNode.WriteInterface(TypeStreamName, Value as IBoldStreamable);
      aSubNode.Free;
    end;
end;

{ TBoldXMLFloatContentStreamer }

function TBoldXMLFloatContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Float;
end;

procedure TBoldXMLFloatContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldFloatContent).asFloat := Node.ReadFloat;
end;

procedure TBoldXMLFloatContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  Node.WriteFloat((Item as IBoldFloatContent).asFloat);
end;

{ TBoldXMLCurrencyContentStreamer }

function TBoldXMLCurrencyContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Currency;
end;

procedure TBoldXMLCurrencyContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldCurrencyContent).asCurrency := Node.ReadCurrency;
end;

procedure TBoldXMLCurrencyContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  Node.WriteCurrency((Item as IBoldCurrencyContent).asCurrency);
end;

{ TBoldXMLBooleanContentStreamer }

function TBoldXMLBooleanContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Boolean;
end;

procedure TBoldXMLBooleanContentStreamer.ReadContent(const Item: IBoldValue;
  Node: TBoldXMLNode);
begin
  (Item as IBoldBooleanContent).asBoolean := Node.ReadBoolean;
end;

procedure TBoldXMLBooleanContentStreamer.WriteContent(const Item: IBoldValue;
  Node: TBoldXMLNode);
begin
  Node.WriteBoolean((Item as IBoldBooleanContent).asBoolean);
end;

{ TBoldXMLDateContentStreamer }

function TBoldXMLDateContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Date;
end;

procedure TBoldXMLDateContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldDateContent).asDate := Node.ReadDate;
end;

procedure TBoldXMLDateContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  Node.WriteDate((Item as IBoldDateContent).asDate);
end;

{ TBoldXMLTimeContentStreamer }

function TBoldXMLTimeContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Time;
end;

procedure TBoldXMLTimeContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldTimeContent).asTime := Node.ReadTime;
end;

procedure TBoldXMLTimeContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  Node.WriteTime((Item as IBoldTimeContent).asTime); 
end;

{ TBoldXMLDateTimeContentStreamer }

function TBoldXMLDateTimeContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_DateTime;
end;

procedure TBoldXMLDateTimeContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldDateTimeContent).asDateTime := Node.ReadDateTime;
end;

procedure TBoldXMLDateTimeContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  Node.WriteDateTime((Item as IBoldDateTimeContent).asDateTime);
end;

{ TBoldXMLBlobContentStreamer }

function TBoldXMLBlobContentStreamer.GetStreamName: string;
begin
  result := BoldContentName_Blob;
end;

procedure TBoldXMLBlobContentStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  (Item as IBoldBlobContent).asBlob := Node.ReadData;
end;

procedure TBoldXMLBlobContentStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteData((Item as IBoldBlobContent).asBlob);
end;

{ TBoldXMLTypedBlobStreamer }

function TBoldXMLTypedBlobStreamer.GetStreamName: string;
begin
  result := BoldContentName_TypedBlob;
end;

procedure TBoldXMLTypedBlobStreamer.ReadContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  (Item as IBoldTypedBlob).ContentTypeContent := Node.ReadSubNodeString(BoldNodeName_ContentType);
end;

procedure TBoldXMLTypedBlobStreamer.WriteContent(const Item: IBoldValue; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeString(BoldNodeName_ContentType, (Item as IBoldTypedBlob).ContentTypeContent);
end;

{ TBoldExpressionNameIndex }

function TBoldExpressionNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(Item is TBoldXMLModelElementStreamer, 'Wrong element type');
  result := TBoldXMLModelElementStreamer(Item).ExpressionName;
end;

{ TBoldXMLModelElementStreamerList }

constructor TBoldXMLModelElementStreamerList.Create;
begin
  inherited;
  SetIndexVariable(IX_ExpressionNameIndex, AddIndex(TBoldExpressionNameIndex.Create));
end;

function TBoldXMLModelElementStreamerList.GetStreamerByName(Name: string): TBoldXMLModelElementStreamer;
begin
  result := (Indexes[IX_ExpressionNameIndex] as TBoldStringHashIndex).FindByString(Name) as TBoldXMLModelElementStreamer;
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLStringContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLIntegerContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLFloatContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLCurrencyContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLBooleanContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLDateContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTimeContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLDateTimeContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLBlobContentStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTypedBlobStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLIdRefStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLIdRefPairStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLIdListRefStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLIdListRefPairStreamer.Create);

finalization
  FreeAndNil(G_MainRegistry);  

end.