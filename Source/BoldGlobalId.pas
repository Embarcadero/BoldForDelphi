{ Global compiler directives }
{$include bold.inc}
unit BoldGlobalId;

interface

uses
  BoldId,
  BoldStreams;
    
const
  BOLDGLOBALIDNAME = 'BoldGlobalId';

type
  TBoldGlobalId = class;

  TBoldGlobalId = class(TBoldObjectId)
  private
    fId: String;
    fClassExpressionName: string;
  protected
    function GetAsString: string; override;
    function GetHash: Cardinal; override;
    function GetIsStorable: Boolean; override;
    function GetStreamName: string; override;
  public
    constructor CreateWithInfo(GlobalId: String; TopSortedIndex: integer; Exact: Boolean; ClassExpressionName: String); virtual;
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid; override;
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
    property ClassExpressionName: string read fClassExpressionName;
    property AsString: String read GetAsString;
  end;

implementation

uses
  SysUtils,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldDefaultStreamNames,
  BoldUtils,
  BoldHashIndexes,
  BoldBase;

const
  BoldNodeName_Id = 'Id';

type
  { TBoldXMLDefaultIdStreamer }
  TBoldXMLGlobalIdStreamer = class(TBoldXMLObjectIdStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

{ TBoldGlobalId }

function TBoldGlobalId.CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid;
begin
  Result := TBoldGlobalId.CreateWithInfo(fId, TopSortedIndex, Exact, ClassExpressionName);
end;

constructor TBoldGlobalId.CreateWithInfo(GlobalId: String; TopSortedIndex: integer; Exact: Boolean; ClassExpressionName: String);
begin
  inherited CreateWithClassID(TopSortedIndex, Exact);
  fId := GlobalId;
  fClassExpressionName := ClassExpressionName;
end;

function TBoldGlobalId.GetAsString: string;
begin
  result := fId;
end;

function TBoldGlobalId.GetHash: Cardinal;
begin
  result := TBoldStringKey.HashString(fId, bscCaseDependent);
end;

function TBoldGlobalId.GetIsEqual(MatchID: TBoldObjectID): Boolean;
begin
  result := Assigned(MatchId) and (MatchID.ClassType = ClassType) and
            (TBoldGlobalId(MatchID).fId = fId);
end;

function TBoldGlobalId.GetIsStorable: Boolean;
begin
  result := False;
end;

function TBoldGlobalId.GetStreamName: string;
begin
  result := BOLDGLOBALIDNAME;
end;

{ TBoldXMLDefaultIdStreamer }

function TBoldXMLGlobalIdStreamer.CreateObject: TObject;
begin
  result := TBoldGlobalId.Create;
end;

function TBoldXMLGlobalIdStreamer.GetStreamName: string;
begin
  result := BOLDGLOBALIDNAME;
end;

procedure TBoldXMLGlobalIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  GlobalId: TBoldGlobalId;
begin
  inherited;
  GlobalId := Obj as TBoldGlobalId;
  GlobalId.fId := Node.ReadSubNodeString(BoldNodeName_Id);
  GlobalId.fClassExpressionName :=
    (Node.Manager as TBoldDefaultXMLStreamManager).Model.Classes[GlobalId.TopSortedIndex].Name;
end;

procedure TBoldXMLGlobalIdStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeString(BoldNodeName_Id, (Obj as TBoldGlobalId).fId);
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLGlobalIdStreamer.Create);

end.
