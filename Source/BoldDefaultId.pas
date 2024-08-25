
{ Global compiler directives }
{$include bold.inc}
unit BoldDefaultId;

interface

uses
  BoldDefs,
  BoldID;

type
  { forward declarations }
  TBoldDefaultID = class;
  TBoldTimestampedDefaultId = class;

  { TBoldDefaultID }
  TBoldDefaultID = class(TBoldExternalObjectID)
  protected
    fDBValue: integer;
    procedure SetAsInteger(NewValue: integer);
    function GetAsInteger: Integer;
    function GetAsString: string; override;
    function GetHash: cardinal; override;
    function GetStreamName: string; override;
  public
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectId; override;
    function CloneWithTimeStamp(Time: TBoldTimeStampType): TBoldDefaultId;
    function CloneWithClassIdAndTimeStamp(TopSortedIndex: integer; Exact: Boolean; Time: TBoldTimeStampType): TBoldDefaultId;
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  { TBoldTimestampedDefaultId }
  TBoldTimestampedDefaultId = class(TBoldDefaultId)
  private
    fTimeStamp: TBoldTimeStampType;
  protected
    function GetStreamName: string; override;
    function GetTimeStamp: TBoldTimeStampType; override;
    function GetHash: cardinal; override;
  public
    constructor CreateWithTimeAndClassId(TimeStamp: TBoldTimeStampType; TopSortedIndex: integer; Exact: Boolean);
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectId; override;
    property TimeStamp: TBoldTimeStampType read GetTimeStamp write fTimeStamp;
  end;

implementation

uses
  BoldBase,
  SysUtils,
  BoldXMLStreaming,
  BoldDefaultStreamNames;

type
  TBoldObjectIdClass = class of TBoldObjectId;

const
  BoldNodeName_DbValue = 'DbValue';
  BoldNodeName_Timestamp = 'Timestamp';

type
  { TBoldXMLDefaultIdStreamer }
  TBoldXMLDefaultIdStreamer = class(TBoldXMLObjectIdStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLTimestampedDefaultIdStreamer }
  TBoldXMLTimestampedDefaultIdStreamer = class(TBoldXMLDefaultIdStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

{---TBoldDefaultID---}
function TBoldDefaultID.GetAsString: string;
begin
  Result := IntToStr(FDbValue);
end;

function TBoldDefaultID.GetAsInteger: Integer;
begin
  Result := FDbValue;
end;

Function TBoldDefaultID.GetIsEqual(MatchID: TBoldObjectID): Boolean;
begin
  if not assigned(MatchId) then
    Result := false
  else if MatchID.ClassType = TBoldDefaultId then
    Result := fdbValue = TBoldDefaultId(MatchId).fDbValue
  else if MatchID.ClassType = TBoldTimestampedDefaultId  then
    Result := (fdbValue = TBoldTimestampedDefaultId(MatchId).fDbValue) and
      (MatchId.TimeStamp = BOLDMAXTIMESTAMP)  
  else
    Result := false;
end;

function TBoldDefaultID.GetHash: cardinal;
begin
  result := cardinal(fDBValue) + cardinal(BOLDMAXTIMESTAMP);
end;

procedure TBoldDefaultId.SetAsInteger(NewValue: integer);
begin
  fdbValue := NewValue;
end;

function TBoldDefaultId.CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectId;
begin
  Result := TBoldObjectIdClass(self.classtype).CreateWithClassID(TopSortedIndex, Exact);
  (Result as TBoldDefaultId).fDbValue := fDBValue;
end;

function TBoldDefaultId.GetStreamName: string;
begin
  result := BOLDDEFAULTIDNAME;
end;

{ TBoldTimestampedDefaultId }

function TBoldTimestampedDefaultId.CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectId;
begin
  result := inherited CloneWithClassId(TopSortedIndex, Exact);
  (result as TBoldTimestampedDefaultId).fTimeStamp := fTimeStamp;
end;

constructor TBoldTimestampedDefaultId.CreateWithTimeAndClassId(TimeStamp: TBoldTimeStampType; TopSortedIndex: integer; Exact: Boolean);
begin
  inherited CreateWithClassId(TopSortedIndex, Exact);
  fTimeStamp := TimeStamp;
end;

function TBoldTimestampedDefaultId.GetHash: cardinal;
begin
  result := cardinal(fDBValue) + cardinal(fTimestamp);
end;

function TBoldTimestampedDefaultId.GetIsEqual(MatchID: TBoldObjectID): Boolean;
var
  TimestampedMatchId: TBoldTimestampedDefaultId;
begin
  if not assigned(MatchId) then
    Result := false
  else if MatchID.ClassType = TBoldDefaultId then
    Result := (fdbValue = TBoldDefaultId(MatchId).fDbValue)  and (fTimeStamp = BOLDMAXTIMESTAMP)
  else if MatchID.ClassType = TBoldTimestampedDefaultId  then
  begin
    TimestampedMatchId := TBoldTimestampedDefaultId(MatchId);
    Result := (fdbValue = TimestampedMatchId.fDbValue) and
      (TimestampedMatchId.fTimeStamp = fTimeStamp)
  end
  else
    Result := false;
end;

function TBoldTimestampedDefaultId.GetStreamName: string;
begin
  result := BOLDTIMESTAMPEDDEFAULTIDNAME;
end;

function TBoldTimestampedDefaultId.GetTimeStamp: TBoldTimeStampType;
begin
  result := fTimeStamp;
end;

function TBoldDefaultID.CloneWithClassIdAndTimeStamp(TopSortedIndex: integer; Exact: Boolean; Time: TBoldTimeStampType): TBoldDefaultId;
begin
  if Time = BOLDMAXTIMESTAMP then
    result := TBoldDefaultId.CreateWithClassID(TopSortedIndex, Exact)
  else
    result := TBoldTimestampedDefaultId.createWithTimeAndClassId(Time, TopSortedIndex, Exact);
  result.fDBValue := fDBValue;
end;


{ TBoldXMLDefaultIdStreamer }

function TBoldXMLDefaultIdStreamer.CreateObject: TObject;
begin
  result := TBoldDefaultId.Create;
end;

function TBoldXMLDefaultIdStreamer.GetStreamName: string;
begin
  result := BOLDDEFAULTIDNAME;
end;

procedure TBoldXMLDefaultIdStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aSubNode := Node.GetSubNode(BoldNodeName_DbValue);
  (Obj as TBoldDefaultId).fDBValue := aSubNode.ReadInteger;
  aSubNode.Free;
end;

procedure TBoldXMLDefaultIdStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aSubNode := Node.NewSubNode(BoldNodeName_DbValue);
  aSubNode.WriteInteger((Obj as TBoldDefaultId).fDBValue);
  aSubNode.Free;
end;

{ TBoldXMLTimestampedDefaultIdStreamer }

function TBoldXMLTimestampedDefaultIdStreamer.CreateObject: TObject;
begin
  result := TBoldTimestampedDefaultId.Create;
end;

function TBoldXMLTimestampedDefaultIdStreamer.GetStreamName: string;
begin
  result := BOLDTIMESTAMPEDDEFAULTIDNAME;
end;

procedure TBoldXMLTimestampedDefaultIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
begin
  inherited;
  (Obj as TBoldTimestampedDefaultId).TimeStamp := Node.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldXMLTimestampedDefaultIdStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeInteger(BoldNodeName_Timestamp, (Obj as TBoldTimestampedDefaultId).TimeStamp);
end;


function TBoldDefaultID.CloneWithTimeStamp(Time: TBoldTimeStampType): TBoldDefaultId;
begin
  result := CloneWithClassIdAndTimeStamp(TopSortedIndex, TopSortedIndexExact, time);
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLDefaultIdStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTimestampedDefaultIdStreamer.Create);

end.
