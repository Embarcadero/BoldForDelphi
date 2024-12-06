
{ Global compiler directives }
{$include bold.inc}
unit BoldStringId;

interface

uses
  BoldId;


type
  TBoldStringID = class(TBoldExternalObjectID)
  protected
    fIdValue: String;
    procedure SetAsString(NewValue: String); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetAsString: string; override;
    function GetHash: cardinal; override;
    function GetStreamName: string; override;
  public
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectId; override;
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
    property AsString: String read GetAsString write SetAsString;
  end;


implementation

uses
  BoldXMLStreaming,
  BoldDefaultStreamNames,
  BoldBase;

const
  BoldNodeName_IdValue = 'IdValue';

type
  { TBoldXMLDefaultIdStreamer }
  TBoldXMLStringIdStreamer = class(TBoldXMLObjectIdStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

{ TBoldStringID }

function HashString(const S: string): CARDINAL; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := ((Result shl 3) and 2147483647) or
              (Result shr (32-3)) xor ord(S[i]);
end;


function TBoldStringID.CloneWithClassId(TopSortedIndex: integer;
  Exact: Boolean): TBoldObjectId;
begin
  Result := TBoldObjectIdClass(self.classtype).CreateWithClassID(TopSortedIndex, Exact);
  TBoldStringID(Result).fIdValue := fIdValue;
end;

function TBoldStringID.GetAsString: string;
begin
  result := fIdValue;
end;

function TBoldStringID.GetHash: cardinal;
begin
  result := HashString(fIdValue);
end;

function TBoldStringID.GetIsEqual(MatchID: TBoldObjectID): Boolean;
begin
  result := assigned(MatchId) and (MatchID.ClassType = TBoldStringID) and
            (fIdValue = TBoldStringID(MatchId).fIdValue);
end;

function TBoldStringID.GetStreamName: string;
begin
  result := BOLDSTRINGIDNAME;
end;

procedure TBoldStringID.SetAsString(NewValue: String);
begin
  fIdValue := NewValue;
end;
{ TBoldXMLStringIdStreamer }

function TBoldXMLStringIdStreamer.CreateObject: TObject;
begin
  result := TBoldStringID.Create;
end;

function TBoldXMLStringIdStreamer.GetStreamName: string;
begin
  result := BOLDSTRINGIDNAME;
end;

procedure TBoldXMLStringIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
begin
  inherited;
  assert(Obj is TBoldStringID);
  TBoldStringID(Obj).fIdValue := Node.ReadSubNodeString(BoldNodeName_IdValue);
end;

procedure TBoldXMLStringIdStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeString(BoldNodeName_IdValue, (Obj as TBoldStringID).fIdValue);
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLStringIdStreamer.Create);
end.
