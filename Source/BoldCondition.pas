
{ Global compiler directives }
{$include bold.inc}
unit BoldCondition;

interface

uses
  DB,
  BoldDefs,
  BoldBase,
  BoldStreams,
  BoldXMLStreaming,
  BoldId;

type
  {---Forward declaration of classes---}
  TBoldCondition = class;
  TBoldConditionWithClass = class;
  TBoldSQLCondition = class;
  TBoldTimestampCondition = class;
  TBoldChangePointCondition = class;

  {---TBoldCondition---}
  TBoldCondition = class(TBoldNonRefCountedObject, IBoldStreamable)
  private
    fMaxAnswers: integer;
    fOffset: integer;
    fAvailableAnswers: integer;
  public
    constructor create;
    function GetStreamName: string; virtual; abstract;
    property MaxAnswers: integer read fMaxAnswers write fMaxAnswers;
    property Offset: integer read fOffset write fOffset;
    property AvailableAnswers: integer read fAvailableAnswers write fAvailableAnswers;
  end;

  {---TBoldConditionWithClass---}
  TBoldConditionWithClass = class(TBoldCondition)
  private
    fTopSortedIndex: integer;
    fTime: TBoldTimestampType;
  public
    constructor create;
    function GetStreamName: string; override;
    property TopSortedIndex: integer read fTopSortedIndex write fTopSortedIndex;
    property Time: TBoldTimestampType read fTime write fTime;
  end;

  {---TBoldSQLCondition---}
  TBoldSQLCondition = class(TBoldConditionWithClass)
  private
    fWhereFragment: string;
    fOrderBy: string;
    fParams: TParams;
    fJoinInheritedTables: Boolean;
  public
    constructor Create;
    function GetStreamName: string; override;
    property WhereFragment: string read fWhereFragment write fWhereFragment;
    property OrderBy: string read fOrderBy write fOrderBy;
    property Params: TParams read fParams write fParams;
    property JoinInheritedTables: Boolean read fJoinInheritedTables write fJoinInheritedTables;
  end;

  {---TBoldRawSQLCondition---}
  TBoldRawSQLCondition = class(TBoldConditionWithClass)
  private
    fSQL: string;
    fParams: TParams;
  public
    function GetStreamName: string; override;
    property Params: TParams read fParams write fParams;
    property SQL: string read fSQL write fSQL;
  end;

  {---TBoldTimestampCondition---}
  TBoldTimestampCondition = class(TBoldConditionWithClass)
  private
    fTimestamp: TBoldTimeStampType;
  public
    function GetStreamName: string; override;
    property Timestamp: TBoldTimeStampType read fTimestamp write fTimestamp;
  end;

  {---TBoldChangePointCondition---}
  TBoldChangePointCondition = class(TBoldCondition)
  private
    fIdList: TBoldObjectIdList;
    fStart: TBoldTimestampType;
    fEnd: TBoldTimestampType;
    fMemberIds: TBoldMemberIdList;
  public
    function GetStreamName: string; override;
    property IdList: TBoldObjectIdList read fIdList write fIdList;
    property StartTime: TBoldTimestampType read fStart write fStart;
    property EndTime: TBoldTimestampType read fEnd write fEnd;
    property MemberIdList: TBoldMemberIdList read fMemberIds write fMemberIds;
  end;

  { TBoldXMLConditionWithClassStreamer }
  TBoldXMLConditionStreamer = class(TBoldXMLObjectStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLConditionWithClassStreamer }
  TBoldXMLConditionWithClassStreamer = class(TBoldXMLConditionStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

implementation

uses
  classes,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldDefaultStreamNames;

const
  BoldNodeName_MaxAnswers = 'MaxAnswers';
  BoldNodeName_Offset = 'Offset';
  BoldNodeName_AvailableAnswers = 'AvailableAnswers';
  BoldNodeName_TopSortedIndex = 'TopSortedIndex';
  BoldNodeName_Time = 'Time';
  BoldNodeName_WhereFragment = 'WhereFragment';
  BoldNodeName_OrderBy = 'OrderBy';
  BoldNodeName_JoinInheritedTables = 'JoinInheritedTables';
  BoldNodeName_Param = 'Param';
  BoldNodeName_Params = 'Params';
  BoldNodeName_Name = 'Name';
  BoldNodeName_DataType = 'DataType';
  BoldNodeName_ParamType = 'ParamType';
  BoldNodeName_Data = 'Data';
  BoldNodeName_Timestamp = 'Timestamp';
  BoldNodeName_IdList = 'IdList';
  BoldNodeName_StartTime = 'StartTime';
  BoldNodeName_EndTime = 'EndTime';
  BoldNodeName_MemberIds = 'MemberIds';

type
  {$IFNDEF BOLD_DELPHI17_OR_LATER}
  TValueBuffer = Pointer;
  {$ENDIF}

  { TBoldXMLSQLConditionStreamer}
  TBoldXMLSQLConditionStreamer = class(TBoldXMLConditionWithClassStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLTimestampConditionStreamer }
  TBoldXMLTimestampConditionStreamer = class(TBoldXMLConditionWithClassStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLChangePointConditionStreamer }
  TBoldXMLChangePointConditionStreamer = class(TBoldXMLConditionStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

const
  SQLConditionName = 'SQLCondition';
  RawSQLConditionName = 'RawSQLCondition';
  ClassConditionName = 'ClassCondition';
  TimestampConditionName = 'TimestampCondition';
  OclConditionName = 'OclCondition';
  ChangePointConditionName = 'ChangePointCondition';

{ TBoldConditionWithClass }

function TBoldConditionWithClass.GetStreamName: string;
begin
  result := ClassConditionName;
end;

constructor TBoldConditionWithClass.create;
begin
  inherited;
  fTime := BOLDMAXTIMESTAMP;
end;

{ TBoldSQLCondition }

constructor TBoldSQLCondition.Create;
begin
  inherited;
  fJoinInheritedTables := true;
end;

function TBoldSQLCondition.GetStreamName: string;
begin
  result := SQLConditionName;
end;

{ TBoldRawSQLCondition }

function TBoldRawSQLCondition.GetStreamName: string;
begin
  Result := RawSQLConditionName;
end;

{ TBoldTimestampCondition }

function TBoldTimestampCondition.GetStreamName: string;
begin
  result := TimestampConditionName;
end;

{ TBoldChangePointCondition }

function TBoldChangePointCondition.GetStreamName: string;
begin
  result := ChangePointConditionName;
end;

{ TBoldXMLConditionStreamer }

procedure TBoldXMLConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  Condition: TBoldCondition;
begin
  inherited;
  Condition := Obj as TBoldCondition;
  Condition.MaxAnswers := Node.ReadSubNodeInteger(BoldNodeName_MaxAnswers);
  Condition.Offset := Node.ReadSubNodeInteger(BoldNodeName_Offset);
  Condition.AvailableAnswers := Node.ReadSubNodeInteger(BoldNodeName_AvailableAnswers);
end;

procedure TBoldXMLConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldCondition;
begin
  inherited;
  Condition := Obj as TBoldCondition;
  Node.WriteSubNodeInteger(BoldNodeName_MaxAnswers, Condition.MaxAnswers);
  Node.WriteSubNodeInteger(BoldNodeName_Offset, Condition.Offset);
  Node.WriteSubNodeInteger(BoldNodeName_AvailableAnswers, Condition.AvailableAnswers);
end;

{ TBoldXMLConditionWithClassStreamer }

function TBoldXMLConditionWithClassStreamer.CreateObject: TObject;
begin
  result := TBoldConditionWithClass.create;
end;

function TBoldXMLConditionWithClassStreamer.GetStreamName: string;
begin
  result := ClassConditionName;
end;

procedure TBoldXMLConditionWithClassStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  Condition: TBoldConditionWithClass;
begin
  inherited;
  Condition := Obj as TBoldConditionWithClass;
  Condition.TopSortedIndex := Node.ReadSubNodeInteger(BoldNodeName_TopSortedIndex);
  Condition.Time := Node.ReadSubNodeInteger(BoldNodeName_Time);
end;

procedure TBoldXMLConditionWithClassStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldConditionWithClass;
begin
  inherited;
  Condition := Obj as TBoldConditionWithClass;
  Node.WriteSubNodeInteger(BoldNodeName_TopSortedIndex, Condition.TopSortedIndex);
  Node.WriteSubNodeInteger(BoldNodeName_Time, Condition.Time);
end;

{ TBoldXMLSQLConditionStreamer }

function TBoldXMLSQLConditionStreamer.CreateObject: TObject;
begin
  result := TBoldSQLCondition.create;
end;

function TBoldXMLSQLConditionStreamer.GetStreamName: string;
begin
  result := SQLConditionName;
end;

procedure TBoldXMLSQLConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  anSQLCond: TBoldSQLCondition;
  {$IFDEF OXML}
  aNodeEnumerator: TXMLResNodeListEnumerator;
  aNodeList: IXMLNodeList;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  {$ENDIF}
  aSubNode: TBoldXMLNode;
  ParamsNode: TBoldXMLNode;
  DataNode: TBoldXMLNode;
  aParam: TParam;
  Buf: TBoldAnsiString;
begin
  inherited;
  anSQLCond := Obj as TBoldSQLCondition;
  anSQLCond.WhereFragment :=  Node.ReadSubNodeString(BoldNodeName_WhereFragment);
  anSQLCond.OrderBy := Node.ReadSubNodeString(BoldNodeName_OrderBy);
  anSQlCond.JoinInheritedTables := Node.ReadSubNodeBoolean(BoldNodeName_JoinInheritedTables);

  ParamsNode := Node.GetSubNode(BoldNodeName_Params);
  if not ParamsNode.IsNull then
  begin
    anSQLCond.Params := TParams.Create;
    {$IFDEF OXML}
    if Node.XMLDomElement.GetElementsByTagName(BoldNodeName_Param, aNodeList) then
    begin
      aNodeEnumerator := aNodeList.GetEnumerator;
      try
        while aNodeEnumerator.MoveNext do begin
          aNode := aNodeEnumerator.Current;
          aSubNode := Node.MakeNodeForElement(aNode);
          aParam := anSQLCond.Params.Add as TParam;
          aParam.Name := aSubNode.ReadSubNodeString(BoldNodeName_Name);
          aParam.DataType := TFieldType(aSubNode.ReadSubNodeInteger(BoldNodeName_DataType));
          aParam.ParamType := TParamType(aSubNode.ReadSubNodeInteger(BoldNodeName_ParamType));
          DataNode := aSubNode.GetSubNode(BoldNodeName_Data);
          if Assigned(DataNode) then
          begin
            Buf := DataNode.ReadData;
            aParam.SetData(TValueBuffer(PAnsiChar(Buf)));
          end;
          DataNode.Free;
          aSubNode.Free;
        end;
      finally
        aNodeEnumerator.Free;
      end;
    end;
    {$ELSE}
    aNodeList := Node.XMLDomElement.getElementsByTagName(BoldNodeName_Param);
    aNode := aNodeList.nextNode;

    while assigned(aNode) do
    begin
      aSubNode := Node.MakeNodeForElement(aNode as IXMLDOMElement);

      aParam := anSQLCond.Params.Add as TParam;
      aParam.Name := aSubNode.ReadSubNodeString(BoldNodeName_Name);
      aParam.DataType := TFieldType(aSubNode.ReadSubNodeInteger(BoldNodeName_DataType));
      aParam.ParamType := TParamType(aSubNode.ReadSubNodeInteger(BoldNodeName_ParamType));
      DataNode := aSubNode.GetSubNode(BoldNodeName_Data);
      if assigned(DataNode) then
      begin
        Buf := DataNode.ReadData;
        aParam.SetData(TValueBuffer(PAnsiChar(Buf)));
      end;
      DataNode.Free;
      aSubNode.Free;
      aNode := aNodeList.nextNode;
    end;
    {$ENDIF}
  end
  else
    anSQLCond.Params := nil;
  ParamsNode.Free;
end;

procedure TBoldXMLSQLConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  anSQLCond: TBoldSQLCondition;
  Buf: TBoldAnsiString;
  i: Integer;
  SubNode: TBoldXMLNode;
  ParamsNode: TBoldXMLNode;
begin
  inherited;
  anSQLCond := Obj as TBoldSQLCondition;
  Node.WriteSubNodeString(BoldNodeName_WhereFragment, anSQLCond.WhereFragment);
  Node.WriteSubNodeString(BoldNodeName_OrderBy, anSQLCond.OrderBy);
  Node.WriteSubNodeBoolean(BoldNodeName_JoinInheritedTables, anSqlCond.JoinInheritedTables);

  ParamsNode := Node.NewSubNode(BoldNodeName_Params);
  if assigned(anSQLCond.Params) then
  begin
    for i := 0 to anSQLCond.Params.Count - 1 do
    begin
      SubNode := ParamsNode.NewSubNode(BoldNodeName_Param);
      SetLength(Buf, anSQLCond.Params[i].GetDataSize);
      anSQLCond.Params[i].GetData(TValueBuffer(PAnsiChar(Buf)));
      SubNode.WriteSubNodeString(BoldNodeName_Name, anSQLCond.Params[i].Name);
      SubNode.WriteSubNodeInteger(BoldNodeName_DataType, Integer(anSQLCond.Params[i].DataType));
      SubNode.WriteSubNodeInteger(BoldNodeName_ParamType, Integer(anSQLCond.Params[i].ParamType));
      if not anSQLCond.Params[i].IsNull then
        SubNode.WriteSubNodeData(BoldNodeName_Data, Buf);
      SubNode.Free;
    end;
  end else
    ParamsNode.SetToNull;
  ParamsNode.Free;
end;

{ TBoldXMLTimestampConditionStreamer }

function TBoldXMLTimestampConditionStreamer.CreateObject: TObject;
begin
  result := TBoldTimestampCondition.create;
end;

function TBoldXMLTimestampConditionStreamer.GetStreamName: string;
begin
  result := TimestampConditionName;
end;

procedure TBoldXMLTimestampConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
begin
  inherited;
  (Obj as TBoldTimestampCondition).Timestamp := Node.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldXMLTimestampConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeInteger(BoldNodeName_Timestamp, (Obj as TBoldTimestampCondition).Timestamp);
end;

{ TBoldXMLChangePointConditionStreamer }

function TBoldXMLChangePointConditionStreamer.CreateObject: TObject;
begin
  result := TBoldChangePointCondition.Create;
end;

function TBoldXMLChangePointConditionStreamer.GetStreamName: string;
begin
  result := ChangePointConditionName;
end;

procedure TBoldXMLChangePointConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  aCond: TBoldChangePointCondition;
begin
  inherited;
  aCond := Obj as TBoldChangePointCondition;
  aCond.IdList := Node.ReadSubNodeObject(BoldNodeName_IdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  aCond.StartTime := Node.ReadSubNodeInteger(BoldNodeName_StartTime);
  aCond.EndTime := Node.ReadSubNodeInteger(BoldNodeName_EndTime);
  aCond.MemberIdList := Node.ReadSubNodeObject(BoldNodeName_MemberIds, BOLDMEMBERIDLISTNAME) as TBoldMemberIdList;
end;

procedure TBoldXMLChangePointConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aCond: TBoldChangePointCondition;
begin
  inherited;
  aCond := Obj as TBoldChangePointCondition;
  Node.WriteSubNodeObject(BoldNodeName_IdList, BOLDOBJECTIDLISTNAME, aCond.IdList);
  Node.WriteSubNodeInteger(BoldNodeName_StartTime, aCond.StartTime);
  Node.WriteSubNodeInteger(BoldNodeName_EndTime, aCond.EndTime);
  Node.WriteSubNodeObject(BoldNodeName_MemberIds, BOLDMEMBERIDLISTNAME, aCond.MemberIdList);
end;


{ TBoldCondition }

constructor TBoldCondition.create;
begin
  inherited;
  fMaxAnswers := -1;
  fOffset := -1;
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLConditionWithClassStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLSQLConditionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTimestampConditionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLChangePointConditionStreamer.Create);

end.