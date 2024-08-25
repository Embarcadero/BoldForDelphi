
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceOperationXMLStreaming;

interface

uses
  BoldXMLStreaming,
  BoldPersistenceController,
  BoldId,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldDefs,
  BoldSOAP_TLB,
  BoldValueSpaceInterfaces,
  BoldFreeStandingValues,
  BoldDefaultXMLStreaming;

type
  TBoldPersistenceOperation = class
  private
    fStreamManager: TBoldDefaultXMLStreamManager;
  protected
    function GetName: string; virtual; abstract;
    property StreamManager: TBoldDefaultXMLStreamManager read fStreamManager;
  public
    constructor Create(Manager: TBoldDefaultXMLStreamManager);
    procedure FreeParams; virtual;
    procedure InitParams; virtual;
    procedure WriteInParams(XMLNode: TBoldXMLNode); virtual; abstract;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); virtual; abstract;
    procedure ReadInParams(XMLNode: TBoldXMLNode); virtual; abstract;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); virtual; abstract;
    procedure Execute(PersistenceController: TBoldPersistenceController); virtual; abstract;
    procedure RemoteExecute(Stub: IBoldSOAPService);
    procedure ExecuteStreamed(PersistenceController: TBoldPersistenceController; RequestBodyNode: TBoldXMLNode; ReplyBodyNode: TBoldXMLNode);
    property Name: string read GetName;
  end;

  TBoldPMFetchOperation = class(TBoldPersistenceOperation)
  private
    fObjectIdList: TBoldObjectIdList;
    fValueSpace: IBoldValueSpace;
    fFreestanding: TBoldFreeStandingValueSpace;
    fMemberIdList: TBoldMemberIdList;
    fFetchMode: Integer;
    fBoldClientID: TBoldClientID;
  protected
    function GetName: string; override;
  public
    procedure FreeParams; override;
    procedure InitParams; override;
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ObjectIdList: TBoldObjectIdList read fObjectIdList write fObjectIdList;
    property ValueSpace: IBoldValueSpace read fValueSpace write fValueSpace;
    property MemberIdList: TBoldMemberIdList read fMemberIdList write fMemberIdList;
    property FetchMode: Integer read fFetchMode write fFetchMode;
    property BoldClientID: TBoldClientID read fBoldClientId write fBoldClientId;
  end;

  TBoldPMFetchIdListOperation = class(TBoldPersistenceOperation)
  private
    fObjectIdList: TBoldObjectIdList;
    fFetchMode: Integer;
    fCondition: TBoldCondition;
    fBoldClientID: TBoldClientID;
  protected
    function GetName: string; override;
  public
    procedure FreeParams; override;
    procedure InitParams; override;
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ObjectIdList: TBoldObjectIdList read fObjectIdList write fObjectIdList;
    property FetchMode: Integer read fFetchMode write fFetchMode;
    property Condition: TBoldCondition read fCondition write fCondition;
    property BoldClientID: TBoldClientID read fBoldClientID write fBoldClientID;
  end;

  TBoldPMExactifyIdsOperation = class(TBoldPersistenceOperation)
  private
    fObjectIdList: TBoldObjectIdList;
    fTranslationList: TBoldIdTranslationList;
    fHandleNonExisting: Boolean;
  protected
    function GetName: string; override;
  public
    procedure FreeParams; override;
    procedure InitParams; override;
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ObjectIdList: TBoldObjectIdList read fObjectIdList write fObjectIdList;
    property TranslationList: TBoldIdTranslationList read fTranslationList write fTranslationList;
    property HandleNonExisting: Boolean read fHandleNonExisting write fHandleNonExisting;
  end;

  TBoldPMUpdateOperation = class(TBoldPersistenceOperation)
  private
    fObjectIdList: TBoldObjectIdList;
    fValueSpace: IBoldValueSpace;
    fFreestanding: TBoldFreeStandingValueSpace;
    fPrecondition: TBoldUpdatePrecondition;
    fOld_Values: IBoldValueSpace;
    fFreestanding_old: TBoldFreeStandingValueSpace;
    fTranslationList: TBoldIdTranslationList;
    fTimestamp: TBoldTimestampType;
    fTimeOfTimeStamp: TDateTIme;
    fBoldClientID: TBoldClientID;
  protected
    function GetName: string; override;
  public
    procedure FreeParams; override;
    procedure InitParams; override;
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;
    procedure FreeTranslationList;

    property ObjectIdList: TBoldObjectIdList read fObjectIdList write fObjectIdList;
    property ValueSpace: IBoldValueSpace read fValueSpace write fValueSpace;
    property Old_Values: IBoldValueSpace read fOld_Values write fOld_Values;
    property Precondition: TBoldUpdatePrecondition read fPrecondition write fPrecondition;
    property TranslationList: TBoldIdTranslationList read fTranslationList write fTranslationList;
    property Timestamp: TBoldTimestampType read fTimestamp write fTimestamp;
    property BoldClientID: TBoldClientID read fBoldClientId write fBoldClientId;
  end;

  TBoldPMReserveNewIdsOperation = class(TBoldPersistenceOperation)
  private
    fObjectIdList: TBoldObjectIdList;
    fTranslationList: TBoldIdTranslationList;
  protected
    function GetName: string; override;
  public
    procedure FreeParams; override;
    procedure InitParams; override;
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ObjectIdList: TBoldObjectIdList read fObjectIdList write fObjectIdList;
    property TranslationList: TBoldIdTranslationList read fTranslationList write fTranslationList;
  end;

  TBoldPMTimestampForTimeOperation = class(TBoldPersistenceOperation)
  private
    fClockTime: TDateTime;
    fTimestamp: TBoldTimestampType;
  protected
    function GetName: string; override;
  public
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ClockTime: TDateTime read fClockTime write fClockTime;
    property Timestamp: TBoldTimestampType read fTimestamp write fTimestamp;
  end;

  TBoldPMTimeForTimestampOperation = class(TBoldPersistenceOperation)
  private
    fClockTime: TDateTime;
    fTimestamp: TBoldTimestampType;
  protected
    function GetName: string; override;
  public
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;

    property ClockTime: TDateTime read fClockTime write fClockTime;
    property Timestamp: TBoldTimestampType read fTimestamp write fTimestamp;
  end;

implementation

uses
 {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  SysUtils,
  BoldDefaultStreamNames;

const
  BoldNodeName_ObjectIdList = 'ObjectIdList';
  BoldNodeName_MemberIdList = 'MemberIdList';
  BoldNodeName_FetchMode = 'FetchMode';
  BoldNodeName_BoldClientID = 'BoldClientID';
  BoldNodeName_ValueSpace = 'ValueSpace';
  BoldNodeName_Condition = 'Condition';
  BoldNodeName_TranslationList = 'TranslationList';
  BoldNodeName_Precondition = 'Precondition';
  BoldNodeName_Old_Values = 'Old_Values';
  BoldNodeName_Timestamp = 'Timestamp';
  BoldNodeName_ClockTime = 'ClockTime';
  BoldNodeName_HandleNonExisting = 'HandleNonExisting';  

{ TBoldPMFetchOperation }

procedure TBoldPMFetchOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
  PersistenceController.PMFetch(ObjectIdList, ValueSpace, MemberIdList, FetchMode, BoldClientID);
end;

procedure TBoldPMFetchOperation.FreeParams;
begin
  inherited;
  FreeAndNil(fObjectIdList);
  FreeAndNil(fMemberIdList);
  fValueSpace := nil;
  FreeAndNil(fFreestanding);
end;

function TBoldPMFetchOperation.GetName: string;
begin
  result := 'PMFetch';
end;

procedure TBoldPMFetchOperation.InitParams;
begin
  inherited;
  fFreestanding := TBoldFreeStandingValueSpace.Create;
  fValueSpace := fFreestanding;
end;

procedure TBoldPMFetchOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  ObjectIdList := XMLNode.ReadSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  MemberIdList := XMLNode.ReadSubNodeObject(BoldNodeName_MemberIdList, BOLDMEMBERIDLISTNAME) as TBoldMemberIdList;
  FetchMode := XMLNode.ReadSubNodeInteger(BoldNodeName_FetchMode);
  BoldClientId := XMLNode.ReadSubNodeInteger(BoldNodeName_BoldClientID);
end;

procedure TBoldPMFetchOperation.ReadOutParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := XMLNode.GetSubNode(BoldNodeName_ValueSpace);
  StreamManager.ReadValueSpace(ValueSpace, aSubNode);
  aSubNode.Free;
end;

procedure TBoldPMFetchOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME, ObjectIdList);
  XMLNode.WriteSubNodeObject(BoldNodeName_MemberIdList,BOLDMEMBERIDLISTNAME, MemberIdList);
  XMLNode.WriteSubNodeInteger(BoldNodeName_FetchMode, FetchMode);
  XMLNode.WriteSubNodeInteger(BoldNodeName_BoldClientID, BoldClientID);
end;

procedure TBoldPMFetchOperation.WriteOutParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := XMLNode.NewSubNode(BoldNodeName_ValueSpace);
  StreamManager.WriteValueSpace(ValueSpace, ObjectIdList, nil, aSubNode);
  aSubNode.Free;
end;

{ TBoldPersistenceOperation }

constructor TBoldPersistenceOperation.Create(Manager: TBoldDefaultXMLStreamManager);
begin
  inherited Create;
  fStreamManager := Manager;
end;

procedure TBoldPersistenceOperation.ExecuteStreamed(
  PersistenceController: TBoldPersistenceController; RequestBodyNode,
  ReplyBodyNode: TBoldXMLNode);
var
  OpNode: TBoldXMLNode;
begin
  InitParams;

  OpNode := RequestBodyNode.GetSubNode(Name);
  ReadInParams(OpNode);
  OpNode.Free;
  try
    Execute(PersistenceController);

    OpNode := ReplyBodyNode.NewSubNode(Name + 'Response');
    WriteOutParams(OpNode);
  except on E: Exception do
    begin
    OpNode := ReplyBodyNode.NewSubNode('SOAP-ENV:Fault');
    OpNode := OpNode.NewSubNode('SOAP-ENV:faultstring');
    OpNode.WriteString(E.Message);
    end;
  end;
  OpNode.Free;

  FreeParams;
end;

procedure TBoldPersistenceOperation.FreeParams;
begin
end;

procedure TBoldPersistenceOperation.InitParams;
begin
end;

procedure TBoldPersistenceOperation.RemoteExecute(Stub: IBoldSOAPService);
var
  RequestDoc,
  ReplyDoc: {$IFDEF OXML}TXMLDocument{$ELSE}TDOMDocument{$ENDIF};
  BodyNode: TBoldXMLNode;
  OpNode: TBoldXMLNode;
  replyXML: WideString;
begin
  {$IFDEF OXML}
  RequestDoc := TXMLDocument.Create;
  ReplyDoc := TXMLDocument.Create;
  {$ELSE}
  RequestDoc := TDOMDocument.Create(nil);
  ReplyDoc := TDOMDocument.Create(nil);
  {$ENDIF}
  try
    BodyNode := StreamManager.NewSOAP(RequestDoc);
    OpNode := BodyNode.NewSubNode(Name);
    try
      WriteInParams(OpNode);
    finally
      BodyNode.Free;
      OpNode.Free;
    end;
    Stub.Get({$IFDEF OXML}RequestDoc.XML{$ELSE}
        RequestDoc.DefaultInterface.xml{$ENDIF}, replyXML);

    if not {$IFDEF OXML}ReplyDoc.LoadFromXML(replyXML){$ELSE}
      ReplyDoc.loadXML(replyXML){$ENDIF} then
    begin
      raise EBoldXMLLoadError.CreateFmt('%s: %s', [Replydoc.parseError.Reason, ReplyXML]) ;
    end;

    try
      BodyNode := StreamManager.GetSOAP(ReplyDoc);
    except
      raise EBoldInvalidSOAP.CreateFmt('Invalid SOAP data : %s', [replyXML]);
    end;

    OpNode := BodyNode.GetSubNode(Name + 'Response');
    if not Assigned(OpNode) then
    begin
      OpNode := BodyNode.GetSubNode('SOAP-ENV:Fault');
      if not Assigned(OpNode) then
        raise EBold.Create('Empty SOAP data.')
      else
      begin
        OpNode := OpNode.GetSubNode('SOAP-ENV:faultstring');
        if Assigned(OpNode) then
          raise EBold.CreateFmt('Error: %s', [OpNode.XMLDomElement.text])
        else
          raise EBold.Create('Error: empty error message') ;
      end;
    end;
    
    try
      ReadOutParams(OpNode);
    finally
      BodyNode.Free;
      OpNode.Free;
    end;
  finally
    RequestDoc.Free;
    ReplyDoc.Free;
  end;
end;

{ TBoldPMFetchIdListOperation }

procedure TBoldPMFetchIdListOperation.Execute(PersistenceController: TBoldPersistenceController);
var
  aValueSpace: TBoldFreeStandingValueSpace;
begin
  aValueSpace := TBoldFreeStandingValueSpace.create;
  try
    PersistenceController.PMFetchIDListWithCondition(ObjectIdList, aValueSpace, FetchMode, Condition, BoldClientId);
  finally
    aValueSpace.Free;
  end;
end;

procedure TBoldPMFetchIdListOperation.FreeParams;
begin
  inherited;
  FreeAndNil(fObjectIdList);
  FreeAndNil(fCondition);
end;

function TBoldPMFetchIdListOperation.GetName: string;
begin
  result := 'PMFetchIDListWithCondition';
end;

procedure TBoldPMFetchIdListOperation.InitParams;
begin
  inherited;
  fObjectIdList := TBoldObjectIdList.Create;
end;

procedure TBoldPMFetchIdListOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  FetchMode := XMLNode.ReadSubNodeInteger(BoldNodeName_FetchMode);
  Condition := XMLNode.ReadSubNodeObject(BoldNodeName_Condition, '') as TBoldCondition;
  BoldClientId := XMLNode.ReadSubNodeInteger(BoldNodeName_BoldClientID);
end;

procedure TBoldPMFetchIdListOperation.ReadOutParams(XMLNode: TBoldXMLNode);
var
  anIdList: TBoldObjectIdList;
  i: Integer;
begin
  anIDList := XMLNode.ReadSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  ObjectIdList.Clear;
  for i := 0 to anIdList.Count - 1 do
    ObjectIdList.Add(anIdList[i]);
  anIdList.Free;
end;

procedure TBoldPMFetchIdListOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeInteger(BoldNodeName_FetchMode, FetchMode);
  XMLNode.WriteSubNodeObject(BoldNodeName_Condition, '', Condition);
  XMLNode.WriteSubNodeInteger(BoldNodeName_BoldClientID, BoldClientID);
end;

procedure TBoldPMFetchIdListOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME, ObjectIdList);
end;

{ TBoldPMExactifyIdsOperation }

procedure TBoldPMExactifyIdsOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
  PersistenceController.PMExactifyIds(ObjectIdList, TranslationList, HandleNonExisting);
end;

procedure TBoldPMExactifyIdsOperation.FreeParams;
begin
  inherited;
  FreeAndNil(fObjectIdList);
  FreeAndNil(fTranslationList);
end;

function TBoldPMExactifyIdsOperation.GetName: string;
begin
  result := 'PMExactifyIds';
end;

procedure TBoldPMExactifyIdsOperation.InitParams;
begin
  inherited;
  fTranslationList := TBoldIDTranslationList.Create;
end;

procedure TBoldPMExactifyIdsOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  ObjectIdList := XMLNode.ReadSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  HandleNonExisting := XMLNode.ReadSubNodeBoolean(BoldNodeName_HandleNonExisting);  
end;

procedure TBoldPMExactifyIdsOperation.ReadOutParams(XMLNode: TBoldXMLNode);
var
  aTranslationList: TBoldIDTranslationList;
  i: Integer;
begin
  aTranslationList := XMLNode.ReadSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME) as TBoldIDTranslationList;
  try
    for i := 0 to aTranslationList.Count - 1 do
      TranslationList.AddTranslation(aTranslationList.OldIds[i], aTranslationList.NewIds[i]);
  finally
    FreeAndNil(aTranslationList);
  end;
end;

procedure TBoldPMExactifyIdsOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME, ObjectIdList);
  XMLNode.WriteSubNodeBoolean(BoldNodeName_HandleNonExisting, fHandleNonExisting);
end;

procedure TBoldPMExactifyIdsOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME, TranslationList);
end;

{ TBoldPMUpdateOperation }

procedure TBoldPMUpdateOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
  PersistenceController.PMUpdate(ObjectIdList, ValueSpace, Old_Values, Precondition, TranslationList, fTimestamp, fTimeOfTimeStamp, BoldClientID);
end;

procedure TBoldPMUpdateOperation.FreeParams;
begin
  FreeAndNil(fObjectIdList);
  fValueSpace := nil;
  FreeAndNil(fFreestanding);
  fOld_Values := nil;
  FreeAndNil(fFreestanding_old);
  FreeAndNil(fTranslationList);
  FreeAndNil(fPrecondition);
end;

procedure TBoldPMUpdateOperation.FreeTranslationList;
begin
  FreeAndNil(fTranslationList);
end;

function TBoldPMUpdateOperation.GetName: string;
begin
  result := 'PMUpdate';
end;

procedure TBoldPMUpdateOperation.InitParams;
begin
  fFreestanding := TBoldFreeStandingValueSpace.Create;
  fValueSpace := fFreestanding;
  fFreestanding_old := TBoldFreeStandingValueSpace.Create;
  fOld_Values := fFreestanding_old;
  fTranslationList := TBoldIDTranslationList.Create;
end;

procedure TBoldPMUpdateOperation.ReadInParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  ObjectIdList := XMLNode.ReadSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  aSubNode := XMLNode.GetSubNode(BoldNodeName_ValueSpace);
  StreamManager.ReadValueSpace(ValueSpace, aSubNode);
  aSubNode.Free;
  Precondition := XMLNode.ReadSubNodeObject(BoldNodeName_Precondition, '') as TBoldUpdatePrecondition;
  BoldClientID := XMLNode.ReadSubNodeInteger(BoldNodeName_BoldClientID);
end;

procedure TBoldPMUpdateOperation.ReadOutParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
  aTranslationList: TBoldIdTranslationList;
  TempPrecondition: TBoldUpdatePrecondition;
  i: Integer;
begin
  aSubNode := XMLNode.GetSubNode(BoldNodeName_Old_Values);
  StreamManager.ReadValueSpace(Old_Values, aSubNode);
  aSubNode.Free;

  if assigned(Precondition) then
  begin
    TempPrecondition := XMLNode.ReadSubNodeObject(BoldNodeName_Precondition, '') as TBoldUpdatePrecondition;
    Precondition.AssignOutValues(TempPrecondition);
    TempPrecondition.Free;
  end;

  aTranslationList := XMLNode.ReadSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME) as TBoldIDTranslationList;
  for i := 0 to aTranslationList.Count - 1 do
    TranslationList.AddTranslation(aTranslationList.OldIds[i], aTranslationList.NewIds[i]);
  aTranslationList.Free;

  fTimestamp := XMLNode.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldPMUpdateOperation.WriteInParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME, ObjectIdList);
  aSubNode := XMLNode.NewSubNode(BoldNodeName_ValueSpace);
  StreamManager.WriteValueSpace(ValueSpace, ObjectIdList, nil, aSubNode);
  aSubNode.Free;
  XMLNode.WriteSubNodeObject(BoldNodeName_Precondition, '', Precondition);
  XMLNode.WriteSubNodeInteger(BoldNodeName_BoldClientID, BoldClientID);
end;

procedure TBoldPMUpdateOperation.WriteOutParams(XMLNode: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  aSubNode := XMLNode.NewSubNode(BoldNodeName_Old_Values);
  StreamManager.WriteValueSpace(Old_Values, ObjectIdList, nil, aSubNode);
  aSubNode.Free;
  XMLNode.WriteSubNodeObject(BoldNodeName_Precondition, '', Precondition);
  XMLNode.WriteSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME, TranslationList);
  XMLNode.WriteSubNodeInteger(BoldNodeName_Timestamp, Timestamp);
end;

{ TBoldPMReserveNewIdsOperation }

procedure TBoldPMReserveNewIdsOperation.Execute(PersistenceController: TBoldPersistenceController);
var
  aValueSpace: TBoldFreeStandingValueSpace;
begin
  aValueSpace := TBoldFreeStandingValueSpace.create;
  try
    PersistenceController.ReserveNewIds(aValueSpace, ObjectIdList, TranslationList);
  finally
    aValueSpace.Free;
  end;
end;

procedure TBoldPMReserveNewIdsOperation.FreeParams;
begin
  FreeAndNil(fObjectIdList);
  FreeAndNil(fTranslationList);
end;

function TBoldPMReserveNewIdsOperation.GetName: string;
begin
  result := 'ReserveNewIds';
end;

procedure TBoldPMReserveNewIdsOperation.InitParams;
begin
  TranslationList := TBoldIDTranslationList.Create;
end;

procedure TBoldPMReserveNewIdsOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  ObjectIdList := XMLNode.ReadSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
end;

procedure TBoldPMReserveNewIdsOperation.ReadOutParams(XMLNode: TBoldXMLNode);
var
  aTranslationList: TBoldIDTranslationList;
  i: Integer;
begin
  aTranslationList := TBoldIDTranslationList.Create;
  try
    aTranslationList := XMLNode.ReadSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME) as TBoldIDTranslationList;
    for i := 0 to aTranslationList.Count - 1 do
      TranslationList.AddTranslation(aTranslationList.OldIds[i], aTranslationList.NewIds[i]);
  finally
    aTranslationList.Free;
  end;
end;

procedure TBoldPMReserveNewIdsOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_ObjectIdList, BOLDOBJECTIDLISTNAME, ObjectIdList);
end;

procedure TBoldPMReserveNewIdsOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeObject(BoldNodeName_TranslationList, BOLDIDTRANSLATIONLISTNAME, TranslationList);
end;

{ TBoldPMTimestampForTimeOperation }

procedure TBoldPMTimestampForTimeOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
  PersistenceController.PMTimestampForTime(ClockTime, fTimestamp);
end;

function TBoldPMTimestampForTimeOperation.GetName: string;
begin
  result := 'PMTimestampForTime';
end;

procedure TBoldPMTimestampForTimeOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  ClockTime := StrToDateTime(XMLNode.ReadSubNodeString(BoldNodeName_ClockTime));
end;

procedure TBoldPMTimestampForTimeOperation.ReadOutParams(XMLNode: TBoldXMLNode);
begin
  Timestamp := XMLNode.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldPMTimestampForTimeOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeString(BoldNodeName_ClockTime, DateTimeToStr(ClockTime));
end;

procedure TBoldPMTimestampForTimeOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeInteger(BoldNodeName_Timestamp, Timestamp);
end;

{ TBoldPMTimeForTimestampOperation }

procedure TBoldPMTimeForTimestampOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
  PersistenceController.PMTimeForTimestamp(Timestamp, fClockTime);
end;

function TBoldPMTimeForTimestampOperation.GetName: string;
begin
  result := 'PMTimeForTimestamp';
end;

procedure TBoldPMTimeForTimestampOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
  Timestamp := XMLNode.ReadSubNodeInteger(BoldNodeName_Timestamp);
end;

procedure TBoldPMTimeForTimestampOperation.ReadOutParams(XMLNode: TBoldXMLNode);
begin
  ClockTime := StrToDateTime(XMLNode.ReadSubNodeString(BoldNodeName_ClockTime));
end;

procedure TBoldPMTimeForTimestampOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeInteger(BoldNodeName_Timestamp, Timestamp);
end;

procedure TBoldPMTimeForTimestampOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeString(BoldNodeName_ClockTime, DateTimeToStr(ClockTime));
end;

end.
