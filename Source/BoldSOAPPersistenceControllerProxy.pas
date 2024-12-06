
{ Global compiler directives }
{$include bold.inc}
unit BoldSOAPPersistenceControllerProxy;

interface

uses
  BoldComConnection,
  BoldId,
  BoldCondition,
  BoldSubscription,
  BoldValueSpaceInterfaces,
  BoldDefaultXMLStreaming,
  BoldAbstractComPersistenceControllerProxy,
  BoldSOAP_TLB,
  BoldUpdatePrecondition,
  BoldPersistenceOperationXMLStreaming,
  BoldMeta,
  BoldDefs,
  BoldElements;

type
  { forward declarations }
  TBoldSOAPPersistenceControllerProxy = class;

  {-- TBoldSOAPPersistenceControllerProxy --}
  TBoldSOAPPersistenceControllerProxy = class(TBoldAbstractComPersistenceControllerProxy)
  private
    fStreamManager: TBoldDefaultXMLStreamManager;
    fFetchIdListOp: TBoldPMFetchIdListOperation;
    fFetchOp: TBoldPMFetchOperation;
    fExactifyOp: TBoldPMExactifyIdsOperation;
    fUpdateOp: TBoldPMUpdateOperation;
    function GetExactifyOp: TBoldPMExactifyIdsOperation;
    function GetFetchIdListOp: TBoldPMFetchIdListOperation;
    function GetFetchOp: TBoldPMFetchOperation;
    function GetUpdateOp: TBoldPMUpdateOperation;
  protected
    fStub: IBoldSOAPService;
    function GetIsConnected: Boolean; override;
    property FetchIdListOp: TBoldPMFetchIdListOperation read GetFetchIdListOp;
    property FetchOp: TBoldPMFetchOperation read GetFetchOp;
    property ExactifyOp: TBoldPMExactifyIdsOperation read GetExactifyOp;
    property UpdateOp: TBoldPMUpdateOperation read GetUpdateOp;
  public
    constructor Create(Model: TMoldModel);
    destructor Destroy; override;
    procedure Connect(const Provider: IBoldProvider; const ObjectName: string); override;
    procedure Disconnect; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
      MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
      Old_Values: IBoldValueSpace;
      Precondition: TBoldUpdatePrecondition;
      TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime;
      BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList;
      TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList;
      TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                  TranslationList: TBoldIdTranslationList); override;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
  end;

implementation

uses
  BoldComUtils,
  SysUtils,
  BoldCursorGuard;

{ TBoldSOAPPersistenceControllerProxy }

function TBoldSOAPPersistenceControllerProxy.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
const
  sMethodNotImplemented = '%s.%s: not supported/implemented';  
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'CanEvaluateInPS']); // do not localize
end;

procedure TBoldSOAPPersistenceControllerProxy.Connect(
  const Provider: IBoldProvider; const ObjectName: string);
var
  Unk: IUnknown;
  ObjectNamews: WideString;
begin
  ObjectNamews := Objectname;
  Unk := Provider.GetObject(ObjectNameWS);
  if not Assigned(Unk) then
    raise EBoldCom.CreateFmt('Failed connecting to COM object ''%s''.', [ObjectName]);
  if Unk.QueryInterface(IBoldSOAPService, fStub) <> 0 then
    raise EBoldCom.CreateFmt('COM object ''%s'' is not a persistence controller.', [ObjectName]);
end;

constructor TBoldSOAPPersistenceControllerProxy.Create(Model: TMoldModel);
begin
  inherited Create;
  fStreamManager := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
                                                        Model);
end;

destructor TBoldSOAPPersistenceControllerProxy.Destroy;
begin
  Disconnect;
  FreeAndNil(fFetchIdListOp);
  FreeAndNil(fFetchOp);
  FreeAndNil(fExactifyOp);
  FreeAndNil(fUpdateOp);
  if Assigned(fStreamManager) then
    FreeAndNil(fStreamManager);
  inherited;
end;

procedure TBoldSOAPPersistenceControllerProxy.Disconnect;
begin
  fStub := nil;
end;

function TBoldSOAPPersistenceControllerProxy.GetExactifyOp: TBoldPMExactifyIdsOperation;
begin
  if not Assigned(fExactifyOp) then
    fExactifyOp := TBoldPMExactifyIdsOperation.Create(fStreamManager);
  Result := fExactifyOp;
end;

function TBoldSOAPPersistenceControllerProxy.GetFetchIdListOp: TBoldPMFetchIdListOperation;
begin
  if not Assigned(fFetchIdListOp) then
    fFetchIdListOp := TBoldPMFetchIdListOperation.Create(fStreamManager);
  Result := fFetchIdListOp;
end;

function TBoldSOAPPersistenceControllerProxy.GetFetchOp: TBoldPMFetchOperation;
begin
  if not Assigned(fFetchOp) then
    fFetchOp := TBoldPMFetchOperation.Create(fStreamManager);
  Result := fFetchOp;
end;

function TBoldSOAPPersistenceControllerProxy.GetIsConnected: Boolean;
begin
  result := assigned(fStub);
end;

function TBoldSOAPPersistenceControllerProxy.GetUpdateOp: TBoldPMUpdateOperation;
begin
  if not Assigned(fUpdateOp) then
    fUpdateOp := TBoldPMUpdateOperation.Create(fStreamManager);
  Result := fUpdateOp;
end;

procedure TBoldSOAPPersistenceControllerProxy.PMExactifyIds(
  ObjectIdList: TBoldObjectidList;
  TranslationList: TBoldIDTranslationList;
  HandleNonExisting: Boolean);
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMFetch: Not connected.', [ClassName]);
  ExactifyOp.ObjectIdList := ObjectIdList;
  ExactifyOp.TranslationList := TranslationList;
  ExactifyOp.RemoteExecute(fStub);
end;

procedure TBoldSOAPPersistenceControllerProxy.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
var
  CursorGuard: IBoldCursorGuard;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMFetch: Not connected.', [ClassName]);

  try
    CursorGuard := TBoldCursorGuard.Create;
    SendExtendedEvent(bpeStartFetch, [ObjectIdList, MemberIdList]);
    ExactifyIDs(ObjectIdList, ValueSpace);

    FetchOp.ObjectIdList := ObjectIdList;
    FetchOp.ValueSpace := ValueSpace;
    FetchOp.MemberIdList := MemberIdList;
    FetchOp.FetchMode := FetchMode;
    FetchOp.BoldClientID := BoldClientID;
    SendEvent(bpeFetchObject);
    FetchOp.RemoteExecute(fStub);
  finally
    SendEvent(bpeEndFetch);
  end;
end;

procedure TBoldSOAPPersistenceControllerProxy.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition;
  BoldClientID: TBoldClientID);
var
  CursorGuard: IBoldCursorGuard;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMFetchIDListWithCondition: Not connected.', [ClassName]);

  CursorGuard := TBoldCursorGuard.Create;
  SendExtendedEvent(bpeStartFetchId, [Condition]);
  FetchIdListOp.ObjectIdList := ObjectIdList;
  FetchIdListOp.FetchMode := FetchMode;
  FetchIdListOp.Condition := Condition;
  FetchIdListOp.BoldClientID := BoldClientID;

  FetchIdListOp.RemoteExecute(fStub);
  SendEvent(bpeEndFetchId);
end;

procedure TBoldSOAPPersistenceControllerProxy.PMSetReadOnlyness(
  ReadOnlyList, WriteableList: TBoldObjectIdList);
begin

end;

procedure TBoldSOAPPersistenceControllerProxy.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
var
  Op: TBoldPMTimeForTimestampOperation;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMUpdate: Not connected.', [ClassName]);

  Op := TBoldPMTimeForTimestampOperation.Create(fStreamManager);
  try
    Op.Timestamp := Timestamp;
    Op.RemoteExecute(fStub);

    ClockTime := Op.ClockTime;
  finally
    Op.Free;
  end;
end;

procedure TBoldSOAPPersistenceControllerProxy.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
var
  Op: TBoldPMTimestampForTimeOperation;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMUpdate: Not connected.', [ClassName]);

  Op := TBoldPMTimestampForTimeOperation.Create(fStreamManager);
  try
    Op.ClockTime := ClockTime;
    Op.RemoteExecute(fStub);

    Timestamp := Op.Timestamp;
  finally
    Op.Free;
  end;
end;

procedure TBoldSOAPPersistenceControllerProxy.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin

end;

procedure TBoldSOAPPersistenceControllerProxy.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin

end;

procedure TBoldSOAPPersistenceControllerProxy.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace, Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition; 
  TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
var
  CursorGuard: IBoldCursorGuard;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.PMUpdate: Not connected.', [ClassName]);

  try
    CursorGuard := TBoldCursorGuard.Create;
    SendExtendedEvent(bpeStartUpdate, [ObjectIdList, valueSpace]);
    UpdateOp.ObjectIdList := ObjectIdList;
    UpdateOp.ValueSpace := ValueSpace;
    UpdateOp.Old_Values := Old_Values;
    UpdateOp.Precondition := Precondition;
    if assigned(TranslationList) then
      UpdateOp.TranslationList := TranslationList
    else
      UpdateOp.TranslationList := TBoldIDTranslationList.Create;
    UpdateOp.BoldClientID := BoldClientID;
    SendEvent(bpeUpdateObject);
    UpdateOp.RemoteExecute(fStub);

    if assigned(Precondition) and Precondition.failed then
    else
    begin
      TimeStamp := UpdateOp.Timestamp;
      TimeOfLatestUpdate := now; // TODO implement
      ValueSpace.ApplytranslationList(UpdateOp.TranslationList);
    end;
  finally
    if not Assigned(TranslationList) then
      UpdateOp.FreeTranslationList;
    SendEvent(bpeEndUpdate);  
  end;
end;

procedure TBoldSOAPPersistenceControllerProxy.ReserveNewIds(
  ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
var
  ReserveOp: TBoldPMReserveNewIdsOperation;
begin
  if not Connected then
    raise EBoldCom.CreateFmt('%s.ReserveNewIds: Not connected.', [ClassName]);

  ReserveOp := TBoldPMReserveNewIdsOperation.Create(fStreamManager);
  try
    ReserveOp.ObjectIdList := ObjectIdList;
    ReserveOp.TranslationList := TranslationList;

    ReserveOp.RemoteExecute(fStub);
  finally
    ReserveOp.Free;
  end;
end;

procedure TBoldSOAPPersistenceControllerProxy.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  if Events <> [] then
    AddSmallSubscription(Subscriber, Events)
  else
  begin
    AddSubscription(Subscriber, bpeStartFetch, bpeStartFetch);
    AddSubscription(Subscriber, bpeEndFetch, bpeEndFetch);

    AddSubscription(Subscriber, bpeStartUpdate, bpeStartUpdate);
    AddSubscription(Subscriber, bpeEndUpdate, bpeEndUpdate);

    AddSubscription(Subscriber, bpeStartFetchId, bpeStartFetchId);
    AddSubscription(Subscriber, bpeEndFetchId, bpeEndFetchId);

    AddSubscription(Subscriber, bpeFetchObject, bpeFetchObject);
    AddSubscription(Subscriber, bpeUpdateObject, bpeUpdateObject);
    AddSubscription(Subscriber, bpeCreateObject, bpeCreateObject);
    AddSubscription(Subscriber, bpeDeleteObject, bpeDeleteObject);
  end;
end;

end.
