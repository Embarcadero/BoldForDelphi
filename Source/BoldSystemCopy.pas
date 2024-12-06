unit BoldSystemCopy;

interface

uses
  Classes,
  BoldSystemRT,
  BoldSystem,
  BoldSystemHandle,
  BoldId,
  BoldDomainElement,
  BoldDefs,
  BoldUMLTypes,
  System.SysUtils,
  Winapi.Windows;

type
  TBoldSystemCopy = class;

  // user events
  TBoldSystemCopyClassEvent = procedure(Sender: TBoldSystemCopy; const AClass: TBoldClassTypeInfo; var ASkip: boolean) of object;
  TBoldSystemCopyObjectEvent = procedure(Sender: TBoldSystemCopy; const ASourceObject: TBoldobject; var ASkip: boolean) of object;
  TBoldSystemCopyLinkEvent = procedure(Sender: TBoldSystemCopy; const ASourceLink: TBoldMember; var ASkip: boolean) of object;
  TBoldSystemCopyAttributesEvent = procedure(Sender: TBoldSystemCopy; const ASourceObject: TBoldObject; const ADestinationObject: TBoldObject) of object;
  TBoldSystemCopyFindObjectEvent = procedure(Sender: TBoldSystemCopy; const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject) of object;
  TBoldSystemCopyProgressEvent = procedure(Sender: TBoldSystemCopy; AProgress: integer; AMax: integer; AEstimatedRemainingTime: TDateTime) of object;
  TBoldSystemCopyLogEvent = procedure(Sender: TBoldSystemCopy; const aStatus: string) of object;

  EBoldSystemCopyInterrupt = class(Exception);

  TBoldSystemCopy = class(TComponent)
  private
    fObjectEvent: TBoldSystemCopyObjectEvent;
    fDestinationSystemHandle: TBoldSystemHandle;
    fSourceSystemHandle: TBoldSystemHandle;
    fAttributesEvent: TBoldSystemCopyAttributesEvent;
    fBatchSize: integer;
    fClassEvent: TBoldSystemCopyClassEvent;
    fProgressEvent: TBoldSystemCopyProgressEvent;
    fLogEvent: TBoldSystemCopyLogEvent;
    fHeadTailRoles: TBoldRoleRTInfoList;
    fDelayedProcessList: TBoldObjectList;
    fProcessedObjects: TBoldObjectList;
    fSkippedObjects: TBoldObjectList;
    fLastUsedId: integer;
    fStopped: boolean;
    function GetDestinationSystem: TBoldSystem;
    function GetSourceSystem: TBoldSystem;
    function FindHead(AClassTypeInfo: TBoldClassTypeInfo): TBoldRoleRTInfo;
    function FindTail(AClassTypeInfo: TBoldClassTypeInfo): TBoldRoleRTInfo;
    procedure CollectMembers(AClassTypeInfo: TBoldClassTypeInfo; AMemberIdList: TBoldMemberIdList; ARoleTypes: TBoldRoleSet; AAttributes: boolean; ADerived: boolean = false; ADelayedFetched: boolean = false);
    procedure CopyInstances;
    procedure CopyMembers(ASource: TBoldObjectLocator; AMemberIdList: TBoldMemberIdList = nil; ATouchedList: TBoldObjectList = nil);
    procedure TranslateId(ASourceObject, ADestinationObject: TBoldObject);
    procedure UpdateLastUsedID;
    procedure CheckStop;
  protected
    procedure DoLogEvent(const Msg: string);
    procedure Report(const Msg: string; const Args: array of const);
    function SkipObject(const SourceObject: TBoldObject): boolean;
    function EnsureDestinationObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject): boolean;
    function EnsureDestinationLocator(const ASourceLocator: TBoldObjectLocator; var ADestinationLocator: TBoldObjectLocator): boolean;
    procedure FindDestinationObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
    procedure FindDestinationLocator(const ASourceLocator: TBoldObjectLocator; var ADestinationLocator: TBoldObjectLocator);
    procedure CloneObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
    function GetSourceAllInstanceCount: integer;
    function GetDestinationAllInstanceCount: integer;
    function GetAllInstanceCount(ASystemHandle: TBoldSystemHandle): integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Run;
    procedure Stop;
    procedure CheckModelCompatibility;
    procedure AnalyzeModel;
    property SourceSystem: TBoldSystem read GetSourceSystem;
    property DestinationSystem: TBoldSystem read GetDestinationSystem;
    property SourceAllInstanceCount: integer read GetSourceAllInstanceCount;
    property DestinationAllInstanceCount: integer read GetDestinationAllInstanceCount;
  published
    property SourceSystemHandle: TBoldSystemHandle read fSourceSystemHandle write fSourceSystemHandle;
    property DestinationSystemHandle: TBoldSystemHandle read fDestinationSystemHandle write fDestinationSystemHandle;
    property OnClass: TBoldSystemCopyClassEvent read fClassEvent write fClassEvent;
    property OnObject: TBoldSystemCopyObjectEvent read fObjectEvent write fObjectEvent;
    property OnAttributes: TBoldSystemCopyAttributesEvent read fAttributesEvent write fAttributesEvent;
    property OnLog: TBoldSystemCopyLogEvent read fLogEvent write fLogEvent;
    property OnProgress: TBoldSystemCopyProgressEvent read fProgressEvent write fProgressEvent;
    property BatchSize: integer read fBatchSize write fBatchSize;
  end;

implementation

uses
  Vcl.Dialogs,
  Vcl.Controls,
  System.UITypes,

  BoldLinks,
  BoldDBInterfaces,
  BoldTaggedValueSupport,
  BoldUMLModelSupport,
  BoldDefaultID,
  BoldMath,
  BoldGuard,
  BoldBase,
  BoldIndexableList,
  BoldIndex,
  BoldElements,
  BoldUndoHandler,
  BoldMetaElementList,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldUNdoInterfaces,
  BoldDefaultTaggedValues,
  BoldModel;

{ TBoldSystemCopy }

procedure TBoldSystemCopy.AfterConstruction;
begin
  inherited;
  fHeadTailRoles := TBoldRoleRTInfoList.Create;
  fHeadTailRoles.OwnsEntries := false;
  fDelayedProcessList := TBoldObjectList.Create;
  fDelayedProcessList.DuplicateMode := bldmMerge;
  fProcessedObjects := TBoldObjectList.Create;
  fSkippedObjects := TBoldObjectList.Create;
  BatchSize := 1000;
end;

procedure TBoldSystemCopy.BeforeDestruction;
begin
  fHeadTailRoles.free;
  fDelayedProcessList.free;
  fProcessedObjects.free;
  fSkippedObjects.free;
  inherited;
end;

procedure TBoldSystemCopy.AnalyzeModel;
var
  TopSortedClasses: TBoldClassTypeInfoList;
  ClassTypeInfo: TBoldClassTypeInfo;
  RoleTypeInfo: TBoldRoleRTInfo;
  HeadRoleTypeInfo: TBoldRoleRTInfo;
  i: integer;
begin
  TopSortedClasses := self.fSourceSystemHandle.StaticSystemTypeInfo.TopSortedClasses;
  for ClassTypeInfo in TopSortedClasses do
  begin
    with ClassTypeInfo do
      if IsAbstract or IsLinkClass or (not Persistent) then
        Continue;

    for RoleTypeInfo in ClassTypeInfo.AllRoles do
    begin
      if not RoleTypeInfo.Persistent then
        Continue;
      if (RoleTypeInfo.Aggregation = akComposite) and RoleTypeInfo.RoleRTInfoOfOtherEnd.IsMultiRole then
        Report(RoleTypeInfo.DebugInfo, []);
      if (RoleTypeInfo.Aggregation = akComposite) {and RoleTypeInfo.RoleRTInfoOfOtherEnd.Mandatory} then
        fHeadTailRoles.Add(RoleTypeInfo);
    end;
  end;

  for ClassTypeInfo in TopSortedClasses do
  begin
    with ClassTypeInfo do
      if IsAbstract or IsLinkClass or (not Persistent) then
        Continue;

    for RoleTypeInfo in ClassTypeInfo.AllRoles do
    begin
      if not RoleTypeInfo.Persistent then
        Continue;
      if (RoleTypeInfo.RoleRTInfoOfOtherEnd.Aggregation = akComposite) {and RoleTypeInfo.Mandatory }then
      begin
        HeadRoleTypeInfo := FindHead(ClassTypeInfo);
        if Assigned(HeadRoleTypeInfo) then
          fHeadTailRoles.Remove(HeadRoleTypeInfo);
      end;
    end;
  end;
  for i := fHeadTailRoles.Count-1 downto 0 do
  begin
    if FindTail(fHeadTailRoles[i].ClassTypeInfo) <> nil then
      fHeadTailRoles.RemoveByIndex(i);
  end;
end;

procedure TBoldSystemCopy.CheckModelCompatibility;
var
  SourceClasses, DestinationClasses: TBoldClassTypeInfoList;
  SourceClass, DestinationClass: TBoldClassTypeInfo;
  SourceMember, DestinationMember: TBoldMemberRtInfo;
  Errors: TStringList;
begin
  SourceClasses := fSourceSystemHandle.StaticSystemTypeInfo.TopSortedClasses;
  DestinationClasses := fDestinationSystemHandle.StaticSystemTypeInfo.TopSortedClasses;
  Errors := TStringList.Create;
  try
    for SourceClass in SourceClasses do
    begin
      DestinationClass := DestinationClasses.ItemsByExpressionName[SourceClass.ExpressionName];
      if not Assigned(DestinationClasses) then
        Errors.Add(Format('Class %s does not exist in destination model.', [SourceClass.ExpressionName]));
      for SourceMember in SourceClass.AllMembers do
      begin
        DestinationMember := DestinationClass.MemberRTInfoByExpressionName[SourceMember.ExpressionName];
        if not Assigned(DestinationMember) then
          Errors.Add(Format('Member %s.%s does not exist in destination class.', [SourceClass.ExpressionName, SourceMember.ExpressionName]));
      end;
    end;
    if Errors.Count > 0 then
      raise Exception.Create(Errors.Text);
  finally
    Errors.Free;
  end;
end;

procedure TBoldSystemCopy.CheckStop;
begin
  if fStopped then
  begin
    fStopped := false;
    DoLogEvent('Use requested abort');
    raise EBoldSystemCopyInterrupt.Create('Use requested abort.');
  end;
end;

procedure TBoldSystemCopy.CloneObject(const ASourceObject: TBoldObject;
  var ADestinationObject: TBoldObject);
begin
  if ASourceObject.BoldClassTypeInfo.IsLinkClass then
  begin
    ADestinationObject := nil;
    exit;
  end;
  ADestinationObject := DestinationSystem.CreateNewObjectFromClassTypeInfo(DestinationSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ASourceObject.BoldClassTypeInfo.ExpressionName]);
  TranslateId(ASourceObject, ADestinationObject);
  TBoldCopyAndClone.BoldCopy(ADestinationObject, ASourceObject, bcmAttributes, false);
  if Assigned(fAttributesEvent) then
    OnAttributes(self, ASourceObject, ADestinationObject);
end;

procedure TBoldSystemCopy.CollectMembers(AClassTypeInfo: TBoldClassTypeInfo;
  AMemberIdList: TBoldMemberIdList; ARoleTypes: TBoldRoleSet; AAttributes,
  ADerived, ADelayedFetched: boolean);
var
  RoleRTInfo: TBoldRoleRTInfo;
  MemberRtInfo: TBoldMemberRTInfo;
begin
  Assert(Assigned(AMemberIdList));
  for MemberRtInfo in AClassTypeInfo.AllMembers do
  begin
    if not MemberRtInfo.Persistent then
      continue;
    if MemberRtInfo.IsDerived then
      continue;
    if MemberRtInfo.IsAttribute and AAttributes then
    begin
      AMemberIdList.Add(TBoldMemberId.Create(MemberRtInfo.index));
    end
    else
    if MemberRtInfo.IsRole then
    begin
      RoleRTInfo := MemberRtInfo as TBoldRoleRTInfo;
      if (RoleRTInfo.RoleType in ARoleTypes) then
        AMemberIdList.Add(TBoldMemberId.Create(RoleRTInfo.index));
    end;
  end;
end;

procedure TBoldSystemCopy.CopyInstances;
var
  SourceLocator, DestinationLocator: TBoldObjectLocator;
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
  DestinationList: TBoldObjectList;
  BatchList: TBoldObjectList;
  FilteredList: TBoldObjectList;
  MemberIdList1, MemberIdList2: TBoldMemberIdList;
  RoleTypeInfo: TBoldRoleRTInfo;
  i, iStart, iEnd: integer;
  DirtyObject: TBoldObject;
  DirtyObjects: TList;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(BatchList, MemberIdList1, MemberIdList2, DestinationList, DirtyObjects);
  BatchList := TBoldObjectList.Create;
  BatchList.Capacity := BatchSize;
  MemberIdList1 := TBoldMemberIdList.Create;
  MemberIdList1.OwnsEntries := true;
  MemberIdList2 := TBoldMemberIdList.Create;
  MemberIdList2.OwnsEntries := true;
  DestinationList := TBoldObjectList.Create;
  DirtyObjects := TList.Create;
  Report('-------------',[]);
  Report('Copying objects', []);
  Report('-------------',[]);

  for ClassTypeInfo in SourceSystem.BoldSystemTypeInfo.TopSortedClasses do
  begin
    CheckStop;
    if ClassTypeInfo.IsAbstract or ClassTypeInfo.IsLinkClass or not ClassTypeInfo.Persistent then
      continue; // skip abstract, link and transient classes
    RoleTypeInfo := FindTail(ClassTypeInfo);
    if Assigned(RoleTypeInfo) then
    begin
      Report('Skipping tail class %s', [ClassTypeInfo.DebugInfo]);
      Continue; // skip tail classes
    end;
    ClassList := SourceSystem.Classes[ClassTypeInfo.TopSortedIndex];
    if ClassList.Empty then
    begin
      Report('Skipping empty class %s', [ClassTypeInfo.DebugInfo]);
      continue;
    end;
    MemberIdList1.Clear;
    CollectMembers(ClassTypeInfo, MemberIdList1, [rtRole, rtLinkRole], true, false, true);
    Report('%d IDs loaded for class %s', [ClassList.Count, ClassTypeInfo.DebugInfo]);
    iStart := 0;
    iEnd := MinIntValue([ClassList.Count-1, BatchSize]);
    repeat
      CheckStop;
      Assert(iStart<=iEnd);
      Report('Loading Objects %d-%d for: %s', [iStart, iEnd, ClassTypeInfo.DisplayName]);
      ClassList.EnsureRange(iStart, iEnd);
      for i := iStart to iEnd do
      begin
        Assert(Assigned(ClassList.Locators[i].BoldObject));
        BatchList.AddLocator(ClassList.Locators[i]);
      end;
      FilteredList := BatchList.FilterOnType(ClassTypeInfo, false);
      Assert(iStart<=iEnd);
      Report('Loading %d links %d-%d for: %s', [MemberIdList1.Count, iStart, iEnd, ClassTypeInfo.DisplayName]);
      SourceSystem.FetchMembersWithObjects(FilteredList, MemberIdList1);
      Report('Copying links %d-%d for: %s', [iStart, iEnd, ClassTypeInfo.DisplayName]);
      CheckStop;
      FilteredList.EnsureObjects;
      CheckStop;
      DestinationSystem.StartTransaction;
      try
        for I := 0 to FilteredList.Count - 1 do
        begin
          SourceLocator := FilteredList.Locators[i];
          DestinationLocator := nil;
          if SkipObject(SourceLocator.EnsuredBoldObject) then
            Continue;
          EnsureDestinationLocator(SourceLocator, DestinationLocator);
          CopyMembers(SourceLocator, MemberIdList1, DestinationList);
        end;
        if DestinationSystem.BoldDirty then
        begin
          Report('Writing %d objects', [DestinationSystem.DirtyObjects.Count]);
          while (DestinationSystem.DirtyObjects.Count <> DirtyObjects.Count) do
          begin
            {$WARN UNSAFE_CAST OFF}
            DirtyObject := TBoldObject(DestinationSystem.DirtyObjects.Items[0]);
            DirtyObjects.Add(DirtyObject);
            {$WARN UNSAFE_CAST ON}
            if DirtyObject.BoldClassTypeInfo.IsLinkClass then
            begin
              DestinationLocator := DirtyObject.BoldObjectLocator;
              SourceLocator := SourceSystem.Locators.LocatorByID[DestinationLocator.BoldObjectID];
              MemberIdList2.clear;
              CollectMembers(SourceLocator.BoldClassTypeInfo, MemberIdList2, [rtRole], true, false, true);
              if MemberIdList2.Count = 0 then
                continue;
              CopyMembers(SourceLocator, MemberIdList2, DestinationList);
              if Assigned(fAttributesEvent) then
                OnAttributes(self, SourceLocator.BoldObject, DestinationLocator.BoldObject);
            end;
          end;
        end;
        DirtyObjects.Clear;
        DestinationSystem.CommitTransaction;
        DestinationSystem.UpdateDatabase;
        for i := DestinationList.Count-1 downto 0 do
          DestinationList.Locators[i].UnloadBoldObject;
        DestinationList.Clear;
      except
        DestinationSystem.RollbackTransaction;
        raise;
      end;
      for i := FilteredList.Count-1 downto 0 do
        FilteredList.Locators[i].UnloadBoldObject;
      BatchList.Clear;
      FilteredList.free;
      if (iEnd = ClassList.Count-1) then
        break;
      Assert(iStart<=iEnd);
      Inc(iStart, BatchSize);
      Inc(iEnd, BatchSize);
      iEnd := MinIntValue([iEnd, ClassList.Count-1]);
      Assert(iStart<=iEnd);
    until false;
  end;
  if DestinationSystem.BoldDirty then
    Report('Writing %d objects', [DestinationSystem.DirtyObjects.Count]);
  DestinationSystem.UpdateDatabase;
  CheckStop;
  if not fDelayedProcessList.Empty then
  begin
    Report('Processing %d delayed objects.', [fDelayedProcessList.Count]);
    fDelayedProcessList.EnsureRange(fDelayedProcessList.Count-BatchSize, fDelayedProcessList.Count-BatchSize*2);
    iStart := fDelayedProcessList.Count div BatchSize;
    repeat
      i := fDelayedProcessList.Count-1;
      if iStart <> (i div BatchSize) then
      begin
        iStart := (i div BatchSize);
        fDelayedProcessList.EnsureRange(iStart*batchsize, (iStart+1)*batchsize-1);
        DestinationSystem.UpdateDatabase;
      end;
      CopyMembers(fDelayedProcessList.Locators[i]);
    until fDelayedProcessList.Empty
  end;
  fDelayedProcessList.Clear;
  DestinationSystem.UpdateDatabase;
end;

procedure TBoldSystemCopy.CopyMembers(ASource: TBoldObjectLocator;
  AMemberIdList: TBoldMemberIdList; ATouchedList: TBoldObjectList);

  procedure CopyAttribute(SourceAttribute, DestinationAttribute: TBoldAttribute);
  begin
    if SourceAttribute.IsNull and not DestinationAttribute.IsNull and not DestinationAttribute.CanSetToNull(nil) then
      DestinationAttribute.AsIBoldValue[bdepContents].AssignContent(SourceAttribute.AsIBoldValue[bdepContents])
    else
      DestinationAttribute.Assign(SourceAttribute);
  end;

var
  SourceOtherEnd: TBoldObject;
  DestinationObject: TBoldObject;
  DestinationOtherEnd: TBoldObject;
  RoleRTInfo: TBoldRoleRTInfo;
  SourceList: TBoldObjectList;
  FetchList: TBoldObjectList;
  DestinationMultilink: TBoldObjectList;
  ObjectReference: TBoldObjectReference;
  SourceLinkObject, DestinationlinkObject: TBoldObject;
  SourceLinkList, DestinationLinkList: TBoldObjectList;
  BoldMemberRTInfo: TBoldMemberRTInfo;
  SourceAttribute, DestinationAttribute: TBoldAttribute;
  j, k, m: integer;
  MemberIdList: TBoldMemberIdList;
  LocatorUnloadList: TBoldSystemLocatorList;
  Locator: TBoldObjectLocator;
  g: IBoldGuard;
begin
  if SkipObject(ASource.EnsuredBoldObject) then
    exit;
  if fProcessedObjects.LocatorInList(ASource) then
    exit;
  fProcessedObjects.AddLocator(ASource);
  j := fDelayedProcessList.IndexOfLocator(ASource);
  if j <> -1 then
    fDelayedProcessList.RemoveByIndex(j);
  MemberIdList := AMemberIDList;
  if not Assigned(MemberIdList) then
  begin
    g := TBoldGuard.Create(MemberIdList);
    MemberIdList := TBoldMemberIdList.Create;
    MemberIdList.OwnsEntries := true;
    CollectMembers(ASource.BoldClassTypeInfo, MemberIdList, [rtRole], true, false, true);
  end;
  FindDestinationObject(ASource.EnsuredBoldObject, DestinationObject);
  Assert(Assigned(DestinationObject));
  if Assigned(ATouchedList) then
    ATouchedList.Add(DestinationObject);
  FetchList := TBoldObjectList.Create;
  LocatorUnloadList := TBoldSystemLocatorList.Create;
  LocatorUnloadList.OwnsEntries := false;
  try
    for j := MemberIdList.Count-1 downto 0 do
    begin
      BoldMemberRTInfo := ASource.BoldClassTypeInfo.AllMembers[MemberIdList[j].MemberIndex];
      if BoldMemberRTInfo.IsAttribute then
      begin
        SourceAttribute := ASource.EnsuredBoldObject.BoldMembers[BoldMemberRTInfo.index] as TBoldAttribute;
        DestinationAttribute := DestinationObject.BoldMembers[BoldMemberRTInfo.index] as TBoldAttribute;
        CopyAttribute(SourceAttribute, DestinationAttribute);
      end
      else
      begin
        RoleRTInfo := ASource.BoldClassTypeInfo.AllMembers[MemberIdList[j].MemberIndex] as TBoldRoleRTInfo;
        if RoleRTInfo.IsSingleRole then
        begin
          SourceOtherEnd := ((ASource.EnsuredBoldObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectReference).BoldObject;
          if RoleRTInfo.Mandatory and not Assigned(SourceOtherEnd) then
            Report('WARNING - Single role %s %s pointing to %s is mandatory but empty.', [RoleRTInfo.Debuginfo, ASource.BoldObjectID.AsString, RoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
          if Assigned(SourceOtherEnd) then
          begin
            if SkipObject(SourceOtherEnd) then
              Continue;
            EnsureDestinationObject(SourceOtherEnd, DestinationOtherEnd);
            if Assigned(ATouchedList) then
              ATouchedList.Add(DestinationOtherEnd);
            ObjectReference := (DestinationObject.BoldMembers[RoleRTInfo.index] as TBoldObjectReference);
            ObjectReference.BoldObject := DestinationOtherEnd;
            if Assigned(ObjectReference.BoldRoleRTInfo.LinkClassTypeInfo) then
            begin
              SourceLinkObject := (ASource.EnsuredBoldObject.BoldMembers[ObjectReference.BoldRoleRTInfo.IndexOfLinkObjectRole] as TBoldObjectReference).BoldObject;
              DestinationLinkObject := (DestinationObject.BoldMembers[ObjectReference.BoldRoleRTInfo.IndexOfLinkObjectRole] as TBoldObjectReference).BoldObject;
              TranslateId(  SourceLinkObject, DestinationlinkObject);
              Assert( SourceLinkObject.DebugInfo = DestinationlinkObject.DebugInfo);
            end;
            if not fProcessedObjects.Includes(SourceOtherEnd) then
              fDelayedProcessList.Add(SourceOtherEnd);
          end;
        end
        else // multirole
        begin
          SourceList := ASource.EnsuredBoldObject.BoldMembers[RoleRTInfo.index] as TBoldObjectList;
          if RoleRTInfo.Mandatory and SourceList.Empty  then
            Report('WARNING - Multi role %s %s pointing to %s is mandatory but empty.', [RoleRTInfo.Debuginfo, ASource.BoldObjectID.AsString, RoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
          if SourceList.empty then
            Continue;
          SourceList.EnsureObjects;
          DestinationMultilink := (DestinationObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectList;
          for m := 0 to SourceList.Count -1 do
          begin
            SourceOtherEnd := SourceList[m];
            if SkipObject(SourceOtherEnd) then
              Continue;
            EnsureDestinationObject(SourceOtherEnd, DestinationOtherEnd);
            if Assigned(DestinationOtherEnd) then
            begin
              if Assigned(ATouchedList) then
                ATouchedList.Add(DestinationOtherEnd);
              k := DestinationMultilink.IndexOf(DestinationOtherEnd);
              if k <> m then
              begin
                if k = -1  then
                  (DestinationMultilink).Add(DestinationOtherEnd)
                else
                if DestinationMultilink.BoldRoleRTInfo.IsOrdered then
                  (DestinationMultilink).move(k, MinIntValue([m, DestinationMultilink.count-1]));
                k := DestinationMultilink.IndexOf(DestinationOtherEnd);
                if k <> MinIntValue([m, DestinationMultilink.count-1]) then
                  (DestinationMultilink).move(k, MinIntValue([m, DestinationMultilink.count-1]));
                if Assigned(RoleRTInfo.LinkClassTypeInfo) then
                begin
                  k := DestinationMultilink.IndexOf(DestinationOtherEnd);
                  SourceLinkList := (ASource.EnsuredBoldObject.BoldMembers[RoleRTInfo.IndexOfLinkObjectRole] as TBoldObjectList);
                  DestinationLinkList := (DestinationObject.BoldMembers[RoleRTInfo.IndexOfLinkObjectRole] as TBoldObjectList);
                  SourceLinkObject := SourceLinkList[m];
                  DestinationLinkObject := DestinationLinkList[k];
                  TranslateId(SourceLinkObject, DestinationlinkObject);
                  Assert(SourceLinkObject.DebugInfo = DestinationlinkObject.DebugInfo);
                end;
              end;
              if not fProcessedObjects.Includes(SourceOtherEnd) then
                fDelayedProcessList.Add(SourceOtherEnd);
            end;
            LocatorUnloadList.Add(SourceList[m].BoldObjectLocator);
          end;
        end;
      end;
    end;
  finally
    ASource.UnloadBoldObject;
    FetchList.Free;
    for Locator in LocatorUnloadList do
      Locator.UnloadBoldObject;
    LocatorUnloadList.Free;
  end;
end;

procedure TBoldSystemCopy.DoLogEvent(const Msg: string);
begin
  if Assigned(fLogEvent) then
    fLogEvent(self, Msg);
end;

function TBoldSystemCopy.EnsureDestinationLocator(
  const ASourceLocator: TBoldObjectLocator;
  var ADestinationLocator: TBoldObjectLocator): boolean;
var
 DestinationObject: TBoldObject;
begin
  FindDestinationLocator(ASourceLocator, ADestinationLocator);
  if not Assigned(ADestinationLocator) then
  begin
    CloneObject(ASourceLocator.EnsuredBoldObject , DestinationObject);
    ADestinationLocator := DestinationObject.BoldObjectLocator;
  end;
  result := Assigned(ADestinationLocator);
end;

function TBoldSystemCopy.EnsureDestinationObject(
  const ASourceObject: TBoldObject;
  var ADestinationObject: TBoldObject): boolean;
begin
  FindDestinationObject(ASourceObject, ADestinationObject);
  if not Assigned(ADestinationObject) then
    CloneObject(ASourceObject, ADestinationObject);
  result := Assigned(ADestinationObject);
  if result then
    Assert(ASourceObject.DebugInfo = ADestinationObject.DebugInfo);
end;

procedure TBoldSystemCopy.FindDestinationLocator(
  const ASourceLocator: TBoldObjectLocator;
  var ADestinationLocator: TBoldObjectLocator);
begin
  ADestinationLocator := nil;
  Assert(ASourceLocator is TBoldObjectLocator);
  Assert(TBoldObjectLocator(ASourceLocator).BoldObjectID.IsStorable) ;
  ADestinationLocator := DestinationSystem.Locators.LocatorByIdString[ASourceLocator.BoldObjectID.AsString];
end;

procedure TBoldSystemCopy.FindDestinationObject(
  const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
var
  ADestinationLocator: TBoldObjectLocator;
begin
  ADestinationObject := nil;
  Assert(ASourceObject is TBoldObject);
  ADestinationLocator := DestinationSystem.Locators.LocatorByIdString[ASourceObject.BoldObjectLocator.BoldObjectID.AsString];
  if Assigned(ADestinationLocator) then
    ADestinationObject := ADestinationLocator.EnsuredBoldObject;
end;

function TBoldSystemCopy.GetDestinationSystem: TBoldSystem;
begin
  result := DestinationSystemHandle.System;
end;

procedure TBoldSystemCopy.UpdateLastUsedID;
var
  Query: IBoldQuery;
  ExecQuery: IBoldExecQuery;
begin
  Query := DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.GetQuery;
  ExecQuery := DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.GetExecQuery;
  try
    Query.SQLText := 'Select max(bold_id) from ' + DestinationSystemHandle.StaticSystemTypeInfo.RootClassTypeInfo.ExpressionName;
    Query.Open;
    fLastUsedId := Query.Fields[0].AsInteger;
    Query.Close;
    ExecQuery.AssignSQLText(Format('Update Bold_ID set Bold_ID = %d', [fLastUsedId+1]));
    ExecQuery.ExecSQL;
  finally
    DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.ReleaseQuery(Query);
    DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.ReleaseExecQuery(ExecQuery);
  end;
end;

function TBoldSystemCopy.GetSourceSystem: TBoldSystem;
begin
  result := SourceSystemHandle.System;
end;

function TBoldSystemCopy.FindHead(AClassTypeInfo: TBoldClassTypeInfo): TBoldRoleRTInfo;
var
  i: integer;
begin
  for i := 0 to fHeadTailRoles.Count -1 do
  begin
    result := fHeadTailRoles[i];
    if result.ClassTypeInfo = AClassTypeInfo then
      exit;
  end;
  result := nil;
end;

function TBoldSystemCopy.FindTail(AClassTypeInfo: TBoldClassTypeInfo): TBoldRoleRTInfo;
var
  i: integer;
begin
  for i := 0 to fHeadTailRoles.Count -1 do
  begin
    result := fHeadTailRoles[i];
    if result.ClassTypeInfoOfOtherEnd = AClassTypeInfo then
      exit;
  end;
  result := nil;
end;

function TBoldSystemCopy.SkipObject(
  const SourceObject: TBoldObject): boolean;

  function isTailOfSkippedHead: boolean;
  var
    j: integer;
    RoleRTInfo: TBoldRoleRTInfo;
    SourceOtherEnd: TBoldObject;
  begin
    result := false;
    for j := SourceObject.BoldClassTypeInfo.AllRolesCount -1 downto 0 do
    begin
      RoleRTInfo := SourceObject.BoldClassTypeInfo.AllRoles[j];
      if fHeadTailRoles.Includes(RoleRTInfo) or fHeadTailRoles.Includes(RoleRTInfo.RoleRTInfoOfOtherEnd) then
      begin
        if (RoleRTInfo.ClassTypeInfoOfOtherEnd = SourceObject.BoldClassTypeInfo) then
        begin
          if RoleRTInfo.IsSingleRole then
          begin
            if not RoleRTInfo.IsSingleRole then
              continue; //Assert(RoleRTInfo.IsSingleRole, 'Multi ownership not supported.');
            SourceOtherEnd := ((SourceObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectReference).BoldObject;
            result := SkipObject(SourceOtherEnd);
          end;
        end
        else
        if (RoleRTInfo.ClassTypeInfo = SourceObject.BoldClassTypeInfo) then
        begin
          if RoleRTInfo.RoleRTInfoOfOtherEnd.Aggregation = akComposite then
          begin
            if not RoleRTInfo.IsSingleRole then
              continue; //Assert(RoleRTInfo.IsSingleRole, 'Multi ownership not supported.');
            SourceOtherEnd := ((SourceObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectReference).BoldObject;
            if Assigned(SourceOtherEnd) then
              result := SkipObject(SourceOtherEnd);
          end;
        end;
      end;
    end;
  end;

begin
  Assert(assigned(SourceObject));
  result := fSkippedObjects.Includes(SourceObject) or isTailOfSkippedHead;
  if not result and Assigned(fObjectEvent) then
  begin
    OnObject(self, SourceObject, result);
    if result then
      fSkippedObjects.Add(SourceObject);
  end;
  if result then
    Report('Skipping instance %s:%s', [SourceObject.ClassName, SourceObject.BoldObjectLocator.BoldObjectID.AsString]);
end;

procedure TBoldSystemCopy.Stop;
begin
  fStopped := true;
end;

procedure TBoldSystemCopy.Report(const Msg: string; const Args: array of const);
begin
  if Assigned(fLogEvent) then
  begin
    if Length(Args) = 0 then
      DoLogEvent(msg)
    else
      DoLogEvent(Format(msg, Args));
  end;
end;

procedure TBoldSystemCopy.Run;
begin
  fStopped := false;
  if not Assigned(SourceSystemHandle) then
    raise Exception.Create('Source system handle not set.');
  if not Assigned(DestinationSystemHandle) then
    raise Exception.Create('Destination system handle not set.');
  if SourceSystemHandle = DestinationSystemHandle then
    raise Exception.Create('Source and destination systems can not be same.');
  SourceSystemHandle.Active := true;
  if DestinationSystemHandle.StaticSystemTypeInfo.OptimisticLocking <> bolmOff then // turn off optimistic locking for performance reasons
  begin
    Report('Optimistic locking turned off.', []);
    TBoldModel(DestinationSystemHandle.PersistenceHandleDB.BoldModel).EnsuredUMLModel.SetBoldTV(TAG_OPTIMISTICLOCKING, TV_OPTIMISTICLOCKING_OFF);
  end;
  DestinationSystemHandle.Active := true;
  if (DestinationAllInstanceCount > 0) and (MessageDlg('Non empty destination, are you sure you want to proceed ?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    exit;
  DestinationSystem.UndoHandlerInterface.Enabled := false;
  BoldLinks.BoldPerformMultilinkConsistencyCheck := false;
  BoldLinks.BoldPerformMultiLinkConsistenceCheckLimit := 0;
  try
    CheckStop;
    CheckModelCompatibility;
    AnalyzeModel;
    Report('Source objects: %d Destination objects %d', [SourceAllInstanceCount, DestinationAllInstanceCount]);
    CheckStop;
    CopyInstances;
    UpdateLastUsedID;
    Report('Completed succesfully', []);
  except
    on e:EBoldSystemCopyInterrupt do
    ; // silent
  end;
end;

procedure TBoldSystemCopy.TranslateId(ASourceObject,
  ADestinationObject: TBoldObject);
var
  DestinationID: TBoldDefaultID;
  TranslationList: TBoldIdTranslationList;
  Guard: IBoldGuard;
begin
  if ASourceObject.BoldObjectLocator.BoldObjectID.IsEqual[ADestinationObject.BoldObjectLocator.BoldObjectID] then
    exit;
  Guard := TBoldGuard.Create(TranslationList, DestinationId);
  TranslationList := TBoldIdTranslationList.Create;
  DestinationID := TBoldDefaultId.CreateWithClassID(ASourceObject.BoldClassTypeInfo.TopSortedIndex, true);
  DestinationID.AsInteger := (ASourceObject.BoldObjectLocator.BoldObjectID as TBoldDefaultID).AsInteger;
  TranslationList.AddTranslation(ADestinationObject.BoldObjectLocator.BoldObjectID, DestinationID);
  DestinationSystem.AsIBoldvalueSpace[bdepContents].ApplytranslationList(TranslationList);
  Assert(ASourceObject.DebugInfo = ADestinationObject.DebugInfo);
end;

function TBoldSystemCopy.GetAllInstanceCount(ASystemHandle: TBoldSystemHandle): integer;
var
  Query: IBoldQuery;
  RootTableName: string;
begin
  RootTableName := ASystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName;
  Query := ASystemHandle.System.PersistenceController.DatabaseInterface.GetQuery;
  try
    Query.SQLText := 'select count(*) from ' + RootTableName;
    Query.Open;
    result := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    ASystemHandle.System.PersistenceController.DatabaseInterface.ReleaseQuery(Query);
  end;
end;

function TBoldSystemCopy.GetSourceAllInstanceCount: integer;
begin
  result := GetAllInstanceCount(SourceSystemHandle);
end;

function TBoldSystemCopy.GetDestinationAllInstanceCount: integer;
begin
  result := GetAllInstanceCount(DestinationSystemHandle);
end;

end.
