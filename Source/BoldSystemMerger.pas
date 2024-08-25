{ Global compiler directives }
{$include bold.inc}

unit BoldSystemMerger;

interface

uses
  Classes,
  BoldSubscription,
  BoldSystemRT,
  BoldSystem,
  BoldSystemHandle,
  BoldIndexableList,
  BoldId,
  BoldDomainElement,
  BoldDefs,
  BoldExternalizedReferences,
  BoldSystemComparer,
  BoldUMLTypes;

type
  TBoldSystemMerger = class;
  // user events
  TClassEvent = procedure(Sender: TBoldSystemMerger; const AClass: TBoldClassTypeInfo; var ASkip: boolean) of object;
  TObjectEvent = procedure(Sender: TBoldSystemMerger; const ASourceObject: TBoldobject; var ASkip: boolean) of object;
  TSingleLinkEvent = procedure(Sender: TBoldSystemMerger; const ASourceSingleLink, ADestinationSingleLink: TBoldObjectReference; var ANewValue: TBoldObject) of object;
  TMultiLinkBeforeEvent = procedure(Sender: TBoldSystemMerger; const ASourceMultiLink, ADestinationMultiLink: TBoldObjectList; var ASkip: boolean) of object;
  TMultiLinkAfterEvent = procedure(Sender: TBoldSystemMerger; const ASourceMultiLink, ADestinationBefore, ADestinationAfter: TBoldObjectList) of object;
  TAttributesEvent = procedure(Sender: TBoldSystemMerger; const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject) of object;
  TProgressEvent = procedure(Sender: TBoldSystemMerger; const aStatus: string) of object;
  TFindObjectEvent = procedure(Sender: TBoldSystemMerger; const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject) of object;
  TCompareDifferenceEvent = function(Sender: TBoldSystemMerger; ALeft, ARight: TBoldDomainElement;
  AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean of object;

  TBooleanArray = array of boolean;

  TBoldSystemMerger = class(TBoldSubscribableComponent)
  private
    fIsRunning: boolean;
    fStopped: boolean;
    fSourceSystemHandle: TBoldSystemHandle;
    fDestinationSystemHandle: TBoldSystemHandle;
    fClassEvent: TClassEvent;
    fObjectEvent: TObjectEvent;
    fSingleLinkEvent: TSingleLinkEvent;
    fMultiLinkBeforeEvent: TMultiLinkBeforeEvent;
    fMultiLinkAfterEvent: TMultiLinkAfterEvent;
    fAttributesEvent: TAttributesEvent;
    fProgressEvent: TProgressEvent;
    fFindObjectEvent: TFindObjectEvent;
    fCompareDifferenceEvent: TCompareDifferenceEvent;
    fMapping: TBoldExternalizedReferenceList;
    fBatchSize: integer;
    fArrayOfClassesToIgnore: TBooleanArray;
    fSkipppedLinks: TStrings;
    fLastUseId: integer;
    function GetSourceSystem: TBoldSystem;
    function GetDestinationSystem: TBoldSystem;
    function GetMapping: TBoldExternalizedReferenceList;
    procedure CollectMembers(AClassTypeInfo: TBoldClassTypeInfo; AMemberIdList: TBoldMemberIdList; ARoleTypes: TBoldRoleSet; AAttributes: boolean; ADerived: boolean = false; ADelayedFetched: boolean = false);
    function GetCorrespondingObject(Sender: TBoldSystemComparer; LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject;
    function OnDifferenceEvent(Sender: TBoldSystemComparer; ALeft, ARight: TBoldDomainElement; AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean;
    procedure CopyLinks(ASource: TBoldObject; MemberIdList: TBoldMemberIdList; ATouchedList: TBoldObjectList);
    procedure CopyAssociationObjects;
    procedure SetReference(SourceObject, DestinationObject: TBoldObject); overload;
    function SkipLink(AMultiLink: TBoldRoleRTInfo): boolean;
    procedure GetLastUsedId;
    function OtherEndIsSingleLink(ARoleTypeInfo: TBoldRoleRtInfo): boolean;
  protected
    procedure Report(const Msg: string; const Args: array of const);
    procedure FirstPass;
    procedure SecondPass;
    procedure CompareSystems;
    function FilterClass(const AClass: TBoldClassTypeInfo): boolean;
    function ProcessObject(const SourceObject: TBoldObject): boolean;
    procedure ProcessSingleLink(const ASourceSingleLink, ADestinationSingleLink: TBoldObjectReference; var ANewValue: TBoldObject);
    procedure BeforeProcessMultiLink(const ASourceMultiLink, ADestinationMultiLink: TBoldObjectList; var ASkip: boolean);
    procedure AfterProcessMultiLink(const ASourceMultiLink, ADestinationBefore, ADestinationAfter: TBoldObjectList);
    procedure ProcessSourceObjectList(const ASourceList: TBoldObjectList);
    function EnsureDestinationObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject): boolean;
    procedure FindDestinationObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
    procedure FindDestinationLocator(const ASourceLocator: TBoldObjectLocator; var ADestinationLocator: TBoldObjectLocator);
    procedure CloneObject(const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Run;
    procedure Stop;
    property SourceSystem: TBoldSystem read GetSourceSystem;
    property DestinationSystem: TBoldSystem read GetDestinationSystem;
    property SkipppedLinks: TStrings read fSkipppedLinks;
    property Mapping: TBoldExternalizedReferenceList read GetMapping;
  published
    property SourceSystemHandle: TBoldSystemHandle read fSourceSystemHandle write fSourceSystemHandle;
    property DestinationSystemHandle: TBoldSystemHandle read fDestinationSystemHandle write fDestinationSystemHandle;
    property OnClass: TClassEvent read fClassEvent write fClassEvent;
    property OnObject: TObjectEvent read fObjectEvent write fObjectEvent;
    property OnSingleLink: TSingleLinkEvent read fSingleLinkEvent write fSingleLinkEvent;
    property OnMultiLinkBefore: TMultiLinkBeforeEvent read fMultiLinkBeforeEvent write fMultiLinkBeforeEvent;
    property OnMultiLinkAfter: TMultiLinkAfterEvent read fMultiLinkAfterEvent write fMultiLinkAfterEvent;
    property OnAttributes: TAttributesEvent read fAttributesEvent write fAttributesEvent;
    property OnFindObject: TFindObjectEvent read fFindObjectEvent write fFindObjectEvent;
    property OnProgress: TProgressEvent read fProgressEvent write fProgressEvent;
    property OnCompareDifference: TCompareDifferenceEvent read fCompareDifferenceEvent write fCompareDifferenceEvent;
    property BatchSize: integer read fBatchSize write fBatchSize;
  end;

implementation

uses
  SysUtils,
  BoldUMLModelSupport,
  BoldMath,
  BoldGuard,
  BoldTaggedValueSupport,
  BoldElements,
  BoldLinks,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig;

{ TBoldSystemMerger }

procedure TBoldSystemMerger.AfterConstruction;
begin
  inherited;
  fMapping := TBoldExternalizedReferenceList.Create;
  fSkipppedLinks := TStringList.Create;
  TStringList(fSkipppedLinks).Sorted := true;
  TStringList(fSkipppedLinks).Duplicates := dupIgnore;
  BatchSize := 1000;
end;

procedure TBoldSystemMerger.BeforeDestruction;
begin
  inherited;
  FreeAndNil(fMapping);
  FreeAndNil(fSkipppedLinks);
end;

procedure TBoldSystemMerger.BeforeProcessMultiLink(const ASourceMultiLink,
  ADestinationMultiLink: TBoldObjectList; var ASkip: boolean);
begin
  if Assigned(OnMultiLinkBefore) then
    OnMultiLinkBefore(self, ASourceMultiLink, ADestinationMultiLink, ASkip);
end;

procedure TBoldSystemMerger.AfterProcessMultiLink(const ASourceMultiLink,
  ADestinationBefore, ADestinationAfter: TBoldObjectList);
begin
  if Assigned(OnMultiLinkAfter) then
    OnMultiLinkAfter(self, ASourceMultiLink, ADestinationBefore, ADestinationAfter);
end;

procedure TBoldSystemMerger.CloneObject(const ASourceObject: TBoldObject;
  var ADestinationObject: TBoldObject);
begin
  ADestinationObject := DestinationSystem.CreateNewObjectFromClassTypeInfo(DestinationSystem.BoldSystemTypeInfo.ClassTypeInfoByClass[ASourceObject.ClassType]);
  TBoldCopyAndClone.BoldCopy(ADestinationObject, ASourceObject, bcmAttributes, false);
  if Assigned(fAttributesEvent) then
    OnAttributes(self, ASourceObject, ADestinationObject);
end;

procedure TBoldSystemMerger.CollectMembers(AClassTypeInfo: TBoldClassTypeInfo;
  AMemberIdList: TBoldMemberIdList; ARoleTypes: TBoldRoleSet;
  AAttributes: boolean; ADerived: boolean = false;
  ADelayedFetched: boolean = false);
var
  i: integer;
  RoleRTInfo: TBoldRoleRTInfo;
  MemberRtInfo: TBoldMemberRTInfo;
begin
  for I := 0 to AClassTypeInfo.AllMembersCount -1 do
  begin
    MemberRtInfo := AClassTypeInfo.AllMembers[i];
    if not MemberRtInfo.Persistent then
      continue;
    if not ADerived and MemberRtInfo.IsDerived then
      continue;
    if not ADelayedFetched and MemberRtInfo.DelayedFetch then
      continue;
    if not AAttributes and MemberRtInfo.IsAttribute then
      continue;
    if (ARoleTypes = []) and MemberRtInfo.IsRole then
      continue;
    if MemberRtInfo.IsAttribute  then
    begin
      AMemberIdList.Add(TBoldMemberId.Create(MemberRtInfo.index));
    end
    else
    if MemberRtInfo.IsRole then
    begin
      RoleRTInfo := AClassTypeInfo.AllMembers[i] as TBoldRoleRTInfo;
      if fArrayOfClassesToIgnore[RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex] then
        SkipppedLinks.Add(RoleRTInfo.DisplayName)
      else
        if (RoleRTInfo.RoleType in ARoleTypes) and not SkipLink(RoleRTInfo) then
          AMemberIdList.Add(TBoldMemberId.Create(RoleRTInfo.index));
    end;
  end;
end;

procedure TBoldSystemMerger.CopyAssociationObjects;
var
  SourceObject, DestinationObject: TBoldObject;
  SourceObject1, SourceObject2: TBoldObject;
  LinkObject1, LinkObject2: TBoldObject;
  ClassTypeInfo: TBoldClassTypeInfo;
  Link1,Link2: TBoldRoleRTInfo;
  ClassList: TBoldObjectList;
  BatchList, DestinationList: TBoldObjectList;
  i, j, c, iStart, iEnd: integer;
  g: IBoldGuard;
  found: boolean;
  CountBefore, Reused: Integer;
  DestinationClassList: TBoldObjectList;
  TopSortedClasses: TBoldClassTypeInfoList;
  DelayProcessing: boolean;
begin
  g := TBoldGuard.Create(TopSortedClasses, BatchList, DestinationList);
  BatchList := TBoldObjectList.Create;
  BatchList.Capacity := BatchSize;
  DestinationList := TBoldObjectList.Create;
  Report('-----------------------',[]);
  Report('Processing link classes',[]);
  Report('-----------------------',[]);
  TopSortedClasses := TBoldClassTypeInfoList.Create;
  TopSortedClasses.OwnsEntries := false;
  for i := 0 to SourceSystem.BoldSystemTypeInfo.TopSortedClasses.Count -1 do
  begin
    ClassTypeInfo := SourceSystem.BoldSystemTypeInfo.TopSortedClasses[i] as TBoldClassTypeInfo;
    if ClassTypeInfo.IsAbstract or not ClassTypeInfo.IsLinkClass or not ClassTypeInfo.Persistent then
      continue;
    TopSortedClasses.Add(ClassTypeInfo);
  end;
  while TopSortedClasses.Count > 0 do
  for c := TopSortedClasses.Count - 1 downto 0 do
  begin
    if fStopped then
      exit;
    ClassTypeInfo := TopSortedClasses[c] as TBoldClassTypeInfo;
    ClassList := SourceSystem.Classes[ClassTypeInfo.TopSortedIndex];
    if ClassList.Empty then
    begin
      TopSortedClasses.RemoveByIndex(c);
      continue;
    end;
    Reused := 0;
    iStart := 0;
    iEnd := MinIntValue([ClassList.Count-1, BatchSize]);
    DestinationClassList := DestinationSystem.Classes[ClassTypeInfo.TopSortedIndex];
    CountBefore := DestinationClassList.Count;
    Link1 := nil;
    Link2 := nil;
    for I := 0 to ClassTypeInfo.AllRolesCount-1 do
      if ClassTypeInfo.AllRoles[i].RoleType = rtInnerLinkRole then
      begin
        Link1 := ClassTypeInfo.AllRoles[i];
        Link2 := ClassTypeInfo.AllMembers[Link1.OtherIndexInLinkClass] as TBoldRoleRTInfo;
        break;
      end;
    Assert(Assigned(link1));
    if fArrayOfClassesToIgnore[Link1.ClassTypeInfoOfOtherEnd.TopSortedIndex] then
    begin
      Report('Skipping LinkClass %s, because associationend is %s', [ClassTypeInfo.DisplayName, Link1.ClassTypeInfoOfOtherEnd.DisplayName]);
      TopSortedClasses.RemoveByIndex(c);
      continue;
    end;
    if fArrayOfClassesToIgnore[Link2.ClassTypeInfoOfOtherEnd.TopSortedIndex] then
    begin
      Report('Skipping LinkClass %s, because associationend is %s', [ClassTypeInfo.DisplayName, Link2.ClassTypeInfoOfOtherEnd.DisplayName]);
      TopSortedClasses.RemoveByIndex(c);
      continue;
    end;
    DelayProcessing := false;
    repeat
      Report('%s: Loading Objects %d-%d', [ClassTypeInfo.DisplayName, iStart, iEnd]);
      BatchList.Clear;
      for i := iStart to iEnd do
      begin
        BatchList.AddLocator(ClassList.Locators[i]);
      end;
      Assert(iStart<=iEnd);
      BatchList.EnsureObjects;
      DestinationSystem.StartTransaction;
      for I := 0 to BatchList.Count-1 do
      begin
        SourceObject := BatchList[i];
        SourceObject1 := TBoldObjectReference(SourceObject.BoldMembers[link1.Index]).BoldObject;
        FindDestinationObject(SourceObject1, LinkObject1);
        if not Assigned(LinkObject1) then
        begin
          DelayProcessing := true;
          Report('Delayed processing for %s as link %s requires that %s is processed first', [ClassTypeInfo.DisplayName, link1.DisplayName, link1.ClassTypeInfoOfOtherEnd.DisplayName]);
          break;
        end;
        SourceObject2 := TBoldObjectReference(SourceObject.BoldMembers[link2.Index]).BoldObject;
        FindDestinationObject(SourceObject2, LinkObject2);
        if not Assigned(LinkObject2) then
        begin
          DelayProcessing := true;
          Report('Delayed processing for %s as link %s requires that %s is processed first', [ClassTypeInfo.DisplayName, link2.DisplayName, link2.ClassTypeInfoOfOtherEnd.DisplayName]);
          break;
        end;
        found := false;
        for j := 0 to DestinationClassList.Count-1 do
          begin
          SourceObject := BatchList.Locators[i].EnsuredBoldObject;
          if (TBoldObjectReference(SourceObject.BoldMembers[Link1.Index]).BoldObject = LinkObject1) and
             (TBoldObjectReference(SourceObject.BoldMembers[Link2.Index]).BoldObject = LinkObject2) then
             begin
               DestinationObject := DestinationClassList.Locators[j].EnsuredBoldObject;
               SetReference(SourceObject, DestinationObject);
               TBoldCopyAndClone.BoldCopy(DestinationObject, SourceObject, bcmAttributes, false);
               found := true;
               if StrToInt(DestinationObject.BoldObjectLocator.BoldObjectID.AsString) <= fLastUseId then
                 Inc(Reused);
               break;
             end;
          end;
          if not found then
          begin
            if LinkObject1.BoldMembers[Link1.IndexOfOtherEnd] is TBoldObjectList then
            begin
              TBoldObjectList(LinkObject1.BoldMembers[Link1.IndexOfOtherEnd]).Add(LinkObject2);
              DestinationObject := TBoldObject(DestinationClassList.last);
              SetReference(SourceObject, DestinationObject);
            end
            else
            if LinkObject1.BoldMembers[Link1.IndexOfOtherEnd] is TBoldObjectReference then
            begin
              TBoldObjectReference(LinkObject1.BoldMembers[Link1.IndexOfOtherEnd]).BoldObject := LinkObject2;
              DestinationObject := TBoldObject(DestinationClassList.last);
              SetReference(SourceObject, DestinationObject);
            end
            else
              raise Exception.Create('TBoldSystemMerger.CopyAssociationObjects: Unexpected condition.');
            TBoldCopyAndClone.BoldCopy(DestinationObject, SourceObject, bcmAttributes, false);
          end;
      end;
      DestinationSystem.CommitTransaction();
      if DelayProcessing then
      begin
        break;
      end;
      Report('Writing %d objects', [DestinationSystem.DirtyObjects.Count]);
      DestinationSystem.UpdateDatabase;
      for i := BatchList.Count-1 downto 0 do
        BatchList.Locators[i].UnloadBoldObject;
      BatchList.Clear;
      if iEnd = ClassList.Count-1 then
        break;
      inc(iStart, BatchSize);
      inc(iEnd, BatchSize);
      iEnd := MinIntValue([iEnd, ClassList.Count-1]);
    until false;
    Report('Linkclass %s Source:%d Destination before:%d after:%d reused:%d', [ClassTypeInfo.DebugInfo, ClassList.Count, CountBefore, DestinationClassList.Count, Reused]);
    DestinationList.AddList(DestinationClassList);
    for i := DestinationList.Count-1 downto 0 do
      DestinationList.Locators[i].UnloadBoldObject;
    TopSortedClasses.RemoveByIndex(c);
  end;
end;

procedure TBoldSystemMerger.CopyLinks(ASource: TBoldObject; MemberIdList: TBoldMemberIdList; ATouchedList: TBoldObjectList);
var
  SourceOtherEnd: TBoldObject;
  DestinationObject: TBoldObject;
  DestinationOtherEnd: TBoldObject;
  RoleRTInfo: TBoldRoleRTInfo;
  SourceSingleLink, DestinationSingleLink: TBoldObjectReference;
  SourceList: TBoldObjectList;
  FetchList: TBoldObjectList;
  DestinationMultilink: TBoldObjectList;
  Locator: TBoldObjectLocator;
  j, k, m, indexInDestinationList: integer;
  Skip: boolean;
  MultiLinkBeforeChanges: TBoldObjectList;
begin
  if MemberIdList.Count = 0 then
    exit;
  FindDestinationObject(ASource, DestinationObject);
  FetchList := TBoldObjectList.Create;
  try
    for j := MemberIdList.Count-1 downto 0 do
    begin
      RoleRTInfo := ASource.BoldClassTypeInfo.AllMembers[MemberIdList[j].MemberIndex] as TBoldRoleRTInfo;
      if fArrayOfClassesToIgnore[RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex] then
      begin
        SkipppedLinks.Add(RoleRTInfo.DisplayName);
        continue;
      end;
      if SkipLink(RoleRTInfo) then
        continue;
      if RoleRTInfo.IsSingleRole then
      begin
        SourceSingleLink := (ASource.BoldMembers[RoleRTInfo.index]) as TBoldObjectReference;
        SourceOtherEnd := SourceSingleLink.BoldObject;
        if Assigned(SourceOtherEnd) then
        begin
          if fArrayOfClassesToIgnore[SourceOtherEnd.BoldClassTypeInfo.TopSortedIndex] then
            continue;
          Assert(Assigned(DestinationObject));
          if Assigned(ATouchedList) then
            ATouchedList.Add(DestinationObject);
          FindDestinationObject(SourceOtherEnd, DestinationOtherEnd);
          if Assigned(ATouchedList) then
            ATouchedList.Add(DestinationOtherEnd);
          DestinationSingleLink := (DestinationObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectReference;
          ProcessSingleLink(SourceSingleLink, DestinationSingleLink, DestinationOtherEnd);
          Assert(Assigned(DestinationOtherEnd));
          DestinationSingleLink.BoldObject := DestinationOtherEnd;
        end;
      end
      else // multirole
      begin
        SourceList := ASource.BoldMembers[RoleRTInfo.index] as TBoldObjectList;
        DestinationMultilink := (DestinationObject.BoldMembers[RoleRTInfo.index]) as TBoldObjectList;
        SourceList.Count;
        SourceList.EnsureObjects;
        BeforeProcessMultiLink(SourceList, DestinationMultilink, Skip);
        if Skip or (SourceList.empty and DestinationMultilink.Empty) then
          Continue;
        MultiLinkBeforeChanges := SourceList.Clone as TBoldObjectList;
        try
          for m := 0 to SourceList.Count -1 do
          begin
            if Assigned(ATouchedList) then
              ATouchedList.Add(DestinationObject);
            if fArrayOfClassesToIgnore[SourceList[m].BoldClassTypeInfo.TopSortedIndex] then
            begin
              SkipppedLinks.Add(SourceList.DebugInfo+'.'+SourceList[m].DebugInfo);
              continue;
            end;
            FindDestinationObject(SourceList[m], DestinationOtherEnd);
            if not Assigned(DestinationOtherEnd) then
            begin
              Report('1DestinationOtherEnd not found for %s while cloning into %s', [SourceList[m].DebugInfo, DestinationObject.DebugInfo]);
              SourceList[m].BoldObjectLocator.UnloadBoldObject;
              Continue;
            end;
            if Assigned(ATouchedList) then
              ATouchedList.Add(DestinationOtherEnd);
            if Assigned(DestinationOtherEnd) then
            begin
              if OtherEndIsSingleLink(RoleRTInfo) then
              begin
                SourceSingleLink := SourceList[m].BoldMembers[RoleRTInfo.IndexOfOtherEnd] as TBoldObjectReference;
                DestinationSingleLink := DestinationOtherEnd.BoldMembers[RoleRTInfo.IndexOfOtherEnd] as TBoldObjectReference;
                ProcessSingleLink(SourceSingleLink, DestinationSingleLink, DestinationOtherEnd);
              end;
              k := DestinationMultilink.IndexOf(DestinationOtherEnd);
              if k <> m then
              begin
                if k = -1  then
                begin
                  if DestinationMultilink.Empty then
                    DestinationMultilink.Add(DestinationOtherEnd)
                  else
                  begin
                    indexInDestinationList := BoldMath.MinIntValue([m, DestinationMultilink.Count-1]);
                    (DestinationMultilink).Insert(indexInDestinationList,DestinationOtherEnd);
                  end;
                end
                else
                if DestinationMultilink.BoldRoleRTInfo.IsOrdered then
                  (DestinationMultilink).move(k,m);
              end;
            end;
            SourceList[m].BoldObjectLocator.UnloadBoldObject;
          end;
    //      Assert(SourceList.Count = (DestinationMultilink).count);
          for m := 0 to SourceList.Count -1 do
          begin
            FindDestinationLocator(SourceList.Locators[m], Locator);
            if Assigned(Locator) then
              FetchList.AddLocator(Locator);
          end;
          FetchList.EnsureObjects;
          FetchList.Clear;
          SourceList.EnsureObjects;
          for m := 0 to SourceList.Count -1 do
          begin
            if Assigned(ATouchedList) then
              ATouchedList.Add(DestinationObject);
            if fArrayOfClassesToIgnore[SourceList[m].BoldClassTypeInfo.TopSortedIndex] then
            begin
              SkipppedLinks.Add(SourceList.DebugInfo+'.'+SourceList[m].DebugInfo); // brk
              continue;
            end;
            FindDestinationObject(SourceList[m], DestinationOtherEnd);
            if not Assigned(DestinationOtherEnd) then
            begin
              Report('2DestinationOtherEnd not found for %s while cloning into %s', [SourceList[m].DebugInfo, DestinationObject.DebugInfo]);
              SourceList[m].BoldObjectLocator.UnloadBoldObject;
              Continue;
            end;
            if OtherEndIsSingleLink(RoleRTInfo) then
            begin
              SourceSingleLink := SourceList[m].BoldMembers[RoleRTInfo.IndexOfOtherEnd] as TBoldObjectReference;
              DestinationSingleLink := DestinationOtherEnd.BoldMembers[RoleRTInfo.IndexOfOtherEnd] as TBoldObjectReference;
              ProcessSingleLink(SourceSingleLink, DestinationSingleLink, DestinationOtherEnd);
            end;
            if Assigned(ATouchedList) then
              ATouchedList.Add(DestinationOtherEnd);
            indexInDestinationList := DestinationMultilink.IndexOf(DestinationOtherEnd);
            if indexInDestinationList = -1 then
            begin
              DestinationMultilink.InsertLocator(m, DestinationOtherEnd.BoldObjectLocator);
              indexInDestinationList := DestinationMultilink.IndexOf(DestinationOtherEnd);
            end;
            if indexInDestinationList <> m then
            if not Assigned(DestinationMultilink.BoldRoleRTInfo.LinkClassTypeInfo) and DestinationMultilink.BoldRoleRTInfo.IsOrdered then
            begin
              DestinationMultilink.Move(DestinationMultilink.IndexOf(DestinationOtherEnd), m);
              Assert(DestinationMultilink.IndexOf(DestinationOtherEnd) = m);
            end;
            SourceList[m].BoldObjectLocator.UnloadBoldObject;
          end;
          AfterProcessMultiLink(SourceList, MultiLinkBeforeChanges, DestinationMultilink);
        finally
          MultiLinkBeforeChanges.free;
        end;
      end;
    end;
    ASource.BoldObjectLocator.UnloadBoldObject;
  finally
    FetchList.Free;
  end;
end;

function TBoldSystemMerger.EnsureDestinationObject(
  const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject): boolean;
begin
  FindDestinationObject(ASourceObject, ADestinationObject);
  result := Assigned(ADestinationObject);
  if not Assigned(ADestinationObject) then
    CloneObject(ASourceObject, ADestinationObject);
end;

procedure TBoldSystemMerger.FindDestinationLocator(
  const ASourceLocator: TBoldObjectLocator;
  var ADestinationLocator: TBoldObjectLocator);
begin
  ADestinationLocator := nil;
  Assert(ASourceLocator is TBoldObjectLocator);
  try
    if Assigned(Mapping.ReferencedObjects[ASourceLocator]) then
      ADestinationLocator := (Mapping.ReferencedObjects[ASourceLocator] as TBoldObjectLocator);
  finally
    Assert(not Assigned(ADestinationLocator) or (ADestinationLocator is TBoldObjectLocator), ADestinationLocator.ClassName);
  end;
end;

procedure TBoldSystemMerger.FindDestinationObject(
  const ASourceObject: TBoldObject; var ADestinationObject: TBoldObject);
begin
  // first check if we already have it in mapping
  ADestinationObject := nil;
  Assert(ASourceObject is TBoldObject);
  try
    if Assigned(Mapping.ReferencedObjects[ASourceObject.BoldObjectLocator]) then
      ADestinationObject := (Mapping.ReferencedObjects[ASourceObject.BoldObjectLocator] as TBoldObjectLocator).EnsuredBoldObject;
    if Assigned(ADestinationObject) then
      exit;
    if not ProcessObject(ASourceObject) then
      exit;
    if Assigned(fFindObjectEvent) then
    begin
      OnFindObject(self, ASourceObject, ADestinationObject);
      if Assigned(ADestinationObject) then
        Assert(ADestinationObject.BoldSystem = DestinationSystem, 'FindObject returned object in wrong system.');
    end;
  finally
    Assert(not Assigned(ADestinationObject) or (ADestinationObject is TBoldObject), ADestinationObject.ClassName);
  end;
end;

function TBoldSystemMerger.GetSourceSystem: TBoldSystem;
begin
  result := SourceSystemHandle.System;
end;

function TBoldSystemMerger.OnDifferenceEvent(Sender: TBoldSystemComparer; ALeft, ARight: TBoldDomainElement;
  AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean;
var
  s: string;
begin
  result := true;
  if (ALeft is TBoldMember) and TBoldMember(ALeft).BoldMemberRTInfo.IsRole and SkipLink(TBoldMember(ALeft).BoldMemberRtInfo as TBoldRoleRtInfo) then
    exit;
  if (ALeft is TBoldList) and (AAditionalElement is TBoldObject) then
  begin
    s := ALeft.DebugInfo + '.'+ AAditionalElement.DebugInfo;
    if (SkipppedLinks.IndexOf(s) <> -1) then
      exit;
  end;
  if Assigned(fCompareDifferenceEvent) then
    if not fCompareDifferenceEvent(Self, ALeft, ARight, AMessage, AAditionalElement) then
      exit;
  Report(AMessage,[]);
end;

function TBoldSystemMerger.OtherEndIsSingleLink(
  ARoleTypeInfo: TBoldRoleRtInfo): boolean;
begin
  result := (ARoleTypeInfo.RoleRTInfoOfOtherEnd.RoleType = rtRole)
        and (ARoleTypeInfo.RoleType = rtRole)
        and ARoleTypeInfo.RoleRTInfoOfOtherEnd.IsSingleRole;
end;

function TBoldSystemMerger.GetCorrespondingObject(Sender: TBoldSystemComparer; LeftObject: TBoldObject;
  RightSystem: TBoldSystem): TBoldObject;
begin
  self.FindDestinationObject(LeftObject, result);
end;

function TBoldSystemMerger.GetDestinationSystem: TBoldSystem;
begin
  result := DestinationSystemHandle.System;
end;

procedure TBoldSystemMerger.GetLastUsedId;
var
  Query: IBoldQuery;
begin
  Query := DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.GetQuery;
  try
    Query.SQLText := 'Select max(bold_id) from ' + DestinationSystemHandle.StaticSystemTypeInfo.RootClassTypeInfo.ExpressionName;
    Query.Open;
    fLastUseId := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    DestinationSystemHandle.PersistenceHandleDB.DatabaseInterface.ReleaseQuery(Query);
  end;
end;

function TBoldSystemMerger.GetMapping: TBoldExternalizedReferenceList;
begin
  if not Assigned(fMapping) then
    fMapping := TBoldExternalizedReferenceList.create;
  result := fMapping;
end;

function TBoldSystemMerger.FilterClass(const AClass: TBoldClassTypeInfo): boolean;
begin
  result := false;
  if Assigned(fClassEvent) then
    OnClass(self, AClass, result);
end;

procedure TBoldSystemMerger.ProcessSingleLink(const ASourceSingleLink, ADestinationSingleLink: TBoldObjectReference; var ANewValue: TBoldObject);
begin
  if Assigned(fSingleLinkEvent) then
    fSingleLinkEvent(self, ASourceSingleLink, ADestinationSingleLink, ANewValue)
end;

function TBoldSystemMerger.ProcessObject(
  const SourceObject: TBoldObject): boolean;
begin
  result := true;
  if Assigned(fObjectEvent) then
    OnObject(self, SourceObject, result);
end;

procedure TBoldSystemMerger.ProcessSourceObjectList(
  const ASourceList: TBoldObjectList);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  SourceObject: TBoldObject;
  DestinationObject: TBoldObject;
  BatchList: TBoldObjectList;
  TempList: TBoldObjectList;
  DestinationList: TBoldObjectList;
  i, iStart,iEnd, CountBefore, reused: integer;
  AttributeList: TBoldMemberIdList;
  DelayedFetchedList: TBoldMemberIdList;
  g: IBoldGuard;
begin
  if ASourceList.Empty then
    exit;
  g := TBoldGuard.Create(DestinationList, TempList, BatchList, AttributeList, DelayedFetchedList);
  ClassTypeInfo := (ASourceList.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;
  Report('%s: Loaded %d IDs', [ClassTypeInfo.DisplayName, ASourceList.Count]);
  TempList := ASourceList.FilterOnType(ClassTypeInfo, false);
  if TempList.Empty then
    exit;
  if ASourceList.count <> TempList.Count then
    Report('%s: Filtered %d IDs', [ClassTypeInfo.DisplayName, ASourceList.Count]);
  DestinationList := TBoldObjectList.Create;
  BatchList := TBoldObjectList.Create;
  BatchList.Capacity := BatchSize;
  iStart := 0;
  iEnd := MinIntValue([TempList.Count-1, BatchSize]);
  AttributeList:= TBoldMemberIdList.Create;
  DelayedFetchedList:= TBoldMemberIdList.Create;
  AttributeList.OwnsEntries := true;
  DelayedFetchedList.OwnsEntries := true;
  CollectMembers(ClassTypeInfo, AttributeList, [], true, false, false);
  CollectMembers(ClassTypeInfo, DelayedFetchedList, [], true, false, true);
  reused := 0;
  CountBefore := DestinationSystem.Classes[ClassTypeInfo.TopSortedIndex].Count;
  try
  repeat
    for i := iStart to iEnd do
      BatchList.AddLocator(TempList.Locators[i]);
    Report('%s: Loading Objects %d-%d', [ClassTypeInfo.DisplayName, iStart, iEnd]);
    if AttributeList.IsEmpty  then
      BatchList.EnsureObjects
    else
      SourceSystem.FetchMembersWithObjects(BatchList, AttributeList);
    if not DelayedFetchedList.IsEmpty then
    begin
      Report('%s: Loading delyed fetched members for objects %d-%d', [ClassTypeInfo.DisplayName, iStart, iEnd]);
      SourceSystem.FetchMembersWithObjects(BatchList, DelayedFetchedList);
    end;
    Report('%s: Cloning Objects %d-%d', [ClassTypeInfo.DisplayName, iStart, iEnd]);
    DestinationSystem.StartTransaction;
    try
      for i := 0 to BatchList.Count-1 do
      begin
        if not Assigned(Mapping.ReferencedObjects[BatchList.Locators[i]]) then
        begin
          SourceObject := BatchList.Locators[i].BoldObject;
          if EnsureDestinationObject(SourceObject, DestinationObject) then
            if StrToInt(DestinationObject.BoldObjectLocator.BoldObjectID.AsString) <= fLastUseId then
              Inc(Reused);
          DestinationList.Add(DestinationObject);
          SetReference(SourceObject, DestinationObject);
        end;
      end;
      DestinationSystem.CommitTransaction;
    except
      DestinationSystem.RollbackTransaction;
      raise;
    end;
    try
      if DestinationSystem.BoldPersistent then
      begin
        Report('Writing %d objects', [DestinationList.Count]);
        DestinationSystem.UpdateDatabaseWithList(DestinationList);
        for i := DestinationList.Count - 1 downto 0 do
          DestinationList.Locators[i].UnloadBoldObject;
      end;
    except
      on e: exception do
      begin
        Report('%s %s', [e.ClassName, e.Message]);
        raise;
      end;
    end;
    DestinationList.Clear;
    for i := BatchList.Count-1 downto 0 do
      BatchList.Locators[i].UnloadBoldObject;
    BatchList.Clear;
    if iEnd = TempList.Count-1 then
      break;
    inc(iStart, BatchSize);
    inc(iEnd, BatchSize);
    iEnd := MinIntValue([iEnd, TempList.Count-1]);
  until false;
  finally
    Report('%s Source:%d Destination before:%d after:%d reused:%d', [ClassTypeInfo.DebugInfo, ASourceList.Count, CountBefore, DestinationSystem.Classes[ClassTypeInfo.TopSortedIndex].Count, Reused]);
  end;
end;

procedure TBoldSystemMerger.FirstPass;
var
  SourceClassList: TBoldObjectList;
  ClassTypeInfo: TBoldClassTypeInfo;
  i: integer;
begin
  Report('---------------',[]);
  Report('Copying objects',[]);
  Report('---------------',[]);
  for i := 0 to SourceSystem.BoldSystemTypeInfo.TopSortedClasses.Count - 1do
  begin
    ClassTypeInfo := SourceSystem.BoldSystemTypeInfo.TopSortedClasses[i];
    if ClassTypeInfo.IsAbstract or ClassTypeInfo.IsLinkClass or not ClassTypeInfo.Persistent then
      continue; // skip link classes in first pass
    if not fArrayOfClassesToIgnore[ClassTypeInfo.TopSortedIndex] then
    begin
      SourceClassList := SourceSystem.Classes[ClassTypeInfo.TopSortedIndex];
      ProcessSourceObjectList(SourceClassList);
    end;
  end;
end;

procedure TBoldSystemMerger.SecondPass;
var
  SourceLocator: TBoldObjectLocator;
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
  DestinationList: TBoldObjectList;
  BatchList: TBoldObjectList;
  FilteredList: TBoldObjectList;
  MemberIdList: TBoldMemberIdList;
  i, c, iStart, iEnd: integer;
  g: IBoldGuard;
  Comparer: TBoldSystemComparer;
begin
  g := TBoldGuard.Create(BatchList, MemberIdList, DestinationList, Comparer);
  BatchList := TBoldObjectList.Create;
  BatchList.Capacity := BatchSize;
  MemberIdList := TBoldMemberIdList.Create;
  MemberIdList.OwnsEntries := true;
  DestinationList := TBoldObjectList.Create;
  Report('-------------',[]);
  Report('Copying links', []);
  Report('-------------',[]);
  for c := SourceSystem.BoldSystemTypeInfo.TopSortedClasses.Count - 1 downto 0 do
  begin
    if fStopped then
      exit;
    ClassTypeInfo := SourceSystem.BoldSystemTypeInfo.TopSortedClasses[c] as TBoldClassTypeInfo;
    if ClassTypeInfo.IsAbstract or ClassTypeInfo.IsLinkClass or not ClassTypeInfo.Persistent then
      continue;
    if fArrayOfClassesToIgnore[ClassTypeInfo.TopSortedIndex] then
      continue;
    ClassList := SourceSystem.Classes[c];
    if ClassList.Empty then
      continue;
    MemberIdList.Clear;
    CollectMembers(ClassTypeInfo, MemberIdList, [rtRole, rtLinkRole], false, false, true);
    if MemberIdList.Count = 0 then
      continue;
    Report('%d IDs loaded for class %s', [ClassList.Count, ClassTypeInfo.DebugInfo]);
    iStart := 0;
    iEnd := MinIntValue([ClassList.Count-1, BatchSize]);
    repeat
      if fStopped then
        exit;
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
      Report('Loading %d links %d-%d for: %s', [MemberIdList.Count, iStart, iEnd, ClassTypeInfo.DisplayName]);
      SourceSystem.FetchMembersWithObjects(FilteredList, MemberIdList);
      Report('Merging links %d-%d for: %s', [iStart, iEnd, ClassTypeInfo.DisplayName]);
      FilteredList.EnsureObjects;
      DestinationSystem.StartTransaction;
      try
        for I := 0 to FilteredList.Count - 1 do
        begin
          SourceLocator := FilteredList.Locators[i];
          if not Assigned(SourceLocator.BoldObject) then
            if not Assigned(SourceLocator.EnsuredBoldObject) then
              assert(false);
          CopyLinks(SourceLocator.BoldObject, MemberIdList, DestinationList);
          SourceLocator.UnloadBoldObject;
        end;
        DestinationSystem.CommitTransaction;
        if DestinationSystem.BoldDirty then
        begin
          Report('Writing %d objects', [DestinationSystem.DirtyObjects.Count]);
          DestinationSystem.UpdateDatabaseWithList(DestinationList);
          for i := DestinationList.Count-1 downto 0 do
            DestinationList.Locators[i].UnloadBoldObject;
        end;
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
  DestinationSystem.UpdateDatabase;
end;

function TBoldSystemMerger.SkipLink(AMultiLink: TBoldRoleRTInfo): boolean;
begin
  result := SkipppedLinks.IndexOf(AMultiLink.DisplayName) <> -1;
end;

procedure TBoldSystemMerger.SetReference(SourceObject,
  DestinationObject: TBoldObject);
begin
  Assert((SourceObject.BoldSystem = SourceSystem) and (DestinationObject.BoldSystem = DestinationSystem));
  if Assigned(Mapping.ReferencedObjects[SourceObject.BoldObjectLocator]) and (Mapping.ReferencedObjects[SourceObject.BoldObjectLocator] <> DestinationObject.BoldObjectLocator) then
    raise Exception.Create('Object already mapped');
  Mapping.ReferencedObjects[SourceObject.BoldObjectLocator] := DestinationObject.BoldObjectLocator;
end;

procedure TBoldSystemMerger.Stop;
begin
  fStopped := true;
end;

procedure TBoldSystemMerger.Report(const Msg: string;
  const Args: array of const);
begin
  if Assigned(fProgressEvent) then
  begin
    if Length(Args) = 0 then
      fProgressEvent(self, msg)
    else
      fProgressEvent(self, Format(msg, Args));
  end;
end;

procedure TBoldSystemMerger.CompareSystems;
var
  Comparer: TBoldSystemComparer;
  SourceClassList: TBoldObjectList;
  ClassTypeInfo: TBoldClassTypeInfo;
  BatchList: TBoldObjectList;
  TempList: TBoldObjectList;
  i, c, iStart,iEnd: integer;
  DestinationList: TBoldObjectList;
  DestinationLocator: TBoldObjectLocator;
  g: IBoldGuard;
const
  cSQLServerMillisecondRounding = 4; // SQL Server Timestamp column type does not support preccision higher than 3.3333333333 ms
begin
  g := TBoldGuard.Create(DestinationList, BatchList);
  Comparer := TBoldSystemComparer.Create;
  Comparer.OnGetCorrespondingObject := GetCorrespondingObject;
  Comparer.Options := [];
  Comparer.OnDifference := OnDifferenceEvent;
  if DestinationSystemHandle.PersistenceHandleDB.SQLDatabaseConfig.Engine = dbeSQLServer then
    Comparer.MillisecondRounding := cSQLServerMillisecondRounding;
  Report('-----------------------',[]);
  Report('Comparing Source and Destination',[]);
  Report('-----------------------',[]);
  DestinationList := TBoldObjectList.Create;
  BatchList := TBoldObjectList.Create;
  BatchList.Capacity := BatchSize;
  for c := 0 to SourceSystem.BoldSystemTypeInfo.TopSortedClasses.Count - 1do
  begin
    ClassTypeInfo := SourceSystem.BoldSystemTypeInfo.TopSortedClasses[c];
    if ClassTypeInfo.IsAbstract or not ClassTypeInfo.Persistent then
      continue;
    if fArrayOfClassesToIgnore[ClassTypeInfo.TopSortedIndex] then
      continue;
    if not fArrayOfClassesToIgnore[ClassTypeInfo.TopSortedIndex] then
    begin
      SourceClassList := SourceSystem.Classes[ClassTypeInfo.TopSortedIndex];
      TempList := SourceClassList.FilterOnType(ClassTypeInfo, false);
      try
        if TempList.Empty then
          continue;
        iStart := 0;
        iEnd := MinIntValue([TempList.Count-1, BatchSize]);
        Report('Comparing %s (%d)',[ClassTypeInfo.AsString, TempList.Count]);
        repeat
          BatchList.clear;
          for i := iStart to iEnd do
            BatchList.AddLocator(TempList.Locators[i]);
          BatchList.EnsureObjects;
          for i := 0  to BatchList.Count-1 do
          begin
            if not BatchList[i].BoldPersistent then
              continue;
            if fArrayOfClassesToIgnore[BatchList[i].BoldClassTypeInfo.TopSortedIndex] then
              continue;
            DestinationLocator := Mapping.ReferencedObjects[BatchList.Locators[i]] as TBoldObjectLocator;
            if not Assigned(DestinationLocator) then
              Report('DestinationLocator not found for source %s', [BatchList.Locators[i].EnsuredBoldObject.DebugInfo]);
            DestinationList.AddLocator(DestinationLocator);
          end;
          DestinationList.EnsureObjects;
          for I := 0 to BatchList.Count-1 do
          begin
            DestinationLocator := Mapping.ReferencedObjects[BatchList.Locators[i]] as TBoldObjectLocator;
            if not Assigned(DestinationLocator.EnsuredBoldObject) then
            begin
              Report('DestinationObject not found for source %s', [BatchList.Locators[i].EnsuredBoldObject.DebugInfo]);
              continue;
            end;
            Comparer.CompareObjects(BatchList[i], DestinationLocator.EnsuredBoldObject);
            BatchList[i].BoldObjectLocator.UnloadBoldObject;
            DestinationLocator.UnloadBoldObject;
          end;
          if iEnd = TempList.Count-1 then
            break;
          inc(iStart, BatchSize);
          inc(iEnd, BatchSize);
          iEnd := MinIntValue([iEnd, TempList.Count-1]);
        until false or fStopped;
      finally
        TempList.Free;
      end;
    end;
  end;
end;

procedure TBoldSystemMerger.Run;
var
  i: integer;
begin
  if fIsRunning then
    exit;
  fIsRunning:= true;
  try
    SourceSystemHandle.Active := true;
    DestinationSystemHandle.Active := true;
    FreeAndNil(fMapping);
    DestinationSystem.UndoHandlerInterface.Enabled := false;
    BoldLinks.BoldPerformMultilinkConsistencyCheck := false;
    BoldLinks.BoldPerformMultiLinkConsistenceCheckLimit := 0;
    if DestinationSystem.BoldSystemTypeInfo.OptimisticLocking <> bolmOff then
      raise Exception.Create('OptimisticLocking is turned on, merge is faster with no locking. Adjust the model and retry.');
    GetLastUsedId;
    SetLength(fArrayOfClassesToIgnore, SourceSystemHandle.System.BoldSystemTypeInfo.TopSortedClasses.Count);
    for I := 0 to SourceSystemHandle.System.BoldSystemTypeInfo.TopSortedClasses.Count - 1 do
      fArrayOfClassesToIgnore[i] := FilterClass(SourceSystemHandle.System.BoldSystemTypeInfo.TopSortedClasses[i]);
    FirstPass;
    if fStopped then
      exit;
    CopyAssociationObjects;
    if fStopped then
      exit;
    SecondPass;
    CompareSystems;
  finally
    fIsRunning := false;
  end;
end;

end.
