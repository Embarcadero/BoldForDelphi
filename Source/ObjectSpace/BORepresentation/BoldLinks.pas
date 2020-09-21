unit BoldLinks;

interface

uses
  BoldSystem,
  BoldObjectSpaceLists,
  BoldDomainElement,
  BoldId,
  BoldElements,
  BoldSubscription;

type
  {forward declarations of all classes}
  TBoldDirectSingleLinkController = class;
  TBoldIndirectSingleLinkController = class;
  TBoldMultiLinkController = class;
  TBoldDirectMultiLinkController = class;
  TBoldIndirectMultiLinkController = class;
  TBoldLinkObjectListController = class;
  TBoldLinkObjectReferenceController = class;
  TBoldLinkObjectSingleLinkController = class;

  { TBoldDirectSingleLinkController }
  TBoldDirectSingleLinkController = class(TBoldAbstractObjectReferenceController)
  private
    FOrderno: Integer;
    fLocator: TBoldObjectLocator;
    procedure AddToOtherEnd(Mode: TBoldLinkUnlinkMode);
    procedure RemoveFromOtherEnd(Mode: TBoldLinkUnlinkMode);
    procedure InternalSetLocator(NewLocator: TBoldObjectLocator);
  protected
    function MayUpdate: Boolean; override;
    procedure PreDiscard; override;
    procedure SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode); virtual;
    procedure SetOrderNo(NewORderNo: Integer; Mode: TBoldDomainElementProxyMode);
    function GetStreamName: String; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure MakeDbCurrent; override;
    function GetOtherEndController(aLocator: TBoldObjectLocator; AllowForceOtherEnd: Boolean): TBoldAbstractController;
    procedure SetAndModifyOrderNo(index: Integer);
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function GetLocator: TBoldObjectLocator; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
    property OrderNo: Integer read fOrderNo;
  end;

  { TBoldIndirectSingleLinkController }
  TBoldIndirectSingleLinkController = class(TBoldAbstractObjectReferenceController)
  private
    fLinkObjectLocator: TBoldObjectLocator;
    fOtherEndLocator: TBoldObjectLocator;
    function GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
    function GetLinkObjectOtherLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
    function GetLinkObjectRoleController: TBoldLinkObjectReferenceController;
  protected
    function MayUpdate: Boolean; override;
    function NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
    procedure DeleteLink(Mode: TBoldLinkUnlinkMode);
    procedure SetFromIds(Id1, Id2: TBoldObjectId; Mode: TBoldDomainElementProxyMode);
    function GetStreamName: String; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure MakeDbCurrent; override;
    function AssertIntegrity: Boolean; override;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function GetLocator: TBoldObjectLocator; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
  end;

  { TBoldMultiLinkController }
  TBoldMultiLinkController = class(TBoldAbstractObjectListController)
  private
    fMayBeOutOfOrder: Boolean;
  protected
    function GetCanCreateNew: Boolean; override;
    function CreateNew: TBoldElement; override;
    function IsInOrder: Boolean; virtual; abstract;
    procedure Resort; virtual; abstract; // order after orderno of other end
  public
    procedure MarkPossiblyOutOfOrder;
    procedure EnsureOrder;
  end;

  { TBoldDirectMultiLinkController }
  TBoldDirectMultiLinkController = class(TBoldMultiLinkController)
  private
    fLocatorList: TBoldObjectLocatorList;
    procedure ReOrder;
    property LocatorList: TBoldObjectLocatorList read fLocatorList;
  protected
    function GetOtherEndController(Locator: TBoldObjectLocator): TBoldDirectSingleLinkController;
    procedure SingleLinkUnlink(Locator: TBoldObjectLocator; OldLocator: TBoldObjectLocator;Mode: TBoldLinkUnlinkMode);
    procedure SingleLinkLinkTo(Locator: TBoldObjectLocator; NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
    procedure SetFromIdList(ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
    function GetStreamName: String; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
    procedure PrepareClear; override;
    function CompareOrderNo(Index1, Index2: integer): integer;
    procedure Exchange(Index1, Index2: integer);
    function IsInOrder: Boolean; override;
    procedure ClearNoLongerReferring(NewList: TBoldObjectIdList);
    procedure Resort; override;
  public
    constructor Create(OwningList: TBoldObjectList);
    destructor Destroy; override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    function GetCount: Integer; override;
    function GetLocator(index: Integer): TBoldObjectLocator; override;
    function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; override;
    function IncludesLocator(Locator: TBoldObjectLocator): Boolean; override;
    function IndexOfLocator(Locator: TBoldObjectLocator): Integer; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
    procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator); override;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    procedure SetLocator(index: Integer; Locator: TBoldObjectLocator); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure FreeContent; override;
    procedure Clear; override;
  end;

  { TBoldIndirectMultiLinkController }
  TBoldIndirectMultiLinkController = class(TBoldMultiLinkController)
  private
    fLinkLocatorList: TBoldObjectLocatorList;
    fReferredList: TBoldObjectLocatorList;
    function GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
    function GetLinkObjectOtherLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
    function NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
    function GetLinkObjectListController: TBoldLinkObjectListController;
    procedure DeleteLink(LinkObjectLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
    procedure ReOrder;
    property LinkLocatorList: TBoldObjectLocatorList read fLinkLocatorList;
    property ReferredLocatorList: TBoldObjectLocatorList read FReferredList;
    function ControllerForLinkMember: TBoldAbstractObjectListController;
    procedure ClearNoLongerReferring(NewList: TBoldObjectIdList);
  protected
    procedure SetFromIDLists(ListOfLinkObjects: TBoldObjectIdList; ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
    function GetStreamName: String; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
    procedure PrepareClear; override;
    function CompareOrderNo(Index1, Index2:integer): integer;
    procedure Exchange(Index1, Index2: integer);
    function IsInOrder: Boolean; override;
    procedure Resort; override;
  public
    constructor Create(OwningList: TBoldObjectList);
    destructor Destroy; override;
    function AssertIntegrity: Boolean; override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    function GetCount: Integer; override;
    function GetLocator(index: Integer): TBoldObjectLocator; override;
    function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; override;
    function IncludesLocator(Locator: TBoldObjectLocator): Boolean; override;
    function IndexOfLocator(Locator: TBoldObjectLocator): Integer; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
    procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator); override;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    procedure SetLocator(index: Integer; Locator: TBoldObjectLocator); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure FreeContent; override;
    procedure Clear; override;
  end;

  { TBoldLinkObjectListController }
  TBoldLinkObjectListController = class(TBoldAbstractObjectListController)
  private
    function GetLocatorList: TBoldObjectLocatorList;
    function GetMainListController: TBOldIndirectMultiLinkController;
    function GetMainList: TBoldObjectList;
    property LocatorList: TBoldObjectLocatorList read GetLocatorList;
  protected
    function GetStreamName: string; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    function GetCount: Integer; override;
    function GetLocator(index: Integer): TBoldObjectLocator; override;
    function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; override;
    function IncludesLocator(Locator: TBoldObjectLocator): Boolean; override;
    function IndexOfLocator(Locator: TBoldObjectLocator): Integer; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
    procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator); override;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure RemoveByIndex(index: Integer); override;
    procedure SetLocator(index: Integer; Locator: TBoldObjectLocator); override;
  end;

  { TBoldLinkObjectSingleLinkController }
  TBoldLinkObjectSingleLinkController = class(TBoldDirectSingleLinkController)
  private
    function OtherInnerLinkController: TBoldLinkObjectSingleLinkController;
  protected
    procedure SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode); override;
  public
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
  end;

  { TBoldLinkObjectReferenceController }
  TBoldLinkObjectReferenceController = class(TBoldAbstractObjectReferenceController)
  protected
    function GetStreamName: string; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    function GetLocator: TBoldObjectLocator; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

var
  BoldPerformMultilinkConsistencyCheck: Boolean = true;
  BoldPerformMultiLinkConsistenceCheckLimit: integer = 5000;

implementation

uses
  SysUtils,
  Classes,
  BoldCoreConsts,
  BoldGuard,
  BoldStreams,
  BoldValueSpaceInterfaces,
  BoldDefaultStreamNames,
  BoldSorter,
  BoldDefs,
  BoldValueInterfaces,
  BoldIndexableList,
  BoldSystemRT;

{---Proxies---}
{ TBoldDirectSingleLinkController_Proxy }
type
  TBoldDirectSingleLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdRef)
  private
    function GetDirectSingleLinkController: TBoldDirectSingleLinkController;
    // IBoldObjectIdRef
    procedure SetFromId(Id: TBoldObjectId);
    function GetId: TBoldObjectID;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    property DirectSingleLInkController: TBoldDirectSingleLinkController read GetDirectSingleLinkController;
  end;

  { TBoldIndirectSingleLinkController_Proxy }
  TBoldIndirectSingleLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdRefPair)
  private
    function GetInDirectSingleLinkController: TBoldInDirectSingleLinkController;
    // IBoldObjectIdRefPair
    procedure SetFromIds(Id1, Id2: TBoldObjectId);
    function GetId1: TBoldObjectID;
    function GetId2: TBoldObjectID;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    property InDirectSingleLInkController: TBoldInDirectSingleLinkController read GetInDirectSingleLinkController;
  end;

  { TBoldDirectMultiLinkController_Proxy }
  TBoldDirectMultiLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRef)
  private
    function GetDirectMultiLinkController: TBoldDirectMultiLinkController;
    // IBoldObjectIdListRef
    procedure SetFromIdList(IdLIst: TBoldObjectIdList);
    function GetIdList(Index: Integer): TBoldObjectID;
    function GetCount: integer;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    property DirectMultiLinkController: TBoldDirectMultiLinkController read GetDirectMultiLinkController;
  end;

  { TBoldInDirectMultiLinkController_Proxy }
  TBoldInDirectMultiLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRefPair)
  private
    function GetInDirectMultiLinkController: TBoldInDirectMultiLinkController;
    // IBoldObjectIdListRefPair
    function GetIdList1(Index: Integer): TBoldObjectID;
    function GetIdList2(Index: Integer): TBoldObjectID;
    procedure SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
    function GetCount: integer;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    property InDirectMultiLinkController: TBoldInDirectMultiLinkController read GetInDirectMultiLinkController;
  end;

{ TBoldDirectSingleLinkController_Proxy }

procedure TBoldDirectSingleLinkController_Proxy.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdRef;
  ObjRef: TBoldObjectReference;
begin
  if (not (Assigned(Source) and (source.QueryInterface(IBoldObjectIDRef, S) = S_OK))) then
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
  SetFromId(s.Id);
  if Mode <>bdepContents then { TODO : Check why Orderno not included in contents }
    SetOrderNo(s.OrderNo);
  if Mode in [bdepUnDo, bdepContents] then
  begin
    ObjRef := ProxedMember as TBoldObjectReference;
    ObjRef.HasOldValues := (s.BoldPersistenceState = bvpsInvalid) and
           (s.id <> nil);
  end;
end;

function TBoldDirectSingleLinkController_Proxy.GetDirectSingleLinkController: TBoldDirectSingleLinkController;
begin
  result := ProxedController as TBoldDirectSingleLinkController;
end;

function TBoldDirectSingleLinkController_Proxy.GetId: TBoldObjectID;
begin
  if assigned(DirectSingleLInkController.fLocator) then
    result := DirectSingleLInkController.fLocator.BoldObjectID
  else
    result := nil;
end;

function TBoldDirectSingleLinkController_Proxy.GetOrderNo: integer;
begin
    Result := DirectSingleLInkController.OrderNo;
end;

procedure TBoldDirectSingleLinkController_Proxy.SetFromId(Id: TBoldObjectId);
begin
  if Mode in [bdepPMIn, bdepContents, bdepUndo, bdepInternalInitialize] then
    DirectSingleLInkController.SetFromId(Id, Mode)
  else
   UnsupportedMode(Mode, 'SetFromId'); // do not localize
end;

procedure TBoldDirectSingleLinkController_Proxy.SetOrderNo(NewOrder: Integer);
begin
  if Mode in [bdepPMIn, bdepUnDo, bdepContents] then
    DirectSingleLInkController.SetOrderNo(NewOrder, Mode)
  else
   UnsupportedMode(Mode, 'SetOrderNo'); // do not localize
end;

{ TBoldIndirectSingleLinkController_Proxy }

procedure TBoldIndirectSingleLinkController_Proxy.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdRefPair;
begin
  if (not Assigned(Source)) or (source.QueryInterface(IBoldObjectIDRefPair, S) <> S_OK) then
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
  SetFromIds(s.Id1, s.Id2);
end;

function TBoldIndirectSingleLinkController_Proxy.GetId1: TBoldObjectID;
begin
  if assigned(InDirectSingleLInkController.fLinkObjectLocator) then
    result := InDirectSingleLInkController.fLinkObjectLocator.BoldObjectID
  else
    result := nil;
end;

function TBoldIndirectSingleLinkController_Proxy.GetId2: TBoldObjectID;
begin
  if assigned(InDirectSingleLInkController.fOtherEndLocator) then
    result := InDirectSingleLInkController.fOtherEndLocator.BoldObjectID
  else
    result := nil;
end;

function TBoldIndirectSingleLinkController_Proxy.GetInDirectSingleLinkController: TBoldInDirectSingleLinkController;
begin
  result := ProxedController as TBoldInDirectSingleLinkController;
end;

function TBoldIndirectSingleLinkController_Proxy.GetOrderNo: integer;
begin
  Result := 0;  // Indirect link has no orderno
end;

procedure TBoldIndirectSingleLinkController_Proxy.SetFromIds(Id1, Id2: TBoldObjectId);
begin
  if Mode in [bdepContents, bdepPMIn] then
    InDirectSingleLInkController.SetFromIds(Id1, Id2, Mode)
  else
    UnsupportedMode(Mode, 'SetFromId'); // do not localize
end;

procedure TBoldIndirectSingleLinkController_Proxy.SetOrderNo(NewOrder: Integer);
begin
  UnsupportedMode(Mode, 'SetOrderNo'); // do not localize
end;

{ TBoldDirectMultiLinkController_Proxy }

procedure TBoldDirectMultiLinkController_Proxy.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdListRef;
  i: Integer;
  anIdList: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(anIdList);
  if (not Assigned(Source)) or (source.QueryInterface(IBoldObjectIDListRef, S) <> S_OK) then
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
  anIdList := TBoldObjectIdList.Create;
  for i := 0 to s.Count - 1 do
    anIdList.Add(s.IdList[i]);
  SetFromIdList(anIdList)
end;

function TBoldDirectMultiLinkController_Proxy.GetCount: integer;
begin
  Result := DirectMultiLinkController.Count;
end;

function TBoldDirectMultiLinkController_Proxy.GetDirectMultiLinkController: TBoldDirectMultiLinkController;
begin
  result := ProxedController as TBoldDirectMultiLinkController;
end;

function TBoldDirectMultiLinkController_Proxy.GetIdList(Index: Integer): TBoldObjectID;
begin
  Result := DirectMultiLinkController.LocatorList[Index].BoldObjectId;
end;

procedure TBoldDirectMultiLinkController_Proxy.SetFromIdList(IdList: TBoldObjectIdList);
begin
  if Mode in [bdepContents, bdepPMIn] then
    DirectMultiLinkController.SetFromIdList(IdLIst, Mode)
  else
    UnsupportedMode(Mode, 'SetFromIdList'); // do not localize
end;

{ TBoldInDirectMultiLinkController_Proxy }

procedure TBoldInDirectMultiLinkController_Proxy.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdListRefPair;
  i: Integer;
  anIdList1: TBoldObjectIdList;
  anIdList2: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(anIdList1, anIdList2);
  if (not Assigned(source)) or (source.QueryInterface(IBoldObjectIDListRefPair, S) <> S_OK) then
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
  anIdList1 := TBoldObjectIdList.Create;
  anIdList2 := TBoldObjectIdList.Create;
  for i := 0 to s.Count - 1 do
  begin
    anIdList1.Add(s.IdList1[i]);
    anIdList2.Add(s.IdList2[i]);
  end;
  SetFromIDLists(anIdList1, anIdList2)
end;

function TBoldInDirectMultiLinkController_Proxy.GetCount: integer;
begin
  Result := InDirectMultiLinkController.GetCount;
end;

function TBoldInDirectMultiLinkController_Proxy.GetIdList1(Index: Integer): TBoldObjectID;
begin
  Result := InDirectMultiLinkController.LinkLocatorList[Index].BoldObjectId;
end;

function TBoldInDirectMultiLinkController_Proxy.GetIdList2(Index: Integer): TBoldObjectID;
begin
  Result := InDirectMultiLinkController.ReferredLocatorList[Index].BoldObjectId;
end;

function TBoldInDirectMultiLinkController_Proxy.GetInDirectMultiLinkController: TBoldInDirectMultiLinkController;
begin
  result := ProxedController as TBoldIndirectMultiLinkController;
end;

procedure TBoldInDirectMultiLinkController_Proxy.SetFromIdLists(IdList1,
  IdList2: TBoldObjectIdList);
begin
  if Mode in [bdepPMIn, bDepContents] then
    InDirectMultiLinkController.SetFromIdLists(IdList1, IdList2, Mode)
  else
    UnsupportedMode(Mode, 'SetFromIdLists'); // do not localize
end;

{ TBoldDirectMultiLinkController }

procedure TBoldDirectMultiLinkController.AddLocator(Locator: TBoldObjectLocator);
var
  OtherEndController: TBoldDirectSingleLinkController;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'AddLocator', ''); // do not localize
    PreChange;
    LocatorList.Add(Locator);
    OtherEndController := GetOtherEndController(Locator);
    OtherEndController.LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    if OwningObjectList.BoldRoleRtInfo.IsOrdered then
      OtherEndController.SetAndModifyOrderNo(LocatorList.IndexOf(Locator)); // Complexity warning: A loop of adds will take O(n^2)
    Changed(beItemAdded, [Locator]);

    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

constructor TBoldDirectMultiLinkController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  fLocatorList := TBoldObjectLocatorList.Create;
end;

destructor TBoldDirectMultiLinkController.Destroy;
begin
  FreeAndNil(fLocatorList);
  inherited;
end;

procedure TBoldDirectMultiLinkController.MakeDbCurrent;
begin
  EnsureOrder;
  DbFetchOwningMember;
end;

function TBoldDirectMultiLinkController.GetCount: Integer;
begin
  result := LocatorList.Count;
end;

function TBoldDirectMultiLinkController.GetLocator(index: Integer): TBoldObjectLocator;
begin
  EnsureOrder;
  result := LocatorList[index];
end;

function TBoldDirectMultiLinkController.GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
begin
  EnsureOrder;
  if not LocatorList.HasMembersIndex then
  begin
    if assigned(OwningObjectList.BoldRoleRTInfo) and OwningObjectList.BoldRoleRTInfo.isQualified then
    begin
      OwningObjectList.EnsureObjects;
      LocatorList.InitMembersIndex(OwningObjectList, OwningObjectList.BoldRoleRTInfo.Qualifiers)
    end
    else
      raise EBold.CreateFmt(sRolenotQualified, [ClassName]);
  end;
  result := LocatorList.GetLocatorByAttributesAndSubscribe(MemberList, Subscriber);
end;

function TBoldDirectMultiLinkController.IncludesLocator(Locator: TBoldObjectLocator): Boolean;
begin
  result := LocatorList.LocatorInList[Locator];
end;

function TBoldDirectMultiLinkController.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  EnsureOrder;
  result := LocatorList.IndexOf(Locator);
end;

procedure TBoldDirectMultiLinkController.InsertLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'InsertLocator', ''); // do not localize

    PreChange;
    LocatorList.Insert(Index, Locator);
    GetOtherEndController(Locator).LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    ReOrder;
    Changed(beItemAdded, [Locator]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldDirectMultiLinkController.linkto(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode) ;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent));
  Assert(not IncludesLocator(NewLocator));
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Linkto', ''); // do not localize

  PreChange;
  LocatorList.Add(NewLocator);
  if Mode = blulMarkAdjusted then
    OwningObjectList.Adjusted := True;
  if updateOrderNo and OwningObjectList.BoldRoleRtInfo.IsOrdered then
    GetOtherEndController(NewLocator).SetAndModifyOrderNo(LocatorList.IndexOf(NewLocator)); // Complexity warning: A loop of adds will take O(n^2)
  Changed(beItemAdded, [NewLocator]);
  if Mode = blulMarkModified then
    EndModify;
end;

procedure TBoldDirectMultiLinkController.Move(CurrentIndex, NewIndex: Integer);
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Move', ''); // do not localize

    PreChange;
    LocatorList.Move(CurrentIndex, NewIndex);
    ReOrder;
    Changed(beOrderChanged, []);

    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;


procedure TBoldDirectMultiLinkController.RemoveByIndex(index: Integer);
var
  Locator: TBoldObjectLocator;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', ''); // do not localize

    Locator := LocatorList[index];
    PreChange;
    GetOtherEndController(Locator).Unlink(OwningList.OwningObject.BoldObjectLocator, blulMarkModified);
    LocatorList.RemoveByIndex(index);
    ReOrder;
    Changed(beItemDeleted, [Locator]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldDirectMultiLinkController.ReOrder;
var
{$IFOPT C+} // if Assertions on
  index: Integer;
{$ENDIF}
  I: Integer;
  Locator: TBoldObjectLocator;
begin
  if not OwningObjectList.BoldRoleRtInfo.IsOrdered then
    exit;
{$IFOPT C+} // if Assertions on
  index := OwningObjectList.BoldRoleRtInfo.IndexOfOtherEnd;
  Assert(index <> -1);
{$ENDIF}
  for I := 0 to LocatorList.Count - 1 do
  begin
    Locator := LocatorList[I];
    Locator.EnsureBoldObject; // Note, can give fetch during fetch, save till all fetched
    GetOtherEndController(Locator).SetAndModifyOrderNo(I);
  end;
end;

procedure TBoldDirectMultiLinkController.SetFromIdList(ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
var
  NewListOfOtherEnd: TBoldObjectIdlist;

  // Ajdust NewListOfOtherEnd, return true if adjusted
  function AdjustList: boolean;
    procedure SafeCopyOptimisticValues;
    var
      Value: IBoldObjectIdListRef;
    begin
      Value := NewValueInOptimisticLocking as IBoldObjectIdListRef;
      if Assigned(Value) then
        Value.SetFromIdList(ListOfOtherEnd);
    end;
  var
    I: Integer;
    BoldObject: TBoldObject;
    AnObjectRef: TBoldObjectReference;
    IndexOfOtherEnd: Integer;
    BoldClassTypeInfoOfOtherEnd: TBoldClassTypeInfo;
  begin
    BoldClassTypeInfoOfOtherEnd := OwningObjectList.BoldRoleRtInfo.ClassTypeInfoOfOtherEnd;
    IndexOfOtherEnd := OwningObjectList.BoldRoleRtInfo.IndexOfOtherEnd;
    Result := False;
    {Adjust list}
    for I := newListOfOtherEnd.Count - 1 downto 0 do
    begin
      BoldObject := BoldSystem.Locators.ObjectByID[NewListOfOtherEnd[I]];
      if Assigned(BoldObject) then {Object in core}
      begin
        if BoldObject.BoldMemberAssigned[IndexOfOtherEnd] then
          AnObjectRef := (BoldObject.BoldMembers[IndexOfOtherEnd] as TBoldObjectReference)
        else
          AnObjectRef := nil;

        if (BoldObject.BoldExistenceState = besDeleted)
          or ( Assigned(AnObjectRef) and (AnObjectRef.BoldPersistenceState = bvpsModified) and
          (AnObjectRef.Locator <> OwningList.OwningObject.BoldObjectLocator)) then
        begin
          if not Result then
          begin
            SafeCopyOptimisticValues;
            Result := true;
          end;
          NewListOfOtherEnd.RemoveByIndex(I);
        end;
      end;
    end;

    for I := 0 to BoldSystem.DirtyObjects.Count - 1 do
    begin
      BoldObject := BoldSystem.DirtyObjects[I];
      if (BoldObject.BoldClassTypeInfo.BoldIsA(BoldClassTypeInfoOfOtherEnd)) and
          BoldObject.BoldMemberAssigned[IndexOfOtherEnd] and
          (BoldObject.BoldMembers[IndexOfOtherEnd].BoldPersistenceState <> bvpsInvalid) and
        (((BoldObject.BoldMembers[IndexOfOtherEnd]) as TBoldObjectReference).BoldObject = OwningList.OwningObject) then
      begin
        if not Result then
        begin
          SafeCopyOptimisticValues;
          Result := true;
        end;
        NewListOfOtherEnd.Add(BoldObject.BoldObjectLocator.BoldObjectID);
      end;
    end;
  end;

  var
    PreChangeCalled: Boolean;

  procedure PreChangeIfNeeded;
  begin
    if not PreChangeCalled then
    begin
      PreChange;
      PreChangeCalled := True;
    end;
  end;

var
  OldLocator, NewLocator:TBoldObjectLocator;
  I: integer;
  PreserveOrder: Boolean;
  WasAdjusted: Boolean;
  LinkUnlinkMode: TBoldLinkUnlinkMode;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(NewListOfOtherEnd);
  PreChangeCalled := True;
  if (mode = bdepPMIn) and (OwningMember.OwningObject.BoldObjectLocator.BoldObjectId.TimeStamp <> BOLDMAXTIMESTAMP) then  // fetching old temporal versi
    mode := bdepContents;
  if assigned(ListOfOtherEnd) then
    NewListOfOtherEnd := ListOfOtherEnd.Clone
  else
    NewListOfOtherEnd := TBoldObjectidList.create;
  if (mode = bdepPMIn) then
    WasAdjusted := AdjustList
  else
    WasAdjusted := false;
  if Mode = bdepPMIn then
    ClearNoLongerReferring(newListOfOtherEnd);
  if Mode = bdepPmIn then
    LinkUnlinkMode := blulMarkAdjusted
  else
    LinkUnlinkMode := blulNone;
  PreserveOrder := (mode = bdepContents) or ((mode = bdepPMIn) and OwningObjectList.BoldRoleRTInfo.IsOrdered and not WasAdjusted);

  {we now have a list with the right objects}
  for I := GetCount - 1 downto 0 do
  begin
    OldLocator := LocatorList[i];
    if not NewListOfOtherEnd.IdInList[OldLocator.BoldObjectId] then
    begin
      PreChangeIfNeeded;
      if mode = bdepPMIn then
        SingleLinkUnlink(LocatorList[I], OwningList.OwningObject.BoldObjectLocator, LinkUnlinkMode);
      LocatorList.RemoveByIndex(I);
      Changed(beItemDeleted, [OldLocator]);
    end;
  end;

  if PreserveOrder then
  begin
    for I := 0 to NewListOfOtherEnd.Count - 1 do
    begin
      NewLocator := AssertedLocatorForId(NewListOfOtherEnd[I]);
      if i >= LocatorList.Count then
      begin
        PreChangeIfNeeded;
        if mode = bdepPMIn then
          SingleLinkLinkTo(NewLocator, OwningList.OwningObject.BoldObjectLocator, false, LinkUnlinkMode);
        LocatorList.Add(NewLocator);
        Changed(beItemAdded, [NewLocator]);
      end
      else if NewLocator = LocatorList[i] then
        // All in order, do nothing
      else if LocatorList.IndexOf(NewLocator) <> -1 then  // locator in list, but at wrong place
      begin
        PreChangeIfNeeded;
        LocatorList.Move(LocatorList.IndexOf(NewLocator), I);
        Changed(beOrderChanged, []);
      end
      else
      begin  // locator not in list, insert it,
        PreChangeIfNeeded;
        if mode = bdepPMIn then
          SingleLinkLinkTo(NewLocator, OwningList.OwningObject.BoldObjectLocator, false, LinkUnlinkMode);
        LocatorList.Insert(I, NewLocator);
        Changed(beItemAdded, [NewLocator]);
      end;
    end;
  end
  else // Ignore Order
  begin
    for I := 0 to NewListOfOtherEnd.Count - 1 do
    begin
      NewLocator := AssertedLocatorForId(NewListOfOtherEnd[I]);
      if not IncludesLocator(NewLocator) then
      begin
        PreChangeIfNeeded;
        if mode = bdepPMIn then
          SingleLinkLinkTo(NewLocator, OwningList.OwningObject.BoldObjectLocator, false, LinkUnlinkMode);
        LocatorList.Add(NewLocator);
        Changed(beItemAdded, [NewLocator]);
      end;
    end;
  end;
end;

procedure TBoldDirectMultiLinkController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'SetLocator', ''); // do not localize

    PreChange;
    GetOtherEndController(LocatorList[index]).UnLink(OwningList.OwningObject.BoldObjectLocator, blulMarkModified);
    GetOtherEndController(Locator).LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    LocatorList[index] := Locator;
    ReOrder;
    Changed(beItemReplaced, [Locator, Index]);

    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldDirectMultiLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdListRef;
end;

procedure TBoldDirectMultiLinkController.Unlink(OldLocator: TBoldObjectLocator;  Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent));
  Assert(IncludesLocator(OldLocator));
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Unlink', ''); // do not localize

  PreChange;
  LocatorList.Remove(OldLocator);
  if Mode = blulMarkAdjusted then
    OwningObjectList.Adjusted := True;
  Changed(beItemDeleted, [OldLocator]);
  if Mode = blulMarkModified then
    EndModify;
end;

function TBoldDirectMultiLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRef) then
  begin
    result := ProxyClass.create(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRef', [ClassName]); // do not localize
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldDirectMultiLinkController.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBoldDirectmultiLinkController_Proxy;
end;

procedure TBoldDirectMultiLinkController.FreeContent;
begin
  { TODO : Clear embedded links in locators. }
  LocatorList.Clear;
end;

procedure TBoldDirectMultiLinkController.PrepareClear;
begin
  OwningObjectList.EnsureObjects;
end;

procedure TBoldDirectMultiLinkController.Resort;
begin
  BoldSort(0, LocatorList.Count - 1, CompareOrderNo, Exchange);
  Changed(beOrderChanged, []);
  Assert(IsInOrder);
end;

function TBoldDirectMultiLinkController.IsInOrder: Boolean;
var
  i: integer;
  OrderNo1, OrderNo2: integer;
begin
  Result := True;
  if OwningObjectList.BoldRoleRTInfo.IsOrdered then
    for i:= 0 to LocatorList.Count - 2 do
    begin
      OrderNo1 := GetOtherEndController(LocatorList[i]).FOrderno;
      OrderNo2 := GetOtherEndController(LocatorList[i + 1]).FOrderno;
      Result := (OrderNo1 <= OrderNo2);
      if not Result then
        Exit;
    end;
end;

function TBoldDirectMultiLinkController.CompareOrderNo(Index1, Index2: integer): integer;
var
  OrderNo1, OrderNo2: integer;
begin
  if OwningObjectList.BoldRoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole then
  begin
    OrderNo1 := GetOtherEndController(LocatorList[Index1]).FOrderNo;
    OrderNo2 := GetOtherEndController(LocatorList[Index2]).FOrderNo;
    Result := (OrderNo1 - OrderNo2);
  end
  else
    raise EBold.Create(sOtherEndMustBeSingle);
end;

procedure TBoldDirectMultiLinkController.Exchange(Index1, Index2: integer);
begin
  LocatorList.Exchange(Index1, Index2);
end;

procedure TBoldDirectMultiLinkController.ClearNoLongerReferring(NewList: TBoldObjectIdList);
var
  EmbeddedIndex, IndexOfOtherEnd: integer;
  BoldClassTypeInfoOfOtherEnd: TBoldClassTypeInfo;
  Reference: TBoldObjectReference;

  procedure GetAllPointingAtSelf(ObjectList: TBoldObjectList);
  var
    Traverser: TBoldLocatorListTraverser;
    G: IBoldGuard;
    OtherEndController: TBoldDirectSingleLinkController;
    Locator: TBoldObjectLocator;
    OwnLocator: TBoldObjectLocator;
  begin
    G := TBoldGuard.Create(Traverser);
    Traverser := BoldSystem.Locators.CreateTraverser;
    OwnLocator := OwningMember.OwningObject.BoldObjectLocator;
    while not Traverser.EndOfList do
    begin
      Locator := Traverser.Locator;
      if BoldSystem.BoldSystemTypeInfo.TopSortedClasses[Locator.BoldObjectId.TopSortedIndex].BoldIsA(BoldClassTypeInfoOfOtherEnd) then  //
      begin
        if assigned(Locator.BoldObject) then
        begin
          if Locator.BoldObject.BoldMemberAssigned[IndexOfOtherEnd] then
          begin
            OtherEndController := GetOtherEndController(Locator);
            Reference := OtherEndController.OwningReference;
            if (Reference.BoldPersistenceState = bvpsCurrent) or
                (Reference.BoldPersistenceState = bvpsInvalid) and (Reference.HasOldValues) then
              if OtherEndController.fLocator = OwnLocator then
                ObjectList.AddLocator(Locator);
          end
        end
        else if (EmbeddedIndex <> -1) and (Locator.EmbeddedSingleLinks[EmbeddedIndex] = OwnLocator) then
          ObjectList.AddLocator(Locator);
      end;
      Traverser.Next;
    end;
  end;

var
  i: integer;
  aLocator: TBoldObjectLocator;
  ListOfReferring: TBoldObjectlist;
  OtherEndController: TBoldDirectSingleLinkController;
begin
  Assert(Assigned(NewList));
  EmbeddedIndex := OwningObjectList.BoldRoleRtInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
  IndexOfOtherEnd := OwningObjectList.BoldRoleRtInfo.IndexOfOtherEnd;
  BoldClassTypeInfoOfOtherEnd := OwningObjectList.BoldRoleRtInfo.ClassTypeInfoOfOtherEnd;
  if (OwningList.BoldPersistenceState = bvpsCurrent) then
    ListOfReferring := OwningObjectList
  else
  begin
    ListOfReferring := TBoldObjectList.Create;
    if BoldPerformMultilinkConsistencyCheck or
      (BoldPerformMultiLinkConsistenceCheckLimit > BoldSystem.Locators.Count) then
    begin
      GetAllPointingAtSelf(ListOfReferring);
    end;
  end;

  for i:= 0 to ListOfReferring.Count - 1 do
  begin
    aLocator := ListOfReferring.locators[i];
    if not NewList.IdInList[aLocator.BoldObjectId] then
    begin
      OtherEndController := GetOtherEndController(aLocator);
      if Assigned(OtherEndController) then
      begin
        Assert(OtherEndController.OwningMember.BoldPersistenceState in [bvpsCurrent, bvpsInvalid]);
        OtherEndController.FLocator := nil;
        if OtherEndController.OwningMember.BoldPersistenceState <> bvpsInvalid then
        begin
          OtherEndController.OwningMember.BoldPersistenceState := bvpsInvalid;
          OtherEndController.OwningMember.SendEvent(beValueInvalid);
        end;
        OtherEndController.OwningReference.HasOldValues := false;
      end
      else  // member for other end not instantiated.
      begin
        EmbeddedIndex := OwningObjectList.BoldRoleRtInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
        if (EmbeddedIndex <> -1) and (aLocator.EmbeddedSingleLinks[EmbeddedIndex] <> nil) then
        begin
          Assert(aLocator.EmbeddedSingleLinks[EmbeddedIndex] = OwningMember.OwningObject.BoldObjectLocator);
          aLocator.EmbeddedSingleLinks[EmbeddedIndex] := nil;
        end;
      end;
    end;
  end;
  if ListOfReferring <> OwningObjectList then
    FreeAndNil(ListOfReferring);
end;

procedure TBoldDirectMultiLinkController.Clear;
var
  Locator: TBoldObjectLocator;
  ix: integer;
begin
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Clear', ''); // do not localize

    PreChange;
    while count > 0 do
    begin
      ix := Count-1;
      Locator := LocatorList[ix];
      GetOtherEndController(Locator).Unlink(OwningList.OwningObject.BoldObjectLocator, blulMarkModified);
      LocatorList.RemoveByIndex(ix);
      Changed(beItemDeleted, [Locator]);
    end;
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

{ TBoldDirectSingleLinkController }

procedure TBoldDirectSingleLinkController.AddToOtherEnd(Mode: TBoldLinkUnlinkMode);
var
  OtherEndController: TBoldAbstractController;
begin
  OtherEndController := GetOtherEndController(fLocator, Mode <> blulMarkAdjusted);
  if Assigned(OtherEndController) and ((Mode <> blulMarkAdjusted) or  (OtherEndController.OwningMember.BoldPersistenceState = bvpsCurrent)) then
    OtherEndController.linkto(OwningReference.OwningObject.BoldObjectLocator, Mode <> blulMarkAdjusted, Mode);
end;

procedure TBoldDirectSingleLinkController.SetAndModifyOrderNo(index: Integer);
begin
  // Note: Setting Orderno does not send a message, but does make member modified.
  if index <> FOrderno then
  begin
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetAndModifyOrderNo', ''); // do not localize
    PreChange;
    fOrderNo := Index;
    EndModify;
  end;
end;

function TBoldDirectSingleLinkController.GetLocator: TBoldObjectLocator;
begin
  result := fLocator;
end;

function TBoldDirectSingleLinkController.GetOtherEndController(aLocator: TBoldObjectLocator; AllowForceOtherEnd: Boolean): TBoldAbstractController;
var
  aMember: TBoldMember;
begin
  result := nil;
  if not assigned(aLocator) then
    exit;
  if OwningReference.BoldRoleRtInfo.IndexOfOtherEnd = -1 then
    exit;

  if AllowForceOtherEnd and OwningReference.BoldRoleRtInfo.ForceOtherEnd then
  begin
    aLocator.EnsureBoldObject;
    aLocator.BoldObject.BoldMembers[OwningReference.BoldRoleRtInfo.IndexOfOtherEnd].EnsureContentsCurrent;
  end;

  if not assigned(aLocator.BoldObject) then
    exit;

  aMember := aLocator.BoldObject.BoldMembers[OwningReference.BoldRoleRtInfo.IndexOfOtherEnd];

  if aMember.BoldPersistenceState = bvpsInvalid then
    exit;

  result := GetControllerForMember(aMember);
end;

procedure TBoldDirectSingleLinkController.LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'linkto', ''); // do not localize
  if fLocator <> NewLocator then
  begin
    RemoveFromOtherEnd(Mode);
    InternalSetLocator(NewLocator);
    if OwningMember.BoldPersistenceState = bvpsInvalid then
      OwningReference.HasOldValues := True;
  end;
  if Mode = blulMarkModified then
    EndModify;
end;

procedure TBoldDirectSingleLinkController.RemoveFromOtherEnd(Mode: TBoldLinkUnlinkMode);
var
  OtherEndController: TBoldAbstractController;
  OldLocatorRelevant: Boolean;
begin
  OldLocatorRelevant := ((OwningMember.BoldPersistenceState <> bvpsInvalid) or OwningReference.HasOldValues);
  if OldLocatorRelevant then
  begin
    OtherEndController := GetOtherEndController(fLocator, Mode <> blulMarkAdjusted);
    if Assigned(OtherEndController) then
      OtherEndController.Unlink(OwningReference.OwningObject.BoldObjectLocator, Mode);
  end;
end;

procedure TBoldDirectSingleLinkController.SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode);
var
  NewLocator: TBoldObjectLocator;

  procedure SafeCopyOptimisticValues;
  var
    Value: IBoldObjectIdRef;
  begin
    Value := NewValueInOptimisticLocking as IBoldObjectIdRef;
    if Assigned(Value) then
      Value.SetFromId(Id);
  end;
  // Adjust Newlocator
  procedure AdjustNewLocator;
  var
    i: integer;
    obj: TBoldObject;
    OtherEndController: TBoldAbstractController;
  begin
    // check if link modified at other (embedded) end
    for i := 0 to OwningMember.BoldSystem.DirtyObjects.Count - 1 do
    begin
      Obj := TBoldObject(OwningMember.BoldSystem.DirtyObjects[i]);
      if Obj.BoldClassTypeInfo.Conformsto(OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd) then
      begin
        OtherEndController := GetOtherEndController(Obj.BoldObjectLocator, false);
        if (OtherEndController is TBoldDirectSingleLinkController) and
          (TBoldDirectSingleLinkController(OtherEndController).fLocator = OwningReference.OwningObject.BoldObjectLocator) then
        begin
          SafeCopyOptimisticValues;
          NewLocator := obj.BoldObjectLocator;
          break;
        end;
      end;
    end;
  end;

var
  LinkUnlinkMode: TBoldLinkUnlinkMode;
begin
  if (mode = bdepPMIn) and (OwningMember.OwningObject.BoldObjectLocator.BoldObjectId.TimeStamp <> BOLDMAXTIMESTAMP) then  // fetching old temporal versi
    mode := bdepContents;
  NewLocator := LocatorForId(Id);

  if (mode = bdepPMIn) and not OwningReference.BoldRoleRTInfo.IsStoredInObject then // non-embedded end of 1-1
    AdjustNewLocator;

  if Mode = bdepPmIn then
    LinkUnlinkMode := blulMarkAdjusted
  else
    LinkUnlinkMode := blulNone;
  if ((OwningMember.BoldPersistenceState = bvpsInvalid) and not OwningReference.HasOldValues) or (fLocator <> NewLocator) then
  begin
    if mode <> bdepInternalInitialize then
      PreChange;
    if mode in [bdepPMIn, bdepUndo] then
      RemoveFromOtherEnd(LinkUnlinkMode);
    fLocator := NewLocator;
    if mode in [bdepPMIn, bdepUndo] then
      AddToOtherEnd(LinkUnlinkMode);
    Changed(beValueChanged, [NewLocator]);
  end;
end;

procedure TBoldDirectSingleLinkController.SetLocator(NewLocator: TBoldObjectLocator);
begin
  OwningMember.BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetLocator', ''); // do not localize

    GetOtherEndController(NewLocator, True); // Makes sure other end is fetched if needed before changing this end
    RemoveFromOtherEnd(blulMarkModified);
    InternalSetLocator(NewLocator);
    AddToOtherEnd(blulMarkModified);
    if not Assigned(fLocator) then
      SetAndModifyOrderNo(-1);
    EndModify;
    OwningReference.BoldSystem.CommitTransaction;
  except
    OwningReference.BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldDirectSingleLinkController.InternalSetLocator(NewLocator: TBoldObjectLocator);
begin
  if fLocator <> NewLocator then
  begin
    PreChange;
    fLocator := NewLocator;
    Changed(beValueChanged, [NewLocator]);
  end;
end;

function TBoldDirectSingleLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRef;
end;

procedure TBoldDirectSingleLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (Owningmember.BoldPersistenceState = bvpsCurrent));
  Assert(Not assigned(fLocator) or (fLocator = OldLocator));

  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'Unlink', ''); // do not localize

  InternalSetLocator(nil);
  OwningReference.HasOldValues := False;
  SetAndModifyOrderNo(-1);
  if Mode = blulMarkModified then
    EndModify;
end;

procedure TBoldDirectSingleLinkController.PreDiscard;
  procedure InvalidateNonembeddedOtherEnd(Locator: TBoldObjectLocator);
  var
    OtherEndController: TBoldAbstractController;
  begin
    if assigned(Locator) then
    begin
      OtherEndController := GetOtherEndController(Locator, false);
      if assigned(OtherEndController) and
        not OtherEndController.OwningMember.BoldMemberRTInfo.IsStoredInObject and
        not OtherEndController.OwningMember.OwningObject.BoldObjectIsNew then
      begin
        // normally, the other end will be a multilink, and not dirty, but if the
        // other end is either a singlelink (single-single) or a multilink and the
        // system is using an XML-Persistence (MultilinksAreStoredInObject) then we
        // hope that the other end will be discarded separately.
        if not OtherEndController.OwningMember.BoldDirty then
          OtherEndController.OwningMember.Invalidate;
      end;
    end;
  end;

var
  OldRef: IBoldValue;
  OldIdRef: IBoldObjectIdRef;
begin
  // remove the owningobject from the other end
  RemoveFromOtherEnd(blulNone);

  // Invalidate the old other end if it exists and is loaded
  OldRef := OwningReference.OldValue;
  if assigned(OldRef) then
  begin
    // The OldValue must be an ObjectIdRef since this is a direct singlelink controller
    OldRef.QueryInterface(IBoldObjectIdRef, OldIdRef);
    if assigned(OldIdRef.Id) then
      InvalidateNonembeddedOtherEnd(OwningReference.BoldSystem.EnsuredLocatorByID[OldIdRef.Id]);
  end;
end;

function TBoldDirectSingleLinkController.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBoldDirectSingleLinkController_Proxy;
end;

function TBoldDirectSingleLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdRef) then
  begin
    result := ProxyClass.create(self.OwningReference, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdRef', [ClassName]); // do not localize
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldDirectSingleLinkController.MayUpdate: Boolean;
begin
  result := not OwningReference.BoldRoleRTInfo.IsStoredInObject or
            not assigned(fLocator) or fLocator.ObjectIsPersistent;
end;

procedure TBoldDirectSingleLinkController.MakeDbCurrent;
begin
  DbFetchOwningMember;
end;

procedure TBoldDirectSingleLinkController.SetOrderNo(NewOrderNo: Integer;
  Mode: TBoldDomainElementProxyMode);
var
  OtherEndController: TBoldAbstractController;
begin
  if (NewOrderNo <> fOrderNo) then
  begin
    fOrderNo := NewOrderNo;
    OtherEndController := GetOtherEndController(fLocator, false);
    if Assigned(OtherEndController) and (OtherEndController is TBoldMultiLinkController) then
      TBoldMultiLinkController(OtherEndController).MarkPossiblyOutOfOrder;
  end;
end;

{ TBoldIndirectSingleLinkController }

procedure TBoldIndirectSingleLinkController.DeleteLink(Mode: TBoldLinkUnlinkMode);
var
  OldLinkObject: TBoldObject;
begin
  if assigned(fLinkObjectLocator) then
  begin
    OldLinkObject := fLinkObjectLocator.EnsuredBoldObject;

    GetLinkObjectOtherLinkController(OldLinkObject).RemoveFromOtherEnd(Mode);
    GetLinkObjectOtherLinkController(OldLinkObject).UnLink(GetLinkObjectOtherLinkController(OldLinkObject).fLocator, blulMarkModified);
    GetLinkObjectOwnLinkController(OldLinkObject).PreChange;
    GetLinkObjectOwnLinkController(OldLinkObject).fLocator := nil;
    OldLinkObject.Delete;
  end;
end;

function TBoldIndirectSingleLinkController.GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[OwningReference.BoldRoleRtInfo.OwnIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectSingleLinkController.GetLocator: TBoldObjectLocator;
begin
  result := fOtherEndLocator;
end;

procedure TBoldIndirectSingleLinkController.linkto(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (Owningmember.BoldPersistenceState = bvpsCurrent));
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'Linkto', ''); // do not localize

  if fLinkObjectLocator <> NewLocator then
  begin
    if assigned(fLinkObjectLocator) then
      DeleteLink(Mode);
    PreChange;
    fLinkObjectLocator := NewLocator;
    fOtherEndLocator := GetLinkObjectOtherLinkController(NewLocator.EnsuredBoldObject).fLocator;
  end;
  Changed(beValueChanged, [fOtherEndLocator]);
  GetLinkObjectRoleController.Changed(beValueChanged, [fLinkObjectLocator]);
  if Mode = blulMarkModified then
    EndModify;
end;

function TBoldIndirectSingleLinkController.NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
var
  LinkObject: TBoldObject;
  LinkClassTypeInfo: TBoldClassTypeinfo;
  OtherEndController: TBoldAbstractController;
begin
  LinkClassTypeInfo := OwningReference.BoldRoleRTInfo.LinkClassTypeInfo;
  LinkObject := TBoldObjectClass(LinkClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(LinkClassTypeInfo, OwningReference.BoldSystem,
    OtherLocator.ObjectIsPersistent and OwningReference.OwningObject.BoldPersistent);
  OtherEndController := GetLinkObjectOtherLinkController(LinkObject).GetOtherEndController(OtherLocator, true); // Ensure other end fetched if forced
  GetLinkObjectOwnLinkController(LinkObject).LinkTo(OwningReference.OwningObject.BoldObjectLocator, true, Mode);
  GetLinkObjectOtherLinkController(LinkObject).LinkTo(OtherLocator, true, Mode);
  if Assigned(OtherEndController) then
    OtherEndController.LinkTo(LinkObject.BoldObjectLocator, false, Mode);
  Result := LinkObject;
end;

procedure TBoldIndirectSingleLinkController.SetFromIds(Id1, Id2: TBoldObjectId; Mode: TBoldDomainElementProxyMode);
var
  NewLinkLocator: TBoldObjectLocator;
  NewOtherEndLocator: TBoldObjectLocator;
  procedure SafeCopyOptimisticValues;
  var
    Value: IBoldObjectIdRefPair;
  begin
    Value := NewValueInOptimisticLocking as IBoldObjectIdRefPair;
    if Assigned(Value) then
      Value.SetFromIds(Id1, Id2);
  end;

  // Adjust NewLinkLocator and  NewOtherEndLocator
  procedure AdjustLocators;
  var
    BoldObject: TBoldObject;
    i: integer;
    BoldLinkClassTypeInfo: TBoldClassTypeInfo;
    IndexOfOwnEnd, IndexOfOtherEnd, EmbeddedIndexOfOwnEnd, EmbeddedIndexOfOtherEnd: integer;
  begin
    IndexOfOwnEnd := OwningReference.BoldRoleRTInfo.OwnIndexInLinkClass;
    IndexOfOtherEnd := OwningReference.BoldRoleRTInfo.OtherIndexInLinkClass;
    if Assigned(NewLinkLocator) then
    begin
      BoldObject := NewLinkLocator.BoldObject;
      if Assigned(BoldObject) then {Object in core}
      begin
        if (BoldObject.BoldExistenceState = besDeleted) then
        begin
          SafeCopyOptimisticValues;
          NewLinkLocator := nil;
          NewOtherEndLocator := nil;
        end
      end
      else {object not loaded, set embedded links}  { TODO : Is this really related to adjust? }
      begin
        if Assigned(NewLinkLocator) then
        begin
          EmbeddedIndexOfOwnEnd := OwningReference.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOwnEnd].EmbeddedLinkIndex;
          EmbeddedIndexOfOtherEnd := OwningReference.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOtherEnd].EmbeddedLinkIndex;
          if (EmbeddedIndexOfOwnEnd <> -1) then
            NewLinkLocator.EmbeddedSingleLinks[EmbeddedIndexOfOwnEnd] := OwningMember.OwningObject.BoldObjectLocator;
          if (EmbeddedIndexOfOtherEnd <> -1) then
            NewLinkLocator.EmbeddedSingleLinks[EmbeddedIndexOfOtherEnd] := NewOtherEndLocator;
        end;
      end;
    end;
    for I := 0 to OwningMember.BoldSystem.DirtyObjects.Count - 1 do
    begin
      BoldLinkClassTypeInfo := OwningReference.BoldRoleRtInfo.LinkClassTypeInfo;
      BoldObject := OwningMember.BoldSystem.DirtyObjects[I];
      if (BoldObject.BoldClassTypeInfo.BoldIsA(BoldLinkClassTypeInfo)) and (BoldObject.BoldExistenceState = besExisting) and
        (((BoldObject.BoldMembers[IndexOfOwnEnd]) as TBoldObjectReference).BoldObject = OwningMember.OwningObject) then
      begin
        SafeCopyOptimisticValues;
        NewLinkLocator := BoldObject.BoldObjectLocator;
        NewOtherEndLocator := (BoldObject.BoldMembers[IndexOfOtherEnd] as TBoldObjectReference).Locator;
     end;
    end;
  end;

begin
  if (mode = bdepPMIn) and (OwningMember.OwningObject.BoldObjectLocator.BoldObjectId.TimeStamp <> BOLDMAXTIMESTAMP) then  // fetching old temporal versi
    mode := bdepContents;
  NewLinkLocator  := LocatorForId(Id1);
  NewOtherEndLocator := LocatorForId(Id2);
  if mode = bdepPMIn then
    AdjustLocators;

  if fLinkObjectLocator <> NewLinkLocator then
  begin
    PreChange;
    fLinkObjectLocator := NewLinkLocator;
    fOtherEndLocator := NewOtherEndLocator;
    Changed(beValueChanged, [NewOtherEndLocator]);
    GetLinkObjectRoleController.Changed(beValueChanged, [NewLinkLocator])
  end;
end;

procedure TBoldIndirectSingleLinkController.SetLocator(NewLocator: TBoldObjectLocator);
begin
  if NewLocator = fLinkObjectLocator then
    exit;

  OwningReference.BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetLocator', ''); // do not localize

    DeleteLink(blulMarkModified);
    PreChange;
    fOtherEndLocator := NewLocator;
    if assigned(NewLocator) then
      fLinkObjectLocator := NewLink(NewLocator, blulMarkModified).BoldObjectLocator
    else
      fLinkObjectLocator := nil;
    Changed(beValueChanged, [NewLocator]);
    GetLinkObjectRoleController. Changed(beValueChanged, [fLinkObjectLocator]);
    EndModify;
    OwningReference.BoldSystem.CommitTransaction;
  except
    OwningReference.BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectSingleLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRefPair;
end;

procedure TBoldIndirectSingleLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (Owningmember.BoldPersistenceState = bvpsCurrent));
  Assert(fLinkObjectLocator = OldLocator);
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'Unlink', ''); // do not localize

  PreChange;
  fLinkObjectLocator := nil;
  fOtherEndLocator := nil;
  Changed(beValueChanged, [nil]);
  GetLinkObjectRoleController.Changed( beValueChanged, [nil]);
  if Mode = blulMarkModified then
    EndModify;
end;

function TBoldIndirectSingleLinkController.GetLinkObjectRoleController: TBoldLinkObjectReferenceController;
begin
  result := ControllerForLinkRole as TBoldLinkObjectReferenceController;
end;

function TBoldIndirectSingleLinkController.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBoldIndirectSingleLinkController_Proxy;
end;

function TBoldIndirectSingleLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdRefPair) then
  begin
    result := ProxyClass.create(self.OwningReference, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdRefPair', [ClassName]); // do not localize
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldIndirectSingleLinkController.MayUpdate: Boolean;
begin
  result := not OwningReference.BoldRoleRTInfo.IsStoredInObject or
            not assigned(fLinkObjectLocator) or fLinkObjectLocator.ObjectIsPersistent;
end;

function TBoldIndirectSingleLinkController.GetLinkObjectOtherLinkController(
  LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[OwningReference.BoldRoleRtInfo.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectSingleLinkController.AssertIntegrity: Boolean;
begin
    if fLinkObjectLocator = nil then
      assert(fOtherEndLocator = nil)
    else
    begin
      assert(fOtherEndLocator <> nil);
      if Assigned(fLinkObjectLocator.BoldObject) then
      begin
        Assert(fLinkObjectLocator.BoldObject.BoldExistenceState = besExisting);
        Assert(GetLinkObjectOwnLinkController(fLinkObjectLocator.BoldObject).fLocator = OwningMember.OwningObject.BoldObjectLocator);
        Assert(GetLinkObjectOtherLinkController(fLinkObjectLocator.BoldObject).fLocator = fOtherEndLocator);
        { TODO : Check included in other end, if loaded. }
      end
      else // link object no loaded
      begin
         { TODO : Check values in locator itself }
         { TODO : Check included in other end, if loaded. }
      end;
    end;
  Result := True;
end;

procedure TBoldIndirectSingleLinkController.MakeDbCurrent;
begin
  DbFetchOwningMember;
end;

{ TBoldIndirectMultiLinkController }

procedure TBoldIndirectMultiLinkController.AddLocator(Locator: TBoldObjectLocator);
var
  LinkObject: TBoldObject;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'AddLocator', ''); // do not localize

    PreChange;
    LinkObject := NewLink(Locator, blulMarkModified);
    LinkLocatorList.Add(LinkObject.BoldObjectLocator);
    ReferredLocatorList.Add(Locator);
    Assert(ReferredLocatorList.Count = LinkLocatorList.Count);
    if OwningObjectList.BoldRoleRtInfo.IsOrdered then
      GetLinkObjectOwnLinkController(LinkObject).SetAndModifyOrderNo(LinkLocatorList.IndexOf(LinkObject.BoldObjectLocator)); // Complexity warning: A loop of adds will take O(n^2)
    Changed(beItemAdded, [Locator]);
    GetLinkObjectListController.Changed(beItemAdded, [LinkObject.BoldObjectLocator]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

constructor TBoldIndirectMultiLinkController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  fLinkLocatorList := TBoldObjectLocatorList.Create;
  FReferredList := TBoldObjectLocatorList.Create;
end;

procedure TBoldIndirectMultiLinkController.DeleteLink(LinkObjectLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
var
  OldLinkObject: TBoldObject;
begin
  OldLinkObject := LinkObjectLocator.EnsuredBoldObject;
  GetLinkObjectOtherLinkController(OldLinkObject).RemoveFromOtherEnd(Mode);
  GetLinkObjectOtherLinkController(OldLinkObject).Unlink(GetLinkObjectOtherLinkController(OldLinkObject).fLocator, Mode);
    GetLinkObjectOwnLinkController(OldLinkObject).PreChange;
  GetLinkObjectOwnLinkController(OldLinkObject).Unlink(OwningMember.OwningObject.BoldObjectLocator, Mode);
  OldLinkObject.Delete;
end;

destructor TBoldIndirectMultiLinkController.Destroy;
begin
  FreeAndNil(fLinkLocatorList);
  FreeAndNil(FReferredList);
  inherited;
end;

procedure TBoldIndirectMultiLinkController.MakeDbCurrent;
begin
  EnsureOrder;
  DbFetchOwningMember;
end;

function TBoldIndirectMultiLinkController.GetCount: Integer;
begin
  Result := LinkLocatorList.Count;
end;

function TBoldIndirectMultiLinkController.GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.OwnIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectMultiLinkController.GetLocator(index: Integer): TBoldObjectLocator;
begin
  EnsureOrder;
  result := ReferredLocatorList[index];
end;

function TBoldIndirectMultiLinkController.GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
begin
  EnsureOrder;
  if not ReferredLocatorList.HasMembersIndex then
  begin
    if assigned(OwningObjectList.BoldRoleRTInfo) and OwningObjectList.BoldRoleRTInfo.isQualified then
    begin
      OwningObjectList.EnsureObjects;
      ReferredLocatorList.InitMembersIndex(OwningObjectList, OwningObjectList.BoldRoleRTInfo.Qualifiers)
    end
    else
      raise EBold.CreateFmt(sRolenotQualified, [ClassName]);
  end;
  result := ReferredLocatorList.GetLocatorByAttributesAndSubscribe(MemberList, Subscriber);
end;

function TBoldIndirectMultiLinkController.IncludesLocator(Locator: TBoldObjectLocator): Boolean;
begin
  result := ReferredLocatorList.LocatorInList[Locator];
end;

function TBoldIndirectMultiLinkController.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  EnsureOrder;
  Result := ReferredLocatorList.IndexOf(Locator);
end;

procedure TBoldIndirectMultiLinkController.InsertLocator(index: Integer; Locator: TBoldObjectLocator);
var
  NewLinkLocator: TBoldObjectLocator;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'InsertLocator', ''); // do not localize

    PreChange;
    NewLinkLocator := NewLink(Locator, blulMarkModified).BoldObjectLocator;
    LinkLocatorList.Insert(Index, NewLinkLocator);
    ReferredLocatorList.Insert(Index, Locator);
    ReOrder;
    Changed(beItemAdded, [Locator]);
    GetLinkObjectListController.Changed(beItemAdded, [NewLinkLocator]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldIndirectMultiLinkController.linkto(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
var
  NewReferredLocator: TBoldObjectLocator;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent));
  Assert(not LinkLocatorList.LocatorInList[NewLocator], 'locator already in list');
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Linkto', ''); // do not localize

  PreChange;
  NewReferredLocator := GetLinkObjectOtherLinkController(NewLocator.EnsuredBoldObject).fLocator;
  LinkLocatorList.Add(NewLocator);
  ReferredLocatorList.Add(NewReferredLocator);
  if updateOrderNo and OwningObjectList.BoldRoleRtInfo.IsOrdered then
    GetLinkObjectOwnLinkController(NewLocator.BoldObject).SetAndModifyOrderNo(LinkLocatorList.IndexOf(NewLocator)); // Complexity warning: A loop of adds will take O(n^2)
  if Mode = blulMarkAdjusted then
    OwningObjectList.Adjusted := True;
  Changed(beItemAdded, [NewReferredLocator]);
  GetLinkObjectListController.Changed(beItemAdded, [NewLocator]);
  if Mode = blulMarkModified then
    EndModify;
end;

procedure TBoldIndirectMultiLinkController.Move(CurrentIndex, NewIndex: Integer);
begin
  EnsureOrder;
  if not OwningObjectList.BoldRoleRtInfo.IsOrdered then
    exit;

  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Move', ''); // do not localize

    PreChange;
    LinkLocatorList.Move(CurrentIndex, NewIndex);
    ReferredLocatorList.Move(CurrentIndex, NewIndex);
    ReOrder;
    Changed(beOrderChanged, []);
    GetLinkObjectListController. Changed(beOrderChanged, []);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectMultiLinkController.NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
var
  LinkObject: TBoldObject;
  LinkClassTypeInfo: TBoldClassTypeInfo;
  OtherEndController: TBoldAbstractController;
begin
  LinkClassTypeInfo := OwningObjectList.BoldRoleRTInfo.LinkClassTypeInfo;
  LinkObject := TBoldObjectClass(LinkClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(LinkClassTypeInfo, OwningMember.BoldSystem,
    OtherLocator.ObjectIsPersistent and OwningMember.OwningObject.BoldPersistent);
  OtherEndController := GetLinkObjectOtherLinkController(LinkObject).GetOtherEndController(OtherLocator, true); // Ensure other end fetched if forced
  GetLinkObjectOwnLinkController(LinkObject).LinkTo(OwningMember.OwningObject.BoldObjectLocator, true, Mode);
  GetLinkObjectOtherLinkController(LinkObject).LinkTo(OtherLocator, true, Mode);
  if Assigned(OtherEndController) then
    OtherEndController.LinkTo(LinkObject.BoldObjectLocator, false, Mode);
  Result := LinkObject;
end;

procedure TBoldIndirectMultiLinkController.RemoveByIndex(index: Integer);
var
  Locator: TBoldObjectLocator;
  LinkLocator: TBoldObjectLocator;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', ''); // do not localize

    PreChange;
    Locator := ReferredLocatorList[index];
    LinkLocator := LinkLocatorList[index];
    BoldSystem.DelayObjectDestruction;
    try
      DeleteLink(LinkLocator, blulMarkModified);  // This will remove from other end but not from own
      LinkLocatorList.RemoveByIndex(index);
      ReferredLocatorList.RemoveByIndex(index);
    finally
      BoldSystem.AllowObjectDestruction;
    end;
    ReOrder;
    Changed(beItemDeleted, [Locator]);
    GetLinkObjectListController.Changed(beItemDeleted, [LinkLocator]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldIndirectMultiLinkController.ReOrder;
var
{$IFOPT C+} // if Assertions on
  index: Integer;
{$ENDIF}
  I: Integer;
  Locator: TBoldObjectLocator;
begin
  if OwningObjectList.BoldRoleRtInfo.IsOrdered then
  begin
{$IFOPT C+} // if Assertions on
    index := OwningObjectList.BoldRoleRtInfo.IndexOfOtherEnd;
    Assert(index <> -1);
{$ENDIF}
    for I := 0 to LinkLocatorList.Count - 1 do
    begin
      Locator := LinkLocatorList.Locators[I];
      Locator.EnsureBoldObject; // Note, can give fetch during fetch, save till all fetched
    end;
    for I := 0 to LinkLocatorList.Count - 1 do
    begin
      Locator := LinkLocatorList.Locators[I];
      GetLinkObjectOwnLinkController(Locator.BoldObject).SetAndModifyOrderNo(I);
    end;
  end;
end;
procedure TBoldIndirectMultiLinkController.SetFromIDLists(ListOfLinkObjects, ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
var
  NewListOfLinkObjects: TBoldObjectIdList;
  NewListOfOtherEnd: TBoldObjectIdList;

  procedure SafeCopyOptimisticValues;
  var
    Value: IBoldObjectIdListRefPair;
  begin
    Value := NewValueInOptimisticLocking as IBoldObjectIdListRefPair;
    if Assigned(Value) then
      Value.SetFromIdLists(ListOfLInkObjects, ListOfOtherEnd);
  end;

  // Adjust  NewListOfLinkObjects,NewListOfOtherEnd. Return True if adjusted
  function AdjustLists: Boolean;
  var
    I: integer;
    AObject: TBoldObject;
    BoldLinkClassTypeInfo: TBoldClassTypeInfo;
    IndexOfOtherEnd, IndexOfOwnEnd,
    EmbeddedIndexOfOtherEnd, EmbeddedIndexOfOwnEnd: Integer;
    LinkObjectLocator: TBoldObjectLocator;
  begin
    Result := False;
    BoldLinkClassTypeInfo := OwningObjectList.BoldRoleRtInfo.LinkClassTypeInfo;
    IndexOfOwnEnd := OwningObjectList.BoldRoleRTInfo.OwnIndexInLinkClass;
    IndexOfOtherEnd := OwningObjectList.BoldRoleRTInfo.OtherIndexInLinkClass;
    {Adjust list}
    for I := NewListOfLinkObjects.Count - 1 downto 0 do
    begin
      AObject := BoldSystem.Locators.ObjectByID[NewListOfLinkObjects[I]];
      if Assigned(AObject) then {Object in core}
      begin
        if (AObject.BoldExistenceState = besDeleted) then
        begin
          if not Result then
          begin
            SafeCopyOptimisticValues;
            Result := true;
          end;
          NewListOfLinkObjects.RemoveByIndex(I);
          NewListOfOtherEnd.RemoveByIndex(I);
        end;
      end
      else {object not loaded, set embedded links}  { TODO : Is this really related to adjust? }
      begin
        LinkObjectLocator := AssertedLocatorForId(NewListOfLinkObjects[i]);
        EmbeddedIndexOfOwnEnd := OwningObjectList.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOwnEnd].EmbeddedLinkIndex;
        EmbeddedIndexOfOtherEnd := OwningObjectList.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOtherEnd].EmbeddedLinkIndex;
        if (EmbeddedIndexOfOwnEnd <> -1) then
          LinkObjectLocator.EmbeddedSingleLinks[EmbeddedIndexOfOwnEnd] := OwningList.OwningObject.BoldObjectLocator;
        if (EmbeddedIndexOfOtherEnd <> -1) then
          LinkObjectLocator.EmbeddedSingleLinks[EmbeddedIndexOfOtherEnd] := AssertedLocatorForId(NewListOfOtherEnd[i]);
      end;
    end;
    for I := 0 to BoldSystem.DirtyObjects.Count - 1 do
    begin
      AObject := BoldSystem.DirtyObjects[I];
      if (AObject.BoldClassTypeInfo.BoldIsA(BoldLinkClassTypeInfo)) and (aObject.BoldExistenceState = besExisting) and
        (((AObject.BoldMembers[IndexOfOwnEnd]) as TBoldObjectReference).BoldObject = OwningList.OwningObject) then
      begin
        if not Result then
        begin
          SafeCopyOptimisticValues;
          Result := true;
        end;
        NewListOfLinkObjects.Add(AObject.BoldObjectLocator.BoldObjectID);
        NewListOfOtherEnd.Add((AObject.BoldMembers[IndexOfOtherEnd] as TBoldObjectReference).Locator.BoldObjectID);
      end;
    end;
  end;

var
  PreChangeCalled: Boolean;

  procedure PreChangeIfNeeded;
  begin
    if not PreChangeCalled then
    begin
      PreChange;
//      GetLinkObjectListController.PreChange; No need to save?
      PreChangeCalled := True;
    end;
  end;

var
  I: Integer;
  OldLocator, NewLocator: TBoldObjectLocator;
  PreserveOrder: Boolean;
  WasAdjusted: Boolean;
  ObjectLocator: TBoldObjectLocator;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(NewListOfLinkObjects, NewListOfOtherEnd);
  if (mode = bdepPMIn) and (OwningMember.OwningObject.BoldObjectLocator.BoldObjectId.TimeStamp <> BOLDMAXTIMESTAMP) then  // fetching old temporal versi
    mode := bdepContents;
  if assigned(ListOfLinkObjects) then
  begin
    Assert(Assigned(ListOfOtherEnd));
    NewListOfLinkObjects := ListOfLinkObjects.Clone;
    NewListOfOtherEnd := ListOfOtherEnd.Clone
  end
  else
  begin
    NewListOfLinkObjects := TBoldObjectIdList.create;
    NewListOfOtherEnd := TBoldObjectIdList.create;
  end;
  if mode = bdepPMIn then
    WasAdjusted := AdjustLists
  else
    WasAdjusted := False;
  if Mode = bdepPMIn then
     ClearNoLongerReferring(newListOfOtherEnd);
  PreserveOrder := (mode = bdepContents) or ((mode = bdepPMIn) and OwningObjectList.BoldRoleRTInfo.IsOrdered and not WasAdjusted);

  {we now have a list with the right objects}

  for I := GetCount - 1 downto 0 do
    if not NewListOfLinkObjects.IdInList[LinkLocatorList[I].BoldObjectID] then
    begin
      PreChangeIfNeeded;
      OldLocator := LinkLocatorList[i];
      LinkLocatorList.RemoveByIndex(I);
      Changed(beItemDeleted, [OldLocator]);
      OldLocator := ReferredLocatorList[i];
      ReferredLocatorList.RemoveByIndex(I);
      GetLinkObjectListController.Changed(beItemDeleted, [OldLocator]);
    end;

  if PreserveOrder then
  begin
    for I := 0 to NewListOfOtherEnd.Count - 1 do
    begin
      ObjectLocator := AssertedLocatorForId(NewListOfOtherEnd[i]);
      if i >= ReferredLocatorList.Count then
      begin
        PreChangeIfNeeded;
        NewLocator :=  AssertedLocatorForId(NewListOfLinkObjects[I]);
        LinkLocatorList.Add(NewLocator);
        Changed(beItemAdded, [NewLocator]);
        ReferredLocatorList.Add(ObjectLocator);
        GetLinkObjectListController.Changed(beItemDeleted, [ObjectLocator]);
      end
      else if ObjectLocator = ReferredLocatorList[i] then
        // All in order, do nothing
      else if ReferredLocatorList.IndexOf(ObjectLocator) <> -1 then  // locator in list, but at wrong place
      begin
        PreChangeIfNeeded;
        LinkLocatorList.Move(ReferredLocatorList.IndexOf(ObjectLocator), I);
        ReferredLocatorList.Move(ReferredLocatorList.IndexOf(ObjectLocator), I);
        Changed(beOrderChanged, []);
        GetLinkObjectListController.Changed(beOrderChanged, []);
      end
      else
      begin  // locator not in list, insert it,
        PreChangeIfNeeded;
        NewLocator :=  AssertedLocatorForId(NewListOfLinkObjects[I]);
        LinkLocatorList.Insert(I, NewLocator);
        ReferredLocatorList.Insert(I, ObjectLocator);
        Changed(beItemAdded, [NewLocator]);
      end;
    end;
  end
  else
  begin
    for I := 0 to NewListOfLinkObjects.Count - 1 do
    begin
      ObjectLocator := AssertedLocatorForId(NewListOfOtherEnd[i]);
      // if the database contains linkobject duplicates, the lists could get out of sync, we must avoid
      // adding two linkobjects for the same object
      if not ReferredLocatorList.LocatorInList[ObjectLocator] then
      begin
        PreChangeIfNeeded;
        NewLocator :=  AssertedLocatorForId(NewListOfLinkObjects[I]);
        LinkLocatorList.Add(NewLocator);
        Changed(beItemAdded, [NewLocator]);
        ReferredLocatorList.Add(ObjectLocator);
        GetLinkObjectListController.Changed(beItemDeleted, [ObjectLocator]);
      end;
    end;
  end;
end;

procedure TBoldIndirectMultiLinkController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
var
  OldLinkLocator: TBoldObjectLocator;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'SetLocator', ''); // do not localize
    OldLinkLocator := LinkLocatorList[index];
    PreChange;
    LinkLocatorList[index] := NewLink(Locator, blulMarkModified).BoldObjectLocator;
    ReferredLocatorList[index] := Locator;
    DeleteLink(OldLinkLocator, blulMarkModified);
    Changed(beItemReplaced, [Locator, Index]);
    GetLinkObjectListController.Changed(beItemReplaced, [OldLinkLocator, Index]);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectMultiLinkController.GetStreamName: String;
begin
  result := BoldContentName_ObjectIdListRefPair;
end;

procedure TBoldIndirectMultiLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
var
  OldIndex: Integer;
  OldLinkLocator: TBoldObjectLocator;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent));
  Assert(LinkLocatorList.LocatorInList[OldLocator]);
  BoldClearLastFailure;
  if Mode = blulMarkModified then
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Unlink', ''); // do not localize

  PreChange;
  OldIndex := LinkLocatorList.IndexOf(OldLocator);
  LinkLocatorList.Remove(OldLocator);
  OldLinkLocator := ReferredLocatorList[oldIndex];
  ReferredLocatorList.RemoveByIndex(OldIndex);
  if Mode = blulMarkAdjusted  then
    OwningObjectList.Adjusted := True;
  Changed(beItemDeleted, [OldLocator]);
  GetLinkObjectListController.Changed(beItemDeleted, [OldLinkLocator]);
  if Mode = blulMarkModified then
    EndModify;
end;

function TBoldIndirectMultiLinkController.GetLinkObjectListController: TBoldLinkObjectListController;
begin
  result := ControllerForLinkMember as TBoldLinkObjectListController;
end;

function TBoldIndirectMultiLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRefPair) then
  begin
    result := ProxyClass.create(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRefPair', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldIndirectMultiLinkController.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBoldIndirectMultiLinkController_Proxy;
end;

procedure TBoldIndirectMultiLinkController.FreeContent;
begin
  LinkLocatorList.Clear;
  ReferredLocatorList.Clear;
end;

procedure TBoldIndirectMultiLinkController.PrepareClear;
var
  LinkList: TBoldObjectList;
begin
  LinkList := OwningList.OwningObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.IndexOfLinkObjectRole] as TBoldObjectLIst;
  LinkLIst.EnsureObjects;
end;

procedure TBoldIndirectMultiLinkController.Resort;
begin
  BoldSort(0, LinkLocatorList.Count - 1, CompareOrderNo, Exchange);
  Changed(beOrderChanged, []);
  Assert(IsInOrder);
end;

function TBoldIndirectMultiLinkController.IsInOrder: Boolean;
var
  i: integer;
  CurrentObj, PreviousObj: TBoldObject;
begin
  Result := True;
  if OwningObjectList.BoldRoleRTInfo.IsOrdered then
  begin
    i := 0;
    CurrentObj := nil;
    // Only compare those objects that are loaded.
    while result and (i < LinkLocatorList.Count - 1) do
    begin
      if Assigned(LinkLocatorList[i].BoldObject) then
      begin
        PreviousObj := CurrentObj;
        CurrentObj := LinkLocatorList[i].BoldObject;

        if Assigned(PreviousObj) then
          Result := GetLinkObjectOwnLinkController(PreviousObj).FOrderNo <= GetLinkObjectOwnLinkController(CurrentObj).FOrderNo;
      end;
      Inc(i);
    end;
  end;
end;

function TBoldIndirectMultiLinkController.CompareOrderNo(Index1, Index2: integer): integer;
var
  OrderNo1, OrderNo2: integer;
begin
  if OwningObjectList.BoldRoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole then
  begin
    OrderNo1 := GetLinkObjectOwnLinkController(LinkLocatorList[Index1].BoldObject).FOrderNo;
    OrderNo2 := GetLinkObjectOwnLinkController(LinkLocatorList[Index2].BoldObject).FOrderNo;
    Result := (OrderNo1 - OrderNo2);
  end
  else
    raise EBold.Create(sOtherEndMustBeSingle);
end;

procedure TBoldIndirectMultiLinkController.Exchange(Index1, Index2: integer);
begin
  LinkLocatorList.Exchange(Index1, Index2);
  ReferredLocatorList.Exchange(Index1, Index2);
end;

function TBoldIndirectMultiLinkController.GetLinkObjectOtherLinkController(
  LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectMultiLinkController.AssertIntegrity: Boolean;
var
  i: integer;
begin
  Assert(LinkLocatorList.Count = ReferredLocatorList.Count);
  for i := 0 to LinkLocatorList.Count - 1 do
  begin
    assert(LinkLocatorList[i] <> nil);
    assert(ReferredLocatorList[i] <> nil);
    if Assigned(LinkLocatorList[i].BoldObject) then
    begin
      Assert(LinkLocatorList[i].BoldObject.BoldExistenceState = besExisting);
      Assert(GetLinkObjectOwnLinkController(LinkLocatorList[i].BoldObject).fLocator = OwningObjectList.OwningObject.BoldObjectLocator);
      Assert(GetLinkObjectOtherLinkController(LinkLocatorList[i].BoldObject).fLocator = ReferredLocatorList[i]);
       { TODO : Check included in other end, if loaded. }
    end
    else // link object no loaded
    begin
       { TODO : Check values in locator itself }
       { TODO : Check included in other end, if loaded. }
    end;
  end;
  Result := True;
end;

procedure TBoldIndirectMultiLinkController.ClearNoLongerReferring(
  NewList: TBoldObjectIdList);
begin
{ TODO : What do we want to do here. Free actual link objects that are pointing at us? }
end;

procedure TBoldIndirectMultiLinkController.Clear;
var
  Locator: TBoldObjectLocator;
  LinkLocator: TBoldObjectLocator;
  ix: integer;
begin
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', ''); // do not localize

    PreChange;
    BoldSystem.DelayObjectDestruction;
    try
      while count > 0 do
      begin
        ix := Count-1;
        Locator := ReferredLocatorList[ix];
        LinkLocator := LinkLocatorList[ix];
        DeleteLink(LinkLocator, blulMarkModified);  // This will remove from other end but not from own
        LinkLocatorList.RemoveByIndex(ix);
        ReferredLocatorList.RemoveByIndex(ix);
        Changed(beItemDeleted, [Locator]);
        GetLinkObjectListController.Changed(beItemDeleted, [LinkLocator]);
      end;
    finally
      BoldSystem.AllowObjectDestruction;
    end;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

{ TBoldMultiLinkController }

function TBoldIndirectMultiLinkController.ControllerForLinkMember: TBoldAbstractObjectListController;
var
  LinkMember: TBoldMember;
begin
  LinkMember := OwningList.OwningObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.Index+1];
  result := GetControllerForMember(LinkMember) as TBoldAbstractObjectListController;
end;

function TBoldMultiLinkController.CreateNew: TBoldElement;
begin
  result := OwningList.BoldSystem.CreateNewObjectByExpressionName(OwningObjectList.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName);
end;

procedure TBoldMultiLinkController.EnsureOrder;
begin
  if (fMayBeOutOfOrder) then
  begin
    if not IsInOrder then
      Resort;
    fMayBeOutOfOrder := false;
  end;
end;

function TBoldMultiLinkController.GetCanCreateNew: Boolean;
begin
  result := not OwningObjectList.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.IsAbstract;
end;

procedure TBoldDirectMultiLinkController.SingleLinkUnlink(
  Locator: TBoldObjectLocator;
  OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
var
  aObject: TBoldObject;
  EmbeddedIndex: integer;
begin
  aObject := Locator.BoldObject;
  if Assigned(AObject) then
    GetOtherEndController(Locator).Unlink(OldLocator, Mode)
  else  // member for other end not instantiated.
  begin
    EmbeddedIndex := OwningObjectList.BoldRoleRtInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
    if (EmbeddedIndex <> -1) then
      if Locator.EmbeddedSingleLinks[EmbeddedIndex] <> nil then
      begin
        Assert(Locator.EmbeddedSingleLinks[EmbeddedIndex] = oldLocator);
        Locator.EmbeddedSingleLinks[EmbeddedIndex] := nil;
      end;
  end;
end;

function TBoldDirectMultiLinkController.GetOtherEndController(Locator: TBoldObjectLocator): TBoldDirectSingleLinkController;
begin
  result := GetControllerForMember(Locator.EnsuredBoldObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.IndexOfOtherEnd]) as TBoldDirectSingleLinkController;
end;

procedure TBoldDirectMultiLinkController.SingleLinkLinkTo(Locator,
  NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
var
  aObject: TBoldObject;
  EmbeddedIndex: integer;
begin
  aObject := Locator.BoldObject;
  if Assigned(AObject) then
     GetOtherEndController(Locator).LinkTo(NewLocator, UpdateOrderNo, Mode)
  else  // member for other end not instantiated.
  begin
    EmbeddedIndex := OwningObjectList.BoldRoleRtInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
    Assert(EmbeddedIndex <> -1);
    Assert((Locator.EmbeddedSingleLinks[EmbeddedIndex] = nil) or (Locator.EmbeddedSingleLinks[EmbeddedIndex] = NewLocator));
    Locator.EmbeddedSingleLinks[EmbeddedIndex] := NewLocator;
  end;
end;

procedure TBoldMultiLinkController.MarkPossiblyOutOfOrder;
begin
  if OwningObjectList.BoldRoleRTInfo.IsOrdered then
    fMayBeOutOfOrder := True;
end;

{ TBoldLinkObjectSingleLinkController }

function TBoldLinkObjectSingleLinkController.OtherInnerLinkController: TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(OwningReference.OwningObject.BoldMembers[
              OwningReference.BoldRoleRTInfo.RoleRTInfoOfOtherEnd.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

procedure TBoldLinkObjectSingleLinkController.SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode);
var
  OtherInnerLink: TBoldLinkObjectSingleLinkController;
  OtherOuterLink: TBoldAbstractController;
  OuterMulti: TBoldIndirectMultiLinkController;
  OuterSingle: TBoldIndirectSingleLinkController;
  index: Integer;
begin
  inherited;
  // Note! This code adjust for a possible previous "flaw". When fetching the
  // first of the two inner single links, its "other end" cannot be set up correctly
  // since the value of other inner single link is not known. This code adjust
  // that when the other inner single link is fetched. Note that this requires
  // that both inner single links are fetched in the same operation.
  OtherInnerLink := OtherInnerLinkController;
  if assigned(OtherInnerLink.fLocator) then
  begin
    OtherOuterLink := OtherInnerLink.GetOtherEndController(OtherInnerLink.fLocator, false);
    // will not be assigned if outerlink is bvpsInvalid
    if assigned(OtherOuterLink) then
    begin
      // they have no suitable common superclass...
      if OtherOuterLink is TBoldIndirectMultiLinkController then
      begin
        OuterMulti := OtherOuterLink as TBoldIndirectMultiLinkController;
        index := OuterMulti.fLinkLocatorList.IndexOf(OwningReference.OwningObject.BoldObjectLocator);
        if (index >= 0) and (OuterMulti.fReferredList[index] = nil) then
          OuterMulti.fReferredList[index] := fLocator;
      end
      else if OtherOuterLink is TBoldIndirectSingleLinkController then
      begin
        OuterSingle := OtherOuterLink as TBoldIndirectSingleLinkController;
        if (OuterSingle.fLinkObjectLocator = OwningReference.OwningObject.BoldObjectLocator) and
          (OuterSingle.fOtherEndLocator = nil) then
          OuterSingle.fOtherEndLocator := fLocator;
      end else
        raise EBold.CreateFmt(sUnexpectedControllerType, [classname, OtherOuterLink.classname]);
    end;
  end;
end;

procedure TBoldLinkObjectSingleLinkController.SetLocator(NewLocator: TBoldObjectLocator);
begin
  if assigned(NewLocator) then
    raise EBold.CreateFmt(sCannotChangeLinkObjectSingleLink, [ClassName]);
  inherited;
end;

{ TBoldLinkObjectListController }

procedure TBoldLinkObjectListController.AddLocator(Locator: TBoldObjectLocator);
begin
  raise EBold.CreateFmt(sInvalidForListOfLinkObjects, [ClassName, 'AddLocator']); // do not localize
end;

procedure TBoldLinkObjectListController.MakeDbCurrent;
begin
  GetMainList.EnsureContentsCurrent;
  GetObjectList.BoldPersistenceState := bvpsCurrent;
end;

function TBoldLinkObjectListController.GetCount: Integer;
begin
  result := LocatorList.Count;
end;

function TBoldLinkObjectListController.GetLocator(index: Integer): TBoldObjectLocator;
begin
  result := LocatorList.Locators[index];
end;

function TBoldLinkObjectListController.GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt('%s.GetLocatorByQualifiersAndSubscribe', [ClassName]); // do not localize
end;

function TBoldLinkObjectListController.GetLocatorList: TBoldObjectLocatorList;
begin
  result := GetMainListController.LinkLocatorList;
end;

function TBoldLinkObjectListController.GetMainListController: TBOldIndirectMultiLinkController;
var
  MainMember: TBoldMember;
begin
  MainMember := OwningList.OwningObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.IndexOfMainRole];
  result := GetControllerForMember(MainMember) as TBOldIndirectMultiLinkController;
end;

function TBoldLinkObjectListController.IncludesLocator(Locator: TBoldObjectLocator): Boolean;
begin
  result := (LocatorList.IndexOf(Locator) <> -1)
end;

function TBoldLinkObjectListController.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  result := LocatorList.IndexOf(Locator);
end;

procedure TBoldLinkObjectListController.InsertLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  raise EBold.CreateFmt(sInvalidForListOfLinkObjects, [ClassName, 'InsertLocator']); // do not localize
end;

procedure TBoldLinkObjectListController.Move(CurrentIndex, NewIndex: Integer);
begin
  raise EBold.CreateFmt(sInvalidForListOfLinkObjects, [ClassName, 'Move']); // do not localize
end;

procedure TBoldLinkObjectListController.RemoveByIndex(index: Integer);
begin
  raise EBold.CreateFmt(sInvalidForListOfLinkObjects, [ClassName, 'RemoveByIndex']); // do not localize
end;

procedure TBoldLinkObjectListController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  raise EBold.CreateFmt(sInvalidForListOfLinkObjects, [ClassName, 'SetLocator']); // do not localize
end;

function TBoldLinkObjectListController.GetStreamName: string;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [Classname, 'GetStreamName']); // do not localize
end;

function TBoldLinkObjectListController.GetMainList: TBoldObjectList;
begin
  result := OwningList.OwningObject.BoldMembers[OwningObjectList.BoldRoleRtInfo.IndexOfMainRole] as TBoldObjectList;
end;

function TBoldLinkObjectListController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldValue) or IsEqualGuid(IID, IBoldStreamable) then
  begin
    Pointer(Obj) := nil;
    result := true;
  end
  else
    result := false;
end;

function TBoldLinkObjectListController.ProxyClass: TBoldMember_ProxyClass;
begin
  raise EBoldInternal.Create('Can''t access Link Object List directly');
end;

{ TBoldLinkObjectReferenceController }

function TBoldLinkObjectReferenceController.GetLocator: TBoldObjectLocator;
begin
  result := (ControllerForMainRole as TBoldIndirectSingleLinkController).fLinkObjectLocator;
end;

function TBoldLinkObjectReferenceController.GetStreamName: string;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [Classname, 'GetStreamName']); // do not localize
end;

procedure TBoldLinkObjectReferenceController.MakeDbCurrent;
begin
  ControllerForMainRole.OwningReference.EnsureContentsCurrent;
  OwningReference.BoldPersistenceState := bvpsCurrent;
end;

function TBoldLinkObjectReferenceController.ProxyClass: TBoldMember_ProxyClass;
begin
  raise EBoldInternal.Create('Can''t access objectreference directly');
end;

function TBoldLinkObjectReferenceController.ProxyInterface(
  const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldValue) or IsEqualGuid(IID, IBoldStreamable) then
  begin
    Pointer(Obj) := nil;
    result := true;
  end
  else
    result := false;
end;

procedure TBoldLinkObjectReferenceController.SetLocator(NewLocator: TBoldObjectLocator);
begin
  raise EBold.CreateFmt(sCannotSetLinkObjectReference, [Classname]);
end;

end.
