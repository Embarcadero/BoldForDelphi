{ Global compiler directives }
{$include bold.inc}

unit BoldLinks;

interface

uses
  BoldSystem,
  BoldObjectSpaceLists,
  BoldDomainElement,
  BoldId,
  BoldElements,
  BoldSubscription,
  BoldFreeStandingValues;

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
    fLocator: TBoldObjectLocator;
    procedure AddToOtherEnd(Mode: TBoldLinkUnlinkMode);
    procedure RemoveFromOtherEnd(Mode: TBoldLinkUnlinkMode);
    procedure InternalSetLocator(NewLocator: TBoldObjectLocator);
  protected
    function MayUpdate: Boolean; override;
    procedure PreDiscard; override;
    procedure SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode); virtual;
    function GetStreamName: String; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure SetOrderNo(NewORderNo: Integer; Mode: TBoldDomainElementProxyMode); virtual; abstract;
    function GetOrderNo: Integer; virtual; abstract;
  public
    procedure MakeDbCurrent; override;
    function GetOtherEndController(aLocator: TBoldObjectLocator; AllowForceOtherEnd: Boolean): TBoldAbstractController;
    procedure SetAndModifyOrderNo(index: Integer); virtual;abstract;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function GetLocator: TBoldObjectLocator; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
    property OrderNo: Integer read GetOrderNo;
  end;

  TBoldOrderedDirectSingleLinkController  = class(TBoldDirectSingleLinkController)
  private
    fOrderno: Integer;
  public
    procedure SetAndModifyOrderNo(index: Integer); override;
    procedure SetOrderNo(NewORderNo: Integer; Mode: TBoldDomainElementProxyMode); override;
    function GetOrderNo: Integer; override;
  end;

  TBoldUnOrderedDirectSingleLinkController  = class(TBoldDirectSingleLinkController)
    procedure SetAndModifyOrderNo(index: Integer); override;
    procedure SetOrderNo(NewOrderNo: Integer; Mode: TBoldDomainElementProxyMode); override;
    function GetOrderNo: Integer; override;
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
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    procedure MakeDbCurrent; override;
    function AssertIntegrity: Boolean; override;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); override;
    procedure LinkTo(NewLocator: TBoldObjectLocator; UpdateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); override;
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
    procedure Resort; virtual; abstract;
  public
    procedure MarkPossiblyOutOfOrder;
    procedure EnsureOrder;
    procedure DoEnsureOrder;
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
    procedure SingleLinkLinkTo(Locator: TBoldObjectLocator; NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode; aOrderNo: integer = -1);
    procedure SetFromIdList(ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
    function GetStreamName: String; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure PrepareClear; override;
    function CompareOrderNo(Index1, Index2: integer): integer;
    procedure Exchange(Index1, Index2: integer);
    function IsInOrder: Boolean; override;
    procedure ClearNoLongerReferring(NewList: TBoldObjectIdList);
    procedure Resort; override;
  public
    constructor Create(OwningList: TBoldObjectList); reintroduce;
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
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure PrepareClear; override;
    function CompareOrderNo(Index1, Index2:integer): integer;
    procedure Exchange(Index1, Index2: integer);
    function IsInOrder: Boolean; override;
    procedure Resort; override;
  public
    constructor Create(OwningList: TBoldObjectList); reintroduce;
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
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    function AssertIntegrity: Boolean; override;
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
  TBoldLinkObjectSingleLinkController = class(TBoldOrderedDirectSingleLinkController)
  private
    function OtherInnerLinkController: TBoldLinkObjectSingleLinkController;
  protected
    procedure SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode); override;
  public
    function AssertIntegrity: Boolean; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
  end;

  { TBoldLinkObjectReferenceController }
  TBoldLinkObjectReferenceController = class(TBoldAbstractObjectReferenceController)
  protected
    function GetStreamName: string; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    function AssertIntegrity: Boolean; override;
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
  TypInfo,

  BoldCoreConsts,
  BoldGuard,
  BoldStreams,
  BoldValueSpaceInterfaces,
  BoldDefaultStreamNames,
  BoldSorter,
  BoldDefs,
  BoldValueInterfaces,
  BoldIndexableList,
  BoldSystemRT,
  BoldMath,
  BoldObjectListControllers,
  BoldLogHandler;

{---Proxies---}

{ TBoldDirectSingleLinkController_Proxy }
type
  TBoldDirectSingleLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdRef)
  private
    class var fLastUsed: array[TBoldDomainElementProxyMode] of TBoldMember_Proxy;
    class var fLastUsedAsInterface: array[TBoldDomainElementProxyMode] of IBoldValue;
    function GetDirectSingleLinkController: TBoldDirectSingleLinkController;
    procedure SetFromId(Id: TBoldObjectId; Adopt: Boolean);
    function GetId: TBoldObjectID;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    property DirectSingleLInkController: TBoldDirectSingleLinkController read GetDirectSingleLinkController;
  end;

  { TBoldIndirectSingleLinkController_Proxy }
  TBoldIndirectSingleLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdRefPair)
  private
    function GetInDirectSingleLinkController: TBoldInDirectSingleLinkController;
    procedure SetFromIds(Id1, Id2: TBoldObjectId);
    function GetId1: TBoldObjectID;
    function GetId2: TBoldObjectID;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    property InDirectSingleLInkController: TBoldInDirectSingleLinkController read GetInDirectSingleLinkController;
  end;

  { TBoldDirectMultiLinkController_Proxy }
  TBoldDirectMultiLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRef)
  private
    class var fLastUsed: array[TBoldDomainElementProxyMode] of TBoldMember_Proxy;
    class var fLastUsedAsInterface: array[TBoldDomainElementProxyMode] of IBoldValue;
    function GetDirectMultiLinkController: TBoldDirectMultiLinkController;
    procedure SetFromIdList(IdLIst: TBoldObjectIdList);
    procedure SetList(IdList: TBoldObjectIdList);
    function GetIdList(Index: Integer): TBoldObjectID;
    function GetCount: integer;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    property DirectMultiLinkController: TBoldDirectMultiLinkController read GetDirectMultiLinkController;
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  end;

  { TBoldInDirectMultiLinkController_Proxy }
  TBoldInDirectMultiLinkController_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRefPair)
  private
    function GetInDirectMultiLinkController: TBoldInDirectMultiLinkController;
    function GetIdList1(Index: Integer): TBoldObjectID;
    function GetIdList2(Index: Integer): TBoldObjectID;
    procedure SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
    function GetCount: integer;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    property InDirectMultiLinkController: TBoldInDirectMultiLinkController read GetInDirectMultiLinkController;
  end;

{ TBoldDirectSingleLinkController_Proxy }

procedure TBoldDirectSingleLinkController_Proxy.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdRef;
  ObjRef: TBoldObjectReference;
begin
  if (not (Assigned(Source) and (source.QueryInterface(IBoldObjectIDRef, S) = S_OK))) then
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize

  SetFromId(s.Id, false);
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

class function TBoldDirectSingleLinkController_Proxy.MakeProxy(
  ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := fLastUsed[Mode];
  // Reuse proxy if we hold only reference
  if Assigned(Result) and (Result.RefCount =1) then
  begin
    Result.Retarget(ProxedMember, Mode);
  end
  else
  begin
    Result := Create(ProxedMember, Mode);
    fLastUsed[Mode] := Result;
    fLastUsedAsInterface[Mode] := Result;  // Inc refcount
  end;
end;

procedure TBoldDirectSingleLinkController_Proxy.SetFromId(Id: TBoldObjectId; adopt: Boolean);
begin
  if Mode in [bdepPMIn, bdepContents, bdepUndo, bdepInternalInitialize] then
    DirectSingleLInkController.SetFromId(Id, Mode)
  else
    UnsupportedMode(Mode, 'SetFromId');
  if Adopt then
    Id.Free;
end;

procedure TBoldDirectSingleLinkController_Proxy.SetOrderNo(NewOrder: Integer);
begin
  if Mode in [bdepPMIn, bdepUnDo, bdepContents] then
    DirectSingleLInkController.SetOrderNo(NewOrder, Mode)
  else
   UnsupportedMode(Mode, 'SetOrderNo');
end;

{ TBoldIndirectSingleLinkController_Proxy }

procedure TBoldIndirectSingleLinkController_Proxy.AssignContentValue(const Source: IBoldValue);
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
  Result := 0;
end;

procedure TBoldIndirectSingleLinkController_Proxy.SetFromIds(Id1, Id2: TBoldObjectId);
begin
  if Mode in [bdepContents, bdepPMIn] then
    InDirectSingleLInkController.SetFromIds(Id1, Id2, Mode)
  else
    UnsupportedMode(Mode, 'SetFromId');
end;

procedure TBoldIndirectSingleLinkController_Proxy.SetOrderNo(NewOrder: Integer);
begin
  UnsupportedMode(Mode, 'SetOrderNo');
end;

{ TBoldDirectMultiLinkController_Proxy }

procedure TBoldDirectMultiLinkController_Proxy.AssignContentValue(const Source: IBoldValue);
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
  anIdList.Capacity := s.Count;
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

class function TBoldDirectMultiLinkController_Proxy.MakeProxy(
  ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := fLastUsed[Mode];
  // Reuse proxy if we hold only reference
  if Assigned(Result) and (Result.RefCount =1) then
  begin
    Result.Retarget(ProxedMember, Mode);
  end
  else
  begin
    Result := Create(ProxedMember, Mode);
    fLastUsed[Mode] := Result;
    fLastUsedAsInterface[Mode] := Result;  // Inc refcount
  end;
end;

procedure TBoldDirectMultiLinkController_Proxy.SetFromIdList(IdList: TBoldObjectIdList);
begin
  if Mode in [bdepContents, bdepPMIn] then
    DirectMultiLinkController.SetFromIdList(IdLIst, Mode)
  else
    UnsupportedMode(Mode, 'SetFromIdList');
end;

procedure TBoldDirectMultiLinkController_Proxy.SetList(
  IdList: TBoldObjectIdList);
var
  i: integer;
  LocatorList: TBoldObjectLocatorList;
begin
  IdList.Clear;
  LocatorList := DirectMultiLinkController.LocatorList;
  if LocatorList.Count > 0 then
  begin
    IdList.Capacity := LocatorList.Count;
    for I := 0 to LocatorList.Count - 1 do
        IdList.Add(LocatorList[I].BoldObjectId);
  end;
end;

{ TBoldInDirectMultiLinkController_Proxy }

procedure TBoldInDirectMultiLinkController_Proxy.AssignContentValue(const Source: IBoldValue);
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
  i := s.Count;
  if i > 0 then
  begin
    anIdList1.Capacity := i;
    anIdList2.Capacity := i;
    for i := 0 to s.Count - 1 do
    begin
      anIdList1.Add(s.IdList1[i]);
      anIdList2.Add(s.IdList2[i]);
    end;
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
    UnsupportedMode(Mode, 'SetFromIdLists');
end;

{ TBoldDirectMultiLinkController }

function TBoldDirectMultiLinkController.GetOtherEndController(Locator: TBoldObjectLocator): TBoldDirectSingleLinkController;
begin
  result := GetControllerForMember(Locator.EnsuredBoldObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd]) as TBoldDirectSingleLinkController;
end;

procedure TBoldMultiLinkController.DoEnsureOrder;
begin
  if (fMayBeOutOfOrder) then
  begin
    if not IsInOrder then
      Resort;
    fMayBeOutOfOrder := false;
  end;
end;

procedure TBoldMultiLinkController.EnsureOrder;
begin
  if (fMayBeOutOfOrder) then
    DoEnsureOrder;
end;

procedure TBoldDirectMultiLinkController.AddLocator(Locator: TBoldObjectLocator);
var
  OtherEndController: TBoldDirectSingleLinkController;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'AddLocator', '');
    PreChange;
    LocatorList.Add(Locator);
    OtherEndController := GetOtherEndController(Locator);
    OtherEndController.LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    if RoleRTInfo.IsOrdered then
      OtherEndController.SetAndModifyOrderNo(LocatorList.IndexOf(Locator));
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemAdded, [Locator]);
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

type
  TMultiLinkItem = class(TObject)
  public
    OrderNo: Integer;
    ObjectId: TBoldObjectId;
  end;

  TIndirectMultiLinkItem = class(TMultiLinkItem)
  public
    OtherObjectId: TBoldObjectId;
  end;


function _CompareOrderNo(Item1, Item2: Pointer): Integer;
begin
  Result := TMultiLinkItem(Item1).OrderNo - TMultiLinkItem(Item2).OrderNo;
end;

procedure TBoldDirectMultiLinkController.MakeDbCurrent;
{$IFDEF FetchFromClassList}
  procedure FetchFromClassList;
  var
    ClassList: TBoldObjectList;
    lBoldObjectIdList: TBoldObjectIdList;
    lBoldGuard: IBoldGuard;
    lBoldObject: TBoldObject;
    lBoldObjectReference: TBoldObjectReference;
    lOwnBoldObjectLocator: TBoldObjectLocator;
    lMultiLinkItem: TMultiLinkItem;
    lSortList: TList;
    i: integer;
    lIsOrdered: boolean;
    CheckType: boolean;
    lIndexOfOtherEnd: Integer;
    lTopSortedIndex: integer;
    OtherEndBoldClassTypeInfo: TBOldClassTypeInfo;
  begin
    lBoldGuard := TBoldGuard.Create(lBoldObjectIdList, lSortList);
    OtherEndBoldClassTypeInfo := RoleRTInfo.ClassTypeInfoOfOtherEnd;
    lTopSortedIndex := RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex;
    ClassList := BoldSystem.Classes[lTopSortedIndex];
    lOwnBoldObjectLocator := OwningObject.BoldObjectLocator;
    lBoldObjectIdList := TBoldObjectIdList.Create;
    if RoleRTInfo.IsOrdered then
      lSortList := TList.Create;
    lIsOrdered := RoleRTInfo.IsOrdered;
    lIndexOfOtherEnd := RoleRTInfo.IndexOfOtherEnd;
    CheckType := ClassList.BoldPersistenceState <> bvpsCurrent;
    if CheckType then
      ClassList := TBoldClassListController(GetControllerForMember(ClassList)).ClosestLoadedClassList{.FilterOnType(RoleRTInfo.ClassTypeInfoOfOtherEnd)};
    for i := 0 to ClassList.Count - 1 do
    begin
      if CheckType and (ClassList.Locators[i].BoldObjectID.TopSortedIndex < lTopSortedIndex) or
        not ClassList.Locators[i].BoldClassTypeInfo.BoldIsA(OtherEndBoldClassTypeInfo) then
        Continue;
      lBoldObject := ClassList[i];
      lBoldObjectReference := lBoldObject.BoldMembers{IfAssigned}[lIndexOfOtherEnd] as TBoldObjectReference;
      if Assigned(lBoldObjectReference) and (lBoldObjectReference.Locator = lOwnBoldObjectLocator) then
      begin
        if lIsOrdered then
        begin
          lMultiLinkItem := TMultiLinkItem.Create;
          lMultiLinkItem.ObjectId := lBoldObject.BoldObjectLocator.BoldObjectId;
          lMultiLinkItem.OrderNo := (GetControllerForMember(lBoldObjectReference) as TBoldDirectSingleLinkController).OrderNo;
          lSortList.Add(lMultiLinkItem);
        end
        else
        begin
          lBoldObjectIdList.Add(lBoldObject.BoldObjectLocator.BoldObjectId);
        end;
      end;
    end;
    if lIsOrdered then
    begin
      lSortList.Sort(_CompareOrderNo);
      for i := 0 to lSortList.Count - 1 do
      begin
        lMultiLinkItem := TMultiLinkItem(lSortList[i]);
        lBoldObjectIdList.Add(lMultiLinkItem.ObjectId);
        lMultiLinkItem.free;
      end;
    end;

    SetFromIdList(lBoldObjectIdList, bdepContents);
    OwningObjectList.BoldPersistenceState := bvpsCurrent;
  end;
{$ENDIF}
begin
  EnsureOrder;
{$IFDEF FetchFromClassList}
  if TBoldClassListController(GetControllerForMember(BoldSystem.Classes[RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex])).IsCurrentOrSuperClassIsCurrent then
    FetchFromClassList
  else
{$ENDIF}
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
    if assigned(RoleRTInfo) and RoleRTInfo.isQualified then
    begin
      OwningObjectList.EnsureObjects;
      LocatorList.InitMembersIndex(OwningObjectList, RoleRTInfo.Qualifiers)
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
      BoldRaiseLastFailure(OwningList, 'InsertLocator', '');

    PreChange;
    LocatorList.Insert(Index, Locator);
    GetOtherEndController(Locator).LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemAdded, [Locator]);
end;

procedure TBoldDirectMultiLinkController.LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode) ;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  if IncludesLocator(NewLocator) then // this used to be an assert, but we now just exit
  begin
    exit;
  end;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningList, 'Linkto', '');

    PreChange;
    LocatorList.Add(NewLocator);
    if Mode = blulMarkAdjusted then
      OwningObjectList.Adjusted := True;
    if updateOrderNo and RoleRTInfo.IsOrdered then
      GetOtherEndController(NewLocator).SetAndModifyOrderNo(LocatorList.IndexOf(NewLocator));  //TODO - This could have side effect when mode is blulMarkAdjusted
    if Mode = blulMarkModified then
      EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemAdded, [NewLocator]);
end;

procedure TBoldDirectMultiLinkController.Move(CurrentIndex, NewIndex: Integer);
begin
  EnsureOrder;
  if not RoleRTInfo.IsOrdered then
    exit;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Move', '');

    PreChange;
    LocatorList.Move(CurrentIndex, NewIndex);
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beOrderChanged, []);
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
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', '');

    Locator := LocatorList[index];
    PreChange;
    GetOtherEndController(Locator).Unlink(OwningList.OwningObject.BoldObjectLocator, blulMarkModified);
    LocatorList.RemoveByIndex(index);
    ReOrder;
    EndModify;
    Changed(beItemDeleted, [Locator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldDirectMultiLinkController.ReOrder;
var
{$IFOPT C+}
  index: Integer;
{$ENDIF}
  I: Integer;
  Locator: TBoldObjectLocator;
begin
  if not RoleRTInfo.IsOrdered then
    exit;
{$IFOPT C+}
  index := RoleRTInfo.IndexOfOtherEnd;
  Assert(index <> -1);
{$ENDIF}
  for I := 0 to LocatorList.Count - 1 do
  begin
    Locator := LocatorList[I];
    Locator.EnsureBoldObject;
    GetOtherEndController(Locator).SetAndModifyOrderNo(I);
  end;
end;

procedure TBoldDirectMultiLinkController.SetFromIdList(ListOfOtherEnd: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
var
  BoldSystem: TBoldSystem;
  NewListOfOtherEnd: TBoldObjectIdlist;
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
    BoldSystem := self.BoldSystem;
    BoldClassTypeInfoOfOtherEnd := RoleRTInfo.ClassTypeInfoOfOtherEnd;
    IndexOfOtherEnd := RoleRTInfo.IndexOfOtherEnd;
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
  OrderHasChanged: boolean;
  OwningBoldObjectLocator: TBoldObjectLocator;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(NewListOfOtherEnd);
  PreChangeCalled := True;
  if (mode = bdepPMIn) and (OwningList.OwningObject.IsHistoricVersion) then
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
  PreserveOrder := (mode = bdepContents) or ((mode = bdepPMIn) and RoleRTInfo.IsOrdered and not WasAdjusted);
  OwningBoldObjectLocator := OwningList.OwningObject.BoldObjectLocator;
  {we now have a list with the right objects}
  OrderHasChanged := false;
  for I := GetCount - 1 downto 0 do
  begin
    OldLocator := LocatorList[i];
    if not NewListOfOtherEnd.IdInList[OldLocator.BoldObjectId] then
    begin
      PreChangeIfNeeded;
      if mode = bdepPMIn then
        SingleLinkUnlink(LocatorList[I], OwningBoldObjectLocator, LinkUnlinkMode);
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
          SingleLinkLinkTo(NewLocator, OwningBoldObjectLocator, false, LinkUnlinkMode);
        LocatorList.Add(NewLocator);
        Changed(beItemAdded, [NewLocator]);
      end
      else if NewLocator = LocatorList[i] then
      else if LocatorList.IndexOf(NewLocator) <> -1 then
      begin
        PreChangeIfNeeded;
        LocatorList.Move(LocatorList.IndexOf(NewLocator), I);
        OrderHasChanged := true;
      end
      else
      begin
        PreChangeIfNeeded;
        if mode = bdepPMIn then
          SingleLinkLinkTo(NewLocator, OwningBoldObjectLocator, false, LinkUnlinkMode);
        LocatorList.Insert(I, NewLocator);
        Changed(beItemAdded, [NewLocator]);
      end;
    end;
  end
  else
  begin
    if NewListOfOtherEnd.Count > 0 then
    begin
      LocatorList.Capacity :=  NewListOfOtherEnd.Count;
      for I := 0 to NewListOfOtherEnd.Count - 1 do
      begin
        NewLocator := AssertedLocatorForId(NewListOfOtherEnd[I]);
        if not IncludesLocator(NewLocator) then
        begin
          PreChangeIfNeeded;
          if mode = bdepPMIn then
            SingleLinkLinkTo(NewLocator, OwningBoldObjectLocator, false, LinkUnlinkMode);
          LocatorList.Add(NewLocator);
          Changed(beItemAdded, [NewLocator]);
        end;
      end;
    end;
  end;
  if OrderHasChanged then
  begin
    ReOrder;
    Changed(beOrderChanged, []);
  end;
end;

procedure TBoldDirectMultiLinkController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'SetLocator', '');
    PreChange;
    GetOtherEndController(LocatorList[index]).UnLink(OwningList.OwningObject.BoldObjectLocator, blulMarkModified);
    GetOtherEndController(Locator).LinkTo(OwningList.OwningObject.BoldObjectLocator, true, blulMarkModified);
    LocatorList[index] := Locator;
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemReplaced, [Locator, Index]);
end;

function TBoldDirectMultiLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdListRef;
end;

function TBoldDirectMultiLinkController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  Result := TBFSObjectIdListref;
end;

procedure TBoldDirectMultiLinkController.Unlink(OldLocator: TBoldObjectLocator;  Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  if (Mode=blulMarkAdjusted) and (not IncludesLocator(OldLocator)) then //PATCH
  begin
    exit;  //PATCH
  end;
  //The prevoius assert prevents unlink to work when controller is bvpsInvalid
  //Current implementation of GetOtherEndController never return Invaild Members so this it not a problem
  //But if the other end has fetched a new value from db and is current GetOtherEndController will return it
  //This will lead to data corruption - Unlink will set correct value to nil!

  // PATCH
  // changed from Assert(IncludesLocator(OldLocator)) to if, it's safe to exit if OldLocator is already removed - Daniel
  if not IncludesLocator(OldLocator) then
  begin
    exit;
  end;
  BoldSystem.StartTransaction;
  try

    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningList, 'Unlink', '');

      PreChange;
    LocatorList.Remove(OldLocator);
    if Mode = blulMarkAdjusted then
      OwningObjectList.Adjusted := True;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beItemDeleted, [OldLocator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldDirectMultiLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRef) then
  begin
    result := GetProxy(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRef', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldDirectMultiLinkController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBoldDirectmultiLinkController_Proxy.MakeProxy(Member ,Mode);
end;

procedure TBoldDirectMultiLinkController.FreeContent;
begin
  LocatorList.Clear;
end;

procedure TBoldDirectMultiLinkController.PrepareClear;
begin
  OwningObjectList.EnsureObjects;
end;

procedure TBoldDirectMultiLinkController.Exchange(Index1, Index2: integer);
begin
  LocatorList.Exchange(Index1, Index2);
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
  if RoleRTInfo.IsOrdered then
    for i:= 0 to LocatorList.Count - 2 do
    begin
      OrderNo1 := GetOtherEndController(LocatorList[i]).GetOrderno;
      OrderNo2 := GetOtherEndController(LocatorList[i + 1]).GetOrderno;
      Result := (OrderNo1 <= OrderNo2);
      if not Result then
        Exit;
    end;
end;

function TBoldDirectMultiLinkController.CompareOrderNo(Index1, Index2: integer): integer;
var
  OrderNo1, OrderNo2: integer;
begin
  if RoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole then
  begin
    OrderNo1 := GetOtherEndController(LocatorList[Index1]).GetOrderNo;
    OrderNo2 := GetOtherEndController(LocatorList[Index2]).GetOrderNo;
    Result := (OrderNo1 - OrderNo2);
  end
  else
    raise EBold.Create(sOtherEndMustBeSingle);
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
    BoldSystem: TBoldSystem;
  begin
    G := TBoldGuard.Create(Traverser);
    BoldSystem := self.BoldSystem;
    Traverser := BoldSystem.Locators.CreateTraverser;
    OwnLocator := OwningList.OwningObject.BoldObjectLocator;
    while Traverser.MoveNext do
    begin
      Locator := Traverser.Locator;
      if Locator.BoldClassTypeInfo.BoldIsA(BoldClassTypeInfoOfOtherEnd) then
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
    end;
  end;

var
  i: integer;
  aLocator: TBoldObjectLocator;
  ListOfReferring: TBoldObjectlist;
  OtherEndController: TBoldDirectSingleLinkController;
begin
  Assert(Assigned(NewList));
  EmbeddedIndex := RoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
  IndexOfOtherEnd := RoleRTInfo.IndexOfOtherEnd;
  BoldClassTypeInfoOfOtherEnd := RoleRTInfo.ClassTypeInfoOfOtherEnd;
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
        Assert(OtherEndController.OwningReference.BoldPersistenceState in [bvpsCurrent, bvpsInvalid]);
        OtherEndController.FLocator := nil;
        if OtherEndController.OwningReference.BoldPersistenceState <> bvpsInvalid then
        begin
          OtherEndController.OwningReference.BoldPersistenceState := bvpsInvalid;
          OtherEndController.OwningReference.SendEvent(beValueInvalid);
        end;
        OtherEndController.OwningReference.HasOldValues := false;
      end
      else
      begin
        EmbeddedIndex := RoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
        if (EmbeddedIndex <> -1) and (aLocator.EmbeddedSingleLinks[EmbeddedIndex] <> nil) then
        begin
          Assert(aLocator.EmbeddedSingleLinks[EmbeddedIndex] = OwningObjectList.OwningObject.BoldObjectLocator);
          aLocator.EmbeddedSingleLinks[EmbeddedIndex] := nil;
        end;
      end;
    end;
  end;
  if ListOfReferring <> OwningObjectList then
    FreeAndNil(ListOfReferring);
end;

function TBoldDirectMultiLinkController.AssertIntegrity: Boolean;
var
  i: integer;
begin
  for i := 0 to LocatorList.Count - 1 do
  begin
    Assert(LocatorList[i].AsString <> '');
    Assert(Assigned(GetOtherEndController(LocatorList[i]).fLocator));
    Assert(GetOtherEndController(LocatorList[i]).fLocator.AsString <> '');
  end;
  if Assigned(OwningObjectList.OldValue) then
  begin
//    OwningObjectList.OldValue
  end;
  Result := True;
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
      BoldRaiseLastFailure(OwningList, 'Clear', '');

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
  if Assigned(OtherEndController) and ((Mode <> blulMarkAdjusted) or (OtherEndController.OwningMember.BoldPersistenceState = bvpsCurrent)) then
    OtherEndController.linkto(OwningReference.OwningObject.BoldObjectLocator, Mode <> blulMarkAdjusted, Mode);
end;

function TBoldOrderedDirectSingleLinkController.GetOrderNo: Integer;
begin
  Result := fOrderNo;
end;

procedure TBoldOrderedDirectSingleLinkController.SetAndModifyOrderNo(index: Integer);
begin
  if index <> FOrderno then
  begin
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetAndModifyOrderNo', '');
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
  if RoleRTInfo.IndexOfOtherEnd = -1 then
    exit;

  if AllowForceOtherEnd and RoleRTInfo.ForceOtherEnd then
  begin
    aLocator.EnsureBoldObject;
    aLocator.BoldObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd].EnsureContentsCurrent;
  end;

  if not assigned(aLocator.BoldObject) then
    exit;

  aMember := aLocator.BoldObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd];

  if aMember.BoldPersistenceState = bvpsInvalid then
    exit;

  result := GetControllerForMember(aMember);
end;

procedure TBoldDirectSingleLinkController.LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
  if fLocator = NewLocator then
    exit;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningReference, 'linkto', '');
    RemoveFromOtherEnd(Mode);
    InternalSetLocator(NewLocator);
    if OwningReference.BoldPersistenceState = bvpsInvalid then
      OwningReference.HasOldValues := True;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beValueChanged, [NewLocator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldDirectSingleLinkController.RemoveFromOtherEnd(Mode: TBoldLinkUnlinkMode);
var
  OtherEndController: TBoldAbstractController;
  OldLocatorRelevant: Boolean;
begin
  OldLocatorRelevant := ((OwningReference.BoldPersistenceState <> bvpsInvalid) or OwningReference.HasOldValues);
  if OldLocatorRelevant then
  begin
    OtherEndController := GetOtherEndController(fLocator, Mode <> blulMarkAdjusted);
    if Assigned(OtherEndController) then
      OtherEndController.Unlink(OwningReference.OwningObject.BoldObjectLocator, Mode);
  end;
end;

procedure TBoldDirectSingleLinkController.SetFromId(Id: TBoldObjectId; Mode: TBoldDomainElementProxyMode);
var
  BoldSystem: TBoldSystem;
  NewLocator: TBoldObjectLocator;

  procedure SafeCopyOptimisticValues;
  var
    Value: IBoldObjectIdRef;
  begin
    Value := NewValueInOptimisticLocking as IBoldObjectIdRef;
    if Assigned(Value) then
      Value.SetFromId(Id, false);
  end;
  procedure AdjustNewLocator;
  var
    i: integer;
    obj: TBoldObject;
    OtherEndController: TBoldAbstractController;
  begin
    BoldSystem := self.BoldSystem;
    for i := 0 to BoldSystem.DirtyObjects.Count - 1 do
    begin
      Obj := TBoldObject(BoldSystem.DirtyObjects[i]);
      if Obj.BoldClassTypeInfo.Conformsto(RoleRTInfo.ClassTypeInfoOfOtherEnd) then
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
  if (mode = bdepPMIn) and (OwningReference.OwningObject.IsHistoricVersion) then
    mode := bdepContents;
  NewLocator := LocatorForId(Id);

  if (mode = bdepPMIn) and not RoleRTInfo.IsStoredInObject then
    AdjustNewLocator;

  if Mode = bdepPmIn then
    LinkUnlinkMode := blulMarkAdjusted
  else
    LinkUnlinkMode := blulNone;
  if ((OwningReference.BoldPersistenceState = bvpsInvalid) and not OwningReference.HasOldValues) or (fLocator <> NewLocator) then
  begin
    if Assigned(NewLocator) then
      if not VerifyLocatorType(NewLocator, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd, false) then
          raise EBold.CreateFmt('%s.SetFromId: Object %s is incorrect type %s in %s. Expected type: %s', [ClassName, NewLocator.AsString, NewLocator.BoldClassTypeInfo.ExpressionName, OwningReference.debuginfo, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
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
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetLocator', '');

    GetOtherEndController(NewLocator, True);
    RemoveFromOtherEnd(blulMarkModified);
    InternalSetLocator(NewLocator);
    AddToOtherEnd(blulMarkModified);
    if not Assigned(fLocator) and RoleRTInfo.IsOrdered then
      SetAndModifyOrderNo(-1);
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beValueChanged, [NewLocator]);
end;

procedure TBoldDirectSingleLinkController.InternalSetLocator(NewLocator: TBoldObjectLocator);
begin
  if fLocator <> NewLocator then
  begin
    if Assigned(NewLocator) then
      if not VerifyLocatorType(NewLocator, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd, false) then
          raise EBold.CreateFmt('%s.InternalSetLocator: Object %s is incorrect type %s in %s. Expected type: %s', [ClassName, NewLocator.AsString, NewLocator.BoldClassTypeInfo.ExpressionName, OwningReference.debuginfo, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
    PreChange;
    fLocator := NewLocator;
  end;
end;

function TBoldDirectSingleLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRef;
end;

function TBoldDirectSingleLinkController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSObjectIdRef;
end;

procedure TBoldDirectSingleLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
begin
// Assert removed, remains to be verified if it causes other problems, so we log instead
//  Assert((Mode <> blulMarkAdjusted) or (Owningmember.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  if not ((Mode <> blulMarkAdjusted) or (OwningReference.BoldPersistenceState = bvpsCurrent)) then
  begin
    BoldLog.LogFmt('TBoldDirectSingleLinkController.Unlink: BoldObjectId: %s, Mode: %s Member: %s, MemberPersistenceState: %s. OldLocator: %s; HasOldValues = %s',
    [
    OwningMember.OwningObject.BoldObjectLocator.AsString,
     TypInfo.GetEnumName(TypeInfo(TBoldLinkUnlinkMode), Ord(Mode)),
     OwningMember.DisplayName,
     TypInfo.GetEnumName(TypeInfo(TBoldValuePersistenceState), Ord(Owningmember.BoldPersistenceState)),
     OldLocator.AsString,
     BoolToStr(OwningReference.HasOldValues,True)]);
  end;
  if (Mode=blulMarkAdjusted) and (fLocator<>OldLocator) then //PATCH
  begin
    exit;  //PATCH
  end;
  //The prevoius assert prevents unlink to work when controller is bvpsInvalid
  //Current implementation of GetOtherEndController never return Invaild Members so this it not a problem
  //But if the other end has fetched a new value from db and is current GetOtherEndController will return it
  //This will lead to data corruption - Unlink will set correct value to nil!

//  Assert(Not assigned(fLocator) or (fLocator = OldLocator));
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningReference, 'Unlink', '');

    InternalSetLocator(nil);
    OwningReference.HasOldValues := False;

    if RoleRTInfo.IsOrdered then
    begin
      if Mode = blulMarkModified then //PATCH
        SetAndModifyOrderNo(-1)
      else                            //PATCH
        SetOrderNo(-1, bdepPMIn);     //PATCH Do not call SetAndModifyOrderNo when Mode is bluMarkAdjustd - this creates an unwanted save
    end;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beValueChanged, [nil]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
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
        not OtherEndController.RoleRTInfo.IsStoredInObject and
        not OtherEndController.OwningObject.BoldObjectIsNew then
      begin
        OtherEndController.OwningMember.Invalidate;
      end;
    end;
  end;

var
  OldRef: IBoldValue;
  OldIdRef: IBoldObjectIdRef;
begin
  RemoveFromOtherEnd(blulNone);
  OldRef := OwningReference.OldValue;
  if assigned(OldRef) then
  begin
    OldRef.QueryInterface(IBoldObjectIdRef, OldIdRef);
    if assigned(OldIdRef.Id) then
      InvalidateNonembeddedOtherEnd(BoldSystem.EnsuredLocatorByID[OldIdRef.Id]);
  end;
  fLocator := nil;
end;

function TBoldDirectSingleLinkController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBoldDirectSingleLinkController_Proxy.MakeProxy(Member ,Mode);
end;

function TBoldDirectSingleLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdRef) then
  begin
    result := GetProxy(self.OwningReference, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdRef', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldDirectSingleLinkController.MayUpdate: Boolean;
begin
  result := not RoleRTInfo.IsStoredInObject or
            not assigned(fLocator) or fLocator.ObjectIsPersistent;
end;

procedure TBoldDirectSingleLinkController.MakeDbCurrent;
{$IFDEF FetchFromClassList}
  procedure FetchFromClassList;
  var
    OtherEndBoldClassTypeInfo: TBoldClassTypeInfo;
    ClassList: TBoldObjectList;
    BoldObjectId: TBoldObjectId;
    IndexOfOtherEnd: Integer;
    BoldMember: TBoldMember;
    i: integer;
    Locator: TBoldObjectLocator;
    CheckType: boolean;
    lTopSortedIndex: Integer;
    AllMembersLoaded: boolean;
  begin
    OtherEndBoldClassTypeInfo := RoleRTInfo.ClassTypeInfoOfOtherEnd;
    IndexOfOtherEnd := RoleRTInfo.IndexOfOtherEnd;
    lTopSortedIndex := RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex;
    ClassList := BoldSystem.Classes[lTopSortedIndex];
    Locator := OwningReference.OwningObject.BoldObjectLocator;
    BoldObjectId := nil;
    CheckType := ClassList.BoldPersistenceState <> bvpsCurrent;
    AllMembersLoaded := true;
    if CheckType then
      ClassList := TBoldClassListController(GetControllerForMember(ClassList)).ClosestLoadedClassList;
    for i := ClassList.Count - 1 downto 0 do
    begin
      if CheckType and ((ClassList.Locators[i].BoldObjectID.TopSortedIndex < lTopSortedIndex) or
        not ClassList.Locators[i].BoldClassTypeInfo.BoldIsA(OtherEndBoldClassTypeInfo)) then
        Continue;
      BoldMember := ClassList[i].BoldMemberIfAssigned[IndexOfOtherEnd];
      if Assigned(BoldMember) then
      begin
        if ((BoldMember as TBoldObjectReference).Locator = Locator) then
        begin
          BoldObjectId := ClassList[i].BoldObjectLocator.BoldObjectId;
          break;
        end;
      end
      else
        AllMembersLoaded := false;
    end;
    if Assigned(BoldObjectId) or AllMembersLoaded then
    begin
      SetFromId(BoldObjectId, bdepContents);
      OwningReference.BoldPersistenceState := bvpsCurrent;
    end
    else
    begin
      DbFetchOwningMember;
    end;
  end;
{$ENDIF}
begin
{$IFDEF FetchFromClassList}
  if RoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole and
    TBoldClassListController(GetControllerForMember(BoldSystem.Classes[RoleRTInfo.ClassTypeInfoOfOtherEnd.TopSortedIndex])).IsCurrentOrSuperClassIsCurrent then
  begin
    FetchFromClassList;
    exit;
  end;
{$ENDIF}
    DbFetchOwningMember;
end;

procedure TBoldOrderedDirectSingleLinkController.SetOrderNo(NewOrderNo: Integer;
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

function TBoldIndirectSingleLinkController.GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[RoleRTInfo.OwnIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectSingleLinkController.GetLinkObjectOtherLinkController(
  LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[RoleRTInfo.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectSingleLinkController.GetLinkObjectRoleController: TBoldLinkObjectReferenceController;
begin
  result := ControllerForLinkRole as TBoldLinkObjectReferenceController;
end;

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

function TBoldIndirectSingleLinkController.GetLocator: TBoldObjectLocator;
begin
  result := fOtherEndLocator;
end;

procedure TBoldIndirectSingleLinkController.LinkTo(NewLocator: TBoldObjectLocator; UpdateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningReference.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningReference, 'Linkto', '');

    if fLinkObjectLocator <> NewLocator then
    begin
      if assigned(fLinkObjectLocator) then
        DeleteLink(Mode);
      PreChange;
      fLinkObjectLocator := NewLocator;
      fOtherEndLocator := GetLinkObjectOtherLinkController(NewLocator.EnsuredBoldObject).fLocator;
      if not VerifyLocatorType(fOtherEndLocator, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd, false) then
          raise EBold.CreateFmt('%s.linkto: Object %s is incorrect type %s in %s. Expected type: %s', [ClassName, fOtherEndLocator.AsString, fOtherEndLocator.BoldClassTypeInfo.ExpressionName, OwningReference.debuginfo, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
    end;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beValueChanged, [fOtherEndLocator]);
    GetLinkObjectRoleController.Changed(beValueChanged, [fLinkObjectLocator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectSingleLinkController.NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
{$IFDEF ReuseDeletedLinkObjectOnRelink}
  function FindLinkInOldValues: TBoldObject;
  var
    s: IBoldObjectIdRefPair;
  begin
    result := nil;
    if Supports(OwningReference.OldValue, IBoldObjectIdRefPair, s) and Assigned(s.Id2) then
      if s.Id2.IsEqual[OtherLocator.BoldObjectID] then
      begin
        result := BoldSystem.Locators.ObjectByID[s.Id1];
        Assert(result is RoleRTInfo.LinkClassTypeInfo.ObjectClass);
        result.AsIBoldObjectContents[bdepContents].BoldExistenceState := besExisting;
        result.AsIBoldObjectContents[bdepContents].BoldPersistenceState := bvpsCurrent;
      end;
  end;
{$ENDIF}
var
  LinkObject: TBoldObject;
  LinkClassTypeInfo: TBoldClassTypeinfo;
  OtherEndController: TBoldAbstractController;
begin
  LinkClassTypeInfo := RoleRTInfo.LinkClassTypeInfo;
{$IFDEF ReuseDeletedLinkObjectOnRelink}
  LinkObject := FindLinkInOldValues;
  if not Assigned(LinkObject) then
{$ENDIF}
    LinkObject := TBoldObjectClass(LinkClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(LinkClassTypeInfo, BoldSystem,
    OtherLocator.ObjectIsPersistent and OwningReference.OwningObject.BoldPersistent);
  OtherEndController := GetLinkObjectOtherLinkController(LinkObject).GetOtherEndController(OtherLocator, true);
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
  procedure AdjustLocators;
  var
    BoldObject: TBoldObject;
    i: integer;
    BoldLinkClassTypeInfo: TBoldClassTypeInfo;
    IndexOfOwnEnd, IndexOfOtherEnd, EmbeddedIndexOfOwnEnd, EmbeddedIndexOfOtherEnd: integer;
    BoldSystem: TBoldSystem;
  begin
    IndexOfOwnEnd := RoleRTInfo.OwnIndexInLinkClass;
    IndexOfOtherEnd := RoleRTInfo.OtherIndexInLinkClass;
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
      else {object not loaded, set embedded links}
      begin
        if Assigned(NewLinkLocator) then
        begin
          EmbeddedIndexOfOwnEnd := RoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOwnEnd].EmbeddedLinkIndex;
          EmbeddedIndexOfOtherEnd := RoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOtherEnd].EmbeddedLinkIndex;
          if (EmbeddedIndexOfOwnEnd <> -1) then
            NewLinkLocator.EmbeddedSingleLinks[EmbeddedIndexOfOwnEnd] := OwningReference.OwningObject.BoldObjectLocator;
          if (EmbeddedIndexOfOtherEnd <> -1) then
            NewLinkLocator.EmbeddedSingleLinks[EmbeddedIndexOfOtherEnd] := NewOtherEndLocator;
        end;
      end;
    end;
    BoldSystem := self.BoldSystem;
    for I := 0 to BoldSystem.DirtyObjects.Count - 1 do
    begin
      BoldLinkClassTypeInfo := RoleRTInfo.LinkClassTypeInfo;
      BoldObject := BoldSystem.DirtyObjects[I];
      if (BoldObject.BoldClassTypeInfo.BoldIsA(BoldLinkClassTypeInfo)) and (BoldObject.BoldExistenceState = besExisting) and
        (((BoldObject.BoldMembers[IndexOfOwnEnd]) as TBoldObjectReference).BoldObject = OwningReference.OwningObject) then
      begin
        SafeCopyOptimisticValues;
        NewLinkLocator := BoldObject.BoldObjectLocator;
        NewOtherEndLocator := (BoldObject.BoldMembers[IndexOfOtherEnd] as TBoldObjectReference).Locator;
     end;
    end;
  end;

begin
  if (mode = bdepPMIn) and (OwningReference.OwningObject.IsHistoricVersion) then
    mode := bdepContents;
  NewLinkLocator  := LocatorForId(Id1);
  NewOtherEndLocator := LocatorForId(Id2);
  if mode = bdepPMIn then
    AdjustLocators;

  if fLinkObjectLocator <> NewLinkLocator then
  begin
    if Assigned(NewOtherEndLocator) then
      if not VerifyLocatorType(NewOtherEndLocator, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd, false) then
        raise EBold.CreateFmt('%s.SetFromIds: Object %s is incorrect type %s in %s. Expected type: %s', [ClassName, NewOtherEndLocator.AsString, NewOtherEndLocator.BoldClassTypeInfo.ExpressionName, OwningReference.debuginfo, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);
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
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningReference, 'SetLocator', '');

    if Assigned(NewLocator) then
      if not VerifyLocatorType(NewLocator, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd, false) then
        raise EBold.CreateFmt('%s.SetLocator: Object %s is incorrect type %s in %s. Expected type: %s', [ClassName, NewLocator.AsString, NewLocator.BoldClassTypeInfo.ExpressionName, OwningReference.debuginfo, OwningReference.BoldRoleRTInfo.ClassTypeInfoOfOtherEnd.ExpressionName]);

    DeleteLink(blulMarkModified);
    PreChange;
    fOtherEndLocator := NewLocator;
    if assigned(NewLocator) then
      fLinkObjectLocator := NewLink(NewLocator, blulMarkModified).BoldObjectLocator
    else
      fLinkObjectLocator := nil;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beValueChanged, [NewLocator]);
  GetLinkObjectRoleController.Changed(beValueChanged, [fLinkObjectLocator]);
end;

function TBoldIndirectSingleLinkController.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRefPair;
end;

function TBoldIndirectSingleLinkController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSObjectIdRefPair;
end;

procedure TBoldIndirectSingleLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningReference.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  Assert(fLinkObjectLocator = OldLocator);
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningReference, 'Unlink', '');

    PreChange;
    fLinkObjectLocator := nil;
    fOtherEndLocator := nil;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beValueChanged, [nil]);
    GetLinkObjectRoleController.Changed( beValueChanged, [nil]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectSingleLinkController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBoldIndirectSingleLinkController_Proxy.MakeProxy(Member ,Mode);
end;

function TBoldIndirectSingleLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdRefPair) then
  begin
    result := GetProxy(self.OwningReference, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdRefPair', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldIndirectSingleLinkController.MayUpdate: Boolean;
begin
  result := not RoleRTInfo.IsStoredInObject or
            not assigned(fLinkObjectLocator) or fLinkObjectLocator.ObjectIsPersistent;
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
        Assert(GetLinkObjectOwnLinkController(fLinkObjectLocator.BoldObject).fLocator = OwningReference.OwningObject.BoldObjectLocator);
        Assert(GetLinkObjectOtherLinkController(fLinkObjectLocator.BoldObject).fLocator = fOtherEndLocator);
        Assert(GetLinkObjectRoleController.AssertIntegrity);
      end
      else
      begin
      end;
    end;
  Result := True;
end;

procedure TBoldIndirectSingleLinkController.MakeDbCurrent;
begin
  DbFetchOwningMember;
end;

{ TBoldIndirectMultiLinkController }

function TBoldIndirectMultiLinkController.GetLinkObjectOwnLinkController(LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[RoleRTInfo.OwnIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

function TBoldIndirectMultiLinkController.GetLinkObjectListController: TBoldLinkObjectListController;
begin
  result := ControllerForLinkMember as TBoldLinkObjectListController;
end;

procedure TBoldIndirectMultiLinkController.AddLocator(Locator: TBoldObjectLocator);
var
  LinkObject: TBoldObject;
begin
  EnsureOrder;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'AddLocator', '');

    PreChange;
    LinkObject := NewLink(Locator, blulMarkModified);
    LinkLocatorList.Add(LinkObject.BoldObjectLocator);
    ReferredLocatorList.Add(Locator);
    Assert(ReferredLocatorList.Count = LinkLocatorList.Count);
    if RoleRTInfo.IsOrdered then
      GetLinkObjectOwnLinkController(LinkObject).SetAndModifyOrderNo(LinkLocatorList.IndexOf(LinkObject.BoldObjectLocator));
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemAdded, [Locator]);
  GetLinkObjectListController.Changed(beItemAdded, [LinkObject.BoldObjectLocator]);
  LinkObject.SendEvent(beLinkObjectEstablished);
end;

constructor TBoldIndirectMultiLinkController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  fLinkLocatorList := TBoldObjectLocatorList.Create;
  FReferredList := TBoldObjectLocatorList.Create;
end;

function TBoldIndirectMultiLinkController.GetLinkObjectOtherLinkController(
  LinkObject: TBoldObject): TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(LinkObject.BoldMembers[RoleRTInfo.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
end;

procedure TBoldIndirectMultiLinkController.DeleteLink(LinkObjectLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
var
  OldLinkObject: TBoldObject;
begin
  OldLinkObject := LinkObjectLocator.EnsuredBoldObject;
  GetLinkObjectOtherLinkController(OldLinkObject).RemoveFromOtherEnd(Mode);
  GetLinkObjectOtherLinkController(OldLinkObject).Unlink(GetLinkObjectOtherLinkController(OldLinkObject).fLocator, Mode);
  GetLinkObjectOwnLinkController(OldLinkObject).PreChange;
  GetLinkObjectOwnLinkController(OldLinkObject).Unlink(OwningObjectList.OwningObject.BoldObjectLocator, Mode);
  OldLinkObject.Delete;
end;

destructor TBoldIndirectMultiLinkController.Destroy;
begin
  FreeAndNil(fLinkLocatorList);
  FreeAndNil(FReferredList);
  inherited;
end;

procedure TBoldIndirectMultiLinkController.MakeDbCurrent;
{$IFDEF FetchFromClassList}
  procedure FetchFromClassList;
  var
    ClassList: TBoldObjectList;
    lBoldGuard: IBoldGuard;
    lBoldObject: TBoldObject;
    lOtherIndexInLinkClass: Integer;
    lOwnBoldObjectLocator: TBoldObjectLocator;
    lLinkClassTypeInfo: TBoldClassTypeInfo;
    lOwnIndexInLinkClass: integer;
    lListOfLinkObjects: TBoldObjectIdList;
    lListOfOtherEnd: TBoldObjectIdList;
    lThisEndInLinkClass: TBoldObjectReference;
    lOtherEndInLinkClass: TBoldObjectReference;
    lMultiLinkItem: TIndirectMultiLinkItem;
    lSortList: TList;
    i: integer;
    lIsOrdered: boolean;
    CheckType: boolean;
  begin
    lBoldGuard := TBoldGuard.Create(lListOfLinkObjects, lListOfOtherEnd, lSortList);
    lLinkClassTypeInfo := RoleRTInfo.LinkClassTypeInfo;
    ClassList := BoldSystem.Classes[lLinkClassTypeInfo.TopSortedIndex];
    lOtherIndexInLinkClass := RoleRTInfo.OtherIndexInLinkClass;
    lOwnIndexInLinkClass := RoleRTInfo.OwnIndexInLinkClass ;
    lListOfLinkObjects := TBoldObjectIdList.Create;
    lListOfOtherEnd:= TBoldObjectIdList.Create;
    lIsOrdered := RoleRTInfo.IsOrdered;
    if lIsOrdered then
      lSortList := TList.Create;
    lOwnBoldObjectLocator := OwningObjectList.OwningObject.BoldObjectLocator;
    CheckType := ClassList.BoldPersistenceState <> bvpsCurrent;
    if CheckType then
      ClassList := TBoldClassListController(GetControllerForMember(ClassList)).ClosestLoadedClassList;
    Assert(Assigned(ClassList));
    for i := 0 to ClassList.Count - 1 do
    begin
      if CheckType and not ClassList.Locators[i].BoldClassTypeInfo.BoldIsA(lLinkClassTypeInfo) then
        Continue;
      lBoldObject := ClassList[i];
      lThisEndInLinkClass := lBoldObject.BoldMembers{IfAssigned}[lOwnIndexInLinkClass] as TBoldObjectReference;
      if Assigned(lThisEndInLinkClass) and (lThisEndInLinkClass.Locator = lOwnBoldObjectLocator) then
      begin
        lOtherEndInLinkClass := lBoldObject.BoldMembers[lOtherIndexInLinkClass] as TBoldObjectReference;
        Assert(Assigned(lOtherEndInLinkClass.Locator));
{
        if not Assigned(lOtherEndInLinkClass.Locator) then
        begin
          if lOtherEndInLinkClass.BoldDirty then
            lOtherEndInLinkClass.Discard;
          Assert(Assigned(lOtherEndInLinkClass.Locator));
        end;
}
        if lIsOrdered then
        begin
          lMultiLinkItem := TIndirectMultiLinkItem.Create;
          lMultiLinkItem.ObjectId := lBoldObject.BoldObjectLocator.BoldObjectId;
          lMultiLinkItem.OtherObjectId := lOtherEndInLinkClass.Locator.BoldObjectId;
          lMultiLinkItem.OrderNo := (GetControllerForMember(lThisEndInLinkClass) as TBoldDirectSingleLinkController).OrderNo;
          lSortList.Add(lMultiLinkItem);
        end
        else
        begin
          lListOfLinkObjects.Add(lBoldObject.BoldObjectLocator.BoldObjectId);
          lListOfOtherEnd.Add(lOtherEndInLinkClass.Locator.BoldObjectId);
        end;
      end;
    end;
    if lIsOrdered then
    begin
      lSortList.Sort(_CompareOrderNo);
      for i := 0 to lSortList.Count - 1 do
      begin
        lMultiLinkItem := TIndirectMultiLinkItem(lSortList[i]);
        lListOfLinkObjects.Add(lMultiLinkItem.ObjectId);
        lListOfOtherEnd.Add(lMultiLinkItem.OtherObjectId);
        lMultiLinkItem.free;
      end;
    end;

//    DbFetchOwningMember;
//    Assert(lListOfOtherEnd.count = OwningObjectList.Count);
    SetFromIDLists(lListOfLinkObjects, lListOfOtherEnd, bdepContents);
    OwningObjectList.BoldPersistenceState := bvpsCurrent;
  end;
{$ENDIF}
begin
  if OwningObjectList.BoldPersistenceState <> bvpsCurrent then
  begin
    EnsureOrder;
{$IFDEF FetchFromClassList}
    if TBoldClassListController(GetControllerForMember(BoldSystem.Classes[RoleRTInfo.LinkClassTypeInfo.TopSortedIndex])).IsCurrentOrSuperClassIsCurrent then
      FetchFromClassList
   else
{$ENDIF}
    DbFetchOwningMember;
  end;
end;

function TBoldIndirectMultiLinkController.GetCount: Integer;
begin
  Result := LinkLocatorList.Count;
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
    if assigned(RoleRTInfo) and RoleRTInfo.isQualified then
    begin
      OwningObjectList.EnsureObjects;
      ReferredLocatorList.InitMembersIndex(OwningObjectList, RoleRTInfo.Qualifiers)
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
      BoldRaiseLastFailure(OwningList, 'InsertLocator', '');

    PreChange;
    NewLinkLocator := NewLink(Locator, blulMarkModified).BoldObjectLocator;
    LinkLocatorList.Insert(Index, NewLinkLocator);
    ReferredLocatorList.Insert(Index, Locator);
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beItemAdded, [Locator]);
  GetLinkObjectListController.Changed(beItemAdded, [NewLinkLocator]);
end;

procedure TBoldIndirectMultiLinkController.LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
var
  NewReferredLocator: TBoldObjectLocator;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  if LinkLocatorList.LocatorInList[NewLocator] then // locator already in list so just exit, this used to be an assert - Daniel
  begin
    exit;
  end;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningList, 'Linkto', '');

    PreChange;
    NewReferredLocator := GetLinkObjectOtherLinkController(NewLocator.EnsuredBoldObject).fLocator;
    LinkLocatorList.Add(NewLocator);
    ReferredLocatorList.Add(NewReferredLocator);
    if updateOrderNo and RoleRTInfo.IsOrdered then
      GetLinkObjectOwnLinkController(NewLocator.BoldObject).SetAndModifyOrderNo(LinkLocatorList.IndexOf(NewLocator)); //TODO - This could have side effect when mode is blulMarkAdjusted
    if Mode = blulMarkAdjusted then
      OwningObjectList.Adjusted := True;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beItemAdded, [NewReferredLocator]);
    GetLinkObjectListController.Changed(beItemAdded, [NewLocator]);
    BoldSystem.CommitTransaction();
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldIndirectMultiLinkController.Move(CurrentIndex, NewIndex: Integer);
begin
  EnsureOrder;
//  if not RoleRTInfo.IsOrdered then
//    exit;
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if not StartModify then
      BoldRaiseLastFailure(OwningList, 'Move', '');

    PreChange;
    LinkLocatorList.Move(CurrentIndex, NewIndex);
    ReferredLocatorList.Move(CurrentIndex, NewIndex);
    ReOrder;
    EndModify;
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
  Changed(beOrderChanged, []);
  GetLinkObjectListController. Changed(beOrderChanged, []);
end;

function TBoldIndirectMultiLinkController.NewLink(OtherLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode): TBoldObject;
{$IFDEF ReuseDeletedLinkObjectOnRelink}
  function FindLinkInOldValues: TBoldObject;
  var
    s: IBoldObjectIdListRefPair;
    i: integer;
  begin
    result := nil;
    if Supports(OwningObjectList.OldValue, IBoldObjectIdListRefPair, s) then
    for i := 0 to s.Count-1 do
    begin
      if s.IdList2[i].IsEqual[OtherLocator.BoldObjectID] then
      begin
        result := BoldSystem.Locators.ObjectByID[s.IdList1[i]];
        Assert(result is RoleRTInfo.LinkClassTypeInfo.ObjectClass);
        result.AsIBoldObjectContents[bdepContents].BoldExistenceState := besExisting;
        result.AsIBoldObjectContents[bdepContents].BoldPersistenceState := bvpsCurrent;
        exit;
      end;
    end;
  end;
{$ENDIF}
var
  LinkObject: TBoldObject;
  LinkClassTypeInfo: TBoldClassTypeInfo;
  OtherEndController: TBoldAbstractController;
begin
  LinkClassTypeInfo := RoleRTInfo.LinkClassTypeInfo;
{$IFDEF ReuseDeletedLinkObjectOnRelink}
  LinkObject := FindLinkInOldValues;
  if not Assigned(LinkObject) then
{$ENDIF}
    LinkObject := TBoldObjectClass(LinkClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(LinkClassTypeInfo, BoldSystem,
    OtherLocator.ObjectIsPersistent and OwningObjectList.OwningObject.BoldPersistent);
  OtherEndController := GetLinkObjectOtherLinkController(LinkObject).GetOtherEndController(OtherLocator, true);
  GetLinkObjectOwnLinkController(LinkObject).LinkTo(OwningObjectList.OwningObject.BoldObjectLocator, true, Mode);
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
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', '');

    PreChange;
    Locator := ReferredLocatorList[index];
    LinkLocator := LinkLocatorList[index];
    BoldSystem.DelayObjectDestruction;
    try
      DeleteLink(LinkLocator, blulMarkModified);
      LinkLocatorList.RemoveByIndex(index);
      ReferredLocatorList.RemoveByIndex(index);
    finally
      BoldSystem.AllowObjectDestruction;
    end;
    ReOrder;
    EndModify;
    Changed(beItemDeleted, [Locator]);
    GetLinkObjectListController.Changed(beItemDeleted, [LinkLocator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldIndirectMultiLinkController.ReOrder;
var
{$IFOPT C+}
  index: Integer;
{$ENDIF}
  I: Integer;
  Locator: TBoldObjectLocator;
begin
  if RoleRTInfo.IsOrdered then
  begin
{$IFOPT C+}
    index := RoleRTInfo.IndexOfOtherEnd;
    Assert(index <> -1);
{$ENDIF}
    for I := 0 to LinkLocatorList.Count - 1 do
    begin
      Locator := LinkLocatorList.Locators[I];
      Locator.EnsureBoldObject;
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
  function AdjustLists: Boolean;
  var
    I: integer;
    AObject: TBoldObject;
    BoldLinkClassTypeInfo: TBoldClassTypeInfo;
    IndexOfOtherEnd, IndexOfOwnEnd,
    EmbeddedIndexOfOtherEnd, EmbeddedIndexOfOwnEnd: Integer;
    LinkObjectLocator: TBoldObjectLocator;
    BoldSystem: TBoldSystem;
  begin
    Result := False;
    BoldLinkClassTypeInfo := RoleRTInfo.LinkClassTypeInfo;
    IndexOfOwnEnd := RoleRTInfo.OwnIndexInLinkClass;
    IndexOfOtherEnd := RoleRTInfo.OtherIndexInLinkClass;
    BoldSystem := self.BoldSystem;
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
      else {object not loaded, set embedded links}
      begin
        LinkObjectLocator := AssertedLocatorForId(NewListOfLinkObjects[i]);
        EmbeddedIndexOfOwnEnd := RoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOwnEnd].EmbeddedLinkIndex;
        EmbeddedIndexOfOtherEnd := RoleRTInfo.LinkClassTypeInfo.AllMembers[IndexOfOtherEnd].EmbeddedLinkIndex;
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
  if (mode = bdepPMIn) and (OwningObjectList.OwningObject.IsHistoricVersion) then
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
  PreserveOrder := (mode = bdepContents) or ((mode = bdepPMIn) and RoleRTInfo.IsOrdered and not WasAdjusted);

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
      else if ReferredLocatorList.IndexOf(ObjectLocator) <> -1 then
      begin
        PreChangeIfNeeded;
        LinkLocatorList.Move(ReferredLocatorList.IndexOf(ObjectLocator), I);
        ReferredLocatorList.Move(ReferredLocatorList.IndexOf(ObjectLocator), I);
        Changed(beOrderChanged, []);
        GetLinkObjectListController.Changed(beOrderChanged, []);
      end
      else
      begin
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
      BoldRaiseLastFailure(OwningList, 'SetLocator', '');
    OldLinkLocator := LinkLocatorList[index];
    PreChange;
    LinkLocatorList[index] := NewLink(Locator, blulMarkModified).BoldObjectLocator;
    ReferredLocatorList[index] := Locator;
    DeleteLink(OldLinkLocator, blulMarkModified);
    EndModify;
    Changed(beItemReplaced, [Locator, Index]);
    GetLinkObjectListController.Changed(beItemReplaced, [OldLinkLocator, Index]);
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

function TBoldIndirectMultiLinkController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSObjectIdListRefPair;
end;

procedure TBoldIndirectMultiLinkController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
var
  OldIndex: Integer;
  OldLinkLocator: TBoldObjectLocator;
begin
  Assert((Mode <> blulMarkAdjusted) or (OwningList.BoldPersistenceState = bvpsCurrent), OwningMember.DisplayName);
  if not LinkLocatorList.LocatorInList[OldLocator] then
  begin
    BoldLog.LogFmt('TBoldIndirectMultiLinkController.Unlink: Locator %s (%s) not found in %s (%s) ',
      [OldLocator.AsString, OldLocator.EnsuredBoldObject.DisplayName,
      OwningMember.OwningObject.BoldObjectLocator.AsString, OwningObjectList.DisplayName]);
    exit;
  end;
  Assert(LinkLocatorList.LocatorInList[OldLocator]);
  BoldSystem.StartTransaction;
  try
    BoldClearLastFailure;
    if Mode = blulMarkModified then
      if not StartModify then
        BoldRaiseLastFailure(OwningList, 'Unlink', '');

    PreChange;
    OldIndex := LinkLocatorList.IndexOf(OldLocator);
    LinkLocatorList.Remove(OldLocator);
    OldLinkLocator := ReferredLocatorList[oldIndex];
    ReferredLocatorList.RemoveByIndex(OldIndex);
    if Mode = blulMarkAdjusted  then
      OwningObjectList.Adjusted := True;
    if Mode = blulMarkModified then
      EndModify;
    Changed(beItemDeleted, [OldLocator]);
    GetLinkObjectListController.Changed(beItemDeleted, [OldLinkLocator]);
    BoldSystem.CommitTransaction;
  except
    BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldIndirectMultiLinkController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRefPair) then
  begin
    result := GetProxy(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRefPair', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldIndirectMultiLinkController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBoldIndirectMultiLinkController_Proxy.MakeProxy(Member, Mode);
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
  LinkList := OwningList.OwningObject.BoldMembers[RoleRTInfo.IndexOfLinkObjectRole] as TBoldObjectLIst;
  LinkLIst.EnsureObjects;
end;

procedure TBoldIndirectMultiLinkController.Exchange(Index1, Index2: integer);
begin
  LinkLocatorList.Exchange(Index1, Index2);
  ReferredLocatorList.Exchange(Index1, Index2);
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
  if RoleRTInfo.IsOrdered then
  begin
    i := 0;
    CurrentObj := nil;
    while result and (i < LinkLocatorList.Count - 1) do
    begin
      if Assigned(LinkLocatorList[i].BoldObject) then
      begin
        PreviousObj := CurrentObj;
        CurrentObj := LinkLocatorList[i].BoldObject;

        if Assigned(PreviousObj) then
          Result := GetLinkObjectOwnLinkController(PreviousObj).GetOrderNo <= GetLinkObjectOwnLinkController(CurrentObj).GetOrderNo;
      end;
      Inc(i);
    end;
  end;
end;

function TBoldIndirectMultiLinkController.CompareOrderNo(Index1, Index2: integer): integer;
var
  OrderNo1, OrderNo2: integer;
begin
  if RoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole then
  begin
    OrderNo1 := GetLinkObjectOwnLinkController(LinkLocatorList[Index1].BoldObject).GetOrderNo;
    OrderNo2 := GetLinkObjectOwnLinkController(LinkLocatorList[Index2].BoldObject).GetOrderNo;
    Result := (OrderNo1 - OrderNo2);
  end
  else
    raise EBold.Create(sOtherEndMustBeSingle);
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
//      Assert(GetLinkObjectOwnLinkController(LinkLocatorList[i].BoldObject).AssertIntegrity);
//      Assert(GetLinkObjectOtherLinkController(LinkLocatorList[i].BoldObject).AssertIntegrity);
    end
    else
    begin
    end;
  end;
  Result := True;
end;

procedure TBoldIndirectMultiLinkController.ClearNoLongerReferring(
  NewList: TBoldObjectIdList);
begin
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
      BoldRaiseLastFailure(OwningList, 'RemoveByIndex', '');

    PreChange;
    BoldSystem.DelayObjectDestruction;
    try
      while count > 0 do
      begin
        ix := Count-1;
        Locator := ReferredLocatorList[ix];
        LinkLocator := LinkLocatorList[ix];
        DeleteLink(LinkLocator, blulMarkModified);
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
  LinkMember := OwningList.OwningObject.BoldMembers[RoleRTInfo.Index+1];
  result := GetControllerForMember(LinkMember) as TBoldAbstractObjectListController;
end;

function TBoldMultiLinkController.CreateNew: TBoldElement;
begin
  result := BoldSystem.CreateNewObjectFromClassTypeInfo(RoleRTInfo.ClassTypeInfoOfOtherEnd);
end;

function TBoldMultiLinkController.GetCanCreateNew: Boolean;
begin
  result := not RoleRTInfo.ClassTypeInfoOfOtherEnd.IsAbstract;
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
  else
  begin
    EmbeddedIndex := RoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
    if (EmbeddedIndex <> -1) then
      if Locator.EmbeddedSingleLinks[EmbeddedIndex] <> nil then
      begin
        Assert(Locator.EmbeddedSingleLinks[EmbeddedIndex] = oldLocator);
        Locator.EmbeddedSingleLinks[EmbeddedIndex] := nil;
      end;
  end;
end;

procedure TBoldDirectMultiLinkController.SingleLinkLinkTo(Locator,
  NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode; aOrderNo: integer);
var
  aObject: TBoldObject;
  EmbeddedIndex: integer;
begin
  aObject := Locator.BoldObject;
  if Assigned(AObject) then
     GetOtherEndController(Locator).LinkTo(NewLocator, UpdateOrderNo, Mode)
  else
  begin
    EmbeddedIndex := RoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex;
    Assert(EmbeddedIndex <> -1);
    //Assert(Locator.EmbeddedSingleLinks[EmbeddedIndex] = nil);
    Locator.EmbeddedSingleLinks[EmbeddedIndex] := NewLocator;
  end;
end;

procedure TBoldMultiLinkController.MarkPossiblyOutOfOrder;
begin
  if RoleRTInfo.IsOrdered then
    fMayBeOutOfOrder := True;
end;

{ TBoldLinkObjectSingleLinkController }
function TBoldLinkObjectSingleLinkController.AssertIntegrity: Boolean;
begin
  result := Assigned(self.fLocator);
  if OwningReference.BoldRoleRtInfo.IndexOfMainRole <> -1 then
  begin
    self.ControllerForMainRole.AssertIntegrity;
    self.ControllerForLinkRole.AssertIntegrity;
  end;
end;

function TBoldLinkObjectSingleLinkController.OtherInnerLinkController: TBoldLinkObjectSingleLinkController;
begin
  result := GetControllerForMember(OwningReference.OwningObject.BoldMembers[
              RoleRTInfo.RoleRTInfoOfOtherEnd.OtherIndexInLinkClass]) as TBoldLinkObjectSingleLinkController;
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




  OtherInnerLink := OtherInnerLinkController;
  if assigned(OtherInnerLink.fLocator) then
  begin
    OtherOuterLink := OtherInnerLink.GetOtherEndController(OtherInnerLink.fLocator, false);
    if assigned(OtherOuterLink) then
    begin
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
function TBoldLinkObjectListController.AssertIntegrity: Boolean;
begin
  result := true;
  Assert(self.GetLocatorList.Count = self.GetMainList.Count);
  Assert(count = self.GetMainListController.Count);
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
  raise EBoldFeatureNotImplementedYet.CreateFmt('%s.GetLocatorByQualifiersAndSubscribe', [ClassName]);
end;

function TBoldLinkObjectListController.GetMainListController: TBOldIndirectMultiLinkController;
var
  MainMember: TBoldMember;
begin
  MainMember := OwningList.OwningObject.BoldMembers[RoleRTInfo.IndexOfMainRole];
  result := GetControllerForMember(MainMember) as TBOldIndirectMultiLinkController;
end;

function TBoldLinkObjectListController.GetLocatorList: TBoldObjectLocatorList;
begin
  MakeDbCurrent; //PATCH  This solves problem with invalidation of main role not propagating to link role
  result := GetMainListController.LinkLocatorList;
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

function TBoldLinkObjectListController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [Classname, 'GetFreeStandingClass']); // do not localize
end;

function TBoldLinkObjectListController.GetMainList: TBoldObjectList;
begin
  result := OwningList.OwningObject.BoldMembers[RoleRTInfo.IndexOfMainRole] as TBoldObjectList;
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

function TBoldLinkObjectListController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  raise EBoldInternal.Create('Can''t access Link Object List directly');
end;

{ TBoldLinkObjectReferenceController }


function TBoldLinkObjectReferenceController.GetLocator: TBoldObjectLocator;
begin
  MakeDbCurrent; //PATCH  This solves problem with invalidation of main role not propagating to link role
  result := (ControllerForMainRole as TBoldIndirectSingleLinkController).fLinkObjectLocator;
end;

function TBoldLinkObjectReferenceController.GetStreamName: string;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [Classname, 'GetStreamName']); // do not localize
end;

function TBoldLinkObjectReferenceController.AssertIntegrity: Boolean;
begin
  result := true;
//  if GetLocator <> nil then
//  ControllerForMainRole.AssertIntegrity;
end;

function TBoldLinkObjectReferenceController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  raise EBold.CreateFmt('%s.GetFreeStandingClass: Abstract error', [Classname]);
end;

procedure TBoldLinkObjectReferenceController.MakeDbCurrent;
begin
  ControllerForMainRole.OwningReference.EnsureContentsCurrent;
  OwningReference.BoldPersistenceState := bvpsCurrent;
end;

function TBoldLinkObjectReferenceController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
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

{ TBoldUnOrderedDirectSingleLinkController }

function TBoldUnOrderedDirectSingleLinkController.GetOrderNo: Integer;
begin
  Result := -1;
end;

procedure TBoldUnOrderedDirectSingleLinkController.SetAndModifyOrderNo(
  index: Integer);
begin
  if OrderNo <> -1 then
    raise EBoldInternal.Create('Orderno must -1 on an unordered role');
end;

procedure TBoldUnOrderedDirectSingleLinkController.SetOrderNo(
  NewOrderNo: Integer; Mode: TBoldDomainElementProxyMode);
begin
  if OrderNo <> -1 then
    raise EBoldInternal.Create('Orderno must -1 on an unordered role');
end;

end.

