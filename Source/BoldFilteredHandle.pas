{ Global compiler directives }
{$include bold.inc}
unit BoldFilteredHandle;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldRootedHandles;

type
  { forward declaration of classes }
  TBoldFilter = class;
  TBoldFilteredHandle = class;

  { function prototypes }
  TBoldElementFilter = function (Element: TBoldElement): Boolean of object;

  { TBoldFilter }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldFilter = class(TBoldSubscribableComponentViaBoldElem)
  private
    FOnFilter: TBoldElementFilter;
    FOnSubscribe: TBoldElementSubscribe;
    fPreFetchRoles: TStringList;
    function GetPreFetchRoles: TStrings;
    procedure SetPreFetchRoles(const Value: TStrings);
    function StorePreFetchRoles: Boolean;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Subscribe(boldElement: TBoldElement; Subscriber: TBoldSubscriber); virtual;
    function Filter(Element: TBoldElement): Boolean; virtual;
    procedure FilterList(List: TBoldList);
  published
    property PreFetchRoles: TStrings read GetPreFetchRoles write SetPreFetchRoles stored StorePreFetchRoles;
    property OnSubscribe: TBoldElementSubscribe read FOnSubscribe write FOnSubscribe;
    property OnFilter: TBoldElementFilter read FOnFilter write FOnFilter;
  end;

  { TBoldFilteredHandle }
  TBoldFilteredHandle = class(TBoldRootedHandle)
  private
    FBoldFilter: TBoldFilter;
    procedure SetBoldFilter(NewValue: TBoldFilter);
  protected
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
  public
    property BoldFilter: TBoldFilter read FBoldFilter write SetBoldFilter;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  {$IFDEF ATTRACS}
  AttracsPerformance,
  AttracsDefs,
  AttracsTraceLog,
  {$ENDIF}
  BoldSystemRT;

{---TBoldFilter---}

constructor TBoldFilter.Create(owner: TComponent);
begin
  inherited;
  fPreFetchRoles := TStringList.Create;
end;

destructor TBoldFilter.Destroy;
begin
  FreeAndNil(fPreFetchRoles);
  inherited;                 
end;

function TBoldFilter.Filter(Element: TBoldElement): Boolean;
begin
  Result := True;
  if Assigned(FOnFilter) then
  begin
    Result := OnFilter(Element);
  end;
end;

procedure TBoldFilter.FilterList(List: TBoldList);
var
  i: integer;
begin
  for i := List.Count - 1 downto 0 do
    if not Filter(List[i]) then
      List.RemoveByIndex(i);
end;

function TBoldFilter.GetPreFetchRoles: TStrings;
begin
  result := fPrefetchRoles;
end;

procedure TBoldFilter.SetPreFetchRoles(const Value: TStrings);
begin
  fPrefetchRoles.Assign(value);
end;

function TBoldFilter.StorePreFetchRoles: Boolean;
begin
  result := PreFetchRoles.Count <> 0; 
end;

procedure TBoldFilter.Subscribe(boldElement: TBoldElement; Subscriber: TBoldSubscriber);
begin
  if Assigned(FOnSubscribe) then
    OnSubscribe(boldElement, Subscriber)
end;

{ TBoldFilteredHandle }

procedure TBoldFilteredHandle.DeriveAndSubscribe(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);
var
  I: Integer;
  ValueAsListHolder: TBoldIndirectElement;
  SourceList: TBoldList;
  NewList: TBoldList;
  ListTypeInfo: TBoldListTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  {$IFDEF ATTRACS}
  PerformanceMeasurement : TPerformanceMeasurement;
  HandleLongName : String;
  {$ENDIF}

begin
  if csDestroying in ComponentState then
    raise EBold.CreateFmt('%s.DeriveAndSubscribe: %s Handle is in csDestroying state, can not DeriveAndSubscribe.', [classname, name]);
  {$IFDEF ATTRACS}
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
  {$ENDIF}
  if EffectiveRootValue = nil then
    ResultElement.SetOwnedValue(nil)
  else if not Assigned(BoldFilter) then
    EffectiveRootValue.GetAsList(ResultElement)
  else
  begin
    ValueAsListHolder := TBoldIndirectElement.Create;
    try
      EffectiveRootValue.GetAsList(ValueAsListHolder);
      SourceList := TBoldList(ValueAsListHolder.Value);
      NewList := TBoldMemberfactory.CreateMemberFromBoldType(SourceList.BoldType) as TBoldList;
      NewList.DuplicateMode := bldmAllow;
      if Assigned(Subscriber) then
        SourceList.DefaultSubscribe(Subscriber, breResubscribe);
      if (SourceList.Count > 0) and (SourceLIst is TBoldObjectList) then
      begin
        (SourceList as TBoldObjectList).EnsureObjects;
        ListTypeInfo := SourceLIst.BoldType as TBoldListTypeInfo;
        ClasstypeInfo := ListTypeInfo.ListElementTypeInfo as TBoldClassTypeInfo;
        for i := 0 to BoldFilter.PreFetchRoles.Count - 1 do
        begin
          MemberRTInfo := ClassTypeInfo.MemberRTInfoByExpressionName[BoldFilter.PreFetchRoles[i]];
          if MemberRTInfo is TBoldRoleRTInfo then
            (SourceList as TBoldObjectList)[0].BoldSystem.FetchLinksWithObjects(SourceList as TBoldObjectList, BoldFilter.PreFetchRoles[i]);
        end;
      end;
      for i := 0 to SourceList.Count - 1 do
      begin
        if BoldFilter.Filter(SourceList[i]) then
          NewList.Add(SourceList[i]);
        if Assigned(Subscriber) then
          BoldFilter.Subscribe(SourceList[I], Subscriber);
      end;
      NewList.MakeImmutable;
      ResultElement.SetOwnedValue(NewList);
    finally
      ValueAsListHolder.Free;
    end;
  end;
  SubscribeToValue;
  {$IFDEF ATTRACS}
  if not PerformanceMeasurement.AcceptableTimeForSmallComputation then
  begin
    if Assigned(Self.Owner) then
      HandleLongName := Owner.Name + '.' + Self.Name
    else
      HandleLongName := Self.Name;

    PerformanceMeasurement.WhatMeasured := 'Deriving TBoldFilteredHandle ' + HandleLongName;
    PerformanceMeasurement.Trace;
  end;
  {$ENDIF}
end;

function TBoldFilteredHandle.GetStaticBoldType: TBoldElementTypeInfo;
var
  SRType: TBoldElementTypeInfo;
begin
  SRType := StaticRootType;
  if not Assigned(SrType) then
    result := nil
  else if SRType is TBoldListTypeInfo then
    Result := SRType
  else
    Result := TBoldSystemTypeInfo(SRType.SystemTypeInfo).ListTypeInfoByElement[SRType];
end;

procedure TBoldFilteredHandle.SetBoldFilter(NewValue: TBoldFilter);
begin
  if NewValue <> fBoldFilter then
  begin
    FBoldFilter := NewValue;
    MarkSubscriptionOutOfdate;
  end;
end;

end.