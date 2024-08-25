{ Global compiler directives }
{$include bold.inc}
unit BoldSortedHandle;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldRootedHandles;

type
  { forward declaration of classes }
  TBoldComparer = class;
  TBoldSortedHandle = class;

 { TBoldComparer }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldComparer = class(TBoldSubscribableComponentViaBoldElem)
  private
    FOnCompare: TBoldElementCompare;
    FOnSubscribe: TBoldElementSubscribe;
    fOnPrepareSort: TBoldPrepareListOperation;
    FOnFinishSort: TBoldPrepareListOperation;
  public
    procedure Subscribe(boldElement: TBoldElement; Subscriber: TBoldSubscriber); virtual;
    function Compare(Item1, Item2: TBoldElement): Integer; virtual;
    procedure SortList(List: TBoldList);
    procedure PrepareSort(List: TBoldList);
    procedure FinishSort(List: TBoldList);
  published
    property OnSubscribe: TBoldElementSubscribe read FOnSubscribe write FOnSubscribe;
    property OnCompare: TBoldElementCompare read FOnCompare write FOnCompare;
    property OnPrepareSort: TBoldPrepareListOperation read fOnPrepareSort write fOnPrepareSort;
    property OnFinishSort: TBoldPrepareListOperation read FOnFinishSort write
        FOnFinishSort;
  end;

  { TBoldSortedHandle }
  TBoldSortedHandle = class(TBoldRootedHandle)
  private
    FBoldComparer: TBoldComparer;
    procedure SetBoldComparer(NewValue: TBoldComparer);
  protected
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
  public
    property BoldComparer: TBoldComparer read FBoldComparer write SetBoldComparer;
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
  BoldSystemRT,
  BoldElementList;


{---TBoldComparer---}
procedure TBoldComparer.Subscribe(boldElement: TBoldElement; Subscriber: TBoldSubscriber);
begin
  if Assigned(FOnSubscribe) then
    OnSubscribe(boldElement, Subscriber)
end;

function TBoldComparer.Compare(Item1, Item2: TBoldElement): Integer;
begin
  if Assigned(FOnCompare) then
    Result := OnCompare(Item1, Item2)
  else
    Result := 0;
end;

{ TBoldSortedHandle }

procedure TBoldSortedHandle.DeriveAndSubscribe(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);
var
  I: Integer;
  ValueAsListHolder: TBoldIndirectElement;
  SourceList: TBoldList;
  NewList: TBoldList;
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
  else if not Assigned(BoldComparer) then
    EffectiveRootValue.GetAsList(ResultElement)
  else
  begin
    ValueAsListHolder := TBoldIndirectElement.Create;
    try
      EffectiveRootValue.GetAsList(ValueAsListHolder);
      SourceList := TBoldList(ValueAsListHolder.Value);
      if (SourceList.BoldType is TBoldListTypeInfo) and not Assigned(TBoldListTypeInfo(SourceList.BoldType).ListElementTypeInfo) then
        NewList := TBoldElementList.CreateWithTypeInfo(SourceList.BoldType)
      else
        NewList := TBoldMemberFactory.CreateMemberFromBoldType(SourceList.BoldType) as TBoldList;
      NewList.DuplicateMode := bldmAllow;

      if Assigned(Subscriber) then
        SourceList.DefaultSubscribe(Subscriber, breResubscribe);

      SourceList.EnsureRange(0, SourceList.Count - 1);
      BoldComparer.PrepareSort(SourceList);
      try
        for i := 0 to SourceList.Count - 1 do
        begin
          NewList.Add(SourceList[i]);
          if Assigned(Subscriber) then
            BoldComparer.Subscribe(SourceList[I], Subscriber);
        end;
        NewList.Sort(BoldComparer.Compare);
        NewList.MakeImmutable;
        ResultElement.SetOwnedValue(NewList);
      finally
        BoldComparer.FinishSort(NewList);
      end;
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
    PerformanceMeasurement.WhatMeasured := 'Deriving TBoldSortedHandle ' +  HandleLongName;
    PerformanceMeasurement.Trace;
  end;
  {$ENDIF}
end;

function TBoldSortedHandle.GetStaticBoldType: TBoldElementTypeInfo;
var
  SRType: TBoldElementTypeInfo;
begin
  SRType := StaticRootType;
  if not Assigned(SRType) then
    Result := nil
  else if SRType is TBoldListTypeInfo then
    Result := SRType
  else
    Result := TBoldSystemTypeInfo (SRType.SystemTypeInfo).ListTypeInfoByElement[SRType];
end;

procedure TBoldSortedHandle.SetBoldComparer(NewValue: TBoldComparer);
begin
  if NewValue <> fBoldComparer then
  begin
    FBoldComparer := NewValue;
    MarkSubscriptionOutOfdate;
  end;
end;

procedure TBoldComparer.SortList(List: TBoldList);
begin
  if (List is TBoldObjectList) then
    (List as TBoldObjectLIst).Sort(Compare);
end;

procedure TBoldComparer.PrepareSort(List: TBoldList);
begin
  if assigned(OnPrepareSort) then
    OnPrepareSort(list);
end;

procedure TBoldComparer.FinishSort(List: TBoldList);
begin
  if assigned(OnFinishSort) then
    OnFinishSort(list);
end;

end.