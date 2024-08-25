{ Global compiler directives }
{$include bold.inc}
unit BoldIndexableLoadedObjectsHandle;

interface

uses
  BoldSubscription,
  BoldHandle,
  BoldIndexableLoadedObjectsList,
  BoldHandles,
  BoldSystem,
  BoldSystemRT,
  BoldHashIndexes;

type
  TBoldIndexableLoadedObjectsHandle = class(TBoldSystemExtensionComponent)
  private
    fBoldIndexableLoadedObjectsList: TBoldIndexableLoadedObjectsList;
  protected
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    function GetHandledObject: TObject; override;
  public
    destructor Destroy; override;
    procedure InitializeIndex(AClass: TBoldClassTypeInfo; const IndexedMembers: array of TBoldMemberRTInfo; AStringCompareMode: TBoldStringCompareMode = bscCaseDependent);
    procedure InitializeIndexByNames(AClass: TBoldObjectClass; const IndexedMembers: array of string; AStringCompareMode: TBoldStringCompareMode = bscCaseDependent);
    function GetObjectByMemberValues(const value: array of const): TBoldObject;
    property BoldIndexableLoadedObjectsList: TBoldIndexableLoadedObjectsList read fBoldIndexableLoadedObjectsList;
  end;

implementation

uses
  SysUtils;

{ TBoldIndexableLoadedObjectsHandle }

destructor TBoldIndexableLoadedObjectsHandle.destroy;
begin
  FreeAndNil(fBoldIndexableLoadedObjectsList);
  inherited;
end;

function TBoldIndexableLoadedObjectsHandle.GetObjectByMemberValues(
  const value: array of const): TBoldObject;
begin
  if not Assigned(fBoldIndexableLoadedObjectsList) then
    raise Exception.Create(className + ': InitializeIndex has not been called.');
  result := fBoldIndexableLoadedObjectsList.GetObjectByMemberValues(Value);
end;

function TBoldIndexableLoadedObjectsHandle.GetHandledObject: TObject;
begin
  result := fBoldIndexableLoadedObjectsList;
end;

procedure TBoldIndexableLoadedObjectsHandle.InitializeIndex(
  AClass: TBoldClassTypeInfo; const IndexedMembers: array of TBoldMemberRTInfo;
  AStringCompareMode: TBoldStringCompareMode);
begin
  if not Assigned(StaticSystemHandle) then
    raise Exception.Create(className + ': StaticSystemHandle has not been set.');
  if not Assigned(StaticSystemHandle.System) then
    raise Exception.Create(className + ': StaticSystemHandle not active.');
  fBoldIndexableLoadedObjectsList.free;
  fBoldIndexableLoadedObjectsList := TBoldIndexableLoadedObjectsList.Create(IndexedMembers, AClass, BoldSystem, AStringCompareMode);
end;

procedure TBoldIndexableLoadedObjectsHandle.InitializeIndexByNames(
  AClass: TBoldObjectClass; const IndexedMembers: array of string; AStringCompareMode: TBoldStringCompareMode);
begin
  if not Assigned(AClass) then
    raise Exception.Create('Class has to be specified.');
  fBoldIndexableLoadedObjectsList.free;
  fBoldIndexableLoadedObjectsList := TBoldIndexableLoadedObjectsList.Create(IndexedMembers, AClass, BoldSystem, AStringCompareMode);
end;

procedure TBoldIndexableLoadedObjectsHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if (Originator = StaticSystemHandle) and (OriginalEvent = beDestroying) then
    FreeAndNil(fBoldIndexableLoadedObjectsList);
  inherited;
end;

end.

