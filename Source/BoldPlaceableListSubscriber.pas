unit BoldPlaceableListSubscriber;

interface

uses
  Classes,
  BoldListListControlPack,
  BoldStringControlPack,
  BoldControlPack,
  BoldListHandleFollower,
  BoldElements,
  BoldAbstractListHandle,
  BoldSubscription,
  BoldSystem;

type
  TBoldSubscribeEvent = procedure (Subscriber: TBoldSubscriber; Follower: TBoldFollower) of object;
  TBoldReceiveEvent = procedure (Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const) of object;

  [ComponentPlatforms(pidWin32 or pidWin64)]
  TCustomBoldPlaceableListSubscriber = class(TComponent)
  private
    fBoldRowProperties: TBoldStringFollowerController;
    fBoldProperties: TBoldListAsFollowerListController;
    fHandleFollower: TBoldListHandleFollower;
    fAfterMakeUptoDate: TBoldFollowerEvent;
    fBeforeMakeUptoDate: TBoldFollowerEvent;
    fOnRemoveItem: TBoldSubFollowerEvent;
    fOnAddItem: TBoldSubFollowerEvent;
    fOnRowAfterMakeUptoDate: TBoldFollowerEvent;
    fOnSubscribeToElement: TBoldSubscribeEvent;
    fOnReceive: TBoldReceiveEvent;
    fElementSubscriber: TBoldExtendedPassthroughSubscriber;
    fOnBeforeDelete: TBoldFollowerEvent;
    fOnReplaceItem: TBoldSubFollowerEvent;
    function GetBoldHandle: TBoldAbstractListHandle;
    function GetBoldList: TBoldList;
    function GetFollower: TBoldFollower;
    procedure SetBoldHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldProperties(
      const Value: TBoldListAsFollowerListController);
    procedure SetRowProperties(const Value: TBoldStringFollowerController);
  protected
    function GetContextType: TBoldElementTypeInfo;
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    procedure _InsertItem(index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _ReplaceItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _BeforeDeleteItem(index: Integer; Follower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    property ElementSubscriber: TBoldExtendedPassthroughSubscriber read fElementSubscriber;
    property Follower: TBoldFollower read GetFollower;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldList: TBoldList read GetBoldList;
    property BoldProperties: TBoldListAsFollowerListController read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;
    property OnBeforeMakeUptoDate: TBoldFollowerEvent read fBeforeMakeUptoDate write fBeforeMakeUptoDate;
    property OnAfterMakeUptoDate: TBoldFollowerEvent read fAfterMakeUptoDate write fAfterMakeUptoDate;
    property OnAddItem: TBoldSubFollowerEvent read fOnAddItem write fOnAddItem;
    property OnRemoveItem: TBoldSubFollowerEvent read fOnRemoveItem write fOnRemoveItem;
    property OnBeforeDelete: TBoldFollowerEvent read fOnBeforeDelete write fOnBeforeDelete;
    property OnReplaceItem: TBoldSubFollowerEvent read fOnReplaceItem write fOnReplaceItem;
    property OnRowAfterMakeUptoDate: TBoldFollowerEvent read fOnRowAfterMakeUptoDate write fOnRowAfterMakeUptoDate;
    property OnSubscribeToElement: TBoldSubscribeEvent read fOnSubscribeToElement write fOnSubscribeToElement;
    property OnReceive: TBoldReceiveEvent read fOnReceive write fOnReceive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBoldPlaceableListSubscriber = class(TCustomBoldPlaceableListSubscriber)
  public
    property BoldList;
    property Follower;
    property ElementSubscriber;    
  published
    property BoldHandle;
    property BoldProperties;
    property BoldRowProperties;

    property OnBeforeMakeUptoDate;
    property OnAfterMakeUptoDate;
    property OnAddItem;
    property OnRemoveItem;
    property OnBeforeDelete;
    property OnReplaceItem;
    property OnSubscribeToElement;
    property OnReceive;
  end;

procedure Register;

implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterComponents('Bold Controls', [TBoldPlaceableListSubscriber]);
end;

{ TCustomBoldPlaceableListSubscriber }

constructor TCustomBoldPlaceableListSubscriber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fElementSubscriber:= TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(_Receive);
  fBoldRowProperties := TBoldStringFollowerController.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldListAsFollowerListController.Create(Self, fBoldRowProperties);
  fBoldProperties.OnAfterInsertItem  := _InsertItem;
  fBoldProperties.OnAfterDeleteItem  := _DeleteItem;
  fBoldProperties.OnReplaceItem  := _ReplaceItem;
  fBoldProperties.OnBeforeDeleteItem := _BeforeDeleteItem;
  fBoldProperties.BeforeMakeUptoDate := _BeforeMakeUptoDate;
  fBoldProperties.AfterMakeUptoDate  := _AfterMakeUptoDate;
  fHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldProperties);
end;

destructor TCustomBoldPlaceableListSubscriber.Destroy;
begin
  FreeAndNil(fElementSubscriber);
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited Destroy;
end;

procedure TCustomBoldPlaceableListSubscriber._BeforeMakeUptoDate(
  Follower: TBoldFollower);
begin
//  if assigned(BoldHandle) and assigned(Boldhandle.list) then
//    BoldHandle.list.EnsureRange(0, BoldHandle.list.Count-1);
  if assigned(fBeforeMakeUptoDate) then
    fBeforeMakeUptoDate(Follower);
end;

procedure TCustomBoldPlaceableListSubscriber._AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
  if assigned(fAfterMakeUptoDate) then
    fAfterMakeUptoDate(Follower);
end;

procedure TCustomBoldPlaceableListSubscriber._InsertItem(
  index: Integer; Follower: TBoldFollower);
begin
  if assigned(fOnAddItem) then
    fOnAddItem(index, Follower);
  if assigned(fOnSubscribeToElement) and Assigned(Follower) then
    fOnSubscribeToElement(ElementSubscriber, Follower);
end;

procedure TCustomBoldPlaceableListSubscriber._BeforeDeleteItem(
  index: Integer; Follower: TBoldFollower);
begin
  if assigned(fOnBeforeDelete) then
    fOnBeforeDelete(Follower);
end;

procedure TCustomBoldPlaceableListSubscriber._ReplaceItem(index: Integer;
  OwningFollower: TBoldFollower);
begin
  if assigned(fOnReplaceItem) then
    fOnReplaceItem(index, OwningFollower);
end;

procedure TCustomBoldPlaceableListSubscriber._DeleteItem(index: Integer;
  OwningFollower: TBoldFollower);
begin
  if assigned(fOnRemoveItem) then
    fOnRemoveItem(index, OwningFollower);
end;

procedure TCustomBoldPlaceableListSubscriber._RowAfterMakeUptoDate(
  Follower: TBoldFollower);
begin
  if Assigned(fOnRowAfterMakeUptoDate) then
    fOnRowAfterMakeUptoDate(Follower);
end;

function TCustomBoldPlaceableListSubscriber.GetBoldHandle: TBoldAbstractListHandle;
begin
 Result := fHandleFollower.BoldHandle;
end;

function TCustomBoldPlaceableListSubscriber.GetBoldList: TBoldList;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

function TCustomBoldPlaceableListSubscriber.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TCustomBoldPlaceableListSubscriber.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

procedure TCustomBoldPlaceableListSubscriber.SetBoldHandle(
  const Value: TBoldAbstractListHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TCustomBoldPlaceableListSubscriber.SetBoldProperties(
  const Value: TBoldListAsFollowerListController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TCustomBoldPlaceableListSubscriber.SetRowProperties(
  const Value: TBoldStringFollowerController);
begin
  fBoldRowProperties := Value;
end;

procedure TCustomBoldPlaceableListSubscriber._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  if Assigned(fOnReceive) then
    fOnReceive(Originator, OriginalEvent, RequestedEvent, args);
end;

end.
