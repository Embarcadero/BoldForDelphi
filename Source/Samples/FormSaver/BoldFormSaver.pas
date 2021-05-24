{ Global compiler directives }
{$include bold.inc}
unit BoldFormSaver;

{.$DEFINE DEBUG_BOLDFORMSAVER}

interface

uses
  Classes,
  Forms,
  SysUtils,
  Windows,
  BoldDefs,
  Boldsubscription,
  BoldElements,
  BoldSystem,
  BoldSystemRT,
  BoldHandles,
  BoldDeriver,
  BoldSystemHandle;

type
  TBoldFormSaverExceptionEvent = procedure(Error: Exception) of object;

  TBoldAbstractDirtyList = class(TBoldObjectList)
  private
    fAddingElement: Boolean;
    fAutoRemoveCleanObjects: Boolean;
    procedure _NotifyOutOfDate;
    procedure _MakeValid (DerivedObject: TObject; Subscriber: TBoldSubscriber);
    procedure _ReverseDerive(DerivedObject: TObject);

  protected
    function GetHostSystem: TBoldSystem; virtual; abstract;
    procedure SubscribeToCleanedObjects;
    procedure FreeContent; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    procedure AddElement(Element: TBoldElement); override;
  public
    constructor create;
    property HostSystem: TBoldSystem read GetHostSystem;
    property AutoRemoveCleanObjects: Boolean read fAutoRemoveCleanObjects write fAutoRemoveCleanObjects;
  end;


  TBoldDirtyObjectListWithHandle = class(TBoldAbstractDirtyList)
  private
    fHostSystemHandle: TBoldSystemHandle;
    fHostSystemHandleSubscriber: TBoldPassThroughSubscriber;
    procedure _RecieveHostSystemHandleEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetHostSystemHandle(const Value: TBoldSystemHandle);
  protected
    function GetHostSystem: TBoldSystem; override;
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    constructor Create(HostSystemHandle: TBoldSystemHandle);
    destructor Destroy; override;
  end;

  TCallBackFunction = reference to function(Code: Integer; WParam: WPARAM;
      var Msg: TMsg): LRESULT;

  TBoldFormSaver = class(TBoldElementHandle)
  private
    FSaveToDBOnOk: Boolean;
    fDirtyObjects: TBoldDirtyObjectListWithHandle;
    FSystemHandle: TBoldSystemHandle;
    FOnlyFirstDirty: Boolean;
    fSystemHandleSubscriber: TBoldPassThroughSubscriber;
    FCloseFormOnAction: Boolean;
    FAutoRemoveCleanObjects: Boolean;
    FTargetFormSaver: TBoldFormSaver;
    fTargetFormSaverSubscriber: TBoldPassThroughSubscriber;
    fOnUpdateException: TBoldFormSaverExceptionEvent;
    FOrgActivate: TNotifyEvent;
    FOrgDeactivate: TNotifyEvent;
  class var
    FFormSaverList: TList;
    FWndProcHookHandle: HHOOK;
    class procedure FinalizationFormSavers; static;
    procedure SetSaveToDBOnOk(const Value: Boolean);
    procedure SetSystemHandle(const Value: TBoldSystemHandle);
    function GetDirtyObjects: TBoldDirtyObjectListWithHandle;
    class procedure RegisterFormSaver(FormSaver: TBoldFormSaver); static;
    procedure SetOnlyFirstDirty(const Value: Boolean);
    procedure _Activate(Sender: TObject);
    procedure _DeActivate(Sender: TObject);
    procedure _SystemHandleReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure _TargetFormSaverReveice(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetCloseFormOnAction(const Value: Boolean);
//    procedure CleanDirtyList;
    procedure SetDirtyListInSystem(Value: TBoldObjectList);
    procedure RemoveMyDirtyListFromSystem;
    procedure SetTargetFormSaver(const Value: TBoldFormSaver);
    procedure SaveObjects(Objects: TBoldObjectList);
    procedure SetAutoRemoveCleanObjects(const Value: Boolean);
    class procedure UnregisterFormSaver(FormSaver: TBoldFormSaver); static;
  protected
    procedure DoActive; virtual;
    procedure DoDeActivate; virtual;
    function GetValue: TBoldElement; override;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure PostAction;
    procedure EnsureActive;
  public
    constructor Create(Owner: TComponent); override;
    class constructor Create;
    destructor Destroy; override;
    class destructor Destroy;
    procedure OK;
    procedure Cancel;
    procedure Apply;
    property DirtyObjects: TBoldDirtyObjectListWithHandle read GetDirtyObjects;
    property TargetFormSaver: TBoldFormSaver read FTargetFormSaver write SetTargetFormSaver;
  published
    property SystemHandle: TBoldSystemHandle read FSystemHandle write SetSystemHandle;
    property SaveToDBOnOk: Boolean read FSaveToDBOnOk write SetSaveToDBOnOk default true;
    property OnlyFirstDirty: Boolean read FOnlyFirstDirty write SetOnlyFirstDirty;
    property CloseFormOnAction: Boolean read FCloseFormOnAction write SetCloseFormOnAction default true;
    property AutoRemoveCleanObjects: Boolean read FAutoRemoveCleanObjects write SetAutoRemoveCleanObjects default true;
    property OnUpdateException: TBoldFormSaverExceptionEvent read fOnUpdateException write fOnUpdateException;
  end;

implementation

uses
  Controls,
  Messages,
  Types,
  BoldBase,
  BoldIndex,
  BoldIndexableList,
  BoldMetaElementList,
  BoldUtils;

function WndProcHook(Code: Integer; wParam: WParam; lParam: LParam): LRESULT; stdcall;

  function GetFormSaverByOwner(Owner: TComponent): TBoldFormSaver;
  var
    i: Integer;
  begin
    Result := nil;
    if (Owner <> nil) and (TBoldFormSaver.FFormSaverList <> nil) then begin
      for i := 0 to TBoldFormSaver.FFormSaverList.Count - 1 do begin
        if TBoldFormSaver(TBoldFormSaver.FFormSaverList[i]).Owner = Owner then begin
          Result := TBoldFormSaver.FFormSaverList[i];
          Break;
        end;
      end;
    end;
  end;

  function GetFormSaverByHandle(Handle: HWND): TBoldFormSaver;
  var
    aComp: TComponent;
    i: Integer;
  begin
    aComp := FindControl(Handle);
    if aComp is TBoldFormSaver then begin
      Result := TBoldFormSaver(aComp);
    end else begin
      Result := GetFormSaverByOwner(aComp);

      // on modal dialogs also itertate over the calling forms
      if (Result = nil) and (aComp is TForm) and
         (fsModal in TForm(aComp).FormState) then
      begin
        for i := 0 to Screen.SaveFocusedList.Count - 1 do begin
          Result := GetFormSaverByOwner(TForm(Screen.SaveFocusedList[i]));
          if Assigned(Result) then begin
            Break;
          end;
        end;
      end;
    end;
  end;

var
  aMsg: PCWPStruct;
  aFormSaver: TBoldFormSaver;
begin
  aMsg := PCWPStruct(lParam);

  // Ensure, that there is always an active FormSaver,
  // because OnActivate event is not always called.
  if (aMsg.message = WM_ACTIVATE) and (LOWORD(aMsg.wParam) <> WA_INACTIVE) then begin
    aFormSaver := GetFormSaverByHandle(aMsg.hwnd);
    if Assigned(aFormSaver) then begin
      aFormSaver.DoActive;
    end;
  end;

  Result := CallNextHookEx(TBoldFormSaver.FWndProcHookHandle, Code, wParam, lParam);

{ Everytime a formsaverless form opens, the current FormSaver gets detached.
  This is especially problematic on modal dialogues, because then they no longer
  transfer their changes to the FormSaver of the owning host form.
  if (aMsg.message = WM_ACTIVATE) and (LOWORD(aMsg.wParam) = WA_INACTIVE) then begin
    aFormSaver := GetFormSaverByHandle(aMsg.hwnd);
    if Assigned(aFormSaver) then begin
      aFormSaver.DoDeActivate;
    end;
  end;
}
end;

const
  breSystemHandleDestroying = 100;
  breSystemHandleActivationChange = 101;
  breTargetFormSaverDestroying = 102;
  breHostSystemHandleDestroying = 103;


{ TBoldFormSaver }

procedure TBoldFormSaver.Apply;
begin
  EnsureActive;
  if SaveToDBOnOk then
    SaveObjects(DirtyObjects);
end;

procedure TBoldFormSaver.Cancel;
var
  aObj: TBoldObject;
  aObjList: TBoldObjectList;
  i: Integer;
begin
  EnsureActive;
  aObjList := TBoldObjectList.Create;
  try
    while DirtyObjects.count > 0 do begin
      aObj := DirtyObjects[DirtyObjects.count-1];
      DirtyObjects.removeByIndex(DirtyObjects.count-1);

      if not aObj.BoldObjectIsNew then begin
        aObjList.Add(aObj);
      end;
      aObj.Discard;
    end;
    for i := aObjList.Count - 1 downto 0 do begin
      aObjList[i].ReRead;
    end;
  finally
    aObjList.Free;
  end;
  PostAction;
end;

{procedure TBoldFormSaver.CleanDirtyList;
var
  i: integer;
begin
  // if anyone subscribes to the dirtylist,
  // it might get recursively cleaned during cleaning,
  // beware that the list might be shorter than count
  // already inside the loop
  for i := fDirtyObjects.Count-1 downto 0 do
    if (i < fDirtyObjects.Count) and not fDirtyObjects[i].BoldDirty then
      fDirtyObjects.RemoveByIndex(i);
end;
}

constructor TBoldFormSaver.Create(Owner: TComponent);
begin
  inherited;
  RegisterFormSaver(Self);
  fSystemHandleSubscriber := TBoldPassthroughSubscriber.Create(_SystemHandleReceive);
  fTargetFormSaverSubscriber := TBoldPassthroughSubscriber.Create(_TargetFormSaverReveice);
  if Owner is TForm then begin
    FOrgActivate := TForm(Owner).OnActivate;
    FOrgDeactivate := TForm(Owner).OnDeactivate;
    TForm(Owner).OnActivate := _Activate;
    TForm(Owner).OnDeactivate := _DeActivate;
    if TForm(Owner).Active then begin
      DoActive;
    end;
  end;
  SaveToDBOnOk := true;
  CloseFormOnAction := true;
  fAutoRemoveCleanObjects := true;
end;

class constructor TBoldFormSaver.Create;
begin
  inherited;
  FFormSaverList := TList.Create;
end;

destructor TBoldFormSaver.Destroy;
begin
  RemoveMyDirtyListFromSystem;
  FreeAndNil(fDirtyObjects);
  FreeAndNil(fSystemHandleSubscriber);
  FreeAndNil(fTargetFormSaverSubscriber);
  if Owner is TForm then begin
    TForm(Owner).OnActivate := FOrgActivate;
    TForm(Owner).OnDeactivate := FOrgDeactivate;
  end;
  UnregisterFormSaver(Self);
  inherited;
end;

class destructor TBoldFormSaver.Destroy;
begin
  FinalizationFormSavers;
  FreeAndNil(FFormSaverList);
  inherited;
end;

procedure TBoldFormSaver.DoActive;
begin
  {$IFDEF DEBUG_BOLDFORMSAVER}
  OutputDebugString(PChar(Format(
      'Activating BoldFormSaver %s (%s)', [Name, TForm(Owner).Caption])));
  {$ENDIF}
  SetDirtyListInSystem(DirtyObjects);
end;

procedure TBoldFormSaver.DoDeActivate;
begin
  {$IFDEF DEBUG_BOLDFORMSAVER}
  OutputDebugString(PChar(Format(
      'Deactivating BoldFormSaver %s (%s)', [Name, TForm(Owner).Caption])));
  {$ENDIF}
  SetDirtyListInSystem(nil);
end;

procedure TBoldFormSaver.EnsureActive;
begin
  if not assigned(SystemHandle) or not SystemHandle.Active then
    raise EBold.Create('TBoldFormSaver: No system available');
end;

class procedure TBoldFormSaver.FinalizationFormSavers;
begin
  UnhookWindowsHookEx(FWndProcHookHandle);
  FWndProcHookHandle := 0;
end;

function TBoldFormSaver.GetDirtyObjects: TBoldDirtyObjectListWithHandle;
begin
  if not assigned(fDirtyObjects) then
  begin
    fDirtyObjects := TBoldDirtyObjectListWithHandle.Create(SystemHandle);
    fDirtyObjects.AutoRemoveCleanObjects := AutoRemoveCleanObjects;
  end;
  result := fDirtyObjects;
end;

function TBoldFormSaver.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if assigned(StaticSystemTypeInfo) then
    result := StaticSystemTypeInfo.TopSortedClasses[0].ListTypeInfo
  else
    result := nil;
end;

function TBoldFormSaver.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(SystemHandle) then
    result := SystemHandle.StaticSystemTypeInfo
  else
    result := nil;
end;

function TBoldFormSaver.GetValue: TBoldElement;
begin
  result := DirtyObjects;
end;

procedure TBoldFormSaver.OK;
begin
  EnsureActive;
  if SaveToDBOnOk then
    SaveObjects(DirtyObjects);
  PostAction;
end;

procedure TBoldFormSaver.PostAction;
begin
  if CloseFormOnAction then
    (Owner as TForm).Close;
end;

class procedure TBoldFormSaver.RegisterFormSaver(FormSaver: TBoldFormSaver);
begin
  if FFormSaverList.IndexOf(FormSaver) < 0 then begin
    if FFormSaverList.Count = 0 then begin
      FWndProcHookHandle := SetWindowsHookEx(
          WH_CALLWNDPROC, WndProcHook, 0, GetCurrentThreadId);
    end;
    FFormSaverList.Add(FormSaver);
  end;
end;


procedure TBoldFormSaver.RemoveMyDirtyListFromSystem;
begin
  if assigned(SystemHandle) and SystemHandle.Active and (SystemHandle.System.NewDirtyList = DirtyObjects) then
    SystemHandle.System.NewDirtyList := nil;
  if assigned(SystemHandle) and SystemHandle.Active and (SystemHandle.System.NewModifiedList = DirtyObjects) then
    SystemHandle.System.NewModifiedList := nil;
end;

procedure TBoldFormSaver.SaveObjects(Objects: TBoldObjectList);
var
  tempList: TBoldObjectList;
begin
  EnsureActive;
  if assigned(TargetFormSaver) then
  begin
    TargetFormSaver.DirtyObjects.AddList(Objects);
    Objects.Clear;
  end
  else
  begin
    // Move the objects to a templist since if someone
    // accesses the DirtyObjects during this operation
    // the count will change and "clear" might fail.
    TempList := Objects.Clone as TBoldObjectList;
    try
      try
        SystemHandle.system.UpdateDatabaseWithList(TempList);
      except
        on e: exception do
        begin
          if assigned(OnUpdateException) then
            OnUpdateException(e)
          else
            raise;
        end
      end;
    finally
      TempList.Free;
    end;
    if (not (csDestroying in ComponentState)) and
       (DirtyObjects.Count = 0) then // ensure that every objects is saved
    begin
      Objects.Clear;
    end;
  end;
end;

procedure TBoldFormSaver.SetAutoRemoveCleanObjects(const Value: Boolean);
begin
  FAutoRemoveCleanObjects := Value;
  DirtyObjects.AutoRemoveCleanObjects := AutoRemoveCleanObjects;
end;

procedure TBoldFormSaver.SetCloseFormOnAction(const Value: Boolean);
begin
  FCloseFormOnAction := Value;
end;

procedure TBoldFormSaver.SetDirtyListInSystem(Value: TBoldObjectList);
begin
  if assigned(SystemHandle) and SystemHandle.active then
  begin
    if OnlyFirstDirty then
      SystemHandle.System.newdirtyList := value
    else
      SystemHandle.System.newModifiedList := value;
  end;

end;

procedure TBoldFormSaver.SetOnlyFirstDirty(const Value: Boolean);
begin
  if Value <> fOnlyFirstDirty then
  begin
    RemoveMyDirtyListFromSystem;
    FOnlyFirstDirty := Value;
    if (Owner as TForm).Active then
      SetDirtyListInSystem(DirtyObjects);
  end;
end;

procedure TBoldFormSaver.SetSaveToDBOnOk(const Value: Boolean);
begin
  FSaveToDBOnOk := Value;
end;

procedure TBoldFormSaver.SetSystemHandle(const Value: TBoldSystemHandle);
begin
  RemoveMyDirtyListFromSystem;
  FSystemHandle := Value;
  fSystemHandleSubscriber.CancelAllSubscriptions;
  if assigned(fSystemHandle) then
  begin
    FSystemHandle.AddSmallSubscription(fSystemHandleSubscriber, [beValueIdentityChanged], breSystemHandleActivationChange);
    FSystemHandle.AddSmallSubscription(fSystemHandleSubscriber, [beDestroying], breSystemHandleDestroying);
    FreeAndNil(fDirtyObjects);
    SendEvent(self, beValueIdentityChanged);
  end;
  if (Owner as TForm).Active then
    SetDirtyListInSystem(DirtyObjects);
end;

procedure TBoldFormSaver.SetTargetFormSaver(const Value: TBoldFormSaver);
begin
  if Value <> fTargetFormSaver then
  begin
    fTargetFormSaverSubscriber.CancelAllSubscriptions;
    FTargetFormSaver := Value;
    if assigned(fTargetFormSaver) then
      fTargetFormSaver.AddSmallSubscription(fTargetFormSaverSubscriber, [beDestroying], breTargetFormSaverDestroying);
  end;
end;

class procedure TBoldFormSaver.UnregisterFormSaver(FormSaver: TBoldFormSaver);
begin
  if FFormSaverList <> nil then
  begin
    FFormSaverList.Extract(FormSaver);
    if FFormSaverList.Count = 0 then
      FinalizationFormSavers;
  end;
end;

procedure TBoldFormSaver._Activate(Sender: TObject);
begin
  DoActive;
  if Assigned(FOrgActivate) then begin
    FOrgActivate(Sender);
  end;
end;

procedure TBoldFormSaver._DeActivate(Sender: TObject);
begin
  DoDeActivate;
  if Assigned(FOrgActivate) then begin
    FOrgActivate(Sender);
  end;
end;

procedure TBoldFormSaver._SystemHandleReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (RequestedEvent = breSystemHandleDestroying) and (Originator = SystemHandle) then
    SystemHandle := nil;

  if (RequestedEvent = breSystemHandleActivationChange) and (Originator = SystemHandle) and
    (Owner as TForm).Active then
  begin
    DoDeActivate;
  end;
end;

procedure TBoldFormSaver._TargetFormSaverReveice(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = fTargetFormSaver then
    fTargetFormSaver := nil;
end;

{ TBoldDirtyObjectListWithHandle }

constructor TBoldDirtyObjectListWithHandle.create(HostSystemHandle: TBoldSystemHandle);
begin
  inherited Create;
  fHostSystemHandleSubscriber := TBoldPassthroughSubscriber.Create(_RecieveHostSystemHandleEvents);
  SetHostSystemHandle(HostSystemHandle);

  // how to set the BoldType on the list???

//  BoldType := HostSystem.BoldSystemTypeInfo.ListTypeInfoByElement[HostSystem.BoldSystemTypeInfo.TopSortedClasses[0]];
end;

destructor TBoldDirtyObjectListWithHandle.destroy;
begin
  FreeAndNil(fHostSystemHandleSubscriber);
  inherited;
end;

constructor TBoldAbstractDirtyList.create;
begin
  inherited;
  fAddingElement := false;
  SetBoldPersistenceState(bvpsInvalid);
  SetElementFlag(befDerived, true);
  SubscribeToLocatorsInList := true;
  Deriver.OnDeriveAndSubscribe := _MakeValid;
  Deriver.OnReverseDerive := _ReverseDerive;
  Deriver.OnNotifyOutOfdate := _NotifyOutOfDate;
end;

procedure TBoldAbstractDirtyList.FreeContent;
begin
  // Do nothing, especially not inherited;
end;

function TBoldDirtyObjectListWithHandle.GetBoldType: TBoldElementTypeInfo;
begin
  if assigned(fHostSystemHandle) and assigned(fHostSystemHandle.StaticSystemTypeInfo) then
    result := fHostSystemHandle.StaticSystemTypeInfo.ListTypeInfoByElement[ fHostSystemHandle.StaticSystemTypeInfo.TopSortedClasses[0]]
  else
    result := nil;
end;

function TBoldDirtyObjectListWithHandle.GetHostSystem: TBoldSystem;
begin
  if assigned(fHostSystemHandle) then
    result := fHostSystemHandle.System
  else
    result := TBoldSystem.DefaultSystem;
end;

procedure TBoldDirtyObjectListWithHandle.SetHostSystemHandle(const Value: TBoldSystemHandle);
begin
  if fHostSystemHandle <> Value then
  begin
    fHostSystemHandle := Value;
    if assigned(fHostSystemHandle) then
    begin
      fHostSystemHandle.AddSmallSubscription(fHostSystemHandleSubscriber, [beDestroying], breHostSystemHandleDestroying);
      fHostSystemHandle.AddSmallSubscription(fHostSystemHandleSubscriber, [beValueIdentityChanged], breSystemHandleActivationChange);
    end;
    SubscribeToCleanedObjects;
  end;
end;


function TBoldAbstractDirtyList.GetBoldType: TBoldElementTypeInfo;
begin
  if assigned(HostSystem) then
    result := HostSystem.BoldSystemTypeInfo.ListTypeInfoByElement[HostSystem.BoldSystemTypeInfo.TopSortedClasses[0]]
  else
    result := nil;
end;

procedure TBoldAbstractDirtyList._ReverseDerive(DerivedObject: TObject);
begin
  // Do nothing. Just accept any changes
end;

procedure TBoldAbstractDirtyList.SubscribeToCleanedObjects;
begin
  Deriver.CancelAllSubscriptions;
  if assigned(HostSystem) then
    HostSystem.AddSmallSubscription(Deriver, [beDirtyListInvalidOrItemDeleted], breReEvaluate);
end;

procedure TBoldAbstractDirtyList._MakeValid(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  if not fAddingElement and AutoRemoveCleanObjects then
    for i := Count -1 downto 0 do
      if (i < Count) and not BoldObjects[i].BoldDirty then
        RemoveByIndex(i);

  // the code below is just to make sure that the dirtylist of the system
  // is made current so that we will receive future events.
  if assigned(HostSystem) and HostSystem.BoldDirty and false then
    _MakeValid(DerivedObject, Subscriber);
end;

procedure TBoldDirtyObjectListWithHandle._RecieveHostSystemHandleEvents(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breHostSystemHandleDestroying then
    fHostSystemHandle := nil;
  if RequestedEvent = breSystemHandleActivationChange then
    SubscribeToCleanedObjects;
end;

procedure TBoldAbstractDirtyList._NotifyOutOfDate;
begin
  Invalidate;
end;

procedure TBoldAbstractDirtyList.AddElement(Element: TBoldElement);
var
  OldState: TBoldValuePersistenceState;
begin
  OldState := BoldPersistenceState;
  fAddingElement := true;
  try
    inherited;
  finally
    fAddingElement := false;
  end;
  if OldState = bvpsInvalid then
    Invalidate;
end;

end.

