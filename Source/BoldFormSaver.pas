{ Global compiler directives }
{$include bold.inc}
unit BoldFormSaver;

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

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TBoldFormSaver = class(TBoldNonSystemHandle)
  private
    fBoldDirtyObjectTracker: IBoldDirtyObjectTracker;
    FSaveToDBOnOk: Boolean;
    FCloseFormOnAction: Boolean;
    FTargetFormSaver: TBoldFormSaver;
    fTargetFormSaverSubscriber: TBoldPassthroughSubscriber;
    fOnUpdateException: TBoldFormSaverExceptionEvent;
    function GetDirtyObjects: TBoldObjectList;
    procedure SetTargetFormSaver(const Value: TBoldFormSaver);
    procedure SaveObjects(ATargetFormSaver: TBoldFormSaver = nil);
    procedure _TargetFormSaverReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetForm: TForm;
    function GetIsActive: boolean;
    function GetBoldDirtyObjectTracker: IBoldDirtyObjectTracker;
    property BoldDirtyObjectTracker: IBoldDirtyObjectTracker read GetBoldDirtyObjectTracker;
  protected
    function GetValue: TBoldElement; override;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); override;
    procedure StaticBoldTypeChanged; override;
    procedure PostAction;
    procedure EnsureActive;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure OK;
    procedure Cancel;
    procedure Apply(ATargetFormSaver: TBoldFormSaver = nil);
    property DirtyObjects: TBoldObjectList read GetDirtyObjects;
    property TargetFormSaver: TBoldFormSaver read FTargetFormSaver write SetTargetFormSaver;
    property IsActive: boolean read GetIsActive;
    property Form: TForm read GetForm;
  published
    property SaveToDBOnOk: Boolean read FSaveToDBOnOk write fSaveToDBOnOk default true;
    property CloseFormOnAction: Boolean read FCloseFormOnAction write FCloseFormOnAction default true;
    property OnUpdateException: TBoldFormSaverExceptionEvent read fOnUpdateException write fOnUpdateException;
  end;

implementation

type
  TBoldDirtyObjectTrackerForFormSaver = class(TBoldDirtyObjectTracker)
  private
    fBoldFormSaver: TBoldFormSaver;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
  public
    constructor CreateForFormSaver(ABoldFormSaver: TBoldFormSaver);
  end;

{ TBoldFormSaver }

procedure TBoldFormSaver.Apply(ATargetFormSaver: TBoldFormSaver);
begin
  EnsureActive;
  if not Assigned(ATargetFormSaver) then
    ATargetFormSaver := TargetFormSaver;
  if SaveToDBOnOk then
    SaveObjects(ATargetFormSaver);
end;

procedure TBoldFormSaver.Cancel;
var
  aObj: TBoldObject;
  aObjList: TBoldObjectList;
  i: Integer;
begin
  EnsureActive;
  aObjList := TBoldObjectList.Create;
  aObjList.DuplicateMode := bldmAllow;
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

constructor TBoldFormSaver.Create(Owner: TComponent);
begin
  inherited;
  if not (Owner is TForm) then
    raise Exception.Create('FormSaver requires Form as owner.');
  SaveToDBOnOk := true;
  CloseFormOnAction := true;
end;

destructor TBoldFormSaver.Destroy;
begin
  fBoldDirtyObjectTracker := nil;
  FreeAndNil(fTargetFormSaverSubscriber);
  inherited;
end;

procedure TBoldFormSaver.EnsureActive;
begin
  if not assigned(StaticSystemHandle) or not StaticSystemHandle.Active then
    raise EBold.Create('TBoldFormSaver: No system available');
end;

function TBoldFormSaver.GetBoldDirtyObjectTracker: IBoldDirtyObjectTracker;
begin
  if not Assigned(fBoldDirtyObjectTracker) then
  begin
    if not Assigned(BoldSystem) then
      raise Exception.Create('Formsaver not connected to active SystemHandle.');
    fBoldDirtyObjectTracker := TBoldDirtyObjectTrackerForFormSaver.CreateForFormSaver(self);
  end;
  result := fBoldDirtyObjectTracker;
end;

function TBoldFormSaver.GetDirtyObjects: TBoldObjectList;
begin
  result := BoldDirtyObjectTracker.DirtyObjects;
end;

function TBoldFormSaver.GetForm: TForm;
begin
  result := Owner as TForm;
end;

function TBoldFormSaver.GetIsActive: boolean;
begin
  result := Assigned(Form) and (Form = Screen.ActiveCustomForm); // Form.Active;
end;

function TBoldFormSaver.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if assigned(StaticSystemTypeInfo) then
    result := StaticSystemTypeInfo.RootClassTypeInfo.ListTypeInfo
  else
    result := nil;
end;

function TBoldFormSaver.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(StaticSystemHandle) then
    result := StaticSystemHandle.StaticSystemTypeInfo
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
    SaveObjects(TargetFormSaver);
  PostAction;
end;

procedure TBoldFormSaver.PostAction;
begin
  if CloseFormOnAction then
    (Owner as TForm).Close;
end;

procedure TBoldFormSaver.SaveObjects(ATargetFormSaver: TBoldFormSaver);
begin
  if ATargetFormSaver = self then
    raise Exception.Create('Can not save to self.');
  EnsureActive;
  if assigned(ATargetFormSaver) then
  begin
    ATargetFormSaver.DirtyObjects.AddList(DirtyObjects);
    DirtyObjects.Clear;
  end
  else
  begin
    try
      StaticSystemHandle.System.UpdateDatabaseWithList(DirtyObjects);
    except
      on e: exception do
      begin
        if assigned(OnUpdateException) then
          OnUpdateException(e)
        else
          raise;
      end
    end;
  end;
end;

procedure TBoldFormSaver.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if (Value <> StaticSystemHandle) and Assigned(fBoldDirtyObjectTracker) and not DirtyObjects.Empty then
    raise Exception.Create('Can not change systems with existing dirty objects.');
  inherited SetStaticSystemHandle(Value);
  if not (csDesigning in ComponentState) and Assigned(Value) then
    BoldDirtyObjectTracker; // will ensure BoldDirtyObjectTracker and start tracking dirty objects
end;

procedure TBoldFormSaver.SetTargetFormSaver(const Value: TBoldFormSaver);
begin
  if Value <> fTargetFormSaver then
  begin
    fTargetFormSaverSubscriber.free;
    fTargetFormSaverSubscriber := TBoldPassthroughSubscriber.Create(_TargetFormSaverReceive);
    fTargetFormSaver := Value;
    if assigned(fTargetFormSaver) then
      fTargetFormSaver.AddSmallSubscription(fTargetFormSaverSubscriber, [beDestroying]);
  end;
end;

procedure TBoldFormSaver.StaticBoldTypeChanged;
begin
  inherited;
  if not (csDesigning in ComponentState) and Assigned(BoldSystem) and IsActive then
    BoldDirtyObjectTracker; // will ensure BoldDirtyObjectTracker and start tracking dirty objects
end;

procedure TBoldFormSaver._TargetFormSaverReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = fTargetFormSaver then
    fTargetFormSaver := nil;
end;

{ TBoldDirtyObjectTrackerForFormSaver }

constructor TBoldDirtyObjectTrackerForFormSaver.CreateForFormSaver(
  ABoldFormSaver: TBoldFormSaver);
begin
  fBoldFormSaver := ABoldFormSaver;
  inherited Create(ABoldFormSaver.BoldSystem);
  StartTracking;
end;

procedure TBoldDirtyObjectTrackerForFormSaver.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  // if FormSaver is not active we still want to remove objects which became clean (but we do not want to add dirty objects)
  if fBoldFormSaver.IsActive or (OriginalEvent in [beDirtyListInvalidOrItemDeleted, beObjectBecomingClean]) then
    inherited;
end;

end.
