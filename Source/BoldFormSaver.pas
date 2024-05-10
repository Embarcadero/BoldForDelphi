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
    fIsActive: boolean;
    FSaveToDBOnOk: Boolean;
    FOnlyFirstDirty: Boolean;
    FCloseFormOnAction: Boolean;
    FTargetFormSaver: TBoldFormSaver;
    fTargetFormSaverSubscriber: TBoldPassthroughSubscriber;
    fOnUpdateException: TBoldFormSaverExceptionEvent;
    function GetDirtyObjects: TBoldObjectList;
    procedure SetTargetFormSaver(const Value: TBoldFormSaver);
    procedure SaveObjects;
    procedure _Activate(Sender: TObject);
    procedure _DeActivate(Sender: TObject);
    procedure _TargetFormSaverReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetForm: TForm;
    function GetSystemHandle: TBoldSystemHandle;
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
    procedure Apply;
    property DirtyObjects: TBoldObjectList read GetDirtyObjects;
    property TargetFormSaver: TBoldFormSaver read FTargetFormSaver write SetTargetFormSaver;
    property IsActive: boolean read GetIsActive;
    property Form: TForm read GetForm;
    property SystemHandle: TBoldSystemHandle read GetSystemHandle;
  published
    property SaveToDBOnOk: Boolean read FSaveToDBOnOk write fSaveToDBOnOk default true;
    property OnlyFirstDirty: Boolean read FOnlyFirstDirty write fOnlyFirstDirty;
    property CloseFormOnAction: Boolean read FCloseFormOnAction write FCloseFormOnAction default true;
    property OnUpdateException: TBoldFormSaverExceptionEvent read fOnUpdateException write fOnUpdateException;
  end;

implementation

{ TBoldFormSaver }

procedure TBoldFormSaver.Apply;
begin
  EnsureActive;
  if SaveToDBOnOk then
    SaveObjects;
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

  if Owner is TForm then begin
    TForm(Owner).OnActivate := _Activate;
    TForm(Owner).OnDeactivate := _DeActivate;
    if TForm(Owner).Active then begin
      _Activate(self);
    end;
  end;
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
  if not assigned(SystemHandle) or not SystemHandle.Active then
    raise EBold.Create('TBoldFormSaver: No system available');
end;

function TBoldFormSaver.GetBoldDirtyObjectTracker: IBoldDirtyObjectTracker;
begin
  if not Assigned(fBoldDirtyObjectTracker) then
  begin
    if not Assigned(BoldSystem) then
      raise Exception.Create('Formsaver not connected to active SystemHandle.');
    fBoldDirtyObjectTracker := BoldSystem.CreateDirtyObjectTracker;
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
  result := fIsActive;
//  result := Assigned(Form) and (Form = Screen.ActiveCustomForm); // Form.Active;
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
    SaveObjects;
  PostAction;
end;

procedure TBoldFormSaver.PostAction;
begin
  if CloseFormOnAction then
    (Owner as TForm).Close;
end;

procedure TBoldFormSaver.SaveObjects;
begin
  EnsureActive;
  if assigned(TargetFormSaver) then
  begin
    TargetFormSaver.DirtyObjects.AddList(DirtyObjects);
    DirtyObjects.Clear;
  end
  else
  begin
    try
      SystemHandle.system.UpdateDatabaseWithList(DirtyObjects);
      DirtyObjects.Clear;
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

function TBoldFormSaver.GetSystemHandle: TBoldSystemHandle;
begin
  result := StaticSystemHandle as TBoldSystemHandle;
end;

procedure TBoldFormSaver.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if (Value <> SystemHandle) and Assigned(fBoldDirtyObjectTracker) and not DirtyObjects.Empty then
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
  if not (csDesigning in ComponentState) and Assigned(BoldSystem) and fIsActive then
    BoldDirtyObjectTracker; // will ensure BoldDirtyObjectTracker and start tracking dirty objects
end;

procedure TBoldFormSaver._Activate(Sender: TObject);
begin
  fIsActive := true;
end;

procedure TBoldFormSaver._DeActivate(Sender: TObject);
begin
  fIsActive := false;
end;

procedure TBoldFormSaver._TargetFormSaverReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = fTargetFormSaver then
    fTargetFormSaver := nil;
end;

end.
