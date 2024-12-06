{ Global compiler directives }
{$include bold.inc}
unit BoldQueue;

interface

uses
  Classes,
  BoldBase,
  BoldEventQueue,
  BoldLogHandler;

const
  { Queueable }
  befIsInDisplayList = BoldElementFlag0;
  befStronglyDependedOfPrioritized = BoldElementFlag1;
  befToBeRemovedFromDisplayList = BoldElementFlag2;
  { Follower }
  befFollowerSelected = BoldElementFlag3;

type
  { forward declarations }
  TBoldQueue = class;
  TBoldQueueable = class;

  TBoldQueueDisplayMode = (dmDisplayOne, dmDisplayAll);

  { TBoldQueueable }
  TBoldQueueable = class(TBoldFlaggedObject)
  private
    fMatchObject: TObject;
    fPrioritizedQueuable: TBoldQueueable;
  protected
    procedure AddToApplyList;
    procedure RemoveFromApplyList;
    procedure AddToDisplayList; virtual;
    procedure RemoveFromDisplayList(ADestroying: boolean);
    function MostPrioritizedQueuable: TBoldQueueable;
    function MostPrioritizedQueuableOrSelf: TBoldQueueable;
    function AfterInPriority(Queueable: TBoldQueueable): Boolean;
    procedure Display; virtual; abstract;
    function StronglyPrioritizedSibbling(Queueable: TBoldQueueable): Boolean;
    property ToBeRemovedFromDisplayList: Boolean index befToBeRemovedFromDisplayList read GetElementFlag write SetElementFlag;
    class function DisplayOne: Boolean;
    function GetDebugInfo: string; override;
  public
    constructor Create(aMatchObject: TObject);
    destructor Destroy; override;
    class function DisplayAll: Boolean;
    class procedure ApplyAll;
    class procedure ApplyAllMatching(anObject: TObject);
    class procedure DiscardChangeAll;
    class procedure DiscardChangeAllMatching(anObject: TObject);
    class procedure AddToPreDisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    class procedure AddToPostDisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    class procedure RemoveFromPreDisplayQueue(Receiver: TObject);
    class procedure RemoveFromPostDisplayQueue(Receiver: TObject);
    class function IsDisplayQueueEmpty: Boolean;
    class function IsDisplaying: Boolean;
    class procedure ActivateDisplayQueue;
    class procedure DeactivateDisplayQueue;
    procedure Apply; virtual; abstract;
    procedure DiscardChange; virtual; abstract;
    property MatchObject: TObject read fMatchObject;
    property IsInDisplayList: Boolean index befIsInDisplayList read GetElementFlag;
    property PrioritizedQueuable: TBoldQueueable read fPrioritizedQueuable write fPrioritizedQueuable;
    property StronglyDependedOfPrioritized: Boolean index befStronglyDependedOfPrioritized read GetElementflag write SetElementFlag;
  end;

  { TBoldQueue }
  TBoldQueue = class(TBoldMemoryManagedObject)
  private
    fIsDisplaying: Boolean;
    fDisplayMode: TBoldQueueDisplayMode;
    fPreDisplayQueue: TBoldEventQueue;
    fPostDisplayQueue: TBoldEventQueue;
    fDisplayList: TList;
    fApplyList: TList;
    fDisplayListIndex: Integer;
    function GetApplyCount: integer;
    function GetDisplayCount: integer;
    function GetPostDisplayCount: integer;
    function GetPreDisplayCount: integer;
    function GetEmpty: boolean;
  protected
    function DisplayOne: Boolean;
    function DisplayAll: Boolean;
    procedure AddToApplyList(Queueable: TBoldQueueable);
    procedure RemoveFromApplyList(Queueable: TBoldQueueable);
    procedure AddToDisplayList(Queueable: TBoldQueueable);
    procedure RemoveFromDisplayList(Queueable: TBoldQueueable; ADestroying: boolean);
    procedure EnsureDequeing; virtual;
    function GetIsActive: boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddEventToPredisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    procedure AddEventToPostDisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    procedure RemoveFromPreDisplayQueue(Receiver: TObject);
    procedure RemoveFromPostDisplayQueue(Receiver: TObject);
    procedure PerformPreDisplayQueue;
    procedure PerformPostDisplayQueue;
    procedure DeActivateDisplayQueue; virtual; abstract;
    procedure ActivateDisplayQueue; virtual; abstract;
    property DisplayMode: TBoldQueueDisplayMode read fDisplayMode write fDisplayMode;
    property DisplayCount: integer read GetDisplayCount;
    property ApplyCount: integer read GetApplyCount;
    property PreDisplayCount: integer read GetPreDisplayCount;
    property PostDisplayCount: integer read GetPostDisplayCount;
    property Empty: boolean read GetEmpty;
    property IsActive: boolean read GetIsActive;
  end;

  function BoldQueueFinalized: Boolean;
  function BoldInstalledQueue: TBoldQueue;

var
  BoldQueueLogHandler: TBoldLogHandler = nil;
  BoldQueueLogCount: int64 = 0;

procedure BoldLogQueue(const s: String);

implementation

uses
  SysUtils,
  BoldUtils,
  Forms,
  BoldIsoDateTime,
  BoldGuard,
  BoldEnvironment;

function BoldQueueFinalized: Boolean;
begin
  Result := (BoldEnvironmentsFinalized) or BoldEffectiveEnvironment.QueueFinalized;
end;

function BoldInstalledQueue: TBoldQueue;
begin
  result := BoldEnvironment.BoldInstalledQueue;
end;

procedure BoldLogQueue(const s: String);
begin
  Inc(BoldQueueLogCount);
  if assigned(BoldQueueLogHandler) then
  begin
    BoldQueueLogHandler.Log(
      AsIsoDateTimeMs(now) +':'+
      format('Queue %3d- %s', [BoldQueueLogCount, s]));
  end;
end;

{ TBoldQuable }

constructor TBoldQueueable.Create(aMatchObject: TObject);
begin
  inherited Create;
  fMatchObject := aMatchObject;
end;

destructor TBoldQueueable.Destroy;
begin
  RemoveFromApplyList;
  RemoveFromDisplayList(true);
  Assert(not (IsInDisplayList or ToBeRemovedFromDisplayList));
  inherited Destroy;
end;

function TBoldQueueable.GetDebugInfo: string;
begin
  result := Format('%s', [className]);
  if Assigned(MatchObject) then
    result := result + ' (' + MatchObject.ClassName + ')';
end;

procedure TBoldQueueable.AddToApplyList;
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.AddToApplyList(self);
end;

procedure TBoldQueueable.RemoveFromApplyList;
begin
  if not (BoldQueueFinalized) then
    BoldInstalledQueue.RemoveFromApplyList(self);
end;

procedure TBoldQueueable.AddToDisplayList;
begin
  if not BoldQueueFinalized then
  begin
    Assert(not IsInDisplayList);
    if ToBeRemovedFromDisplayList then
      ToBeRemovedFromDisplayList := false
    else
      BoldInstalledQueue.AddToDisplayList(self);
    SetElementflag(befIsInDisplayList, True);
  end;
end;

procedure TBoldQueueable.RemoveFromDisplayList(ADestroying: boolean);
begin
  if not (BoldQueueFinalized) and (IsInDisplayList or ToBeRemovedFromDisplayList) then
  begin
    BoldInstalledQueue.RemoveFromDisplayList(self, ADestroying);
    SetElementflag(befIsInDisplayList, False);
  end;
end;

function TBoldQueueable.MostPrioritizedQueuableOrSelf: TBoldQueueable;
begin
  Result := MostPrioritizedQueuable;
  if not Assigned(Result) and IsInDisplayList then
    Result := Self;
end;

function TBoldQueueable.MostPrioritizedQueuable: TBoldQueueable;
var
  i: integer;
  Queueable: TBoldQueueable;
begin
  if Assigned(PrioritizedQueuable) then
  begin
    Result := PrioritizedQueuable.MostPrioritizedQueuableOrSelf;
    if not Assigned(Result) and StronglyDependedOfPrioritized then
      with BoldInstalledQueue do
      for i := fDisplayList.Count - 1 downto 0 do
      begin
        Queueable := fDisplayList[i];
        if assigned(Queueable) and
          Queueable.IsInDisplayList and
          Queueable.AfterInPriority(PrioritizedQueuable) and
          not StronglyPrioritizedSibbling(Queueable) and
          not Queueable.AfterInPriority(self) then
        begin
          Result := Queueable.MostPrioritizedQueuableOrSelf;
          break;
        end;
      end;
  end
  else
    Result := nil;
end;

class function TBoldQueueable.DisplayOne: Boolean;
begin
  Result := (BoldInstalledQueue <> nil) and BoldInstalledQueue.DisplayOne;
end;

class function TBoldQueueable.DisplayAll: Boolean;
begin
  Result := DisplayOne;
  if Result then
    while DisplayOne do
      {nothing};
end;

class function TBoldQueueable.IsDisplaying: Boolean;
begin
  Result := (BoldInstalledQueue <> nil) and BoldInstalledQueue.fIsDisplaying;
end;

class function TBoldQueueable.IsDisplayQueueEmpty: Boolean;
begin
  Result := (BoldInstalledQueue = nil) or (BoldInstalledQueue.fDisplayList.Count = 0);
end;

class procedure TBoldQueueable.ApplyAll;
var
  Queueable: TBoldQueueable;
  tempList: TList;
  i: integer;
  g: IBoldGuard;
  vApplyList: TList;
begin
  g := TBoldGuard.Create(TempList);
  if BoldInstalledQueue <> nil then
  begin
    tempList := TList.Create;
    vApplyList := BoldInstalledQueue.fApplyList;
    while vApplyList.Count > 0 do
    begin
      Queueable := vApplyList.Last;
      if Assigned(Queueable) then
      begin
        Queueable.Apply;
        if  (vApplyList.Count > 0) and (Queueable = vApplyList.Last) then
        begin
          // for some reason the apply failed,
          // remove the queueable from the applylist and put it back when it is empty...
          TempList.Add(Queueable);
          vApplyList.Count := vApplyList.Count - 1;
        end;
      end
      else
        vApplyList.Count := vApplyList.Count - 1;
    end;
    for i := 0 to TempList.Count - 1 do
      vApplyList.Add(TempList[i]);
  end;
end;

class procedure TBoldQueueable.ApplyAllMatching(anObject: TObject);
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    with BoldInstalledQueue do
    for I := fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(fApplyList[I]);
      if Assigned(Queueable) and (Queueable.MatchObject = anObject) then
        Queueable.Apply;
    end;
end;

class procedure TBoldQueueable.DiscardChangeAll;
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    with BoldInstalledQueue do
    for I := fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(fApplyList[I]);
      if Assigned(Queueable) then
        Queueable.DiscardChange;
    end;
end;

class procedure TBoldQueueable.DiscardChangeAllMatching(anObject: TObject);
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    with BoldInstalledQueue do
    for I := fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(fApplyList[I]);
      if Assigned(Queueable) and (Queueable.MatchObject = anObject) then
        Queueable.DiscardChange;
    end;
end;

class procedure TBoldQueueable.AddToPostDisplayQueue(Event: TNotifyEvent;
  Sender, Receiver: TObject);
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.AddEventToPostDisplayQueue(Event, Sender, Receiver);
end;

class procedure TBoldQueueable.AddToPreDisplayQueue(Event: TNotifyEvent;
  Sender, Receiver: TObject);
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.AddEventToPredisplayQueue(Event, Sender, Receiver);
end;

class procedure TBoldQueueable.RemoveFromPreDisplayQueue(Receiver: TObject);
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.RemoveFromPreDisplayQueue(receiver);
end;

class procedure TBoldQueueable.RemoveFromPostDisplayQueue(Receiver: TObject);
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.RemoveFromPostDisplayQueue(receiver);
end;

class procedure TBoldQueueable.ActivateDisplayQueue;
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.ActivateDisplayQueue;
end;

class procedure TBoldQueueable.DeActivateDisplayQueue;
begin
  if not BoldQueueFinalized then
    BoldInstalledQueue.DeActivateDisplayQueue;
end;

{ TBoldQueue }

procedure TBoldQueue.AddEventToPostDisplayQueue(Event: TNotifyEvent; Sender,
  Receiver: TObject);
begin
  fPostDisplayQueue.Add(Event, Sender, Receiver);
  EnsureDequeing;
end;

procedure TBoldQueue.AddEventToPredisplayQueue(Event: TNotifyEvent;
  Sender, Receiver: TObject);
begin
  fPreDisplayQueue.Add(Event, Sender, Receiver);
  EnsureDequeing;
end;

procedure TBoldQueue.AddToApplyList(Queueable: TBoldQueueable);
begin
  fApplyList.Add(Queueable);
end;

procedure TBoldQueue.AddToDisplayList(Queueable: TBoldQueueable);
begin
  fDisplayList.Add(Queueable);
  EnsureDequeing;
end;

constructor TBoldQueue.Create;
begin
  inherited;
  fPreDisplayQueue := TBoldEventQueue.Create;
  fPostDisplayQueue := TBoldEventQueue.Create;
  fDisplayList := TList.Create;
  fApplyList := TList.Create;
  DisplayMode := dmDisplayAll;
  ActivateDisplayQueue;
end;

destructor TBoldQueue.Destroy;
begin
  DeActivateDisplayQueue;
  FreeAndNil(fDisplayList);
  FreeAndNil(fApplyList);
  FreeAndNil(fPreDisplayQueue);
  FreeAndNil(fPostDisplayQueue);
  inherited;
end;

function TBoldQueue.DisplayAll: Boolean;
begin
  result := TBoldQueueable.DisplayAll;
end;

function TBoldQueue.DisplayOne: Boolean;

var
  Queueable, vPrioritizedQueuable: TBoldQueueable;
begin
  result := not fIsDisplaying and (fDisplayList.Count > 0);
  if result then
  begin
    Queueable := nil;
    fIsDisplaying := true;
    try
      while (fDisplayListIndex < fDisplayList.Count) do
      begin
        Queueable := TBoldQueueable(fDisplayList[fDisplayListIndex]);

        if not Assigned(Queueable) then
          inc(fDisplayListIndex)
        else
        if not Queueable.IsInDisplayList then
        begin
          fDisplayList[fDisplayListIndex] := nil;
          Queueable.ToBeRemovedFromDisplayList := false;
          inc(fDisplayListIndex);
        end
        else
          break;
      end;
      if fDisplayListIndex < fDisplayList.Count then
      begin
        vPrioritizedQueuable := Queueable.MostPrioritizedQueuableOrSelf;
        if Queueable = vPrioritizedQueuable then
        begin
//          BoldLogQueue('D:'+Queueable.DebugInfo);
          Queueable.Display;
          if Queueable.ToBeRemovedFromDisplayList and (fDisplayList[fDisplayListIndex] = Queueable) then
          begin
            fDisplayList[fDisplayListIndex] := nil;
            Queueable.ToBeRemovedFromDisplayList := false;
          end;
        end
        else
        begin
//          BoldLogQueue('D:'+Queueable.DebugInfo);
          vPrioritizedQueuable.Display;
      end;
      end;
    finally
      fIsDisplaying := false;
      if fDisplayListIndex >= fDisplayList.Count then
      begin
        fDisplayListIndex := 0;
        fDisplayList.Count := 0;
      end;
    end;
  end;
end;

function TBoldQueueable.AfterInPriority(Queueable: TBoldQueueable): Boolean;
begin
  if not Assigned(PrioritizedQueuable) then
    Result := false
  else if PrioritizedQueuable = Queueable then
    Result := true
  else Result :=  PrioritizedQueuable.AfterInPriority(Queueable);
end;

function TBoldQueueable.StronglyPrioritizedSibbling(
  Queueable: TBoldQueueable): Boolean;
begin
  result :=
    Queueable.StronglyDependedOfPrioritized and
    (Queueable.PrioritizedQueuable = PrioritizedQueuable);
end;

procedure TBoldQueue.EnsureDequeing;
begin

end;

function TBoldQueue.GetApplyCount: integer;
begin
  result := fApplyList.Count;
end;

function TBoldQueue.GetDisplayCount: integer;
begin
  result := fDisplayList.Count;
end;

function TBoldQueue.GetPreDisplayCount: integer;
begin
  result := fPreDisplayQueue.Count;
end;

function TBoldQueue.GetPostDisplayCount: integer;
begin
  result := fPostDisplayQueue.Count;
end;

function TBoldQueue.GetEmpty: boolean;
begin
  result := (DisplayCount = 0) and (ApplyCount = 0) and (PreDisplayCount = 0) and (PostDisplayCount = 0);
end;

procedure TBoldQueue.PerformPostDisplayQueue;
begin
  fPostDisplayQueue.DequeueAll;
end;

procedure TBoldQueue.PerformPreDisplayQueue;
begin
  fPreDisplayQueue.DequeueAll;
end;

procedure TBoldQueue.RemoveFromApplyList(Queueable: TBoldQueueable);
var
  index: integer;
begin
  Index := fApplyList.Count - 1;
  while (Index >= 0) and (fApplyList.List[Index] <> Queueable) do
    Dec(Index);
  if Index > -1 then
    if Index = fApplyList.Count - 1 then
      fApplyList.Count := fApplyList.Count - 1
    else
      fApplyList[Index] := nil;
end;

procedure TBoldQueue.RemoveFromDisplayList(Queueable: TBoldQueueable; ADestroying: boolean);
var
  index: integer;
begin
//  Log('R:'+Queueable.ClassName);
  if (fDisplayList.Last = Queueable) then
  begin
    Queueable.ToBeRemovedFromDisplayList := false;
    fDisplayList.Delete(fDisplayList.Count-1);
    exit;
  end;
  if not ADestroying then
  begin
    Queueable.ToBeRemovedFromDisplayList := true;
    exit;
  end;
  Index := fDisplayList.Count - 1;
  while (Index >=0 ) and (fDisplayList.List[Index] <> Queueable) do
    Dec(Index);
  if Index > -1 then
  begin
    Queueable.ToBeRemovedFromDisplayList := false;
    if Index = fdisplayList.Count - 1 then
      fDisplayList.Count := fDisplayList.Count - 1
    else
      fDisplayList[Index] := nil;
  end;
end;

procedure TBoldQueue.RemoveFromPostDisplayQueue(Receiver: TObject);
begin
  fPostDisplayQueue.RemoveAllForReceiver(Receiver);
end;

procedure TBoldQueue.RemoveFromPreDisplayQueue(Receiver: TObject);
begin
  fPreDisplayQueue.RemoveAllForReceiver(Receiver);
end;

end.