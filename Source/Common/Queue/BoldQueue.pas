unit BoldQueue;

interface

uses
  Classes,
  BoldBase,
  BoldEventQueue;

const
  { Queueable }
  befIsInDisplayList = BoldElementFlag0;
  befStronglyDependedOfPrioritized = BoldElementFlag1;
  { Follower }
  befFollowerSelected = BoldElementFlag2;

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
    procedure RemoveFromDisplayList;
    function MostPrioritizedQueuable: TBoldQueueable;
    function MostPrioritizedQueuableOrSelf: TBoldQueueable;
    function AfterInPriority(Queueable: TBoldQueueable): Boolean;
    procedure Display; virtual; abstract;
    function StronglyPrioritizedSibbling(Queueable: TBoldQueueable): Boolean;
    class function DisplayOne: Boolean;
  public
    constructor Create(aMatchObject: TObject);
    destructor Destroy; override;
    class function DisplayAll: Boolean;
    class procedure ApplyAll;
    class procedure ApplyAllMatching(anObject: TObject);
    class procedure DiscardChangeAll;
    class procedure DiscardChangeAllMatching(anObject: TObject);
    class procedure AddToPreDisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    class procedure RemoveFromPreDisplayQueue(Receiver: TObject);
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
    fDisplayList: TList;
    fApplyList: TList;
  protected
    function DisplayOne: Boolean;
    function DisplayAll: Boolean;
    procedure AddToApplyList(Queueable: TBoldQueueable);
    procedure RemoveFromApplyList(Queueable: TBoldQueueable);
    procedure AddToDisplayList(Queueable: TBoldQueueable);
    procedure RemoveFromDisplayList(Queueable: TBoldQueueable);
    procedure EnsureDequeing; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddEventToPredisplayQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    procedure RemoveFromPreDisplayQueue(Receiver: TObject);
    procedure PerformPreDisplayQueue;
    procedure DeActivateDisplayQueue; virtual; abstract;
    procedure ActivateDisplayQueue; virtual; abstract;
    property DisplayMode: TBoldQueueDisplayMode read fDisplayMode write fDisplayMode;
  end;

  function BoldQueueFinalized: Boolean;
  function BoldInstalledQueue: TBoldQueue;

implementation

uses
  SysUtils,
  BoldUtils,
  Forms,  // Application object
  BoldEnvironment,
  BoldGuard;

function BoldQueueFinalized: Boolean;
begin
  Result := (BoldEnvironmentsFinalized) or BoldEffectiveEnvironment.QueueFinalized;
end;

function BoldInstalledQueue: TBoldQueue;
begin
  if not BoldEnvironmentsFinalized then
    Result := BoldEffectiveEnvironment.Queue
  else
    Result := nil;
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
  if IsInDisplayList then
    RemoveFromDisplayList;
  inherited Destroy;
end;

procedure TBoldQueueable.AddToApplyList;
begin
  if not (BoldQueueFinalized) then
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
    BoldInstalledQueue.AddToDisplayList(self);
    SetElementflag(befIsInDisplayList, True);
  end;
end;

procedure TBoldQueueable.RemoveFromDisplayList;
begin
  if not (BoldQueueFinalized) then
    BoldInstalledQueue.RemoveFromDisplayList(self);
  SetElementflag(befIsInDisplayList, False);
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
      for i := 0 to BoldInstalledQueue.fDisplayList.Count - 1 do
      begin
        Queueable := BoldInstalledQueue.fDisplayList[i];
        if assigned(Queueable) and
          Queueable.AfterInPriority(PrioritizedQueuable) and
          not StronglyPrioritizedSibbling(Queueable) and
          not Queueable.AfterInPriority(self) then    // will also eliminate self
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
  if BoldInstalledQueue <> nil then
    Result := BoldInstalledQueue.DisplayOne
  else
    Result := False;
end;

class function TBoldQueueable.DisplayAll: Boolean;
begin
  Result := (BoldInstalledQueue <> nil) and (BoldInstalledQueue.fDisplayList.Count <> 0);
  if Result then
    while DisplayOne do
      {nothing};
end;

class procedure TBoldQueueable.ApplyAll;
var
  Queueable: TBoldQueueable;
  tempList: TList;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(TempList);
  if BoldInstalledQueue <> nil then
  begin
    tempList := TList.Create;
    while BoldInstalledQueue.fApplyList.Count > 0 do
    begin
      Queueable := BoldInstalledQueue.fApplyList.Last;
      if Assigned(Queueable) then
      begin
        Queueable.Apply; // This will remove object from list
        if  (BoldInstalledQueue.fApplyList.Count > 0) and (Queueable = BoldInstalledQueue.fApplyList.LAst) then
        begin
          // for some reason the apply failed,
          // remove the queueable from the applylist and put it back when it is empty...
          TempList.Add(Queueable);
          BoldInstalledQueue.fApplyList.Count := BoldInstalledQueue.fApplyList.Count - 1;
        end;
      end
      else
        BoldInstalledQueue.fApplyList.Count := BoldInstalledQueue.fApplyList.Count - 1; //Skip nil items
    end;
    for i := 0 to TempList.Count - 1 do
      BoldInstalledQueue.fApplyList.Add(TempList[i]);
  end;
end;

class procedure TBoldQueueable.ApplyAllMatching(anObject: TObject);
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    for I := BoldInstalledQueue.fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(BoldInstalledQueue.fApplyList[I]);
      if Assigned(Queueable) and (Queueable.MatchObject = anObject) then
        Queueable.Apply; // This will remove from list
    end;
end;

class procedure TBoldQueueable.DiscardChangeAll;
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    for I := BoldInstalledQueue.fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(BoldInstalledQueue.fApplyList[I]);
      if Assigned(Queueable) then
        Queueable.DiscardChange; // This will remove from list
    end;
end;

class procedure TBoldQueueable.DiscardChangeAllMatching(anObject: TObject);
var
  Queueable: TBoldQueueable;
  I: Integer;
begin
  if BoldInstalledQueue <> nil then
    for I := BoldInstalledQueue.fApplyList.Count - 1 downto 0 do
    begin
      Queueable := TBoldQueueable(BoldInstalledQueue.fApplyList[I]);
      if Assigned(Queueable) and (Queueable.MatchObject = anObject) then
        Queueable.DiscardChange; // This will remove from list
    end;
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

{ TBoldQueue }

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
  fDisplayList := TList.Create;
  fApplyList := TList.Create;
  if BoldRunningAsDesignTimePackage then
    DisplayMode := dmDisplayAll
  else
    DisplayMode := dmDisplayOne;
  ActivateDisplayQueue;
end;

destructor TBoldQueue.Destroy;
begin
  FreeAndNil(fDisplayList);
  FreeAndNil(fApplyList);
  FreeAndNil(fPreDisplayQueue);
  inherited;
end;

function TBoldQueue.DisplayAll: Boolean;
begin
  result := TBoldQueueable.DisplayAll;
end;

function TBoldQueue.DisplayOne: Boolean;
var
  Index: Integer;
  ExchangeWith,
  Queueable: TBoldQueueable;
begin
  if fIsDisplaying then
    result := false
  else
  begin
    fIsDisplaying := true;
    try
      Queueable := nil;
      //Remove Empty slots from the end of the queue.
      while (fDisplayList.Count>0) and (fDisplayList.Last=nil) do
        fDisplayList.Count := fDisplayList.Count - 1;
      Result := fDisplayList.Count>0;
      if Result then
      begin
        try
          //Check if there is a queueable that must be displayed before.
          ExchangeWith := TBoldQueueable(fDisplayList.Last).MostPrioritizedQueuable;
          if Assigned(ExchangeWith) then
          begin
            //Exchange place
            Index := fDisplayList.IndexOf(ExchangeWith);
            if Index <> -1 then
              fDisplayList.Exchange(Index, fDisplayList.Count - 1);
          end;
          //Display Last queueable in list.
          Queueable := TBoldQueueable(fDisplayList.Last);
          Queueable.Display;
        except
          Application.HandleException(Queueable);
        end;
      end;
    finally
      fIsDisplaying := false;
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

procedure TBoldQueue.PerformPreDisplayQueue;
begin
  fPreDisplayQueue.DequeueAll;
end;

procedure TBoldQueue.RemoveFromApplyList(Queueable: TBoldQueueable);
var
  index: integer;
begin
  //Implemented own IndexOf because it is more effective to search the list from the end in this case.
  Index := fApplyList.Count - 1;
  while (Index >= 0) and (fApplyList.List[Index] <> Queueable) do  // marco
    Dec(Index);
  //Insert nil element in list or decrement count if it's the last object
  if Index > -1 then
    if Index = fApplyList.Count - 1 then
      fApplyList.Count := fApplyList.Count - 1
    else
      fApplyList[Index] := nil;
end;

procedure TBoldQueue.RemoveFromDisplayList(Queueable: TBoldQueueable);
var
  index: integer;
begin
  //Implemented own IndexOf because it is more effective to search the list from the end in this case.
  Index := fdisplayList.Count - 1;
  while (Index >=0 ) and (fdisplayList.List[Index] <> Queueable) do  // marco removed ^  (also above)
    Dec(Index);
  //Insert nil element in list or decrement count if it's the last object
  if Index > -1 then
    if Index = fdisplayList.Count - 1 then
      fdisplayList.Count := fdisplayList.Count - 1
    else
      fdisplayList[Index] := nil;
end;

procedure TBoldQueue.RemoveFromPreDisplayQueue(Receiver: TObject);
begin
   fPreDisplayQueue.RemoveAllForReceiver(Receiver);
end;

end.
