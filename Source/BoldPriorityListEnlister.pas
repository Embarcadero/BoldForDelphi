
{ Global compiler directives }
{$include bold.inc}
unit BoldPriorityListEnlister;

interface

uses
  SysUtils,
  BoldUtils,
  BoldClientQueue,
  BoldPriorityQueue,
  BoldDefs,
  BoldClientHandler,
  classes;

type
  {forward declarations}
  TBoldAbstractPriorityListEnlister = class;
  TBoldPriorityListEnlister = class;
  TBoldClientQueueInfo = class;

  TBoldAbstractPriorityListEnlister = class
  public
    procedure EnlistQueue(ClientQueue: TBoldClientQueue); virtual; abstract;
    procedure FlushQueue(ClientQueue: TBoldClientQueue); virtual; abstract;
  end;

  TBoldPriorityListEnlister = class(TBoldAbstractPriorityListEnlister)
  private
    fPriorityList: TBoldPriorityQueue;
  protected
    function GetClientHandler: TBoldClientHandler; virtual;
    property ClientHandler: TBoldClientHandler read GetClientHandler;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure EnlistQueue(ClientQueue: TBoldClientQueue); override;
    procedure FlushQueue(ClientQueue: TBoldClientQueue); override;
    property PriorityList: TBoldPriorityQueue read fPriorityList;
  end;

  TBoldClientQueueInfo = class(TBoldPriorityQueueItem)
  private
    fTimeOut: TTimeStamp;
    fClientQueue: TBoldClientQueue;
    function getQueueAsVarArray: variant;
    function getClientId: TBoldClientID;
    function getCount: Integer;
  public
    constructor Create(const TimeOut: TTimeStamp; ClientQueue: TBoldClientQueue);
    destructor Destroy; override;
    function HasHigherPriorityThan(Item: TBoldPriorityQueueItem): Boolean; override;
    property TimeOut: TTimeStamp read fTimeOut write fTimeOut;
    property QueueAsVarArray: variant read getQueueAsVarArray;
    property ClientID: TBoldClientId read getClientID;
    property Count: Integer read getCount;
  end;

implementation

uses
  BoldPropagatorServer,
  windows;

{ TBoldClientQueueInfo }

constructor TBoldClientQueueInfo.Create(const TimeOut: TTimeStamp;
  ClientQueue: TBoldClientQueue);
begin
  inherited Create;
  fTimeOut.Time := TimeOut.Time;
  fTimeOut.Date := TimeOut.Date;
  fClientQueue := ClientQueue;
end;

destructor TBoldClientQueueInfo.Destroy;
begin
  fClientQueue := nil;
  inherited;
end;

function TBoldClientQueueInfo.getClientId: TBoldClientID;
begin
  Result := fClientQueue.BoldClientID;
end;

function TBoldClientQueueInfo.getCount: Integer;
begin
  Result := fClientQueue.Count;
end;

function TBoldClientQueueInfo.getQueueAsVarArray: variant;
begin
  Result := fClientQueue.AsVarArray;
end;

function TBoldClientQueueInfo.HasHigherPriorityThan(
  Item: TBoldPriorityQueueItem): Boolean;
begin
  Assert(Item is TBoldClientQueueInfo, Format('%s.HasHigherPriority: Item is not a TBoldClientQueueInfo', [ClassName]));
  Result := (TimeOut.Date <= (Item as TBoldClientQueueInfo).TimeOut.Date) and
            (TimeOut.Time < (Item as TBoldClientQueueInfo).TimeOut.Time) ;
end;

{ TBoldPriorityListEnlister }

constructor TBoldPriorityListEnlister.Create;
begin
  inherited;
  fPriorityList := TBoldPriorityQueue.Create;
end;

destructor TBoldPriorityListEnlister.Destroy;
var
  obj: TBoldPriorityQueueItem;
begin
  fPriorityList.OnHeadChanged := nil;
  Obj:= fPriorityList.Head;
  while Assigned(Obj) do
  begin
    fPriorityList.RemoveAndFreeHead;
    Obj := fPriorityList.Head;
  end;
  FreeAndNil(fPriorityList);
  inherited;
end;

procedure TBoldPriorityListEnlister.EnlistQueue(ClientQueue: TBoldClientQueue);
var
  ClientQueueInfo: TBoldClientQueueInfo;
  CurrentTime: TTimeStamp;
  PollingInt: TTimeStamp;
  ClientIdString: string;
  LeaseDuration, PollingInterval: integer;
  LeaseTimeOut: TTimeStamp;
  Initialized: Boolean;
begin
  if Assigned(ClientQueue) then
  begin
    if ClientHandler.HasInfoForClient(TBoldClientQueue(ClientQueue).BoldClientID, ClientIdString, LeaseDuration,
          PollingInterval, LeaseTimeOut, Initialized) and Initialized then
      try
        PollingInt.time := PollingInterval;
        PollingInt.Date := DateTimeToTimeStamp(0).Date;
        CurrentTime := DateTimeToTimeStamp(Now + TimeStampToDateTime(PollingInt));
        ClientQueueInfo := TBoldClientQueueInfo.Create(CurrentTime, ClientQueue);
        fPriorityList.Add(ClientQueueInfo);
      except
        FreeAndNil(ClientQueueInfo);
      end;
  end;
end;

procedure TBoldPriorityListEnlister.FlushQueue(
  ClientQueue: TBoldClientQueue);
var
  ClientID: TBoldClientID;
  ClientQueueInfo: TBoldClientQueueInfo;
  CurrentTime: TTimeStamp;
begin
  if Assigned(ClientQueue) then
  begin
    ClientID:= TBoldClientQueue(ClientQueue).BoldClientID;
    if ClientHandler.IsRegistered(ClientID) then
    begin
      try
        CurrentTime.Time := 0;
        CurrentTime.Date := 0;
        ClientQueueInfo := TBoldClientQueueInfo.Create(CurrentTime, ClientQueue);
        fPriorityList.Add(ClientQueueInfo);
      except
        FreeAndNil(ClientQueueInfo);
      end;
    end
    else
      ClientQueue.Clear;
  end;
end;

function TBoldPriorityListEnlister.GetClientHandler: TBoldClientHandler;
begin
  Result := TBoldPropagatorServer.Instance.AdvancedPropagator.ClientHandler;
end;

end.
