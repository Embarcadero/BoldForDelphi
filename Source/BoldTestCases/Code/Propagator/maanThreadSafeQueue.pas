unit maanThreadSafeQueue;

interface

uses
  TestSuite,
  BoldThreadSafeQueue,
  classes,
  TestFramework;

type

  TTestThread = class(TThread)
  private
    procedure NotifyQueueNotEmpty(Queue:  TBoldThreadSafeQueue);
  public
    constructor Create;
    procedure Execute; override;
    procedure Dequeue;
  end;

  TMaan_TestCaseThreadSafeQueue = class(TBoldTestCase)
  private
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  dialogs,
  windows;

var
  ThreadSafeQueue : TBoldThreadSafeObjectQueue;
  TestThread : TTestThread;
  eventcount: integer;
  fQueueCreatedEvent: THandle;

{ TMaan_TestCaseThreadSafeQueue }

procedure TMaan_TestCaseThreadSafeQueue.SetUp;
begin
  inherited;
  fQueueCreatedEvent := CreateEvent(nil, True, False, '');
  eventcount := 0;
  ThreadSafeQueue := TBoldThreadSafeObjectQueue.Create('TSQ-Testcase/TSQ');
  WaitForSingleObject(fqueueCreatedEvent, 1000);
  TestThread := TTestThread.Create;
  ThreadSafeQueue.OnQueueNotEmpty := TestThread.NotifyQueueNotEmpty;
end;

class procedure TMaan_TestCaseThreadSafeQueue.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('Test', 'Testing TBoldThreadSafeObjectQueue'));
end;

class function TMaan_TestCaseThreadSafeQueue.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', 'Testing TBoldThreadSafeObjectQueue');
end;

procedure TMaan_TestCaseThreadSafeQueue.TearDown;
begin
  inherited;
  FreeAndNil(ThreadSafeQueue);
  FreeAndNil(TestThread);
end;

procedure TMaan_TestCaseThreadSafeQueue.Test;
begin
  ThreadSafeQueue.Enqueue(TObject.Create);
  ThreadSafeQueue.Enqueue(TObject.Create);
  ThreadSafeQueue.Enqueue(TObject.Create);
  sleep(1000);
  Assert(eventcount = 3, 'ThreadSafeQueue');
end;

{ TTestThread }

constructor TTestThread.Create;
begin
  inherited Create(true);
  SetEvent(fQueueCreatedEvent);
end;
                                                      
procedure TTestThread.Dequeue;
var
  obj: TObject;
begin
  while ThreadSafeQueue.count > 0 do
  begin
    obj := ThreadSafeQueue.Dequeue;
    Inc(eventcount);
    FreeAndNil(Obj);
  end;
end;

procedure TTestThread.Execute;
begin
  while not Terminated do
  begin
    try
      Dequeue;
      Suspend;
    except
      ThreadSafeQueue.OnQueueNotEmpty := nil;
    end;
  end;
end;

procedure TTestThread.NotifyQueueNotEmpty(Queue: TBoldThreadSafeQueue);
begin
  Resume;
end;

initialization
  TestGlobal.RegisterTestCase(TMaan_TestCaseThreadSafeQueue);

finalization
  TestGlobal.UnRegisterTestCase(TMaan_TestCaseThreadSafeQueue);

end.
