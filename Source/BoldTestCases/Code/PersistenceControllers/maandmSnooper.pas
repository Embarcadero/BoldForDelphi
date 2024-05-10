unit maandmSnooper;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  BoldUtils,
  SysUtils,
  Controls,
  Forms,
  BoldHandles,
  BoldSystem,
  BoldSystemHandle,
  BoldPersistenceHandlePassthrough,
  BoldSnooperHandle,
  BoldSubscription,
  BoldHandle,
  BoldUpdatePrecondition,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  TestModel1,
  Classes,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldListenerHandle,
  BoldIDAdderHandle,
  dmModel1,
  TestSuite,
  BoldPropagatorInterfaces_TLB,
  BoldPersistenceController,
  BoldID,
  BoldValueSpaceInterfaces,
  Boldcondition,
  BoldDefs,
  comobj,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM,
  BoldAbstractLockManagerHandle,
  BoldLockManagerHandleCom,
  BoldThread,
  BoldPersistenceHandlePTWithModel,
  TestFramework,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  BoldDatabaseAdapterUniDAC,
  DBAccess,
  Uni;

const
  FLOATCONST1: Real = 1.1;
  INTEGERCONST1: Integer = 1;
  STRINGCONST1: String = 'aa';
  BOOLEANCONST1: Boolean = true;
  CURRENCYCONST1: Currency = 0.1;

type
  {forward declarations}
  TDummyPropagator = class;
  TDummyPropagatorHandle = class;
  TTestCaseSnooper_Object = class;
  TTestCaseSnooper_EmbeddedState = class;
  TTestCaseSnooper_NonEmbeddedState = class;
  TTestCaseSnooper_FetchObject = class;
  TTestCaseSnooper_FetchClass = class;
  Tmaan_BoldIDAdder = class;
  TPropagatorThread = class;
  TFakePM = class;

  Tdm = class(TDataModule)
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldIdAdderHandle1: TBoldIdAdderHandle;
    BoldListenerHandle1: TBoldListenerHandle;
    DirectBoldHandle: TBoldSystemHandle;
    blhClassA: TBoldListHandle;
    blhThing: TBoldListHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    UniConnection1: TUniConnection;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fEventlist: TStringlist;
    fSubscriptionList: TStringList;
    fCancelledSubscriptionList: TStringList;
    fPropagatorHandle: TDummyPropagatorHandle;
    fFakePM: TFakePM;


  public
    { Public declarations }
    procedure Save;
    procedure ClearEvents;
    property EventList: TStringlist read fEventList write fEventList;
    property SubscriptionList: TStringList read fSubscriptionlist write fSubscriptionList;
    property CancelledSubscriptionList: TStringList read fCancelledSubscriptionlist write fCancelledSubscriptionList;
    property PropagatorHandle: TDummypropagatorHandle read fPropagatorHandle write fPropagatorHandle;
  end;

  Tmaan_BoldIDAdder = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GetID;
  end;

  TTestCaseSnooper_Object = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NewObject;
    procedure DeleteObject;
  end;

  TTestCaseSnooper_EmbeddedState = class(TBoldTestCase)
  public
    procedure Setup; override;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure TearDown; override;
  published
    procedure ModifyEmbeddedState;
  end;

  TTestCaseSnooper_NonEmbeddedState = class(TBoldTestCase)
  public
    procedure Setup; override;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure TearDown; override;
  published
    procedure ModifyNonEmbeddedState;
    procedure DeleteNonEmbeddedState;
    procedure NewNonEmbeddedState;
  end;

  TTestCaseSnooper_FetchObject = class(TBoldTestCase)
  public
    procedure Setup; override;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure TearDown; override;
  published
    procedure FetchObject;
  end;

  TTestCaseSnooper_FetchClass = class(TBoldTestCase)
  public
    procedure Setup; override;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure TearDown; override;
  published
    procedure FetchClass;
  end;

  {TDummyPropagator}
  TDummyPropagator = class(TInterfacedObject, IBoldEventPropagator, IBoldClientHandler)
    function  SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function  AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
    function  ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out Extended: WordBool): HResult; stdcall;
    function  UnregisterClient(ClientID: Integer): HResult; stdcall;
  end;

  {TDummyPropagatorhandle}
  TDummyPropagatorhandle = class(TBoldAbstractPropagatorHandle)
  private
    FConnected: Boolean;
    FPropagatorThread: TPropagatorThread;
    FPropagator: IUnknown;
  protected
    function GetClientHandler: IBoldClientHandler; override;
    function GetEventPropagator: IBoldEventPropagator; override;
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoPropagatorCallFailed(Sender: TObject; const ErrorMessage: string); override;
    property Connected: Boolean read getConnected write FConnected;
  end;

  {TFakePM}
  TFakePM = class(TBoldPersistenceController)
  public
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var ServerTime: TDateTime; BoldClientID: TBoldClientID); override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                    TranslationList: TBoldIdTranslationList); override;

  end;

  {class for generating data}
  TDataGen = class
  public
    class function NewClassA(System: TBoldSystem): TClassA;
    class function NewThing(System: TBoldSystem): TThing;
    class function NewLinkClass(System: TBoldSystem): TLinkClass;
    class function NewSong(System: TBoldSystem): TSong;
    class function NewHitList(System: TBoldSystem): THitList;
  end;

  {TPropagatorThread}
  TPropagatorThread = class(TBoldNotifiableThread)
  public
    FStream: Pointer;
    procedure Execute; override;
  end;

const
  FAKECLIENTID = 9;

var
  dm: Tdm = nil;

implementation
uses
  BoldSnooper,
  BoldIDAdder,
  BoldObjectSpaceExternalEvents,
  BoldListenerThread,
  BoldListenerCOM,
  BoldGuard,
  dialogs,
  ActiveX,
  windows;

{$R *.DFM}

procedure EnsureDB;
begin
  if not dm.BoldSystemHandle1.Active then
  begin
    dm.BoldDatabaseAdapterUniDAC1.CreateDatabase;
    dm.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dm.BoldSystemHandle1.Active := True;
    dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle;
    dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  end;
end;

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not Assigned(dm) then
    Application.CreateForm(Tdm, dm);
  EnsureDB;
end;

procedure DeleteAllObjects;
var
  i, count: integer;
begin
  dm.blhClassA.Enabled := true;
  dm.blhThing.Enabled := true;
  count:= dm.blhClassA.Count;
  for i:= 0 to count - 1 do
    (dm.blhClassA.CurrentBoldObject).delete;
  count:= dm.blhThing.Count;
  for i:= 0 to count - 1 do
    (dm.blhThing.CurrentBoldObject).Delete;
  dm.Save;
  dm.blhClassA.Enabled := false;
  dm.blhThing.Enabled := false;
end;

procedure FreeDM;
begin
  FreeAndNil(dm);
end;

  {TTestCaseSnooper_NonEmbeddedState}
class procedure TTestCaseSnooper_NonEmbeddedState.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('NewNonEmbeddedState', 'BoldSnooper: NewNonEmbeddedState'));
  ASuite.AddTest(CreateWithComment('ModifyNonEmbeddedState', 'BoldSnooper: ModifyNonEmbeddedState'));
  ASuite.AddTest(CreateWithComment('DeleteNonEmbeddedState', 'BoldSnooper: DeleteNonEmbeddedState'));
end;

procedure TTestCaseSnooper_NonEmbeddedState.NewNonEmbeddedState;
var
  aThing1: TThing;
  aThing2: TThing;
  eventName: string;
begin
  aThing1 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing2 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  dm.Save;
  aThing2.one := aThing1;
  dm.ClearEvents;
  dm.Save;
  // Expected events: ClassEvents for BusinessClassesRoot and LinkClass, the Many-link and the One-link. Total: 4
  Assert(dm.EventList.Count = 4, 'NewNonembeddedState: Incorrect event count');
  Assert(dm.SubscriptionList.Count = 1, 'NewNonembeddedState: no subscriptions expected');
  Assert(dm.CancelledSubscriptionList.Count = 0, 'NewNonembeddedState: no canceled subscriptions expected');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'LinkClass', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'BusinessClassesRoot', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
                           '', '', aThing2.oneLinkClass.BoldObjectLocator.BoldObjectID );
  Assert(dm.SubscriptionList.IndexOf(EventName) <> -1, 'NewNonEmbeddedState: event mismatch');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
                           'one', '', aThing2.BoldObjectLocator.BoldObjectID);
  Assert(dm.EventList.IndexOf(EventName) <> -1, 'NewNonEmbeddedState: event mismatch');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
                          'many', '', aThing1.BoldObjectLocator.BoldObjectID );
  Assert(dm.EventList.IndexOf(EventName) <> -1, 'NewNonEmbeddedState: event mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_NonEmbeddedState.ModifyNonEmbeddedState;
var
  aThing1: TThing;
  aThing2: TThing;
  aThing3: TThing;
  event, subscription: string;
  OldLInkId: TBoldObjectID;
  G: IBoldGuard;
  EventName: String;
begin
  G := TBoldGuard.Create(OldLInkId);
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle;
  aThing1 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing2 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing3 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing2.one := aThing1;
  dm.Save;
  oldLinkId := aThing2.oneLinkClass.BoldObjectLocator.BoldObjectID.Clone;
  aThing2.one := aThing3;
  dm.ClearEvents;
  dm.Save;
  // ExpectedEvents: ClassEvent for BusinessClassesRoot and LinkClass
  // EmbeddedChange for the deleted linkobject
  // Nonembedded: aThing1.Many, aThing3.many and aThing2.one
  // total: 6
  Assert(dm.EventList.Count = 6, 'ModifyNonEmbeddedState: Incorrect event count');
  Assert(dm.SubscriptionList.Count = 1, 'ModifyNonEmbeddedState: no subscriptions expected');
  Assert(dm.CancelledSubscriptionList.Count = 1, 'ModifyNonEmbeddedState: no canceled subscriptions expected');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'BusinessClassesRoot', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'LinkClass', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, '',
                           '','', oldLinkID );
  subscription := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
                           '','', oldLinkID );
  Assert(dm.EventList.IndexOf(event) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(subscription) <> -1, 'ModifyNonEmbeddedState: event mismatch');

  Assert(dm.SubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
    '', '', aThing2.oneLinkClass.BoldObjectLocator.BoldObjectID )) <> -1, 'ModifyNonEmbeddedState: event mismatch');

  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
             'one', '', aThing2.BoldObjectLocator.BoldObjectID)) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
             'many', '', aThing1.BoldObjectLocator.BoldObjectID )) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
             'many', '', aThing3.BoldObjectLocator.BoldObjectID )) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_NonEmbeddedState.DeleteNonEmbeddedState;
var
  aThing1: TThing;
  aThing2: TThing;
  event, subscription: string;
  oldLinkID: TBoldObjectID;
  G: IBoldGuard;
  EventName: String;
begin
  G := TBoldGuard.Create(OldLInkId);
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);
  aThing1 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing2 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  aThing2.one := aThing1;
  dm.Save;
  oldLinkID := aThing2.oneLinkClass.BoldObjectLocator.BoldObjectID.Clone;
  aThing2.one := nil;
  dm.ClearEvents;
  dm.Save;
  // Expected events: ClassEvents for BusinessClassesRoot and LinkClass
  // Embedded change for the deleted linkobject
  // nonembedded statechanges: aThing1.many, aThing2.one
  // total: 5
  Assert(dm.EventList.Count = 5, 'ModifyNonEmbeddedState: Incorrect event count');
  Assert(dm.SubscriptionList.Count = 0, 'ModifyNonEmbeddedState: no subscriptions expected');
  Assert(dm.CancelledSubscriptionList.Count = 1, 'ModifyNonEmbeddedState: no canceled subscriptions expected');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'BusinessClassesRoot', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'LinkClass', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, '',
                           '', '', oldLinkID );
  subscription := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
                           '', '', oldLinkID );
  Assert(dm.EventList.IndexOf(event) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(subscription) <> -1, 'ModifyNonEmbeddedState: event mismatch');

  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
                           'one', '', aThing2.BoldObjectLocator.BoldObjectID)) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfobjectChanged, '',
                          'many', '', aThing1.BoldObjectLocator.BoldObjectID )) <> -1, 'ModifyNonEmbeddedState: event mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_NonEmbeddedState.Setup;
begin
  inherited;
  EnsureDM;
end;

procedure TTestCaseSnooper_NonEmbeddedState.TearDown;
begin
  FreeDm;
end;

class function TTestCaseSnooper_NonEmbeddedState.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'NewNonEmbeddedState', 'BoldSnooper: NewNonEmbeddedState');
  SetCommentForTest(result, 'ModifyNonEmbeddedState', 'BoldSnooper: ModifyNonEmbeddedState');
  SetCommentForTest(result, 'DeleteNonEmbeddedState', 'BoldSnooper: DeleteNonEmbeddedState');
end;

{TTestCaseSnooper_EmbeddedState}
class procedure TTestCaseSnooper_EmbeddedState.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('ModifyEmbeddedState', 'BoldSnooper: ModifyEmbeddedState'));
end;

procedure TTestCaseSnooper_EmbeddedState.ModifyEmbeddedState;
var
  Object1: TClassA;
  oldvalue: string;
begin
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);
  Object1 := TDataGen.NewClassA(dm.BoldSystemHandle1.System);
  dm.Save;
  oldValue := Object1.aString;
  Object1.aString:= 'bla bla bla' + DateTimetoStr(Now);
  Assert(OldValue <> Object1.aString, 'ModifyEmbeddedState: could not generate new value!');
  dm.ClearEvents;
  dm.Save;
  Assert(dm.EventList.Count = 1, 'ModifyEmbeddedState: event count incorrect');
  Assert(dm.SubscriptionList.Count = 0, 'ModifyEmbeddedState: subscription count incorrect');
  Assert(dm.CancelledSubscriptionList.Count = 0, 'ModifyEmbeddedState: cancledsubscription count incorrect');
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfobjectChanged, '',
                       '', '', Object1.BoldObjectLocator.BoldObjectID)) <> -1, 'ModifyEmbeddedState: event mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_EmbeddedState.Setup;
begin
  inherited;
  EnsureDM;
end;

procedure TTestCaseSnooper_EmbeddedState.TearDown;
begin
  FreeDM;
end;

class function TTestCaseSnooper_EmbeddedState.Suite: ITestSuite;
begin
  result := inherited Suite;
  setCommentForTest(result, 'ModifyEmbeddedState', 'BoldSnooper: ModifyEmbeddedState');
end;

{TTestCaseSnooper_FetchClass}
class procedure TTestCaseSnooper_FetchClass.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('FetchClass', 'BoldSnooper: FetchClass'));
end;

procedure TTestCaseSnooper_FetchClass.FetchClass;
var
  object1, object2: TThing;
  objectList: TBoldObjectList;
begin
  object1 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  object2 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  object1.one := object2;
  dm.Save;
  dm.BoldSystemHandle1.Active := False;
  dm.BoldSystemHandle1.Active := True;
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);
  dm.ClearEvents;
  objectList := dm.BoldSystemHandle1.System.ClassByExpressionName['Thing'];
  ObjectList.Count;
  Assert(dm.EventList.Count = 0, 'NewEmbeddedState: event count incorrect');
  Assert(dm.SubscriptionList.Count = 1, 'NewEmbeddedState: subscription count incorrect');
  Assert(dm.CancelledSubscriptionList.Count = 0, 'NewEmbeddedState: cancledsubscription count incorrect');

  Assert(dm.SubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'Thing',
                '', '', nil)) <> -1 , 'FetchClass: subscription mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_FetchClass.Setup;
begin
  inherited;
  EnsureDM;
end;

procedure TTestCaseSnooper_FetchClass.TearDown;
begin
  FreeDm;
end;

class function TTestCaseSnooper_FetchClass.Suite: ITestSuite;
begin
  result := inherited suite;
  SetCommentForTest(result, 'FetchClass', 'BoldSnooper: FetchClass');
end;

{TTestCaseSnooper_FetchObject}
class procedure TTestCaseSnooper_FetchObject.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('FetchObject', 'BoldSnooper: FetchObject'));
end;

procedure TTestCaseSnooper_FetchObject.FetchObject;
var
  ObjectList: TBoldObjectList;
  Object1, object2: TClassA;
  ID1, ID2: TBoldObjectID;
  tpIndex: integer;
  exact: Boolean;
  EventName: string;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ID1, ID2);
  ObjectList := dm.BoldSystemHandle1.System.ClassByExpressionName['ClassA'];
  //delete all
  while ObjectList.Count > 0 do
    ObjectList[0].Delete;
  dm.Save;
  object1 := TDataGen.NewClassA(dm.BoldSystemHandle1.System);
  object2 := TDataGen.NewClassA(dm.BoldSystemHandle1.System);
  object1.part.Add(object2);
  dm.Save;
  tpIndex := object1.BoldObjectLocator.BoldObjectID.TopSortedIndex;
  exact := object1.BoldObjectLocator.BoldObjectID.TopSortedIndexExact;
  ID1 := object1.BoldObjectLocator.BoldObjectID.CloneWithClassId(tpIndex, exact);
  ID2 := object2.BoldObjectLocator.BoldObjectID.CloneWithClassId(tpIndex, exact);
  dm.BoldSystemHandle1.Active := False;
  dm.BoldSystemHandle1.Active := True;
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);
  dm.ClearEvents;
  object1 := TClassA(dm.BoldSystemHandle1.System.EnsuredLocatorByID[ID1].EnsuredBoldObject);
  Assert(dm.EventList.Count = 0, 'incorrect event count');
  Assert(dm.SubscriptionList.count = 1, 'incorrect subscription count');
  Assert(dm.CancelledSubscriptionList.count = 0, 'incorrect cancelled subscription count');
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
                        '', '', Object1.BoldObjectLocator.BoldObjectID);
  Assert(dm.SubscriptionList.IndexOf(EventName) <> -1, 'FetchObject: Subscription mismatch');
  dm.ClearEvents;
  object1.part[0];
//  ObjectList.Locators[1].EnsureBoldObject;
  Assert(dm.EventList.Count = 0, 'incorrect event count');
  // one (nonembedded) subscription for the multilink, and one (embeded) subscription for the related object
  Assert(dm.SubscriptionList.count = 2, 'incorrect subscription count'); // !!!!!!!!!!!!!!!!!
  Assert(dm.CancelledSubscriptionList.count = 0, 'incorrect cancelled subscription count');
  // first check the multilink
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, 'ClassA', 'part', '',
         ID1);
  Assert(dm.SubscriptionList.IndexOf(EventName) <> -1, 'FetchObject: subscription mismatch');
  // then check the related object
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '',
                        '', '', ID2);
  Assert(dm.SubscriptionList.IndexOf(EventName) <> -1, 'FetchObject: Subscription mismatch');
  dm.ClearEvents;
end;

procedure TTestCaseSnooper_FetchObject.Setup;
begin
  inherited;
  EnsureDM;
end;

procedure TTestCaseSnooper_FetchObject.TearDown;
begin
  FreeDm;
end;

class function TTestCaseSnooper_FetchObject.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'FetchObject', 'BoldSnooper: FetchObject');
end;

{TTestCaseSnooper_Object}
class procedure TTestCaseSnooper_Object.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('NewObject', 'BoldSnooper: NewObject'));
  ASuite.AddTest(CreateWithComment('DeleteObject', 'BoldSnooper: DeleteObject'));
end;

procedure TTestCaseSnooper_Object.SetUp;
begin
  EnsureDM;
end;

procedure TTestCaseSnooper_Object.NewObject;
var
  Object1: TClassA;
  Object2: TThing;
  EventName: String;
begin
  EnsureDB;
  //add a class
  Object1 := TDataGen.NewClassA(dm.BoldSystemHandle1.System);
  Object2 := TDataGen.NewThing(dm.BoldSystemHandle1.System);
  dm.ClearEvents;
  dm.Save;
  // ------------ 1 event : 1) C:<ClassName>  C:<SuperClasses...>
  // ------------ 1 subscription : 2) E:<ObjectID>
  Assert(dm.EventList.Count = 3, 'NewObject: Event number incorrect'); // Thing, ClassA and BusinessClassesRoot
  Assert(dm.SubscriptionList.Count = 1*2, 'NewObject: Subscription number incorrect');
  Assert(dm.CancelledSubscriptionList.Count = 0, 'NewObject: Cancelledsubscriptions number incorrect');

  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'BusinessClassesRoot', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');

  //classA
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'ClassA', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');
  Assert(dm.subscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, 'Thing', '',
           '', Object1.BoldObjectLocator.BoldObjectID)) <> -1, 'NewObject: event mismatch');

  //Thing
  EventName := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'Thing', '', '', nil);
  Assert(dm.EventList.IndexOf(EventName) <> -1 , 'NewNonEmbeddedState: event mismatch');
  Assert(dm.subscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, 'Thing', '',
           '', Object2.BoldObjectLocator.BoldObjectID)) <> -1 , 'NewObject: event mismatch');

  dm.ClearEvents;
end;

procedure TTestCaseSnooper_Object.DeleteObject;
var
  objectList1, objectlist2: TBoldObjectList;
  ObjectID1, ObjectID2 : TBoldObjectID;
  event, subscription: string;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectID1, ObjectID2);
  dm.BoldSystemHandle1.Active := False;
  dm.BoldSystemHandle1.Active := True;
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);

  // delete all classA objects
  ObjectList1 := dm.BoldSystemHandle1.System.ClassByExpressionName['classA'];
  while ObjectList1.Count > 0 do
    ObjectList1[0].Delete;
  //classA object
  TDataGen.NewClassA(dm.BoldSystemHandle1.System);
  dm.Save;


  //delete all Thing objects
  ObjectList2 := dm.BoldSystemHandle1.System.ClassByExpressionName['Thing'];
  while ObjectList2.Count > 0 do
    ObjectList2[0].Delete;
  //TThing object
  TDataGen.NewThing(dm.BoldSystemHandle1.System);
  dm.Save;

  dm.BoldSystemHandle1.Active := False;
  dm.BoldSystemHandle1.Active := True;
  dm.BoldSnooperHandle1.PropagatorHandle := dm.propagatorHandle; 
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.SetActive(True);

  ObjectList1 := dm.BoldSystemHandle1.System.ClassByExpressionName['classA'];
  ObjectList2 := dm.BoldSystemHandle1.System.ClassByExpressionName['Thing'];
  ObjectID1 := ObjectList1[0].BoldObjectLocator.BoldObjectID.Clone;
  ObjectID2 := ObjectList2[0].BoldObjectLocator.BoldObjectID.Clone;
  ObjectList1[0].Delete;
  ObjectList2[0].Delete;
  dm.ClearEvents;
  dm.Save;
  // class events for Thing, ClassA and BusinessClassesRoot, and events for the deleted objects, total of 5
  Assert(dm.EventList.Count = 5, 'DeleteObject: Event number incorrect');
  Assert(dm.SubscriptionList.Count = 0, 'DeleteObject: Subscription number incorrect');
  // expected cancelled events:
  // Embedded changes: aThing, aClassA
  // Nonembedded changes: aThing.one+many+many2, aClassA.part+partof+child+next
  // Total: 9
  Assert(dm.CancelledSubscriptionList.Count = 9, 'DeleteObject: Cancelledsubscriptions number incorrect');

  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'BusinessClassesRoot', '',
           '', nil)) <> -1, 'DeleteObject: event mismatch');

  // ClassA
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'ClassA', '',
           '', nil)) <> -1, 'DeleteObject: event mismatch');
  event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, '', '',
           '', ObjectID1);
  subscription := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '',
           '', ObjectID1);
  Assert(dm.EventList.IndexOf(event) <> -1, 'DeleteObject: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(subscription) <> -1, 'DeleteObject: Cancelledsubscription mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'child',
           '', ObjectID1)) <> -1, 'DeleteObject: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'next',
           '', ObjectID1)) <> -1, 'DeleteObject: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'part',
           '', ObjectID1)) <> -1, 'DeleteObject: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'partof',
           '', ObjectID1)) <> -1, 'DeleteObject: event mismatch');
  //TThing
  Assert(dm.EventList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, 'Thing', '',
           '', nil)) <> -1, 'DeleteObject: event mismatch');
  event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, '', '',
           '', ObjectID2);
  subscription := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '',
           '', ObjectID2);
  Assert(dm.EventList.IndexOf(event) <> -1, 'DeleteObject: event mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(subscription) <> -1 , 'DeleteObject: Cancelledsubscription mismatch');
  //cancel subscriptions
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'one',
           '', ObjectID2)) <> -1 , 'DeleteObject: Cancelledsubscription mismatch');
  Assert(dm.CancelledSubscriptionList.IndexOf(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', 'many',
           '', ObjectID2)) <> -1 , 'DeleteObject: Cancelledsubscription mismatch');
  dm.ClearEvents;

end;

procedure TTestCaseSnooper_Object.TearDown;
begin
  FreeDm;
end;

class function TTestCaseSnooper_Object.Suite: ITestSuite;
begin
  result := inherited suite;
  SetCommentForTest(Result, 'NewObject', 'BoldSnooper: NewObject');
  SetCommentForTest(Result, 'DeleteObject', 'BoldSnooper: DeleteObject');
end;

{TDummyPropagator}
function TDummyPropagator.SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
var
  i: Integer;
begin
  Result := S_OK;
  if VarIsArray(Events) then
    for i := VarArrayLowBound(Events, 1) to VarArrayHighBound(Events, 1) do
      if Assigned(dm) then
        dm.EventList.Add(Events[i]);
end;

function TDummyPropagator.AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
var
  i: integer;
begin
  Result := S_OK;
  if VarIsArray(Subscriptions) then
    for i := VarArrayLowBound(Subscriptions, 1) to VarArrayHighBound(Subscriptions, 1) do
      if Assigned(dm) then
        dm.SubscriptionList.Add(Subscriptions[i]);
end;

function TDummyPropagator.CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
var
  i: integer;
begin
  Result := S_OK;
  if VarIsArray(Subscriptions) then
    for i := VarArrayLowBound(Subscriptions, 1) to VarArrayHighBound(Subscriptions, 1) do
      if Assigned(dm) then
        dm.CancelledSubscriptionList.Add(Subscriptions[i]);
end;

function  TDummyPropagator.RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;

begin
  BoldClientID := FAKECLIENTID;
  Result := S_OK;
end;

function  TDummyPropagator.ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out Extended: WordBool): HResult; stdcall;
begin
  Result := S_OK;
end;

function  TDummyPropagator.UnregisterClient(ClientID: Integer): HResult;
begin
 Result := S_OK;
end;

  {Tdm}
procedure Tdm.Save;
begin
  dm.BoldSystemHandle1.System.UpdateDatabase;
end;

procedure Tdm.ClearEvents;
begin
  EventList.Clear;
  SubscriptionList.Clear;
  CancelledSubscriptionList.Clear;
end;

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  EventList := TStringlist.Create;
  SubscriptionList := TStringlist.Create;
  CancelledSubscriptionList := TStringlist.Create;
  PropagatorHandle := TDummyPropagatorHandle.Create(self);
  fFakePm := TFakePM.Create;
  dm.BoldSnooperHandle1.PropagatorHandle := dm.PropagatorHandle;
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
end;

procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fEventList);
  FreeAndNil(fSubscriptionList);
  FreeAndNil(fCancelledSubscriptionList);
  FreeAndNil(fPropagatorHandle);
  FreeAndNil(fFakePM);
end;

procedure TFakePM.PMExactifyIds(ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  inherited;
end;

procedure TFakePM.PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID);
begin
  Assert(BoldClientID = FAKECLIENTID, Format('%s: PMFetch', [ClassName]));
end;

procedure TFakePM.PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
begin
  Assert(BoldClientID = FAKECLIENTID, Format('%s: PMFetchIDListWithCondition', [ClassName]));
end;

procedure TFakePM.PMSetReadOnlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  inherited;
end;

procedure TFakePM.PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  inherited;
end;

procedure TFakePM.PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  inherited;
end;

procedure TFakePM.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var ServerTime: TDateTime; BoldClientID: TBoldClientID);
begin
  Assert(BoldClientID = FAKECLIENTID, Format('%s: PMUpdate', [ClassName]));
end;

class procedure Tmaan_BoldIDAdder.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('GetID', 'TBoldIDAdder'));
end;

procedure Tmaan_BoldIDAdder.SetUp;
begin
  EnsureDM;
end;

procedure Tmaan_BoldIDAdder.GetID;
var
  IDAdder: TBoldIDAdder;
  i: integer;
  TimeOfLatestUpdate: TDateTime;
begin
  dm.PropagatorHandle.Connected := true;
  dm.BoldListenerHandle1.SetActive(True);
  dm.BoldListenerHandle1.PropagatorHandle := dm.PropagatorHandle;
  IDAdder := (dm.BoldIdAdderHandle1.PersistenceController as TBoldIDAdder);
  IDAdder.NextPersistenceController := dm.fFakePM;
  IDAdder.PMFetch(nil, nil,nil, 0, 0);
  IDAdder.PMFetchIDListWithCondition(nil, nil, 0, nil, 0);
  IDAdder.PMUpdate(nil, nil, nil, nil, nil, i, TimeOfLatestUpdate, 0);
end;

class function TDataGen.NewClassA(System: TBoldSystem): TClassA;
var
  anObject: TClassA;
begin
  anObject := TClassA.Create(System);
  anObject.aString := STRINGCONST1;
  anObject.aBoolean := BOOLEANCONST1;
  anObject.aByte := INTEGERCONST1;
  anObject.aCurrency := CURRENCYCONST1;
  anObject.aDate := INTEGERCONST1;
  anObject.aDateTime := FLOATCONST1;
  anObject.aFloat := FLOATCONST1;
  anObject.aInteger := INTEGERCONST1;
  anObject.aShortInt := INTEGERCONST1;
  anObject.aSmallInt := INTEGERCONST1;
  anObject.aTime := Frac(FLOATCONST1);
  anObject.aWord := INTEGERCONST1;
  anObject.aBlob := STRINGCONST1;
  anObject.aBlobContent := STRINGCONST1;
  Result := anObject;
end;

class function TDataGen.NewThing(System: TBoldSystem): TThing;
begin
  Result := TThing.Create(System);
end;

class function TDataGen.NewLinkClass(System: TBoldSystem): TLinkClass;
begin
  Result := TLinkClass.Create(System);
end;

class function TDataGen.NewSong(System: TBoldSystem): TSong;
begin
  Result := TSong.Create(System);
  Result.Title := 'One';
end;

class function TDataGen.NewHitList(System: TBoldSystem): THitList;
begin
  Result := THitList.Create(System);
  Result.Name := 'Top10';
end;


{ TDummyPropagatorhandle }

constructor TDummyPropagatorhandle.Create(AOwner: TComponent);
begin
  inherited;
  FPropagatorThread := TPropagatorThread.Create(False);
  FPropagatorThread.WaitUntilReady(5*1000);
  FConnected := true;
end;

destructor TDummyPropagatorhandle.Destroy;
begin
  FPropagatorThread.Quit(True);
  FreeAndNil(FPropagatorThread);
  inherited;
end;

procedure TDummyPropagatorhandle.DoPropagatorCallFailed(Sender: TObject; const ErrorMessage: string);
begin
  // do nothing
end;

function TDummyPropagatorhandle.GetClientHandler: IBoldClientHandler;
begin
  if not assigned(fPropagator) then
    CoGetInterfaceAndReleaseStream(IStream(FPropagatorThread.FStream), IID_IBoldEventPropagator, fPropagator);
  Result := fPropagator as IBoldClientHandler;
end;

function TDummyPropagatorhandle.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TDummyPropagatorhandle.GetEventPropagator: IBoldEventPropagator;
begin
  if not assigned(fPropagator) then
    CoGetInterfaceAndReleaseStream(IStream(FPropagatorThread.FStream), IID_IBoldEventPropagator, fPropagator);
  Result := fPropagator as IBoldEventPropagator;
end;

procedure Tmaan_BoldIDAdder.TearDown;
begin
  FreeDm;
end;

procedure TDummyPropagatorhandle.SetConnected(const Value: Boolean);
begin
  if (Value <> fConnected) then
    fConnected := Value;
end;

{ TPropagatorThread }

procedure TPropagatorThread.Execute;
var
  rMsg: TMsg;
  Res: Integer;
  DummyPropagator: TDummyPropagator;
  unk: IBoldEventPropagator;
begin
  CoInitialize(nil);
  EnsureMessageQueue;
  DummyPropagator := TDummyPropagator.Create;
  unk := DummyPropagator as IBoldEventPropagator;
  CoMarshalInterThreadinterfaceInstream(IID_IBoldEventPropagator, unk, IStream(FStream));
  SignalReady;
  while not Terminated do
  begin
    res := Integer(GetMessage(rMsg, 0, 0, 0));
    if (res = -1) or (res = 0) then
      Terminate
    else
      DispatchMessage(rMsg);
  end;
  CoUninitialize;
end;

class function Tmaan_BoldIDAdder.Suite: ITestSuite;
begin
  result := inherited suite;
  SetCommentForTest(Result, 'GetID', 'TBoldIDAdder');
end;

procedure TFakePM.ReserveNewIds(ValueSpace: IBoldValueSpace;
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  inherited;

end;

initialization
  TestGlobal.RegisterTestCase(TTestCaseSnooper_Object);
  TestGlobal.RegisterTestCase(TTestCaseSnooper_EmbeddedState);
  TestGlobal.RegisterTestCase(TTestCaseSnooper_NonEmbeddedState);
  TestGlobal.RegisterTestCase(TTestCaseSnooper_FetchObject);
  TestGlobal.RegisterTestCase(TTestCaseSnooper_FetchClass);
  TestGlobal.RegisterTestCase(Tmaan_BoldIDAdder);

finalization
  TestGlobal.UnRegisterTestCase(TTestCaseSnooper_Object);
  TestGlobal.UnRegisterTestCase(TTestCaseSnooper_EmbeddedState);
  TestGlobal.UnRegisterTestCase(TTestCaseSnooper_NonEmbeddedState);
  TestGlobal.UnRegisterTestCase(TTestCaseSnooper_FetchObject);
  TestGlobal.UnRegisterTestCase(TTestCaseSnooper_FetchClass);
  TestGlobal.UnRegisterTestCase(Tmaan_BoldIDAdder);

end.



