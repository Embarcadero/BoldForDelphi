unit dmClient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldHandles, BoldSystemHandle, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, BoldIDAdderHandle, BoldListenerHandle,
  BoldAbstractDequeuer, BoldExternalObjectSpaceEventHandler,
  BoldPersistenceHandlePassthrough, BoldSnooperHandle, BoldSubscription,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  TestClient_TLB,
  dmModel1,
  BoldSystem, ExtCtrls, BoldClientHandles, BoldComClientHandles,
  BoldAbstractPropagatorHandle, BoldPropagatorHandleCOM,
  BoldPersistenceHandlePTWithModel, Data.DB, DBAccess, Uni,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterUniDAC,
  BoldAbstractPersistenceHandleDB;

type
  TUpdateKind = (ukCreate, ukDelete, ukUpdateEmbeddedState, ukUpdateNonEmbeddedState);
  Tdm = class(TDataModule)
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler;
    BoldListenerHandle1: TBoldListenerHandle;
    BoldIdAdderHandle1: TBoldIdAdderHandle;
    BoldListHandle1: TBoldListHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    blhClassA: TBoldListHandle;
    blhThing: TBoldListHandle;
    blhhitList: TBoldListHandle;
    blhSong: TBoldListHandle;
    Timer1: TTimer;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    bcchPropagator: TBoldComConnectionHandle;
    AutoTestTimer: TTimer;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure BoldExternalObjectSpaceEventHandler1ClassChanged(
      TheClass: TBoldObjectList);
    procedure BoldExternalObjectSpaceEventHandler1EmbeddedStateChanged(
      BoldObject: TBoldObject);
    procedure BoldExternalObjectSpaceEventHandler1NonEmbeddedStateChanged(
      BoldMember: TBoldMember);
    procedure BoldIdAdderHandle1RegistrationFailed(Sender: TObject);
    procedure AutoTestTimerTimer(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fTestManager: IPropagatorTestManager;
    fClientID: integer;
  public
    { Public declarations }
    class procedure EnsureDM(const DBAliasName: string);
    class procedure EnsureDB;
    procedure Save;
    procedure GenerateRandomEvent( const Number: integer);    
    property ClientID: integer read fClientID write fClientID;
    property TestManager: IPropagatorTestManager read fTestManager write fTestManager;
  end;

var
  dm: Tdm;

const
  NOEventsGenerated = 1000;

implementation

uses
  BoldObjectSpaceExternalEvents,
  BoldSystemRT,
  maanDataGen,
  TestClientMainForm;
{$R *.DFM}

class procedure Tdm.EnsureDB;
begin
  if not dm.BoldSystemHandle1.Active then
    dm.BoldSystemHandle1.Active := True;
end;

class procedure Tdm.EnsureDM(const DBAliasName: string);
begin
  if not Assigned(dm_Model1) then
    Application.CreateForm(Tdm_Model1, dm_Model1);
  if not Assigned(dm) then
    Application.CreateForm(Tdm, dm);
  dm.UniConnection1.Database := DBAliasName;
  Form1.bgClassA.BoldHandle := dm.blhClassA;
  Form1.bgThing.BoldHandle := dm.blhThing;
  Form1.bgSong.BoldHandle := dm.blhSong;
  Form1.bghitList.BoldHandle := dm.blhhitList;
  EnsureDB;
end;


procedure Tdm.BoldExternalObjectSpaceEventHandler1ClassChanged(
  TheClass: TBoldObjectList);
var
  event: string;
begin
  if Assigned(fTestManager) then
  begin
    event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged,
                TBoldListTypeInfo(TheClass.BoldType).ListElementTypeInfo.ExpressionName,'','',  nil);
    TestManager.EventReceived(WideString(event), ClientID);
  end;
 TheClass.Invalidate;
end;

procedure Tdm.BoldExternalObjectSpaceEventHandler1EmbeddedStateChanged(
  BoldObject: TBoldObject);
var
  event: string;
begin
  if Assigned(fTestManager) then
  begin
    event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged,'','','',
                BoldObject.BoldObjectLocator.BoldObjectID);
    TestManager.EventReceived(event, ClientID);
  end;
  BoldObject.Invalidate;
end;

procedure Tdm.BoldExternalObjectSpaceEventHandler1NonEmbeddedStateChanged(
  BoldMember: TBoldMember);
var
  event: string;
begin
  if Assigned(fTestManager) then
  begin
    event := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '',
          BoldMember.BoldMemberRTInfo.ExpressionName, '', BoldMember.OwningObject.BoldObjectLocator.BoldObjectID);
    TestManager.EventReceived(event, ClientID);
  end;
  BoldMember.Invalidate;
end;

procedure Tdm.Save;
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure Tdm.GenerateRandomEvent( const Number: integer);
var
  aClass: TClassType;
  aModification: TUpdateKind;
  n: integer;
  NOEvents: integer;
  List: TBoldListHandle;
  Obj, Attribute : TBoldObject;
begin
  NOEvents := 0;
  Randomize();
  while NOEvents <> Number do
  begin
    n := Random(1000) mod (Ord(High(TClassType)) + 1);
    if n = 0 then
    begin
      aClass := ctClassA;
      List := dm.blhClassA;
    end
    else if n = 1 then
    begin
      aClass := ctThing;
      List := blhThing;
    end
    else if n = 2 then
    begin
      aClass := ctHitlist;
      List := blhhitList;
    end
    else if n = 3 then
    begin
      aClass := ctSong;
      List := blhSong;
    end;
    n := Random(1000) mod (Ord(High(TUpdateKind)) + 1);
    if n = 0 then
      aModification := ukCreate
    else if n = 1 then
      aModification := ukDelete
    else if n = 2 then
      aModification := ukUpdateEmbeddedState
    else if n = 3 then
      aModification := ukUpdateNonEmbeddedState;

    case aModification of
      ukCreate:
            begin
              TDataGen.CreateObject(aClass, BoldSystemHandle1.System);
              Inc(NOEvents);
            end;
      ukDelete:
            begin
              if TDataGen.DeleteObject(List) then
                Inc(NOEvents);
            end;
      ukUpdateEmbeddedState:
            begin
              Obj := TDataGen.GetRandomObject(List);
              if Assigned(Obj) then
              begin
                if TDataGen.UpdateEmbeddedState(aClass, Obj) then
                  Inc(NOEvents);
              end;
            end;
      ukUpdateNonEmbeddedState:
              begin
                if aClass in [ctSong] then
                begin
                  Obj := TDataGen.GetRandomObject(List);
                  Attribute := TDataGen.GetRandomObject(blhhitList);
                  if Assigned(Obj) and Assigned(Attribute) then
                  begin
                    if TDataGen.UpdateNonEmbeddedState(aClass, Obj, Attribute) then
                      Inc(NOEvents);
                  end;
                end;
              end;
    end; //case
  end; //while
end;

procedure Tdm.BoldIdAdderHandle1RegistrationFailed(Sender: TObject);
begin
  showmessage('could not register with a propagator');
end;

procedure Tdm.AutoTestTimerTimer(Sender: TObject);
begin
  GenerateRandomEvent(100);
end;

procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  if BoldSystemHandle1.Active then
  begin
    BoldSystemHandle1.UpdateDatabase;
    BoldSystemHandle1.Active := false;
  end;
  Timer1.Enabled := false;
  AutoTestTimer.Enabled := false;
end;

end.
