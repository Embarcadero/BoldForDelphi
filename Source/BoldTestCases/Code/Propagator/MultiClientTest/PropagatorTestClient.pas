unit PropagatorTestClient;

interface

uses
  comobj,
  TestClient_TLB,
  BoldListhandle,
  boldSystem;

type
  TPropagatorTestClient = class(TTypedCOMObject, IPropagatorTestClient)
  private
    fID: integer;
    fTestManager: IPropagatorTestManager;
    fdbAlias: string;
    FAutoTesting: Boolean;
    function LocateObject(const className: string; const ObjectID: WideString): TBoldObject;
    function UpdateMember(Member: TBoldMember): Boolean;
    procedure SetAutoTesting(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

      {IPropagatorTestClient}
    function  CreateObject(const ClassName: WideString; out ObjectID: WideString): HResult; stdcall;
    function  UpdateNonEmbeddedState(const ClassName: WideString; const ObjectID: WideString;
                                     const attributeName: WideString): HResult; stdcall;
    function  UpdateEmbeddedState(const ClassName: WideString; const ObjectID: WideString;
                                  const attributeName: WideString): HResult; stdcall;
    function  DeleteObject(const ClassName: WideString; const ObjectID: WideString): HResult; stdcall;
    function  SetID(TestClientID: Integer; const TestManager: IPropagatorTestManager;
                    const DBAlias: WideString): HResult; stdcall;
    function  RefreshObjectSpace: HResult; stdcall;
    function  GenerateEvents(Number: Integer): HResult; stdcall;
    function  UpdateDB: HResult; stdcall;
    function  StartAutoTest: HResult; stdcall;
    function  StopAutoTest: HResult; stdcall;
    function  CloseClient: HResult; stdcall;

    property ID: integer read fID write fID;
    property TestManager: IPropagatorTestManager read fTestManager write fTestManager;
    property DBAlias: string read fdbAlias write fdbAlias;
    property AutoTesting: Boolean read fAutoTesting write SetAutoTesting;
  end;

implementation

uses
  maanDataGen,
  dmClient,
  TestModel1,
  BoldAttributes,
  TestClientMainForm,
  SysUtils,
  windows,
  dialogs,
  comserv;

{ TPropagatorTestClient }

constructor TPropagatorTestClient.Create;
begin
  inherited Create;
//  Tdm.EnsureDM(PropagatorTest);
end;

function TPropagatorTestClient.CreateObject(const ClassName: WideString;
  out ObjectID: WideString): HResult;
var
  ClassAObj: TClassA;
  ThingObj: TThing;
  LinkObj: TLinkClass;
  SongObj: TSong;
  HitListObj: THitList;
begin
  ClassAObj:= nil;
  ThingObj:= nil;
  LinkObj:= nil;
  SongObj:= nil;
  HitListObj:= nil;
  try
    if (ClassName = 'ClassA') then
      ClassAObj := TDataGen.NewClassA(dm.BoldSystemHandle1.System)
    else if (ClassName = 'Thing') then
      ThingObj := TDataGen.NewThing(dm.BoldSystemHandle1.System)
    else if (ClassName = 'LinkClass') then
      LinkObj:= TDataGen.NewLinkClass(dm.BoldSystemHandle1.System)
    else if (ClassName = 'Song') then
      SongObj := TDataGen.NewSong(dm.BoldSystemHandle1.System)
    else if (ClassName = 'HitList') then
      HitListObj := TDataGen.NewHitList(dm.BoldSystemHandle1.System);
    Result := S_OK;
    dm.Save;
    if (ClassName = 'ClassA') then
      ObjectID := WideString(ClassAObj.BoldObjectLocator.BoldObjectID.AsString)
    else if (ClassName = 'Thing') then
      ObjectID := WideString(ThingObj.BoldObjectLocator.BoldObjectID.AsString)
    else if (ClassName = 'LinkClass') then
      ObjectID := WideString(LinkObj.BoldObjectLocator.BoldObjectID.AsString)
    else if (ClassName = 'Song') then
      ObjectID := WideString(SongObj.BoldObjectLocator.BoldObjectID.AsString)
    else if (ClassName = 'HitList') then
      ObjectID := WideString(HitListObj.BoldObjectLocator.BoldObjectID.AsString);
  except
    Result := S_FALSE;
  end;
end;

function TPropagatorTestClient.DeleteObject(const ClassName: WideString;
  const ObjectID: WideString): HResult;
var
  Obj: TBoldObject;
begin
  Obj := LocateObject(className, ObjectID);
  if Assigned(Obj) then
  begin
    Obj.Delete;
    Result := S_OK;
    dm.Save;
  end
  else
    Result := S_FALSE;
end;


destructor TPropagatorTestClient.Destroy;
begin
  TestManager := nil;
  inherited;
end;

function TPropagatorTestClient.LocateObject(const ClassName: string; const ObjectID: Widestring): TBoldObject;
var
  i, count: integer;
  List : TBoldListHandle;
begin
  if (className = 'ClassA') then
    List := dm.blhClassA
  else if (className = 'Thing') then
    List := dm.blhThing
  else if (className = 'HitList') then
    List := dm.blhhitList
  else if (className = 'Song') then
    List := dm.blhSong
  else  
  begin
    Result := nil;
    Exit;
  end  ;
  Result := nil;
  count := List.Count;
  for i:= 0 to Count - 1 do
  begin
    if (List.CurrentBoldObject.BoldObjectLocator.BoldObjectID.AsString = ObjectID) then
    begin
      Result := List.CurrentBoldObject;
      Break;
    end
    else
      List.Next;
  end;
end;

function TPropagatorTestClient.RefreshObjectSpace: HResult;
begin
  dm.BoldSystemHandle1.Active := false;
  dm.BoldSystemHandle1.Active := true;
  Result := S_OK;
end;

function TPropagatorTestClient.SetID(TestClientID: Integer;
  const TestManager: IPropagatorTestManager;
  const DBAlias: WideString): HResult;
begin
  ID := TestClientId;
  Self.TestManager := TestManager;
  self.DBAlias := DBAlias;
  Tdm.EnsureDM(DBAlias);
  dm.ClientID := TestClientID;
  dm.TestManager := TestManager;
  Result := S_OK;
end;

function TPropagatorTestClient.GenerateEvents(Number: Integer): HResult;
begin
  Result := S_OK;
  dm.GenerateRandomEvent(Number);
end;

function TPropagatorTestClient.UpdateEmbeddedState(
  const ClassName: WideString; const ObjectID: WideString;
  const attributeName: WideString): HResult;
var
  Obj: TBoldObject;
begin
  Obj := LocateObject(ClassName, ObjectID);
  if Assigned(Obj) then
  begin
    Sleep(1000*2);
    if ClassName = 'ClassA' then
      UpdateMember((Obj as TClassA).BoldMemberByExpressionName[attributeName])
    else if ClassName = 'Thing' then
      UpdateMember((Obj as TThing).BoldMemberByExpressionName[attributeName]);
   end;
   Result := S_OK;
   dm.Save;
end;

function TPropagatorTestClient.UpdateMember(Member: TBoldMember): Boolean;
begin
  Randomize;
  Result := True;
  if (Member is TBAString) then
    (Member as TBAString).AsString := FloatToStr(Random(100))
  else if (Member is TBABoolean) then
    (Member as TBABoolean).AsBoolean := (Random(100) < 50)
  else if (Member is TBAByte) then
    (Member as TBAByte).AsByte := Trunc(Random(1))
  else if (Member is TBACurrency) then
    (Member as TBACurrency).AsCurrency := Random(1000)
  else if (Member is TBADate) then
    (Member as TBADate).AsDate := Now
  else if (Member is TBADateTime) then
    (Member as TBADateTime).AsDateTime := Now
  else if (Member is TBAFloat) then
    (Member as TBAFloat).AsFloat := Random(1000)
  else if (Member is TBAInteger) then
    (Member as TBAInteger).AsInteger := Trunc(Random(1000))
  else if (Member is TBAShortInt) then
    (Member as TBAShortInt).AsShortInt := Trunc(Random(1000))
  else if (Member is TBASmallInt) then
    (Member as TBASmallInt).AsSmallInt := Trunc(Random(1000));
  dm.Save;
//  else if (Member is ) then
//    (Member as ) :=
end;

function TPropagatorTestClient.UpdateNonEmbeddedState(
  const ClassName: WideString; const ObjectID: WideString;
  const attributeName: WideString): HResult;
var
  Obj: TBoldObject;
  NewObject: TClassA;
  NewHitList: THitList;
begin
  Obj := LocateObject(ClassName, ObjectID);
  Result := S_FALSE;
  if Assigned(Obj) then
  begin
    if ClassName = 'ClassA' then // DOES NOT WORK WITH CLASSA
    begin
      NewObject := TDataGen.NewClassA(dm.BoldSystemHandle1.System);
      dm.Save;
      if attributename = 'part' then
        (Obj as TClassA).part.Add(NewObject)
      else if attributename = 'partof' then
        (Obj as TClassA).partof.Add(NewObject);
      Result := S_OK;
    end
    else if ClassName = 'Song' then
      if attributename = 'HitList' then
      begin
        NewHitList := (dm.blhhitList.CurrentBoldObject as THitList);
        if Assigned(newHitList) then
          (Obj as TSong).hitList.Add(NewHitList);
      end;
  end;
  dm.Save;
end;

function TPropagatorTestClient.UpdateDB: HResult;
begin
  dm.BoldSystemHandle1.UpdateDatabase;
  result := S_OK;
end;

function TPropagatorTestClient.StartAutoTest: HResult;
begin
  AutoTesting := True;
end;

function TPropagatorTestClient.StopAutoTest: HResult;
begin
  AutoTesting := false;
end;

procedure TPropagatorTestClient.SetAutoTesting(Value: Boolean);
begin
  if (Value <> fAutoTesting) then
  begin
    fAutoTesting := Value;
    if fAutoTesting then
      dm.AutoTestTimer.Interval := 1000 + Trunc(Random(5000));
    dm.AutoTestTimer.Enabled := fAutoTesting;
    Form1.cbTesting.Checked := fAutoTesting;
  end;
end;

function TPropagatorTestClient.CloseClient: HResult;
begin
  FreeAndNil(dm);
end;

initialization
  Randomize;
  TComObjectFactory.Create(comserver, TPropagatorTestClient, CLASS_PropagatorTestClient, 'PropagatorTestClient', 'Application for testing BoldPropagator', ciSingleInstance,
              tmSingle);

end.
