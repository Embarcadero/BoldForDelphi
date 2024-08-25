
{ Global compiler directives }
{$include bold.inc}
unit BoldUpdatePrecondition;

interface

uses
  BoldFreeStandingValues,
  BoldId,
  BoldBase,
  BoldStreams,
  BoldvalueSpaceInterfaces;

type
  TBoldUpdatePrecondition = class(TBoldNonRefCountedObject, IBoldStreamable)
  protected
    function GetStreamName: string; virtual; abstract;
    function GetFailureReason: string; virtual;
    function GetFailed: Boolean; virtual;
  public
    constructor Create;
    procedure AssignOutValues(Source: TBoldUpdatePrecondition); virtual; abstract;
    property Failed: Boolean read GetFailed;
    property FailureReason: string read GetFailureReason;
  end;

  TBoldOptimisticLockingPrecondition = class(TBoldUpdatePrecondition)
  private
    fFreeStandingValueSpace: TBoldFreeStandingValueSpace;
    fFailureList: TBoldObjectIdList;
    function GetValueSpace: IBoldValueSpace;
    function GetFreeStandingValueSpace: TBoldFreeStandingValueSpace;
    function GetFailureList: TBoldObjectIdList;
    function GetHasOptimisticLocks: Boolean; // do not inline it causes problems in D2007 at least
  protected
    function GetStreamName: string; override;
    function GetFailureReason: string; override;
    function GetFailed: Boolean; override;
    property FreeStandingValueSpace: TBoldFreeStandingValueSpace read GetFreeStandingValueSpace;
  public
    destructor Destroy; override;
    procedure AssignOutValues(Source: TBoldUpdatePrecondition); override;
    procedure AddFailedObject(ObjectId: TBoldObjectId);
    procedure ClearValueSpace;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property FailureList: TBoldObjectIdList read GetFailureList;
    property HasOptimisticLocks: Boolean read GetHasOptimisticLocks;
  end;


implementation

uses
  SysUtils,
  BoldDefaultStreamNames,
  BoldDefaultXMLStreaming,
  BoldDefs,
  BoldXMLStreaming;

const
  OptimisticLockingPreConditionStreamName = 'OptimisticLockingPreCondition';

type

  { TBoldXMLPreConditionStreamer }
  TBoldXMLPreConditionStreamer = class(TBoldXMLObjectStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
  end;

  { TBoldXMLOptimisticLockingPreConditionStreamer }
  TBoldXMLOptimisticLockingPreConditionStreamer = class(TBoldXMLPreConditionStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldUpdatePrecondition }

constructor TBoldUpdatePrecondition.create;
begin
end;

function TBoldUpdatePrecondition.GetFailed: Boolean;
begin
  result := false;
end;

function TBoldUpdatePrecondition.GetFailureReason: string;
begin
  result := '';
end;

{ TBoldOptimisticLockingPrecondition }

procedure TBoldOptimisticLockingPrecondition.AddFailedObject(ObjectId: TBoldObjectId);
begin
  if not FailureList.IdInList[ObjectId] then
    FailureList.Add(ObjectId);
end;

procedure TBoldOptimisticLockingPrecondition.AssignOutValues(
  Source: TBoldUpdatePrecondition);
begin
  if Source is TBoldOptimisticLockingPrecondition then
    FailureList.AddList((Source as TBoldOptimisticLockingPrecondition).FailureList);
end;

procedure TBoldOptimisticLockingPrecondition.ClearValueSpace;
begin
  FreeAndNil(fFreeStandingValueSpace);
end;

destructor TBoldOptimisticLockingPrecondition.Destroy;
begin
  FreeAndNil(fFreeStandingValueSpace);
  FreeAndNil(fFailureList);
  inherited;
end;

function TBoldOptimisticLockingPrecondition.GetFailed: Boolean;
begin
  result := FailureList.Count <> 0;
end;

function TBoldOptimisticLockingPrecondition.GetFailureList: TBoldObjectIdList;
begin
  if not assigned(fFailureList) then
    fFailureList := TBoldObjectIdList.Create;
  result := fFailureList;
end;

function TBoldOptimisticLockingPrecondition.GetFailureReason: string;
var
  I: Integer;
begin
  result := format('Optimistic locking failed for the following %d objects:', [FailureList.Count]);
  for i := 0 to FailureList.Count - 1 do
  begin
    if i > 0 then
    Result := Result + ', ';
    result := Result + ('Id: '+ FailureList[i].AsString);
  end;
end;

function TBoldOptimisticLockingPrecondition.GetFreeStandingValueSpace: TBoldFreeStandingValueSpace;
begin
  if not assigned(fFReeStandingValueSpace) then
    fFreeStandingValueSpace := TBoldFreeStandingValueSpace.Create;
  result := fFreeStandingValueSpace;
end;

function TBoldOptimisticLockingPrecondition.GetValueSpace: IBoldValueSpace;
begin
  result := FreeStandingValueSpace;
end;

function TBoldOptimisticLockingPrecondition.GetHasOptimisticLocks: Boolean;
begin
  result := not ValueSpace.IsEmpty;
end;

function TBoldOptimisticLockingPrecondition.GetStreamName: string;
begin
  result := OptimisticLockingPreConditionStreamName;
end;

{ TBoldXMLPreConditionStreamer }

procedure TBoldXMLPreConditionStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
begin
  inherited;
end;

procedure TBoldXMLPreConditionStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
begin
  inherited;
end;

{ TBoldXMLOptimisticLockingPreConditionStreamer }

function TBoldXMLOptimisticLockingPreConditionStreamer.CreateObject: TObject;
begin
  result := TBoldOptimisticLockingPreCondition.Create;
end;

function TBoldXMLOptimisticLockingPreConditionStreamer.GetStreamName: string;
begin
  result := OptimisticLockingPreConditionStreamName;
end;

procedure TBoldXMLOptimisticLockingPreConditionStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  Condition: TBoldOptimisticLockingPreCondition;
  Manager: TBoldDefaultXMLStreamManager;
  IdLIst: TBoldObjectIdList;
  SubNode: TBoldXMLNode;
begin
  Condition := Obj as TBoldOptimisticLockingPreCondition;

  if Node.Manager is TBoldDefaultXMLStreamManager then
  begin
    Manager := Node.Manager as TBoldDefaultXMLStreamManager;
    SubNode := Node.GetSubNode('ValueSpace');
    Manager.ReadValueSpace(Condition.ValueSpace, SubNode);
    SubNode.Free;
  end;

  IdList := Node.ReadSubNodeObject('FailureList', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  Condition.FailureList.Clear;
  Condition.FailureList.AddList(IdList);
  IdList.Free;

end;

procedure TBoldXMLOptimisticLockingPreConditionStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldOptimisticLockingPreCondition;
  Manager: TBoldDefaultXMLStreamManager;
  IdLIst: TBoldObjectIdList;
  OldPersistenceStateToBeStreamed: TBoldValuePersistenceStateSet;
  SubNode: TBoldXMLNode;
begin
  Condition := Obj as TBoldOptimisticLockingPreCondition;

  if Node.Manager is TBoldDefaultXMLStreamManager then
  begin
    IdList := TBoldObjectIdList.Create;
    try
      Manager := Node.Manager as TBoldDefaultXMLStreamManager;
      Condition.ValueSpace.AllObjectIds(IdList, true);
      OldPersistenceStateToBeStreamed := Manager.PersistenceStatesToBeStreamed;

      Manager.PersistenceStatesToBeStreamed := [bvpsCurrent];

      SubNode := Node.NewSubNode('ValueSpace');
      Manager.WriteValueSpace(Condition.ValueSpace, IdLIst, nil, SubNode);
      SubNode.Free;

      Manager.PersistenceStatesToBeStreamed := OldPersistenceStateToBeStreamed
    finally
      FreeAndNil(IdList);
    end;
  end;
  Node.WriteSubNodeObject('FailureList', BOLDOBJECTIDLISTNAME, Condition.FailureList);
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOptimisticLockingPreConditionStreamer.Create);
end.
