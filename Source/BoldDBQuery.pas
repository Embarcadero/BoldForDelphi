{ Global compiler directives }
{$include bold.inc}
unit BoldDBQuery;

interface

uses
  Classes,
  BoldDBInterfaces,
  BoldAbstractPersistenceHandleDB,
  db,
  DBClient,
  Provider,
  BoldSubscription;

type
  TCustomBoldDBQuery = class(TCustomClientDataSet)
  private
    fSubscriber: TBoldPassthroughSubscriber;
    fDataSetProvider: TDataSetProvider;
    fBoldPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
    fSql: TStrings;
    fQuery: IBoldQuery;
    fRequestLiveQuery: boolean;
    procedure SetBoldPersistenceHandleDB(
      const Value: TBoldAbstractPersistenceHandleDB);
    procedure OnSqlChange(Sender: TObject);
    procedure SetSql(const Value: TStrings);
    function FindDefaultPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
  protected
    property BoldPersistenceHandleDB: TBoldAbstractPersistenceHandleDB read fBoldPersistenceHandleDB write SetBoldPersistenceHandleDB;
    property Sql: TStrings read fSql write SetSql;
    property RequestLiveQuery: boolean read fRequestLiveQuery write fRequestLiveQuery default true;

    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function EnsureQuery: IBoldQuery;
    procedure ReleaseQuery;

    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
    procedure InitFieldDefs; override;

//    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(const Value: string): TParam;
  end;

  TBoldDBQuery = class(TCustomBoldDBQuery)
  published

    property BoldPersistenceHandleDB;
    property Sql;

    property RequestLiveQuery;
    property Active;
    property AutoCalcFields;
    property Constraints;
    property Filter;
    property Filtered;
    property FilterOptions;
    property MasterFields;
    property MasterSource;
    property Params;
    property ReadOnly default true;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
  end;

implementation

uses
  SysUtils,
  BoldPersistenceHandlePassthrough,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldDefs;

{ TCustomBoldDBQuery }

constructor TCustomBoldDBQuery.Create(AOwner: TComponent);
begin
  inherited;
  fSubscriber:= TBoldPassthroughSubscriber.Create(Receive);
  fDataSetProvider:= TDataSetProvider.Create(nil);
  fSql:= TStringList.Create;
  ReadOnly:= true;
  TStringList(fSql).OnChange:= OnSQLChange;
end;

destructor TCustomBoldDBQuery.Destroy;
begin
  Active:= false;
  fSubscriber.Free;
  fDataSetProvider.free;
  ReleaseQuery;
  fSql.free;
  inherited;
end;

procedure TCustomBoldDBQuery.DoAfterClose;
begin
  inherited;
  fQuery.Close;
  ReleaseQuery;
end;

procedure TCustomBoldDBQuery.DoBeforeOpen;
begin
  inherited;
  with EnsureQuery do
  begin
    if not asDataset.Active then
    begin
      AssignSQL(sql);
      AssignParams(self.Params);
      Open;
    end;
    if not HasAppServer then
    begin
      fDataSetProvider.DataSet:= asDataset;
      SetProvider(fDataSetProvider);
    end;
  end;
end;

function TCustomBoldDBQuery.EnsureQuery: IBoldQuery;
begin
  if fQuery = nil then
  try
    if BoldPersistenceHandleDB = nil then
      BoldPersistenceHandleDB := FindDefaultPersistenceHandleDB;
    if BoldPersistenceHandleDB = nil then
      raise Exception.Create('BoldPersistenceHandleDB = nil');
    if BoldPersistenceHandleDB.DatabaseInterface = nil then
      raise Exception.Create('BoldPersistenceHandleDB.DatabaseInterface = nil');
    fQuery:= BoldPersistenceHandleDB.DatabaseInterface.GetQuery;
    fQuery.RequestLiveQuery:= self.RequestLiveQuery;
  except
    Active:= false;
    raise
  end;
  result:= fQuery  
end;

function TCustomBoldDBQuery.FindDefaultPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
var
  lBoldSystemHandle: TBoldSystemHandle;
  lBoldPersistenceHandle: TBoldPersistenceHandle;
begin
  result := nil;
  lBoldSystemHandle := TBoldSystemHandle.DefaultBoldSystemHandle as TBoldSystemHandle;
  if Assigned(lBoldSystemHandle) then
  begin
    lBoldPersistenceHandle := lBoldSystemHandle.PersistenceHandle;
    while not Assigned(result) and Assigned(lBoldPersistenceHandle) do
      if lBoldPersistenceHandle is TBoldPersistenceHandlePassthrough then
        lBoldPersistenceHandle := TBoldPersistenceHandlePassthrough(lBoldPersistenceHandle).NextPersistenceHandle
      else
      if lBoldPersistenceHandle is TBoldAbstractPersistenceHandleDB then
        result := lBoldPersistenceHandle as TBoldAbstractPersistenceHandleDB;
  end;
end;

procedure TCustomBoldDBQuery.ReleaseQuery;
begin
  if (fQuery <> nil) and Assigned(BoldPersistenceHandleDB) and Assigned(BoldPersistenceHandleDB.DatabaseInterface) then
    BoldPersistenceHandleDB.DatabaseInterface.ReleaseQuery(fQuery);
  fQuery:= nil;
end;

procedure TCustomBoldDBQuery.SetBoldPersistenceHandleDB(
  const Value: TBoldAbstractPersistenceHandleDB);
begin
  if fBoldPersistenceHandleDB = Value then exit;
  fBoldPersistenceHandleDB := Value;
  if Assigned(Value) then
    Value.AddSubscription(fSubscriber, beValueIdentityChanged, beValueIdentityChanged);
end;

procedure TCustomBoldDBQuery.OnSqlChange(Sender: TObject);
var
  List: TParams;
  s: string;
begin
  if not (csReading in ComponentState) then
  begin
    List := TParams.Create(Self);
    try
      s:= List.ParseSQL(SQL.Text, True);
      List.AssignValues(Params);
      Params.Clear;
      Params.Assign(List);
    finally
      List.Free;
    end;
  end;
  if Active then Close;
end;

procedure TCustomBoldDBQuery.SetSql(const Value: TStrings);
begin
  fSql.Assign(Value);
end;

procedure TCustomBoldDBQuery.InitFieldDefs;
begin
  with EnsureQuery do
  if not asDataset.Active then
  begin
    AssignSQL(sql);
    EnsureQuery.AssignParams(self.Params);
    Open;
    fDataSetProvider.DataSet:= asDataset;
    SetProvider(fDataSetProvider);
  end;
  inherited;
end;

function TCustomBoldDBQuery.ParamByName(const Value: string): TParam;
begin
  Result := Params.ParamByName(Value);
end;

procedure TCustomBoldDBQuery.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if OriginalEvent = beDestroying then
    BoldPersistenceHandleDB := nil;
end;
{
procedure TCustomBoldDBQuery.CheckFieldCompatibility(Field: TField;
  FieldDef: TFieldDef);
const
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftInteger, ftInteger, ftInteger, ftBoolean, ftFloat,
    ftFloat, ftBCD, ftDateTime, ftDateTime, ftDateTime, ftBytes, ftVarBytes,
    ftInteger, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftUnknown,
    ftString, ftUnknown, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftBlob, ftBlob, ftVariant, ftInterface, ftInterface, ftString, ftTimeStamp, ftFMTBcd);

  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];

  CheckTypeSizesSmaller = [ftString, ftFixedChar, ftWideString];
var
  lFieldType: TFieldType;
  lFieldDefType: TFieldType;
begin
  with Field do
  begin
    lFieldType := BaseFieldTypes[DataType];
    lFieldDefType := BaseFieldTypes[FieldDef.DataType];
    if (lFieldType <> lFieldDefType) then
    begin
      if not ( ((lFieldType = ftString) and (lFieldDefType = ftBlob))
        or ((lFieldType = ftBlob) and (lFieldDefType = ftString)) )
        then
          inherited;
    end
    else
      inherited;
  end;
end;
}

end.
