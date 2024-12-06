{ Global compiler directives }
{$include bold.inc}
unit BoldSQLHandle;

interface

uses
  Classes,
  Db,
  BoldElements,
  BoldHandles,
  BoldSystem;

type
  { forward declarations }
  TBoldSQLHandle = class;

  { TBoldSQLHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldSQLHandle = class(TBoldNonSystemHandle)
  private
    fBoldObjectClass: TBoldObjectClass;
    fWhereClause: string;
    fOrderByClause: string;
    fClassExpressionName: string;
    fObjectList: TBoldObjectlist;
    fListMode: TBoldListDupMode;
    fClearBeforeExecute: Boolean;
    fParams: TParams;
    fJoinInheritedTables: Boolean;
    fMaxAnswers: integer;
    fOffset: integer;
    procedure SetClassExpressionName(const Value: string);
    procedure SetListMode(const Value: TBoldListDupMode);
    procedure SetParams(const Value: TParams);
  protected
    { Protected declarations }
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    function GetValue: TBoldElement; override;
    procedure EnsureList(RaiseException: Boolean);
    procedure DoAssign(Source: TPersistent); override;    
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteSQL;
    procedure ClearList;
  published
    { Published declarations }
    property SQLWhereClause: string read fWhereClause write fWhereClause;
    property SQLOrderByClause: string read fOrderByClause write fOrderByClause;
    property ListMode: TBoldListDupMode read FListMode write SetListMode default bldmMerge;
    property ClassExpressionName: string read fClassExpressionName write SetClassExpressionName;
    property ClearBeforeExecute: Boolean read fClearBeforeExecute write fClearBeforeExecute default true;
    property MaxAnswers: integer read fMaxAnswers write fMaxAnswers default -1;
    property Offset: integer read fOffset write fOffset default -1;
    property Params: TParams read fParams write SetParams;
    property JoinInheritedTables: Boolean read fJoinInheritedTables write fJoinInheritedTables default true;
  end;

implementation

uses
  SysUtils,

  BoldDefs,
  BoldSubscription,
  BoldSystemRT,
  BoldCoreConsts;

constructor TBoldSQLHandle.Create(Owner: TComponent);
begin
  inherited;
  fClearBeforeExecute := true;
  fListMode := bldmMerge;
  fParams := TParams.Create(self);
  fJoinInheritedTables := true;
  fMaxAnswers := -1;
  fOffset := -1;
end;

destructor TBoldSQLHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fObjectList);
  FreeAndNil(fParams);
  inherited;
end;

function TBoldSQLHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if assigned(StaticSystemTypeInfo) then
    Result := StaticSystemTypeInfo.ClassTypeInfoByExpressionName[fClassExpressionName]
  else
    result := nil;
end;

procedure TBoldSQLHandle.ExecuteSQL;
const
  sExecuteSQL = 'ExecuteSQL';
begin
  if not assigned(StaticSystemHandle) then
    raise EBold.CreateFmt(sNoSystemHandle, [classname, sExecuteSQL, name]); // do not localize
  if not StaticSystemHandle.Active then
    raise EBold.CreateFmt(sSystemHandleNotActive, [classname, sExecuteSQL, name]);

  if ClearBeforeExecute then
    ClearList;
  EnsureList(true);
  StaticSystemHandle.System.GetAllInClassWithSQL(fObjectList, fBoldObjectClass, SQLWhereClause, SQLOrderByClause, Params, JoinInheritedTables, MaxAnswers, Offset);
end;

procedure TBoldSQLHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldSQLHandle then with TBoldSQLHandle(Source) do
  begin
    self.SQLWhereClause := SQLWhereClause;
    self.SQLOrderByClause := SQLOrderByClause;
    self.ListMode := ListMode;
    self.ClassExpressionName := ClassExpressionName;
    self.ClearBeforeExecute := ClearBeforeExecute;
    self.MaxAnswers := MaxAnswers;
    self.Offset := Offset;
    self.Params.Assign(Params);
    self.JoinInheritedTables := JoinInheritedTables;
  end;
end;

procedure TBoldSQLHandle.ClearList;
begin
  if assigned(fObjectList) then
    fObjectlist.Clear;
end;

function TBoldSQLHandle.GetValue: TBoldElement;
begin
  EnsureList(false);
  result := fObjectList;
end;

procedure TBoldSQLHandle.SetClassExpressionName(const Value: string);
begin
  if fClassExpressionName <> Value then
  begin
    fClassExpressionName := Value;
    FreeAndNil(fObjectList);
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

procedure TBoldSQLHandle.EnsureList(RaiseException: Boolean);
var
  ElementTypeInfo: TBoldElementTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  ListTypeInfo: TBoldListTypeInfo;
begin
  if not assigned(fObjectList) then
  begin

    if not assigned(StaticSystemHandle) and RaiseException then
      raise EBold.CreateFmt(sNoSystemHandle, [ClassName, 'EnsureList', name]); // do not localize

    ElementTypeInfo := StaticBoldType;

    if ElementTypeInfo is TBoldClassTypeInfo then
    begin
      ClassTypeInfo := ElementTypeInfo as TBoldClassTypeInfo;
      fBoldObjectClass := TBoldObjectClass(ClassTypeInfo.ObjectClass);
      ListTypeInfo := StaticSystemTypeInfo.ListTypeInfoByElement[ClassTypeInfo];
      fObjectList := TBoldMemberFactory.CreateMemberFromBoldType(ListTypeInfo) as TBoldObjectList;
      fObjectList.DuplicateMode := ListMode;
    end
    else if raiseException then
      raise EBold.CreateFmt(sCannotCreateListDueToInvalidType, [ClassName, Name]);
  end;
end;

procedure TBoldSQLHandle.SetListMode(const Value: TBoldListDupMode);
begin
  if FListMode <> value then
  begin
    FListMode := Value;
    if assigned(fObjectList) then
      fObjectList.DuplicateMode := value;
  end;
end;

procedure TBoldSQLHandle.SetParams(const Value: TParams);
begin
  Params.Assign(Value);
end;

end.
