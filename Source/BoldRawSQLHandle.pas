
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldRawSQLHandle;

interface

uses
  Classes,
  Db,
  BoldElements,
  BoldHandles,
  BoldSystem;

type
  { forward declarations }
  TBoldRawSQLHandle = class;

  { TBoldRawSQLHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldRawSQLHandle = class(TBoldNonSystemHandle)
  private
    fBoldObjectClass: TBoldObjectClass;
    fSQL: string;
    fClassExpressionName: string;
    fObjectList: TBoldObjectlist;
    fListMode: TBoldListDupMode;
    fClearBeforeExecute: Boolean;
    fParams: TParams;
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
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteSQL;
    procedure ClearList;
  published
    { Published declarations }
    property SQL: string read fSQL write fSQL;
    property ListMode: TBoldListDupMode read FListMode write SetListMode default bldmMerge;
    property ClassExpressionName: string read fClassExpressionName write SetClassExpressionName;
    property ClearBeforeExecute: Boolean read fClearBeforeExecute write fClearBeforeExecute default true;
    property MaxAnswers: integer read fMaxAnswers write fMaxAnswers default -1;
    property Offset: integer read fOffset write fOffset default -1;
    property Params: TParams read fParams write SetParams;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldSubscription,
  BoldSystemRT;

constructor TBoldRawSQLHandle.Create(Owner: TComponent);
begin
  inherited;
  fClearBeforeExecute := true;
  fListMode := bldmMerge;
  fParams := TParams.Create(self);
  fMaxAnswers := -1;
  fOffset := -1;
end;

destructor TBoldRawSQLHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fObjectList);
  FreeAndNil(fParams);
  inherited;
end;

function TBoldRawSQLHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if assigned(StaticSystemTypeInfo) then
    Result := StaticSystemTypeInfo.ClassTypeInfoByExpressionName[fClassExpressionName]
  else
    result := nil;
end;

procedure TBoldRawSQLHandle.ExecuteSQL;
begin
  if not assigned(StaticSystemHandle) then
    raise EBold.CreateFmt('%s.ExecuteSQL: %s has no SystemHandle', [classname, name]);
  if not StaticSystemHandle.Active then
    raise EBold.CreateFmt('%s.ExecuteSQL: Systemhandle is not active', [classname]);

  if ClearBeforeExecute then
    ClearList;
  EnsureList(true);
  StaticSystemHandle.System.GetAllInClassWithRawSQL(fObjectList, fBoldObjectClass, SQL, Params, MaxAnswers, Offset);
end;

procedure TBoldRawSQLHandle.ClearList;
begin
  if assigned(fObjectList) then
    fObjectlist.Clear;
end;

function TBoldRawSQLHandle.GetValue: TBoldElement;
begin
  EnsureList(false);
  result := fObjectList;
end;

procedure TBoldRawSQLHandle.SetClassExpressionName(const Value: string);
begin
  if fClassExpressionName <> Value then
  begin
    fClassExpressionName := Value;
    FreeAndNil(fObjectList);
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

procedure TBoldRawSQLHandle.EnsureList(RaiseException: Boolean);
var
  ElementTypeInfo: TBoldElementTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  ListTypeInfo: TBoldListTypeInfo;
begin
  if not assigned(fObjectList) then
  begin

    if not assigned(StaticSystemHandle) and RaiseException then
      raise EBold.CreateFmt('%s.EnsureList: %s not connected to a SystemHandle', [ClassName, name]);

    ElementTypeInfo := StaticBoldType;

    if ElementTypeInfo is TBoldClassTypeInfo then
    begin
      ClassTypeInfo := ElementTypeInfo as TBoldClassTypeInfo;
      fBoldObjectClass := TBoldObjectClass(ClassTypeInfo.ObjectClass);
      ListTypeInfo := StaticSystemTypeInfo.ListTypeInfoByElement[ClassTypeInfo];
      fObjectList := TBoldMemberFactory.CreateMemberFromBoldType(ListTypeInfo) as TBoldObjectList;
      fObjectList.DuplicateMode := ListMode;
    end
    else
      if raiseException then
        raise EBold.CreateFmt('%s.EnsureList: Unable to create list (%s), cant find valid type', [ClassName, Name]);
  end;
end;

procedure TBoldRawSQLHandle.SetListMode(const Value: TBoldListDupMode);
begin
  if FListMode <> value then
  begin
    FListMode := Value;
    if assigned(fObjectList) then
      fObjectList.DuplicateMode := value;
  end;
end;

procedure TBoldRawSQLHandle.SetParams(const Value: TParams);
begin
  Params.Assign(Value);
end;

end.
