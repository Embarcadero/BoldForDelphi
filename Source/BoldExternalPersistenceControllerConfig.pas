
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceControllerConfig;

interface

uses
  Classes,
  BoldValueSpaceInterfaces,
  BoldMeta,
  BoldExternalPersistenceSupport,
  BoldId,
  BoldCollections;

type
  {---forward declarations---}
  TBoldExternalPersistenceConfigItem = class;
  TBoldExternalPersistenceConfigItems = class;

  TBoldExternalPersistenceFetchEvent = procedure(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; FetchContext: TObject) of object;
  TBoldExternalPersistenceCreateEvent = procedure(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldvalueSpace) of object;
  TBoldExternalPersistenceUpdateEvent = procedure(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; ValueSpace: IBoldvalueSpace) of object;
  TBoldExternalPersistenceDeleteEvent = procedure(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId) of object;
  TBoldExternalPersistenceGetListEvent = procedure(const ExternalKeys: TBoldObjectIdList) of object;
  TBoldExternalPersistenceGetInternalSQLForKeysEvent = function(const ExternalKeys: TBoldObjectIdList): String of object;
  TBoldExternalPersistencePrepareFetchEvent = procedure(ExternalKeys: TBoldObjectIdList; Members: TBoldMemberIdList; var FetchContext: TObject) of object;
  TBoldExternalPersistencePostFetchEvent = procedure(FetchContext: TObject) of object;

  TBoldExternalPersistenceReadMemberEvent = procedure(Obj: IPersistentBoldObject; ExternalKey: TBoldObjectId; Member: TMoldMember; FetchContext: TObject) of object;

  TBoldExternalPersistenceGetExistsEvent = function(const ExternalKey: TBoldObjectId): Boolean of object;
  TBoldExternalPersistenceAssignKeyToObjectEvent = procedure(const Obj: IPersistentBoldObject; const ExternalKey: TBoldObjectId) of object;
  TBoldExternalPersistenceGetKeyFromObject = function(const Obj: IPersistentBoldObject): TBoldObjectId of object;

  TBoldExternalPersistenceConfigItem = class(TBoldUniquelyNamedCollectionItemWithNameStorage)
  private
    FFetchAllMembersWhenFetchingKey: boolean;
    FOnCreateObject: TBoldExternalPersistenceCreateEvent;
    FOnReadObject: TBoldExternalPersistenceFetchEvent;
    FOnUpdateObject: TBoldExternalPersistenceUpdateEvent;
    FOnDeleteObject: TBoldExternalPersistenceDeleteEvent;
    FOnExists: TBoldExternalPersistenceGetExistsEvent;
    FOnGetKeyList: TBoldExternalPersistenceGetListEvent;
    fOnGetKeyFromObject: TBoldExternalPersistenceGetKeyFromObject;
    fOnAssignKeyToObject: TBoldExternalPersistenceAssignKeyToObjectEvent;
    fOnReadMember: TBoldExternalPersistenceReadMemberEvent;
    fOnPostFetch: TBoldExternalPersistencePostFetchEvent;
    fOnPrepareFetch: TBoldExternalPersistencePrepareFetchEvent;
    fOnGetInternalSQLForKeys: TBoldExternalPersistenceGetInternalSQLForKeysEvent;
    procedure SetExpressionName(const Value: String);
    function GetExpressionName: String;
  protected
    function GetDisplayName: String; override;
  public
  published
    property ExpressionName: String read GetExpressionName write SetExpressionName;
    property FetchAllMembersWhenFetchingKey: boolean read FFetchAllMembersWhenFetchingKey write FFetchAllMembersWhenFetchingKey default false;
    property OnCreateObject: TBoldExternalPersistenceCreateEvent read FOnCreateObject write FOnCreateObject;
    property OnReadObject: TBoldExternalPersistenceFetchEvent read FOnReadObject write FOnReadObject;
    property OnUpdateObject: TBoldExternalPersistenceUpdateEvent read FOnUpdateObject write FOnUpdateObject;
    property OnReadMember: TBoldExternalPersistenceReadMemberEvent read fOnReadMember write fOnReadMember;
    property OnDeleteObject: TBoldExternalPersistenceDeleteEvent read FOnDeleteObject write FOnDeleteObject;
    property OnExists: TBoldExternalPersistenceGetExistsEvent read FOnExists write FOnExists;
    property OnGetKeyList: TBoldExternalPersistenceGetListEvent read FOnGetKeyList write FOnGetKeyList;
    property OnGetKeyFromObject: TBoldExternalPersistenceGetKeyFromObject read fOnGetKeyFromObject write fOnGetKeyFromObject;
    property OnAssignKeyToObject: TBoldExternalPersistenceAssignKeyToObjectEvent read fOnAssignKeyToObject write fOnAssignKeyToObject;
    property OnPrepareFetch: TBoldExternalPersistencePrepareFetchEvent read fOnPrepareFetch write fOnPrepareFetch;
    property OnPostFetch: TBoldExternalPersistencePostFetchEvent read fOnPostFetch write fOnPostFetch;
    property OnGetInternalSQLForKeys: TBoldExternalPersistenceGetInternalSQLForKeysEvent read fOnGetInternalSQLForKeys write fOnGetInternalSQLForKeys;
  end;

  TBoldExternalPersistenceConfigItems = class(TBoldCollectionWithUniquelyNamedItems)
  private
    function GetItem(Index: Integer): TBoldExternalPersistenceConfigItem;
  public
    constructor Create(AOwner: TComponent);
    function Add: TBoldExternalPersistenceConfigItem;
    function FindExpressionName(ExpressionName: String): TBoldExternalPersistenceConfigItem;
    property Items[Index: Integer]: TBoldExternalPersistenceConfigItem read GetItem;
  end;


implementation


{ TBoldExternalPersistenceConfigItem }

procedure TBoldExternalPersistenceConfigItem.SetExpressionName(const Value: String);
begin
  UniqueName := Value;
end;

function TBoldExternalPersistenceConfigItem.GetExpressionName: String;
begin
  result := UniqueName;
end;

function TBoldExternalPersistenceConfigItem.GetDisplayName: String;
begin
  Result := ExpressionName;
end;

{ TBoldExternalPersistenceConfigItems }

function TBoldExternalPersistenceConfigItems.Add: TBoldExternalPersistenceConfigItem;
begin
  Result := inherited Add as TBoldExternalPersistenceConfigItem;
end;

constructor TBoldExternalPersistenceConfigItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TBoldExternalPersistenceConfigItem);
end;

function TBoldExternalPersistenceConfigItems.FindExpressionName(ExpressionName: String): TBoldExternalPersistenceConfigItem;
begin
  result := ItemByName[ExpressionName] as TBoldExternalPersistenceConfigItem;
end;

function TBoldExternalPersistenceConfigItems.GetItem(Index: Integer): TBoldExternalPersistenceConfigItem;
begin
  Result := inherited GetItem(Index) as TBoldExternalPersistenceConfigItem;
end;

end.
