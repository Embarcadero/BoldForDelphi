
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractObjectUpgrader;

interface

uses
  Classes,
  BoldId,
  BoldDbInterfaces,
  BoldCollections,
  BoldBase;

type
  TBoldObjectUpgraderConfigurationItem = class;
  TBoldObjectUpgraderConfiguration = class;
  TBoldAbstractObjectUpgrader = class;
  TBoldObjectUpgraderConfigItemClass = class of TBoldObjectUpgraderConfigurationItem;
  TBoldObjectUpgraderConfigClass = class of TBoldObjectUpgraderConfiguration;

  TBoldObjectUpgraderConfigurationItem = class(TBoldUniquelyNamedCollectionItemWithNameStorage)
  private            
    FUpgradeOlderThanVersion: integer;
    procedure SetExpressionName(const Value: String);
    procedure SetUpgradeOlderThanVersion(const Value: integer);
    function GetConfig: TBoldObjectUpgraderConfiguration;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(source: TPersistent); override;
    property Config: TBoldObjectUpgraderConfiguration read GetConfig;
  published
    property ExpressionName: String read GetUniqueName write SetExpressionName;
    property UpgradeOlderThanVersion: integer read FUpgradeOlderThanVersion write SetUpgradeOlderThanVersion;
  end;

  TBoldObjectUpgraderConfiguration = class(TBoldCollectionWithUniquelyNamedItems)
  private
    function GetItems(i: integer): TBoldObjectUpgraderConfigurationItem;
    function GetConfigOwner: TPersistent;
    function GetItemByName(ExpressionName: String): TBoldObjectUpgraderConfigurationItem;
  protected
    function GetItemClass: TBoldObjectUpgraderConfigItemClass; virtual;
  public
    constructor create(Owner: TPersistent);
    property ConfigOwner: TPersistent read GetConfigOwner;
    property ItemByName[ExpressionName: String]: TBoldObjectUpgraderConfigurationItem read GetItemByName;
    property items[i: integer]: TBoldObjectUpgraderConfigurationItem read GetItems; default;
  end;

  TBoldAbstractObjectUpgrader = class(TBoldMemoryManagedObject)
  private
    fConfig: TBoldObjectUpgraderConfiguration;
  public
    constructor create(Config: TBoldObjectUpgraderConfiguration);
    procedure UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery); virtual; abstract;
    function NeedsManualUpdate(ExpressionName: string; Version: Integer): boolean;
    procedure StartTransaction; virtual; abstract;
    procedure EndTransaction; virtual; abstract;
    procedure FailTransaction; virtual; abstract;
    property Config: TBoldObjectUpgraderConfiguration read fConfig;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldNameExpander,
  BoldTaggedValueSupport;

{ TBoldObjectUpgraderConfigurationItem }

procedure TBoldObjectUpgraderConfigurationItem.Assign(source: TPersistent);
begin
  if source is TBoldObjectUpgraderConfigurationItem then
  begin
    ExpressionName := (source as TBoldObjectUpgraderConfigurationItem).ExpressionName;
    UpgradeOlderThanVersion := (source as TBoldObjectUpgraderConfigurationItem).UpgradeOlderThanVersion;
  end;
end;

function TBoldObjectUpgraderConfigurationItem.GetConfig: TBoldObjectUpgraderConfiguration;
begin
  result := Collection as TBoldObjectUpgraderConfiguration;
end;

function TBoldObjectUpgraderConfigurationItem.GetDisplayName: string;
begin
  if ExpressionName <> '' then
    result := ExpressionName
  else
    result := sDisplayNameUnassigned;
  result := Format(sUpgradeIfOlderThan, [Result, UpgradeOlderThanVersion]);
end;


procedure TBoldObjectUpgraderConfigurationItem.SetExpressionName(const Value: String);
begin
  UniqueName := BoldExpandName(Value, '', xtExpression, -1, nccDefault);
end;

procedure TBoldObjectUpgraderConfigurationItem.SetUpgradeOlderThanVersion(const Value: integer);
begin
  FUpgradeOlderThanVersion := Value;
end;

{ TBoldObjectUpgraderConfiguration }

constructor TBoldObjectUpgraderConfiguration.create(Owner: TPersistent);
begin
  inherited Create(Owner, GetItemClass);
end;

function TBoldObjectUpgraderConfiguration.GetConfigOwner: TPersistent;
begin
  result := GetOwner;
end;

function TBoldObjectUpgraderConfiguration.GetItemByName(ExpressionName: String): TBoldObjectUpgraderConfigurationItem;
begin
  result := inherited ItemByName[ExpressionName] as TBoldObjectUpgraderConfigurationItem;
end;

function TBoldObjectUpgraderConfiguration.GetItemClass: TBoldObjectUpgraderConfigItemClass;
begin
  result := TBoldObjectUpgraderConfigurationItem;
end;

function TBoldObjectUpgraderConfiguration.GetItems(i: integer): TBoldObjectUpgraderConfigurationItem;
begin
  result := inherited items[i] as TBoldObjectUpgraderConfigurationItem;
end;

{ TBoldAbstractObjectUpgrader }

constructor TBoldAbstractObjectUpgrader.create(Config: TBoldObjectUpgraderConfiguration);
begin
  inherited Create;
  fConfig := Config;
end;

function TBoldAbstractObjectUpgrader.NeedsManualUpdate(ExpressionName: string; Version: Integer): boolean;
var
  anItem: TBoldObjectUpgraderConfigurationItem;
begin
  anItem := Config.ItemByName[ExpressionName];
  result := assigned(anItem) and (anItem.UpgradeOlderThanVersion > Version);
end;

end.
