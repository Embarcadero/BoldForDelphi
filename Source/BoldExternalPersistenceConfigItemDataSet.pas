
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceConfigItemDataSet;

interface

uses
  DB,
  Classes,
  SysUtils,
  BoldCollections;

type
  TBoldExternalPersistenceConfigDataSetItem = class(TBoldUniquelyNamedCollectionItemWithNameStorage)
  private
    FDataSet: TDataSet;
    procedure SetExpressionName(const Value: String);
    function GetExpressionName: String;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
  published
    property ClassExpressionName: String read GetExpressionName write SetExpressionName;
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

  TBoldExternalPersistenceConfigDataSetItems = class(TBoldCollectionWithUniquelyNamedItems)
  private
    function GetItem(Index: Integer): TBoldExternalPersistenceConfigDataSetItem;
  public
    constructor Create(AOwner: TComponent);
    function Add: TBoldExternalPersistenceConfigDataSetItem;
    function FindExpressionName(ExpressionName: String): TBoldExternalPersistenceConfigDataSetItem;
    property Items[Index: Integer]: TBoldExternalPersistenceConfigDataSetItem read GetItem; default;
  end;


implementation

constructor TBoldExternalPersistenceConfigDataSetItem.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);
end;

destructor TBoldExternalPersistenceConfigDataSetItem.Destroy;
begin
  inherited Destroy;
end;

function TBoldExternalPersistenceConfigDataSetItem.GetDisplayName: String;
begin
  Result := UniqueName;
end;

function TBoldExternalPersistenceConfigDataSetItem.GetExpressionName: String;
begin
  Result := UniqueName;
end;

procedure TBoldExternalPersistenceConfigDataSetItem.SetExpressionName(
  const Value: String);
begin
  UniqueName := Value;
end;

{ TBoldExternalPersistenceConfigDataSetItems }

function TBoldExternalPersistenceConfigDataSetItems.Add: TBoldExternalPersistenceConfigDataSetItem;
begin
  Result := inherited Add as TBoldExternalPersistenceConfigDataSetItem;
end;

constructor TBoldExternalPersistenceConfigDataSetItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TBoldExternalPersistenceConfigDataSetItem);
end;

function TBoldExternalPersistenceConfigDataSetItems.FindExpressionName(
  ExpressionName: String): TBoldExternalPersistenceConfigDataSetItem;
begin
  result := ItemByName[ExpressionName] as TBoldExternalPersistenceConfigDataSetItem;
end;

function TBoldExternalPersistenceConfigDataSetItems.GetItem(
  Index: Integer): TBoldExternalPersistenceConfigDataSetItem;
begin
  result := inherited GetItem(Index) as TBoldExternalPersistenceConfigDataSetItem;
end;

end.
