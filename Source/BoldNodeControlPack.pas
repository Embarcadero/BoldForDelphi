{ Global compiler directives }
{$include bold.inc}
unit BoldNodeControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldDefs,
  BoldControlPackDefs,
  BoldControlPack,
  BoldStringControlPack,
  BoldNumericControlPack,
  BoldListControlPack,
  BoldControllerListControlPack,
  BoldGenericListControlPack,
  BoldElements,
  BoldSystemRT,
  BoldSubscription;

type
  { Forward declarations }
  TBoldNodeFollowerController = class;
  TBoldTreeFollowerController = class;
  TBoldNodeDescription = class;
  TBoldNodeDescriptions = class;

  TBoldNodeDescriptionClass = class of TBoldNodeDescription;
  TBoldNodeDescriptionsClass = class of TBoldNodeDescriptions;

  TBoldTreeFollowerControllerClass = class of TBoldTreeFollowerController;
  TBoldNodeFollowerControllerClass = class of TBoldNodeFollowerController;

  { TBoldTreeFollowerController }
  TBoldTreeFollowerController = class(TBoldGenericListController)
  private
    FNodeDescriptions: TBoldNodeDescriptions;
    FDefaultNodeDescription: TBoldNodeDescription;
    FOnIconChanged: TBoldFollowerEvent;
    FOnTextChanged: TBoldFollowerEvent;
    FOnGetFollowerController: TGetFollowerControllerByNameEvent;
    procedure SetNodeDescriptions(Value: TBoldNodeDescriptions);
    function GetDefaultNodeDescriptionName: string;
    procedure SetDefaultNodeDescriptionName(const Value: string);
  protected
    class function PrecreateFollowers: boolean; override;
    procedure DoIconChanged(Follower: TBoldFollower);
    procedure DoTextChanged(Follower: TBoldFollower);
    {$IFDEF BOLD_BCB}
    procedure DoGetFollowerController(const Name: string; var FollowerController: TBoldFollowerController);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    function DoGetFollowerController(const Name: string): TBoldFollowerController;
    {$ENDIF}
    procedure DoAssign(Source: TPersistent); override;
    function GetBoldNodeDescriptionsClass: TBoldNodeDescriptionsClass; virtual;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    function DefaultGetNodeFollowerControllerByName(const Name: string): TBoldFollowerController;
    property OnIconChanged: TBoldFollowerEvent read FOnIconChanged write FOnIconChanged;
    property OnTextChanged: TBoldFollowerEvent read FOnTextChanged write FOnTextChanged;
  published
    property NodeDescriptions: TBoldNodeDescriptions read FNodeDescriptions write SetNodeDescriptions;
    property DefaultNodeDescriptionName: string read GetDefaultNodeDescriptionName write SetDefaultNodeDescriptionName;
    property OnGetFollowerController: TGetFollowerControllerByNameEvent read FOnGetFollowerController write FOnGetFollowerController;
  end;

  { TBoldNodeDescriptions }
  TBoldNodeDescriptions = class(TCollection)
  private
    FTreeFollowerController: TBoldTreeFollowerController;
    function GetNodeControllerItem(Index: Integer): TBoldNodeDescription;
    procedure SetNodeControllerItem(Index: Integer; Value: TBoldNodeDescription);
  protected
    function GetNodeDescriptionClass: TBoldNodeDescriptionClass; virtual;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aTreeFollowerController: TBoldTreeFollowerController);
    function FindByName(const Name: string): TBoldNodeDescription;
    function Add: TBoldNodeDescription;
    property Items[Index: Integer]: TBoldNodeDescription read GetNodeControllerItem write SetNodeControllerItem; default;
  end;

  { TBoldNodeDescription }
  TBoldNodeDescription = class(TCollectionItem)
  private
    FName: string;
    FNodeFollowerController: TBoldNodeFollowerController;
    procedure SetName(const Value: string);
    function GetHideNodeWithNoChildren: Boolean;
    procedure SetHideNodeWithNoChildren(Value: Boolean);
    function GetListController: TBoldGenericListController;
    function GetIconController: TBoldIntegerFollowerController;
    function GetTextController: TBoldStringFollowerController;
    procedure SetListController(Value: TBoldGenericListController);
    procedure SetIconController(Value: TBoldIntegerFollowerController);
    procedure SetTextController(Value: TBoldStringFollowerController);
    function GetContextTypeName: String;
    procedure SetContextTypeName(const Value: String);
    function GetContextType: TBoldElementTypeInfo;
  protected
    function GetDisplayName: string; override;
    function GetBoldNodeFollowerControllerClass: TBoldNodeFollowerControllerClass;virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NodeFollowerController: TBoldNodeFollowerController read FNodeFollowerController;
  published
    property Name: string read FName write SetName;
    property ContextTypeName: String read GetContextTypeName write SetContextTypeName;
    property HideNodeWithNoChildren: Boolean read GetHideNodeWithNoChildren write SetHideNodeWithNoChildren;
    property ListController: TBoldGenericListController read GetListController write SetListController;
    property IconController: TBoldIntegerFollowerController read GetIconController write SetIconController;
    property TextController: TBoldStringFollowerController read GetTextController write SetTextController;
  end;

  { TBoldNodeFollowerController }
  TBoldNodeFollowerController = class(TBoldControllerList)
  private
    FTreeFollowerController: TBoldTreeFollowerController;
    FHideNodeWithNoChildren: Boolean;
    FContextTypeName: String;
    fGenericListFollowerController: TBoldGenericListController;
    fIconFollowerController: TBoldIntegerFollowerController;
    fTextFollowerController: TBoldStringFollowerController;
    procedure SetHideNodeWithNoChildren(Value: Boolean);
    procedure SetContextTypeName(const Value: String);
    function GetSystemTypeInfo: TBoldSystemTypeInfo;
  protected
    class function PrecreateFollowers: boolean; override;  
  public
    constructor Create(aTreeFollowerController: TBoldTreeFollowerController);virtual;
    destructor Destroy; override;
    property HideNodeWithNoChildren: Boolean read FHideNodeWithNoChildren write SetHideNodeWithNoChildren;
    property ContextTypeName: String read FContextTypeName write SetContextTypeName;
    property SystemTypeInfo: TBoldSystemTypeInfo read GetSystemTypeInfo;
    property GenericListFollowerController: TBoldGenericListController read fGenericListFollowerController;
    property IconFollowerController: TBoldIntegerFollowerController read fIconFollowerController;
    property TextFollowerController: TBoldStringFollowerController read fTextFollowerController;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStrings,
  BoldUtils;

{-- TBoldTreeFollowerController --}

constructor TBoldTreeFollowerController.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent, DoGetFollowerController);
  FNodeDescriptions := GetBoldNodeDescriptionsClass.Create(Self);
end;

function TBoldTreeFollowerController.GetBoldNodeDescriptionsClass: TBoldNodeDescriptionsClass;
begin
  result := TBoldNodeDescriptions;
end;

destructor TBoldTreeFollowerController.Destroy;
begin
  FreePublisher;
  FreeAndNil(FNodeDescriptions);
  inherited Destroy;
end;

procedure TBoldTreeFollowerController.SetNodeDescriptions(Value: TBoldNodeDescriptions);
begin
  FNodeDescriptions.Assign(Value);
end;

function TBoldTreeFollowerController.GetDefaultNodeDescriptionName: string;
begin
  if Assigned(FDefaultNodeDescription) then
    Result := FDefaultNodeDescription.Name
  else
    Result := '';
end;

class function TBoldTreeFollowerController.PrecreateFollowers: boolean;
begin
  result := true;
end;

procedure TBoldTreeFollowerController.SetDefaultNodeDescriptionName(const Value: string);
var
  NodeDescription: TBoldNodeDescription;
begin
  if Value <> DefaultNodeDescriptionName then
  begin
    NodeDescription := FNodeDescriptions.FindByName(Value);
    if Assigned(NodeDescription) or (Value = '') then
    begin
      FDefaultNodeDescription := NodeDescription;
      Changed;
    end
    else
      raise EBold.CreateFmt('%s.SetDefaultNodeDescriptionName: ''%s'' is not a valid name', [ClassName, Value]);
  end;
end;

function TBoldTreeFollowerController.DefaultGetNodeFollowerControllerByName(const Name: string): TBoldFollowerController;
var
  NodeDescription: TBoldNodeDescription;
begin
  NodeDescription := FNodeDescriptions.FindByName(Name);
  if Assigned(NodeDescription) then
    Result := NodeDescription.NodeFollowerController
  else
    if Assigned(FDefaultNodeDescription) and (AnsiCompareStr(Name, DEFAULTNAME) = 0) then
      Result := FDefaultNodeDescription.NodeFollowerController
    else
      Result := nil;
end;

procedure TBoldTreeFollowerController.DoIconChanged(Follower: TBoldFollower);
begin
  if Assigned(FOnIconChanged) then
    FOnIconChanged(Follower);
end;

procedure TBoldTreeFollowerController.DoTextChanged(Follower: TBoldFollower);
begin
  if Assigned(FOnTextChanged) then
    FOnTextChanged(Follower);
end;

{$IFDEF BOLD_BCB}
procedure TBoldTreeFollowerController.DoGetFollowerController(const Name: string; var FollowerController: TBoldFollowerController);
{$ENDIF}
{$IFDEF BOLD_DELPHI}
function TBoldTreeFollowerController.DoGetFollowerController(const Name: string): TBoldFollowerController;
{$ENDIF}
begin
{$IFDEF BOLD_BCB}
  if Assigned(FOnGetFollowerController) then
  begin
    FollowerController := nil;
    fOnGetFollowerController(Name, FollowerController);
  end
  else
    FollowerController := DefaultGetNodeFollowerControllerByName(Name);
{$ENDIF}

{$IFDEF BOLD_DELPHI}
  if Assigned(FOnGetFollowerController) then
    Result := FOnGetFollowerController(Name)
  else
    Result := DefaultGetNodeFollowerControllerByName(Name);
{$ENDIF}
end;

{-- TBoldNodeDescriptions --}

constructor TBoldNodeDescriptions.Create(aTreeFollowerController: TBoldTreeFollowerController);
begin
  inherited Create(GetNodeDescriptionClass);
  FTreeFollowerController := aTreeFollowerController;
end;

function TBoldNodeDescriptions.GetNodeDescriptionClass: TBoldNodeDescriptionClass;
begin
  Result := TBoldNodeDescription;
end;

function TBoldNodeDescriptions.GetNodeControllerItem(Index: Integer): TBoldNodeDescription;
begin
  Result := TBoldNodeDescription(GetItem(Index));
end;

procedure TBoldNodeDescriptions.SetNodeControllerItem(Index: Integer; Value: TBoldNodeDescription);
begin
  SetItem(Index, Value);
end;

function TBoldNodeDescriptions.GetOwner: TPersistent;
begin
  Result := FTreeFollowerController;
end;

procedure TBoldNodeDescriptions.Update(Item: TCollectionItem);
begin
  FTreeFollowerController.Changed;
end;

function TBoldNodeDescriptions.FindByName(const Name: string): TBoldNodeDescription;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TBoldNodeDescription(GetItem(I));
    if CompareText(Result.Name, Name) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TBoldNodeDescriptions.Add: TBoldNodeDescription;
begin
  BeginUpdate;
  try
    Result := GetNodeDescriptionClass.Create(Self);
  finally
    EndUpdate;
  end;
end;

{-- TBoldNodeDescription-- }

constructor TBoldNodeDescription.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNodeFollowerController := GetBoldNodeFollowerControllerClass.Create(TBoldNodeDescriptions(Collection).FTreeFollowerController);
  FNodeFollowerController.OnGetContextType := GetContextType;
end;

function TBoldNodeDescription.GetBoldNodeFollowerControllerClass:TBoldNodeFollowerControllerClass;
begin
  result := TBoldNodeFollowerController;
end;

destructor TBoldNodeDescription.Destroy;
begin
  FreeAndNil(FNodeFollowerController);
  inherited Destroy;
end;

procedure TBoldNodeDescription.Assign(Source: TPersistent);
begin
  if Source is TBoldNodeDescription then
  begin
    Name := TBoldNodeDescription(Source).Name;
    HideNodeWithNoChildren := TBoldNodeDescription(Source).HideNodeWithNoChildren;
    ContextTypeName := TBoldNodeDescription(Source).ContextTypeName;
    ListController.Assign(TBoldNodeDescription(Source).ListController);
    IconController.Assign(TBoldNodeDescription(Source).IconController);
    TextController.Assign(TBoldNodeDescription(Source).TextController);
  end
  else
    inherited Assign(Source);
end;

function TBoldNodeDescription.GetDisplayName: string;
begin
  Result := FName;
  if (Result = '') then
    Result := inherited GetDisplayName;
end;

procedure TBoldNodeDescription.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    if (Value <> '') and
       (CompareText(FName, Value) <> 0) and
       (TBoldNodeDescriptions(Collection).FindByName(Value) <> nil) then
      raise EBold.CreateFmt(sBoldInvalidName, [Value]);
    FName := Value;
    Changed(False);
  end;
end;

function TBoldNodeDescription.GetHideNodeWithNoChildren: Boolean;
begin
  Result := FNodeFollowerController.HideNodeWithNoChildren;
end;

procedure TBoldNodeDescription.SetHideNodeWithNoChildren(Value: Boolean);
begin
  FNodeFollowerController.HideNodeWithNoChildren := Value;
end;

function TBoldNodeDescription.GetListController: TBoldGenericListController;
begin
  Result := FNodeFollowerController.Items[BoldNodeListIndex] as TBoldGenericListController;
end;

function TBoldNodeDescription.GetIconController: TBoldIntegerFollowerController;
begin
  Result := FNodeFollowerController.Items[BoldNodeIconIndex] as TBoldIntegerFollowerController;
end;

function TBoldNodeDescription.GetTextController: TBoldStringFollowerController;
begin
  Result := FNodeFollowerController.Items[BoldNodeTextIndex] as TBoldStringFollowerController;
end;

procedure TBoldNodeDescription.SetListController(Value: TBoldGenericListController);
begin
  GetListController.Assign(Value);
end;

procedure TBoldNodeDescription.SetIconController(Value: TBoldIntegerFollowerController);
begin
  GetIconController.Assign(Value);
end;

procedure TBoldNodeDescription.SetTextController(Value: TBoldStringFollowerController);
begin
  GetTextController.Assign(Value);
end;

{-- TBoldNodeFollowerController --}

constructor TBoldNodeFollowerController.Create(aTreeFollowerController: TBoldTreeFollowerController);
var
  OwningComponent: TComponent;
begin
  OwningComponent := aTreeFollowerController.OwningComponent;
  inherited Create(OwningComponent);
  FTreeFollowerController := aTreeFollowerController;

  fGenericListFollowerController := TBoldGenericListController.Create(OwningComponent, FTreeFollowerController.DoGetFollowerController);
  fGenericListFollowerController.OnBeforeInsertItem := FTreeFollowerController.DoBeforeInsertItem;
  fGenericListFollowerController.OnAfterInsertItem := FTreeFollowerController.DoAfterInsertItem;
  fGenericListFollowerController.OnBeforeDeleteItem := FTreeFollowerController.DoBeforeDeleteItem;
  fGenericListFollowerController.OnAfterDeleteItem := FTreeFollowerController.DoAfterDeleteItem;
  fGenericListFollowerController.BeforeMakeUptoDate := FTreeFollowerController.BeforeMakeUptoDate;
//  fGenericListFollowerController.OnReplaceitem := FTreeFollowerController.OnReplaceitem;
  fGenericListFollowerController.OnGetContextType := GetContextType;
  Add(fGenericListFollowerController);

  fIconFollowerController := TBoldIntegerFollowerController.Create(OwningComponent);
  fIconFollowerController.AfterMakeUptoDate := FTreeFollowerController.DoIconChanged;
  fIconFollowerController.Expression := '-1';
  fIconFollowerController.OnGetContextType := GetContextType;
  Add(IconFollowerController);

  fTextFollowerController := TBoldStringFollowerController.Create(OwningComponent);
  fTextFollowerController.AfterMakeUptoDate := FTreeFollowerController.DoTextChanged;
  fTextFollowerController.OnGetContextType := GetContextType;
  Add(TextFollowerController);
end;

procedure TBoldNodeFollowerController.SetHideNodeWithNoChildren(Value: Boolean);
begin
  if FHideNodeWithNoChildren <> Value then
  begin
    FHideNodeWithNoChildren := Value;
    Changed;
  end;
end;

destructor TBoldNodeFollowerController.Destroy;
begin
  FreePublisher;
  inherited Destroy;
end;

procedure TBoldTreeFollowerController.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldTreeFollowerController);
  inherited DoAssign(Source);
  NodeDescriptions.Assign(TBoldTreeFollowerController(Source).NodeDescriptions);
  DefaultNodeDescriptionName := TBoldTreeFollowerController(Source).DefaultNodeDescriptionName;
  OnGetFollowerController := TBoldTreeFollowerController(Source).OnGetFollowerController;
end;

procedure TBoldNodeFollowerController.SetContextTypeName(
  const Value: String);
begin
  if FContextTypeName <> Value then
  begin
    FContextTypeName := Value;
    Changed;
  end;
end;

function TBoldNodeDescription.GetContextTypeName: String;
begin
  Result := FNodeFollowerController.ContextTypeName;
end;

procedure TBoldNodeDescription.SetContextTypeName(const Value: String);
begin
  FNodeFollowerController.ContextTypeName := Value;
end;

function TBoldNodeFollowerController.GetSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if FTreeFollowerController.GetContextType <> nil then
    result := FTreeFollowerController.GetContextType.SystemTypeInfo as TBoldSystemTypeInfo
  else
    result := nil;
end;

class function TBoldNodeFollowerController.PrecreateFollowers: boolean;
begin
  result := true;
end;

function TBoldNodeDescription.GetContextType: TBoldElementTypeInfo;
var
  ContextType: TBoldElementTypeInfo;
begin
  ContextType := NodeFollowerController.fTreeFollowerController.GetContextType;
  if assigned(ContextType) then
  begin
    if ContextTypeName <> '' then
      result := (ContextType.SystemTypeInfo as TBoldSystemTypeInfo).ElementTypeInfoByExpressionName[ContextTypeName]
    else
      result := (ContextType.SystemTypeInfo as TBoldSystemTypeInfo).ElementTypeInfoByExpressionName[Name];
  end
  else
    result := nil;
end;

end.