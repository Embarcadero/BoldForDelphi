
{ Global compiler directives }
{$include bold.inc}
unit BoldNodeControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

interface

uses
  Classes,
  BoldControlPackCom,
  BoldStringControlPackCom,
  BoldNumericControlPackCom,
  BoldControllerListControlPackCom,
  BoldGenericListControlPackCom,
  BoldComObjectSpace_TLB;
  {!! DO NOT REMOVE !! BoldSystemRT ,}

type
  { Forward declarations }
  TBoldNodeFollowerControllerCom = class;
  TBoldTreeFollowerControllerCom = class;
  TBoldNodeDescriptionCom = class;
  TBoldNodeDescriptionsCom = class;

  TBoldNodeDescriptionClassCom = class of TBoldNodeDescriptionCom;
  TBoldNodeDescriptionsClassCom = class of TBoldNodeDescriptionsCom;

  TBoldTreeFollowerControllerClassCom = class of TBoldTreeFollowerControllerCom;
  TBoldNodeFollowerControllerClassCom = class of TBoldNodeFollowerControllerCom;

  { TBoldTreeFollowerControllerCom }
  TBoldTreeFollowerControllerCom = class(TBoldGenericListControllerCom)
  private
    FNodeDescriptions: TBoldNodeDescriptionsCom;
    FDefaultNodeDescription: TBoldNodeDescriptionCom;
    FOnIconChanged: TBoldFollowerEventCom;
    FOnTextChanged: TBoldFollowerEventCom;
    FOnGetFollowerController: TGetFollowerControllerByNameEventCom;
    procedure SetNodeDescriptions(Value: TBoldNodeDescriptionsCom);
    function GetDefaultNodeDescriptionName: string;
    procedure SetDefaultNodeDescriptionName(const Value: string);
  protected
    procedure DoIconChanged(Follower: TBoldFollowerCom);
    procedure DoTextChanged(Follower: TBoldFollowerCom);
    {$IFDEF BOLD_BCB}
    procedure DoGetFollowerController(Name: string; var FollowerController: TBoldFollowerControllerCom);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    function DoGetFollowerController(Name: string): TBoldFollowerControllerCom;
    {$ENDIF}
    procedure DoAssign(Source: TPersistent); override;
    function GetBoldNodeDescriptionsClass: TBoldNodeDescriptionsClassCom; virtual;
  public
    constructor Create(aOwningComponent: TComponent);
    destructor Destroy; override;
    function DefaultGetNodeFollowerControllerByName(const Name: string): TBoldFollowerControllerCom;
    property OnIconChanged: TBoldFollowerEventCom read FOnIconChanged write FOnIconChanged;
    property OnTextChanged: TBoldFollowerEventCom read FOnTextChanged write FOnTextChanged;
  published
    property NodeDescriptions: TBoldNodeDescriptionsCom read FNodeDescriptions write SetNodeDescriptions;
    property DefaultNodeDescriptionName: string read GetDefaultNodeDescriptionName write SetDefaultNodeDescriptionName;
    property OnGetFollowerController: TGetFollowerControllerByNameEventCom read FOnGetFollowerController write FOnGetFollowerController;
  end;

  { TBoldNodeDescriptionsCom }
  TBoldNodeDescriptionsCom = class(TCollection)
  private
    FTreeFollowerController: TBoldTreeFollowerControllerCom;
    function GetNodeControllerItem(Index: Integer): TBoldNodeDescriptionCom;
    procedure SetNodeControllerItem(Index: Integer; Value: TBoldNodeDescriptionCom);
  protected
    function GetNodeDescriptionClass: TBoldNodeDescriptionClassCom; virtual;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aTreeFollowerController: TBoldTreeFollowerControllerCom);
    function FindByName(const Name: string): TBoldNodeDescriptionCom;
    function Add: TBoldNodeDescriptionCom;
    property Items[Index: Integer]: TBoldNodeDescriptionCom read GetNodeControllerItem write SetNodeControllerItem; default;
  end;

  { TBoldNodeDescriptionCom }
  TBoldNodeDescriptionCom = class(TCollectionItem)
  private
    FName: string;
    FNodeFollowerController: TBoldNodeFollowerControllerCom;
    procedure SetName(Value: string);
    function GetHideNodeWithNoChildren: Boolean;
    procedure SetHideNodeWithNoChildren(Value: Boolean);
    function GetListController: TBoldGenericListControllerCom;
    function GetIconController: TBoldIntegerFollowerControllerCom;
    function GetTextController: TBoldStringFollowerControllerCom;
    procedure SetListController(Value: TBoldGenericListControllerCom);
    procedure SetIconController(Value: TBoldIntegerFollowerControllerCom);
    procedure SetTextController(Value: TBoldStringFollowerControllerCom);
    function GetContextTypeName: String;
    procedure SetContextTypeName(const Value: String);
    function GetContextType: IBoldElementTypeInfo;
  protected
    function GetDisplayName: string; override;
    function GetBoldNodeFollowerControllerClass: TBoldNodeFollowerControllerClassCom;virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NodeFollowerController: TBoldNodeFollowerControllerCom read FNodeFollowerController;
  published
    property Name: string read FName write SetName;
    property ContextTypeName: String read GetContextTypeName write SetContextTypeName;
    property HideNodeWithNoChildren: Boolean read GetHideNodeWithNoChildren write SetHideNodeWithNoChildren;
    property ListController: TBoldGenericListControllerCom read GetListController write SetListController;
    property IconController: TBoldIntegerFollowerControllerCom read GetIconController write SetIconController;
    property TextController: TBoldStringFollowerControllerCom read GetTextController write SetTextController;
  end;

  { TBoldNodeFollowerControllerCom }
  TBoldNodeFollowerControllerCom = class(TBoldControllerListCom)
  private
    FTreeFollowerController: TBoldTreeFollowerControllerCom;
    FHideNodeWithNoChildren: Boolean;
    FContextTypeName: String;
    fGenericListFollowerController: TBoldGenericListControllerCom;
    fIconFollowerController: TBoldIntegerFollowerControllerCom;
    fTextFollowerController: TBoldStringFollowerControllerCom;
    procedure SetHideNodeWithNoChildren(Value: Boolean);
    procedure SetContextTypeName(const Value: String);
    function GetSystemTypeInfo: IBoldSystemTypeInfo;
  public
    constructor Create(aTreeFollowerController: TBoldTreeFollowerControllerCom);virtual;
    destructor Destroy; override;
    property HideNodeWithNoChildren: Boolean read FHideNodeWithNoChildren write SetHideNodeWithNoChildren;
    property ContextTypeName: String read FContextTypeName write SetContextTypeName;
    property SystemTypeInfo: IBoldSystemTypeInfo read GetSystemTypeInfo;
    property GenericListFollowerController: TBoldGenericListControllerCom read fGenericListFollowerController;
    property IconFollowerController: TBoldIntegerFollowerControllerCom read fIconFollowerController;
    property TextFollowerController: TBoldStringFollowerControllerCom read fTextFollowerController;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs,
  BoldDefs,
  BoldGuiResourceStringsCom,
  BoldListControlPackCom,
  BoldSubscription;

{-- TBoldTreeFollowerControllerCom --}

constructor TBoldTreeFollowerControllerCom.Create(aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent, DoGetFollowerController);
  FNodeDescriptions := GetBoldNodeDescriptionsClass.Create(Self);
end;

function TBoldTreeFollowerControllerCom.GetBoldNodeDescriptionsClass: TBoldNodeDescriptionsClassCom;
begin
  result := TBoldNodeDescriptionsCom;
end;

destructor TBoldTreeFollowerControllerCom.Destroy;
begin
  FreePublisher;
  FreeAndNil(FNodeDescriptions);
  inherited Destroy;
end;

procedure TBoldTreeFollowerControllerCom.SetNodeDescriptions(Value: TBoldNodeDescriptionsCom);
begin
  FNodeDescriptions.Assign(Value);
end;

function TBoldTreeFollowerControllerCom.GetDefaultNodeDescriptionName: string;
begin
  if Assigned(FDefaultNodeDescription) then
    Result := FDefaultNodeDescription.Name
  else
    Result := '';
end;

procedure TBoldTreeFollowerControllerCom.SetDefaultNodeDescriptionName(const Value: string);
var
  NodeDescription: TBoldNodeDescriptionCom;
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

function TBoldTreeFollowerControllerCom.DefaultGetNodeFollowerControllerByName(const Name: string): TBoldFollowerControllerCom;
var
  NodeDescription: TBoldNodeDescriptionCom;
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

procedure TBoldTreeFollowerControllerCom.DoIconChanged(Follower: TBoldFollowerCom);
begin
  if Assigned(FOnIconChanged) then
    FOnIconChanged(Follower);
end;

procedure TBoldTreeFollowerControllerCom.DoTextChanged(Follower: TBoldFollowerCom);
begin
  if Assigned(FOnTextChanged) then
    FOnTextChanged(Follower);
end;

{$IFDEF BOLD_BCB}
procedure TBoldTreeFollowerControllerCom.DoGetFollowerController(Name: string; var FollowerController: TBoldFollowerControllerCom);
{$ENDIF}
{$IFDEF BOLD_DELPHI}
function TBoldTreeFollowerControllerCom.DoGetFollowerController(Name: string): TBoldFollowerControllerCom;
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

{-- TBoldNodeDescriptionsCom --}

constructor TBoldNodeDescriptionsCom.Create(aTreeFollowerController: TBoldTreeFollowerControllerCom);
begin
  inherited Create(GetNodeDescriptionClass);
  FTreeFollowerController := aTreeFollowerController;
end;

function TBoldNodeDescriptionsCom.GetNodeDescriptionClass: TBoldNodeDescriptionClassCom;
begin
  Result := TBoldNodeDescriptionCom;
end;

function TBoldNodeDescriptionsCom.GetNodeControllerItem(Index: Integer): TBoldNodeDescriptionCom;
begin
  Result := TBoldNodeDescriptionCom(GetItem(Index));
end;

procedure TBoldNodeDescriptionsCom.SetNodeControllerItem(Index: Integer; Value: TBoldNodeDescriptionCom);
begin
  SetItem(Index, Value);
end;

function TBoldNodeDescriptionsCom.GetOwner: TPersistent;
begin
  Result := FTreeFollowerController;
end;

procedure TBoldNodeDescriptionsCom.Update(Item: TCollectionItem);
begin
  FTreeFollowerController.Changed;
end;

function TBoldNodeDescriptionsCom.FindByName(const Name: string): TBoldNodeDescriptionCom;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TBoldNodeDescriptionCom(GetItem(I));
    if CompareText(Result.Name, Name) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TBoldNodeDescriptionsCom.Add: TBoldNodeDescriptionCom;
begin
  BeginUpdate;
  try
    Result := GetNodeDescriptionClass.Create(Self);
  finally
    EndUpdate;
  end;
end;

{-- TBoldNodeDescriptionCom-- }

constructor TBoldNodeDescriptionCom.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNodeFollowerController := GetBoldNodeFollowerControllerClass.Create(TBoldNodeDescriptionsCom(Collection).FTreeFollowerController);
  FNodeFollowerController.OnGetContextType := GetContextType;
end;

function TBoldNodeDescriptionCom.GetBoldNodeFollowerControllerClass:TBoldNodeFollowerControllerClassCom;
begin
  result := TBoldNodeFollowerControllerCom;
end;

destructor TBoldNodeDescriptionCom.Destroy;
begin
  FreeAndNil(FNodeFollowerController);
  inherited Destroy;
end;

procedure TBoldNodeDescriptionCom.Assign(Source: TPersistent);
begin
  if Source is TBoldNodeDescriptionCom then
  begin
    Name := TBoldNodeDescriptionCom(Source).Name;
    HideNodeWithNoChildren := TBoldNodeDescriptionCom(Source).HideNodeWithNoChildren;
    ContextTypeName := TBoldNodeDescriptionCom(Source).ContextTypeName;
    ListController.Assign(TBoldNodeDescriptionCom(Source).ListController);
    IconController.Assign(TBoldNodeDescriptionCom(Source).IconController);
    TextController.Assign(TBoldNodeDescriptionCom(Source).TextController);
  end
  else
    inherited Assign(Source);
end;

function TBoldNodeDescriptionCom.GetDisplayName: string;
begin
  Result := FName;
  if (Result = '') then
    Result := inherited GetDisplayName;
end;

procedure TBoldNodeDescriptionCom.SetName(Value: string);
begin
  if FName <> Value then
  begin
    if (Value <> '') and
       (CompareText(FName, Value) <> 0) and
       (TBoldNodeDescriptionsCom(Collection).FindByName(Value) <> nil) then
      raise EBold.CreateFmt(sBoldInvalidName, [Value]);
    FName := Value;
    Changed(False);
  end;
end;

function TBoldNodeDescriptionCom.GetHideNodeWithNoChildren: Boolean;
begin
  Result := FNodeFollowerController.HideNodeWithNoChildren;
end;

procedure TBoldNodeDescriptionCom.SetHideNodeWithNoChildren(Value: Boolean);
begin
  FNodeFollowerController.HideNodeWithNoChildren := Value;
end;

function TBoldNodeDescriptionCom.GetListController: TBoldGenericListControllerCom;
begin
  Result := FNodeFollowerController.Items[BoldNodeListIndex] as TBoldGenericListControllerCom;
end;

function TBoldNodeDescriptionCom.GetIconController: TBoldIntegerFollowerControllerCom;
begin
  Result := FNodeFollowerController.Items[BoldNodeIconIndex] as TBoldIntegerFollowerControllerCom;
end;

function TBoldNodeDescriptionCom.GetTextController: TBoldStringFollowerControllerCom;
begin
  Result := FNodeFollowerController.Items[BoldNodeTextIndex] as TBoldStringFollowerControllerCom;
end;

procedure TBoldNodeDescriptionCom.SetListController(Value: TBoldGenericListControllerCom);
begin
  GetListController.Assign(Value);
end;

procedure TBoldNodeDescriptionCom.SetIconController(Value: TBoldIntegerFollowerControllerCom);
begin
  GetIconController.Assign(Value);
end;

procedure TBoldNodeDescriptionCom.SetTextController(Value: TBoldStringFollowerControllerCom);
begin
  GetTextController.Assign(Value);
end;

{-- TBoldNodeFollowerControllerCom --}

constructor TBoldNodeFollowerControllerCom.Create(aTreeFollowerController: TBoldTreeFollowerControllerCom);
var
  OwningComponent: TComponent;
begin
  OwningComponent := aTreeFollowerController.OwningComponent;
  inherited Create(OwningComponent);
  FTreeFollowerController := aTreeFollowerController;

  fGenericListFollowerController := TBoldGenericListControllerCom.Create(OwningComponent, FTreeFollowerController.DoGetFollowerController);
  fGenericListFollowerController.OnBeforeInsertItem := FTreeFollowerController.DoBeforeInsertItem;
  fGenericListFollowerController.OnAfterInsertItem := FTreeFollowerController.DoAfterInsertItem;
  fGenericListFollowerController.OnBeforeDeleteItem := FTreeFollowerController.DoBeforeDeleteItem;
  fGenericListFollowerController.OnAfterDeleteItem := FTreeFollowerController.DoAfterDeleteItem;
  fGenericListFollowerController.BeforeMakeUptoDate := FTreeFollowerController.BeforeMakeUptoDate;
  fGenericListFollowerController.OnGetContextType := GetContextType;
  Add(fGenericListFollowerController);

  fIconFollowerController := TBoldIntegerFollowerControllerCom.Create(OwningComponent);
  fIconFollowerController.AfterMakeUptoDate := FTreeFollowerController.DoIconChanged;
  fIconFollowerController.Expression := '-1';
  fIconFollowerController.OnGetContextType := GetContextType;
  Add(IconFollowerController);

  fTextFollowerController := TBoldStringFollowerControllerCom.Create(OwningComponent);
  fTextFollowerController.AfterMakeUptoDate := FTreeFollowerController.DoTextChanged;
  fTextFollowerController.OnGetContextType := GetContextType;
  Add(TextFollowerController);
end;

procedure TBoldNodeFollowerControllerCom.SetHideNodeWithNoChildren(Value: Boolean);
begin
  if FHideNodeWithNoChildren <> Value then
  begin
    FHideNodeWithNoChildren := Value;
    Changed;
  end;
end;

destructor TBoldNodeFollowerControllerCom.Destroy;
begin
  FreePublisher;
  inherited Destroy;
end;

procedure TBoldTreeFollowerControllerCom.DoAssign(Source: TPersistent);
begin
  Assert(Source is TBoldTreeFollowerControllerCom);
  inherited DoAssign(Source);
  NodeDescriptions.Assign(TBoldTreeFollowerControllerCom(Source).NodeDescriptions);
  DefaultNodeDescriptionName := TBoldTreeFollowerControllerCom(Source).DefaultNodeDescriptionName;
  OnGetFollowerController := TBoldTreeFollowerControllerCom(Source).OnGetFollowerController;
end;

procedure TBoldNodeFollowerControllerCom.SetContextTypeName(
  const Value: String);
begin
  if FContextTypeName <> Value then
  begin
    FContextTypeName := Value;
    Changed;
  end;
end;

function TBoldNodeDescriptionCom.GetContextTypeName: String;
begin
  Result := FNodeFollowerController.ContextTypeName;
end;

procedure TBoldNodeDescriptionCom.SetContextTypeName(const Value: String);
begin
  FNodeFollowerController.ContextTypeName := Value;
end;

function TBoldNodeFollowerControllerCom.GetSystemTypeInfo: IBoldSystemTypeInfo;
begin
  if FTreeFollowerController.GetContextType <> nil then
    result := FTreeFollowerController.GetContextType.SystemTypeInfo as IBoldSystemTypeInfo
  else
    result := nil;
end;

function TBoldNodeDescriptionCom.GetContextType: IBoldElementTypeInfo;
var
  ContextType: IBoldElementTypeInfo;
begin
  ContextType := NodeFollowerController.fTreeFollowerController.GetContextType;
  if assigned(ContextType) then
  begin
    if ContextTypeName <> '' then
      result := (ContextType.SystemTypeInfo as IBoldSystemTypeInfo).ElementTypeInfoByExpressionName[ContextTypeName]
    else
      result := (ContextType.SystemTypeInfo as IBoldSystemTypeInfo).ElementTypeInfoByExpressionName[Name];
  end
  else
    result := nil;
end;

end.
