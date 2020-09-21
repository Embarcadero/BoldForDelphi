unit BoldStringsPropertyController;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldEnvironmentVCL, // Make sure VCL environement loaded, and finalized after
  BoldElements,
  BoldAbstractListHandle,
  BoldControlPack,
  BoldStringControlPack,
  BoldListListControlPack,
  BoldListHandleFollower;

type
  { forward declarations }
  TBoldStringsPropertyController = class;

  { types }
  TBoldControlFollowerEvent = procedure (Sender: TObject; Follower: TBoldFollower) of object;
  TBoldControlSubFollowerEvent = procedure (Sender: TObject; Index: Integer; OwningFollower: TBoldFollower) of object;

  { TBoldStringsPropertyController }
  TBoldStringsPropertyController = class(TComponent)
  private
    fHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldListAsFollowerListController;
    fBoldRowProperties: TBoldStringFollowerController;
    fPropertyName: String;
    fVCLComponent: TComponent;
    fOnListAfterMakeUptoDate: TBoldControlFollowerEvent;
    fOnListBeforeDeleteItem: TBoldControlFollowerEvent;
    fOnItemBeforeMakeUpToDate: TBoldControlFollowerEvent;
    fOnListBeforeMakeUptoDate: TBoldControlFollowerEvent;
    fOnListAfterInsertItem: TBoldControlFollowerEvent;
    fOnListBeforeInsertItem: TBoldControlSubFollowerEvent;
    fOnListAfterDeleteItem: TBoldControlSubFollowerEvent;
    fOnItemAfterMakeUpToDate: TBoldControlFollowerEvent;
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    procedure SetBoldProperties(Value: TBoldListAsFollowerListController);
    procedure SetRowProperties(Value: TBoldStringFollowerController);
    function GetContextType: TBoldElementTypeInfo;
    procedure _ListBeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _ListAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _ListBeforeInsertItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _ListAfterInsertItem(Follower: TBoldFollower);
    procedure _ListAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _ListBeforeDeleteItem(Follower: TBoldFollower);
    procedure _ItemAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _ItemBeforeMakeUptoDate(Follower: TBoldFollower);
    function GetStringsProperty: TStrings;
    procedure SetVCLComponent(const Value: TComponent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property StringsProperty: TStrings read GetStringsProperty;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HandleFollower: TBoldListHandleFollower read FHandleFollower;
  published
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldListAsFollowerListController read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;
    property VCLComponent: TComponent read fVCLComponent write SetVCLComponent;
    property PropertyName: string read fPropertyName write fPropertyName;
    property OnListBeforeMakeUptoDate: TBoldControlFollowerEvent read fOnListBeforeMakeUptoDate write fOnListBeforeMakeUptoDate;
    property OnListAfterMakeUptoDate:  TBoldControlFollowerEvent read fOnListAfterMakeUptoDate write fOnListAfterMakeUptoDate;
    property OnListAfterInsertItem: TBoldControlFollowerEvent read fOnListAfterInsertItem write fOnListAfterInsertItem;
    property OnListAfterDeleteItem:  TBoldControlSubFollowerEvent read fOnListAfterDeleteItem write fOnListAfterDeleteItem;
    property OnListBeforeInsertItem: TBoldControlSubFollowerEvent read fOnListBeforeInsertItem write fOnListBeforeInsertItem;
    property OnListBeforeDeleteItem:  TBoldControlFollowerEvent read fOnListBeforeDeleteItem write fOnListBeforeDeleteItem;
    property OnItemBeforeMakeUpToDate: TBoldControlFollowerEvent read fOnItemBeforeMakeUpToDate write fOnItemBeforeMakeUpToDate;
    property OnItemAfterMakeUpToDate: TBoldControlFollowerEvent read fOnItemAfterMakeUpToDate write fOnItemAfterMakeUpToDate;
  end;

implementation

uses
  SysUtils,
  BoldListControlPack,
  BoldDefs,
  BoldGuiResourceStrings,
  TypInfo;

  {-- TBoldStringsPropertyController ----------------------------------------------------------}

constructor TBoldStringsPropertyController.Create(AOwner: TComponent);
begin
  inherited;
  fBoldRowProperties := TBoldStringFollowerController.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _ItemAfterMakeUptoDate;
  fBoldRowProperties.BeforeMakeUptoDate := _ItemBeforeMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldListAsFollowerListController.Create(Self, fBoldRowProperties);
  with fBoldProperties do
  begin
    OnAfterInsertItem := _ListAfterInsertItem;
    OnAfterDeleteItem := _ListAfterDeleteItem;
    OnBeforeInsertItem := _ListBeforeInsertItem;
    OnBeforeDeleteItem := _ListBeforeDeleteItem;
    BeforeMakeUptoDate := _ListBeforeMakeUptoDate;
    AfterMakeUptoDate := _ListAfterMakeUptoDate;
  end;
  fHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldProperties);
end;

destructor TBoldStringsPropertyController.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited;
end;

procedure TBoldStringsPropertyController.SetBoldProperties(value: TBoldListAsFollowerListController);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldStringsPropertyController.GetBoldHandle: TBoldAbstractListHandle;
begin
  Result := FHandleFollower.BoldHandle;
end;

procedure TBoldStringsPropertyController.SetBoldHandle(Value: TBoldAbstractListHandle);
begin
  FHandleFollower.BoldHandle := Value;
end;

procedure TBoldStringsPropertyController.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if Assigned(VCLComponent) and (not (csDestroying in ComponentState)) then
    if (AComponent = VCLComponent) and (Operation = opRemove) then
      VCLComponent := nil;
end;

procedure TBoldStringsPropertyController._ListAfterMakeUptoDate(Follower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.EndUpdate;
  if Assigned(fOnListAfterMakeUptoDate) then
    fOnListAfterMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyController._ListBeforeMakeUptoDate(Follower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
   Strings.BeginUpdate;
   if Follower.SubFollowerCount <> Strings.Count then
     if Follower.SubFollowerCount = 0 then
       Strings.Clear
     else
       raise EBold.CreateFmt(sStringsControlledByOtherMeans, [Name]);
  end;
  if Assigned(fOnListBeforeMakeUptoDate) then
    fOnListBeforeMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyController._ListAfterDeleteItem(index: Integer;
  OwningFollower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.Delete(Index);
  if Assigned(fOnListAfterDeleteItem) then
    fOnListAfterDeleteItem(self, index, OwningFollower);
end;

procedure TBoldStringsPropertyController._ListAfterInsertItem(Follower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.InsertObject(Follower.Index, '', Follower);
  if Assigned(fOnListAfterInsertItem) then
    fOnListAfterInsertItem(self, Follower);
end;

procedure TBoldStringsPropertyController._ItemAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    index := Follower.index;
    if (index > -1) and (index < Strings.Count) then
      Strings[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
      Strings.Objects[index] := Follower;
  end;
  if Assigned(fOnItemAfterMakeUptoDate) then
    fOnItemAfterMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyController.SetRowProperties(Value: TBoldStringFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

function TBoldStringsPropertyController.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldStringsPropertyController.GetStringsProperty: TStrings;
begin
  // fixme code for properties not at top level
  Result := nil;
  if Assigned(VCLComponent) and (PropertyName <> '') then
  begin
    result := TStrings(GetOrdProp(VCLComponent, PropertyName));
  end;
end;

procedure TBoldStringsPropertyController._ListBeforeDeleteItem(Follower: TBoldFollower);
begin
  if Assigned(fOnListBeforeDeleteItem) then
    fOnListBeforeDeleteItem(self, Follower);
end;

procedure TBoldStringsPropertyController._ListBeforeInsertItem(index: Integer;
  OwningFollower: TBoldFollower);
begin
  if Assigned(fOnListBeforeInsertItem) then
    fOnListBeforeInsertItem(self, index, OwningFollower);
end;

procedure TBoldStringsPropertyController._ItemBeforeMakeUptoDate(Follower: TBoldFollower);
begin
  if Assigned(fOnItemBeforeMakeUptoDate) then
    fOnItemBeforeMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyController.SetVCLComponent(const Value: TComponent);
begin
  //FIXME: Add code to validate PropertyName in new component
  fVCLComponent := Value;
  if Assigned(fVCLComponent) then
    FreeNotification(fVCLComponent);

  if not Assigned(fVCLComponent) then
    PropertyName := '';
end;

end.
