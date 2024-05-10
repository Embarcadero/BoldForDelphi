
{ Global compiler directives }
{$include bold.inc}
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
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
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
    fOnReplaceItem: TBoldControlSubFollowerEvent;
    fStringsChanged: boolean;
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    procedure SetBoldProperties(Value: TBoldListAsFollowerListController);
    procedure SetRowProperties(Value: TBoldStringFollowerController);
    function GetContextType: TBoldElementTypeInfo;
    procedure _ListBeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _ListAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _ListBeforeInsertItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _ListAfterInsertItem(index: Integer; Follower: TBoldFollower);
    procedure _ListAfterDeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _ListBeforeDeleteItem(index: Integer; Follower: TBoldFollower);
    procedure _ItemAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _ItemBeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _ListReplaceItem(index: Integer; AFollower: TBoldFollower);
    function GetStringsProperty: TStrings;
    procedure SetVCLComponent(const Value: TComponent);
    procedure MarkStringsChanged;
    procedure EndUpdate;
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

procedure TBoldStringsPropertyController.MarkStringsChanged;
var
  Strings: TStrings;
begin
  if not fStringsChanged then
  begin
    fStringsChanged := true;
    Strings := StringsProperty;
    if Assigned(Strings) then
      Strings.BeginUpdate;
  end;
end;

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
    OnReplaceitem := _ListReplaceItem;
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
    EndUpdate;
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
   if Follower.SubFollowerCount <> Strings.Count then
     if Follower.SubFollowerCount = 0 then
     begin
       MarkStringsChanged;
       Strings.Clear
     end
     else
       raise EBold.CreateFmt('Strings property controlled by "%s" changed by other means', [Name]);
  end;
  if Assigned(fOnListBeforeMakeUptoDate) then
    fOnListBeforeMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyController._ListReplaceItem(index: Integer;
  AFollower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    AFollower.EnsureDisplayable;
    MarkStringsChanged;
    Strings.Objects[Index] := AFollower;
    Strings[Index] := TBoldStringFollowerController(AFollower.Controller).GetCurrentAsString(AFollower);
  end;
  if Assigned(fOnReplaceItem) then
    fOnReplaceItem(self, index, AFollower);
end;

procedure TBoldStringsPropertyController._ListAfterDeleteItem(index: Integer;
  OwningFollower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    MarkStringsChanged;
    Strings.Delete(Index);
  end;
  if Assigned(fOnListAfterDeleteItem) then
    fOnListAfterDeleteItem(self, index, OwningFollower);
end;

procedure TBoldStringsPropertyController._ListAfterInsertItem(index: Integer; Follower: TBoldFollower);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    Follower.EnsureDisplayable;
    MarkStringsChanged;
    Strings.InsertObject(Index, TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower), Follower);
  end;
  if Assigned(fOnListAfterInsertItem) then
    fOnListAfterInsertItem(self, Follower);
end;

procedure TBoldStringsPropertyController._ItemAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
  Strings: TStrings;
  s: string;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    index := Follower.index;
    if (index > -1) and (index < Strings.Count) then
    begin
      s := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
      if s <> Strings[index] then
      begin
        MarkStringsChanged;
        Strings[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
      end;
      if Strings.Objects[index] <> Follower then
      begin
        MarkStringsChanged;
        Strings.Objects[index] := Follower;
      end;
    end;
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

procedure TBoldStringsPropertyController._ListBeforeDeleteItem(index: Integer; Follower: TBoldFollower);
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

procedure TBoldStringsPropertyController.EndUpdate;
var
  Strings: TStrings;
begin
  if fStringsChanged  then
  begin
    Strings := StringsProperty;
    if Assigned(Strings) then
      Strings.EndUpdate;
    fStringsChanged := false;
  end;
end;

end.
