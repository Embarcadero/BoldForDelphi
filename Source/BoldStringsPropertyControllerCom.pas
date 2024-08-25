
{ Global compiler directives }
{$include bold.inc}
unit BoldStringsPropertyControllerCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  BoldEnvironmentVCL,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldAbstractListHandleCom,
  BoldControlPackCom,
  BoldStringControlPackCom,
  BoldListListControlPackCom,
  BoldListHandleFollowerCom;

type
  { forward declarations }
  TBoldStringsPropertyControllerCom = class;

  { types }
  TBoldControlFollowerEventCom = procedure (Sender: TObject; Follower: TBoldFollowerCom) of object;
  TBoldControlSubFollowerEventCom = procedure (Sender: TObject; Index: Integer; OwningFollower: TBoldFollowerCom) of object;

  { TBoldStringsPropertyControllerCom }
  TBoldStringsPropertyControllerCom = class(TComponent)
  private
    fHandleFollower: TBoldListHandleFollowerCom;
    fBoldProperties: TBoldListAsFollowerListControllerCom;
    fBoldRowProperties: TBoldStringFollowerControllerCom;
    fPropertyName: String;
    fVCLComponent: TComponent;
    fOnListAfterMakeUptoDate: TBoldControlFollowerEventCom;
    fOnListBeforeDeleteItem: TBoldControlFollowerEventCom;
    fOnItemBeforeMakeUpToDate: TBoldControlFollowerEventCom;
    fOnListBeforeMakeUptoDate: TBoldControlFollowerEventCom;
    fOnListAfterInsertItem: TBoldControlFollowerEventCom;
    fOnListBeforeInsertItem: TBoldControlSubFollowerEventCom;
    fOnListAfterDeleteItem: TBoldControlSubFollowerEventCom;
    fOnItemAfterMakeUpToDate: TBoldControlFollowerEventCom;
    function GetBoldHandle: TBoldAbstractListHandleCom;
    procedure SetBoldHandle(value: TBoldAbstractListHandleCom);
    procedure SetBoldProperties(Value: TBoldListAsFollowerListControllerCom);
    procedure SetRowProperties(Value: TBoldStringFollowerControllerCom);
    function GetContextType: IBoldElementTypeInfo;
    procedure _ListBeforeMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _ListAfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _ListBeforeInsertItem(index: Integer; OwningFollower: TBoldFollowerCom);
    procedure _ListAfterInsertItem(Follower: TBoldFollowerCom);
    procedure _ListAfterDeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
    procedure _ListBeforeDeleteItem(Follower: TBoldFollowerCom);
    procedure _ItemAfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _ItemBeforeMakeUptoDate(Follower: TBoldFollowerCom);
    function GetStringsProperty: TStrings;
    procedure SetVCLComponent(const Value: TComponent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property StringsProperty: TStrings read GetStringsProperty;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HandleFollower: TBoldListHandleFollowerCom read FHandleFollower;
  published
    property BoldHandle: TBoldAbstractListHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldListAsFollowerListControllerCom read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldStringFollowerControllerCom read fBoldRowProperties write SetRowProperties;
    property VCLComponent: TComponent read fVCLComponent write SetVCLComponent;
    property PropertyName: string read fPropertyName write fPropertyName;
    property OnListBeforeMakeUptoDate: TBoldControlFollowerEventCom read fOnListBeforeMakeUptoDate write fOnListBeforeMakeUptoDate;
    property OnListAfterMakeUptoDate:  TBoldControlFollowerEventCom read fOnListAfterMakeUptoDate write fOnListAfterMakeUptoDate;
    property OnListAfterInsertItem: TBoldControlFollowerEventCom read fOnListAfterInsertItem write fOnListAfterInsertItem;
    property OnListAfterDeleteItem:  TBoldControlSubFollowerEventCom read fOnListAfterDeleteItem write fOnListAfterDeleteItem;
    property OnListBeforeInsertItem: TBoldControlSubFollowerEventCom read fOnListBeforeInsertItem write fOnListBeforeInsertItem;
    property OnListBeforeDeleteItem:  TBoldControlFollowerEventCom read fOnListBeforeDeleteItem write fOnListBeforeDeleteItem;
    property OnItemBeforeMakeUpToDate: TBoldControlFollowerEventCom read fOnItemBeforeMakeUpToDate write fOnItemBeforeMakeUpToDate;
    property OnItemAfterMakeUpToDate: TBoldControlFollowerEventCom read fOnItemAfterMakeUpToDate write fOnItemAfterMakeUpToDate;
  end;

implementation

uses
  SysUtils,
  BoldListControlPackCom,
  BoldDefs,
  TypInfo;

  {-- TBoldStringsPropertyControllerCom ----------------------------------------------------------}

constructor TBoldStringsPropertyControllerCom.Create(AOwner: TComponent);
begin
  inherited;
  fBoldRowProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _ItemAfterMakeUptoDate;
  fBoldRowProperties.BeforeMakeUptoDate := _ItemBeforeMakeUptoDate;  
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldListAsFollowerListControllerCom.Create(Self, fBoldRowProperties);
  with fBoldProperties do
  begin
    OnAfterInsertItem := _ListAfterInsertItem;
    OnAfterDeleteItem := _ListAfterDeleteItem;
    OnBeforeInsertItem := _ListBeforeInsertItem;
    OnBeforeDeleteItem := _ListBeforeDeleteItem;
    BeforeMakeUptoDate := _ListBeforeMakeUptoDate;
    AfterMakeUptoDate := _ListAfterMakeUptoDate;
  end;
  fHandleFollower := TBoldListHandleFollowerCom.Create(Owner, fBoldProperties);
end;

destructor TBoldStringsPropertyControllerCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited;
end;

procedure TBoldStringsPropertyControllerCom.SetBoldProperties(value: TBoldListAsFollowerListControllerCom);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldStringsPropertyControllerCom.GetBoldHandle: TBoldAbstractListHandleCom;
begin
  Result := FHandleFollower.BoldHandle;
end;

procedure TBoldStringsPropertyControllerCom.SetBoldHandle(Value: TBoldAbstractListHandleCom);
begin
  FHandleFollower.BoldHandle := Value;
end;

procedure TBoldStringsPropertyControllerCom.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if Assigned(VCLComponent) and (not (csDestroying in ComponentState)) then
    if (AComponent = VCLComponent) and (Operation = opRemove) then
      VCLComponent := nil;
end;             

procedure TBoldStringsPropertyControllerCom._ListAfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.EndUpdate;
  if Assigned(fOnListAfterMakeUptoDate) then
    fOnListAfterMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom._ListBeforeMakeUptoDate(Follower: TBoldFollowerCom);
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
       raise EBold.CreateFmt('Strings property controlled by "%s" changed by other means', [Name]);
  end;
  if Assigned(fOnListBeforeMakeUptoDate) then
    fOnListBeforeMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom._ListAfterDeleteItem(index: Integer;
  OwningFollower: TBoldFollowerCom);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.Delete(Index);
  if Assigned(fOnListAfterDeleteItem) then
    fOnListAfterDeleteItem(self, index, OwningFollower);
end;

procedure TBoldStringsPropertyControllerCom._ListAfterInsertItem(Follower: TBoldFollowerCom);
var
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
    Strings.InsertObject(Follower.Index, '', Follower);
  if Assigned(fOnListAfterInsertItem) then
    fOnListAfterInsertItem(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom._ItemAfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  index: Integer;
  Strings: TStrings;
begin
  Strings := StringsProperty;
  if Assigned(Strings) then
  begin
    index := Follower.index;
    if (index > -1) and (index < Strings.Count) then
      Strings[index] := TBoldStringFollowerControllerCom(Follower.Controller).GetCurrentAsString(Follower);
      Strings.Objects[index] := Follower;
  end;
  if Assigned(fOnItemAfterMakeUptoDate) then
    fOnItemAfterMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom.SetRowProperties(Value: TBoldStringFollowerControllerCom);
begin
  fBoldRowProperties.Assign(Value);
end;

function TBoldStringsPropertyControllerCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldStringsPropertyControllerCom.GetStringsProperty: TStrings;
begin
  Result := nil;
  if Assigned(VCLComponent) and (PropertyName <> '') then
  begin
    result := TStrings(GetOrdProp(VCLComponent, PropertyName));
  end;
end;

procedure TBoldStringsPropertyControllerCom._ListBeforeDeleteItem(Follower: TBoldFollowerCom);
begin
  if Assigned(fOnListBeforeDeleteItem) then
    fOnListBeforeDeleteItem(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom._ListBeforeInsertItem(index: Integer;
  OwningFollower: TBoldFollowerCom);
begin
  if Assigned(fOnListBeforeInsertItem) then
    fOnListBeforeInsertItem(self, index, OwningFollower);
end;

procedure TBoldStringsPropertyControllerCom._ItemBeforeMakeUptoDate(Follower: TBoldFollowerCom);
begin
  if Assigned(fOnItemBeforeMakeUptoDate) then
    fOnItemBeforeMakeUptoDate(self, Follower);
end;

procedure TBoldStringsPropertyControllerCom.SetVCLComponent(const Value: TComponent);
begin
  fVCLComponent := Value;
  if Assigned(fVCLComponent) then
    FreeNotification(fVCLComponent);

  if not Assigned(fVCLComponent) then
    PropertyName := '';
end;

end.
