{ Global compiler directives }
{$include bold.inc}
unit BoldSelectionListBox;

interface

uses
  BoldCustomCheckListBox,
  BoldCheckBoxStateControlPack,
  BoldControlPack,
  BoldListHandle,
  BoldElements,
  BoldDefs,
  BoldSubscription,
  StdCtrls,
  Classes;

const
  beSelectionHandleChanged = 400;

type
  {TBoldSelectionListBox}
  TBoldSelectionListBox = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldSelectionListBox = class(TBoldCustomCheckListBox)
  private
    fCheckBoxRenderer: TBoldAsCheckBoxStateRenderer;
    fPublisher: TBoldPublisher;
    fSelectionHandle: TBoldListHandle;
    function GetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure SetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure OnSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    procedure SetSelectionHandle(const Value: TBoldListHandle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BoldListProperties;
    property BoldListHandle;
    property BoldRowStringProperties;
    property Alignment;
    property BoldHandleIndexLock;
    property SelectionHandle: TBoldListHandle read fSelectionHandle write SetSelectionHandle;
  end;

implementation

uses
  SysUtils;

{ TBoldSelectionListBox }

constructor TBoldSelectionListBox.Create(AOwner: TComponent);
begin
  inherited;
  fPublisher := TBoldPublisher.Create(fPublisher);
  fCheckBoxRenderer := TBoldAsCheckBoxStateRenderer.Create(self);
  fCheckBoxRenderer.OnGetAsCheckBoxState := GetAsCheckBoxState;
  fCheckBoxRenderer.OnSetAsCheckBoxState := SetAsCheckBoxState;
  fCheckBoxRenderer.OnSubscribe := OnSubscribe;
  BoldRowCheckBoxProperties.Renderer := fCheckBoxRenderer;
end;

destructor TBoldSelectionListBox.Destroy;
begin
  fPublisher.NotifySubscribersAndClearSubscriptions(self);
  FreeAndNil(fPublisher);
  inherited;
end;

function TBoldSelectionListBox.GetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
begin
  if Assigned(SelectionHandle) then
  begin
    if (SelectionHandle.List.IndexOf(aFollower.Element) <> -1 ) then
      Result := cbChecked
    else
      Result := cbUnChecked;
   end
   else
     Result := cbGrayed;
end;

procedure TBoldSelectionListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = SelectionHandle) and (Operation = opRemove) then
    SelectionHandle := nil;
end;

procedure TBoldSelectionListBox.OnSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  SelectionHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged, beDestroying], breReSubscribe);
  SelectionHandle.List.DefaultSubscribe(Subscriber);
  fPublisher.AddSubscription(Subscriber, beSelectionHandleChanged, breReSubscribe);
end;

procedure TBoldSelectionListBox.SetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  if Assigned(SelectionHandle) then
  begin
    case newValue of
      cbChecked: SelectionHandle.MutableList.Add(aFollower.Element);
      cbUnChecked: if (SelectionHandle.List.IndexOf(aFollower.Element) <> -1) then SelectionHandle.MutableList.Remove(aFollower.Element);
      cbGrayed: ;
    end;
  end;
end;

procedure TBoldSelectionListBox.SetSelectionHandle(
  const Value: TBoldListHandle);
begin
  if (fSelectionHandle <> Value) then
  begin
    fSelectionHandle := Value;
    fPublisher.SendExtendedEvent(self, beSelectionHandleChanged, []);
  end;
end;

end.