unit BoldSelectionListBox;

interface

uses
  BoldCustomCheckListBox,
  BoldCheckBoxStateControlPack,
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

  TBoldSelectionListBox = class(TBoldCustomCheckListBox)
  private
    fCheckBoxRenderer: TBoldAsCheckBoxStateRenderer;
    fPublisher: TBoldPublisher;
    fSelectionHandle: TBoldListHandle;
    function GetAsCheckBoxState(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TCheckBoxState;
    procedure SetAsCheckBoxState(Element: TBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression);
    procedure OnSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber);
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
  fPublisher := TBoldPublisher.Create;
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

function TBoldSelectionListBox.GetAsCheckBoxState(Element: TBoldElement;
  Representation: TBoldRepresentation;
  Expression: TBoldExpression): TCheckBoxState;
begin
  if Assigned(SelectionHandle) then
  begin
    if (SelectionHandle.List.IndexOf(Element) <> -1 ) then
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

procedure TBoldSelectionListBox.OnSubscribe(Element: TBoldElement;
  Representation: TBoldRepresentation; Expression: TBoldExpression;
  Subscriber: TBoldSubscriber);
begin
  SelectionHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged, beDestroying], breReSubscribe);
  SelectionHandle.List.DefaultSubscribe(Subscriber);
  fPublisher.AddSubscription(Subscriber, beSelectionHandleChanged, breReSubscribe);
end;

procedure TBoldSelectionListBox.SetAsCheckBoxState(Element: TBoldElement;
  newValue: TCheckBoxState; Representation: TBoldRepresentation;
  Expression: TBoldExpression);
begin
  if Assigned(SelectionHandle) then
  begin
    case newValue of
      cbChecked: SelectionHandle.MutableList.Add(Element);
      cbUnChecked: if (SelectionHandle.List.IndexOf(Element) <> -1) then SelectionHandle.MutableList.Remove(Element);
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
