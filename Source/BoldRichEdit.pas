{ Global compiler directives }
{$include bold.inc}
unit BoldRichEdit;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Menus,
  Boldhandles,
  BoldControlPack,
  BoldStringControlPack,
  BoldElementHandleFollower;

type
  TBoldCustomRichEdit = class;
  TBoldRichEdit = class;

  {---TBoldCustomRichEdit---}
  TBoldCustomRichEdit = class(TCustomRichEdit)
  private
    {Bold stuff}
    fAlignment: TAlignment;
    fBoldProperties: TBoldStringFollowerController;
    fCanvas: TControlCanvas;
    fFocused: Boolean;
    fHandleFollower: TBoldElementHandleFollower;
    fMyColor: TColor;
    fMyFont: TFont;
    fMyReadOnly: Boolean;
    fStringStream: TStringStream;
    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetBoldHandle: TBoldElementHandle;
    function GetColor: TColor;
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetEffectiveReadOnly: Boolean;
    function GetFollower: TBoldFollower;
    function GetFont: TFont;
    function GetFormattedText: string;
    function GetText: string;
    procedure SetBoldDisplay(Value: TBoldStringFollowerController);
    procedure SetBoldHandle(value: TBoldElementHandle);
    procedure SetColor(value: TColor);
    procedure SetEffectiveColor(v: TColor);
    procedure SetFocused(Value: Boolean);
    procedure SetFont(value: TFont);
(*   procedure SetFormattedText(const Value: string);*)
    procedure SetReadOnly(value: Boolean);
    procedure SetText(value: string);
  protected
    {Bold Stuff}
    procedure Change; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure KeyPress(var Key: Char); override;
    property Alignment: TAlignment read fAlignment write fAlignment;
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldDisplay;
    property Color: TColor read GetColor write SetColor;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read GetEffectiveReadOnly;
    property Follower: TBoldFollower read GetFollower;
    property Font: TFont read GetFont write SetFont;
    property FormattedText: string read GetFormattedText;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  {---TBoldRichEdit---}
  TBoldRichEdit = class(TBoldCustomRichEdit)
  published
    {$IFNDEF T2H}
    {Properties from TBoldCustomRichEdit}
    property BoldHandle;
    property BoldProperties;
    property ReadOnly;
    property Text;
    property Font;
    property Color;
    property Alignment;
    {Properties from TCustomMemo}
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Ctl3D;
    property DragCursor;
    property Enabled;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PlainText;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  Windows,
  StdCtrls,
  BoldDefs,
  BoldControlPackDefs,
  BoldUtils,
  SysUtils;

{---TBoldCustomRichEdit---}
constructor TBoldCustomRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fBoldProperties := TBoldStringFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  fMyColor := EffectiveColor;
  inherited ReadOnly := True;
end;

destructor TBoldCustomRichEdit.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fCanvas);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  FreeAndNil(fStringStream);
  FreeAndNil(fStringStream);
  inherited Destroy;
end;

function TBoldCustomRichEdit.GetFormattedText: string;
begin
  if not Assigned(fStringStream) then
    fStringStream := TStringStream.Create('');
  fStringSTream.Seek(0, soFromBeginning);
  Lines.SaveToStream(fStringStream);
  Result := fStringStream.DataString;
end;
(*
procedure TBoldCustomRichEdit.SetFormattedText(const Value: string);
begin
  if not Assigned(fStringStream) then
    fStringStream := TStringStream.Create('');
  fStringSTream.Seek(0, soFromBeginning);
  Lines.LoadFromStream(fStringStream);
end;
 *)
procedure TBoldCustomRichEdit.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomRichEdit.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomRichEdit.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomRichEdit.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomRichEdit.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomRichEdit.SetBoldDisplay(value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomRichEdit.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomRichEdit.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomRichEdit.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomRichEdit._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomRichEdit.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomRichEdit.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomRichEdit.SetColor(value: TColor);
begin
  if not (csLoading in ComponentState) then
    if fMyColor <> value then
    begin
      fMyColor := value;
      Follower.MarkValueOutOfDate;
    end;
end;

function TBoldCustomRichEdit.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomRichEdit.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomRichEdit.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomRichEdit.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise EBold.Create('TBoldEdit.Text: Not modifiable');
end;

function TBoldCustomRichEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomRichEdit.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldCustomRichEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and
    not BoldProperties.ValidateCharacter(Key, Follower) then
  begin
    MessageBeep(0);
    Key := BOLDNULL;
  end;

  if Key = BOLDESC then
  begin
    Follower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
end;

procedure TBoldCustomRichEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomRichEdit.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(Text, Follower);
end;

procedure TBoldCustomRichEdit.AfterMakeUptoDate(Follower: TBoldFollower);
var
  newText: string;
  ec    : TColor;
begin
  newText := BoldProperties.GetCurrentAsString(Follower);
  if Text <> newText then
    inherited Text := newText;
  inherited ReadOnly := FMyReadOnly or
    not BoldProperties.MayModify(Follower);
  BoldProperties.SetFont(EffectiveFont, Font, Follower);
  ec := EffectiveColor;
  BoldProperties.SetColor(ec, Color, Follower);
  EffectiveColor := ec;
end;


procedure TBoldCustomRichEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomRichEdit.CMExit(var Message: TCMExit);
begin
  if (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  SetFocused(False);
  DoExit;
end;

function TBoldCustomRichEdit.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomRichEdit.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

end.