
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldRichEditCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Menus,
  BoldHandlesCom,
  BoldControlPackCom,
  BoldStringControlPackCom,
  BoldElementHandleFollowerCom;

type
  TBoldCustomRichEditCom = class;
  TBoldRichEditCom = class;

  {---TBoldCustomRichEditCom---}
  TBoldCustomRichEditCom = class(TCustomRichEdit)
  private
    {Bold stuff}
    fAlignment: TAlignment;
    fBoldProperties: TBoldStringFollowerControllerCom;
    fCanvas: TControlCanvas;
    fFocused: Boolean;
    fHandleFollower: TBoldElementHandleFollowerCom;
    fMyColor: TColor;
    fMyFont: TFont;
    fMyReadOnly: Boolean;
    fStringStream: TStringStream;
    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetBoldHandle: TBoldElementHandleCom;
    function GetColor: TColor;
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetEffectiveReadOnly: Boolean;
    function GetFollower: TBoldFollowerCom;
    function GetFont: TFont;
    function GetFormattedText: string;
    function GetText: string;
    procedure SetBoldDisplay(Value: TBoldStringFollowerControllerCom);
    procedure SetBoldHandle(value: TBoldElementHandleCom);
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
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldDisplay;
    property Color: TColor read GetColor write SetColor;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read GetEffectiveReadOnly;
    property Follower: TBoldFollowerCom read GetFollower;
    property Font: TFont read GetFont write SetFont;
    property FormattedText: string read GetFormattedText;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  {---TBoldRichEditCom---}
  TBoldRichEditCom = class(TBoldCustomRichEditCom)
  published
    {$IFNDEF T2H}
    {Properties from TBoldCustomRichEditCom}
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
  SysUtils;

{---TBoldCustomRichEditCom---}
constructor TBoldCustomRichEditCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  fMyColor := EffectiveColor;
  inherited ReadOnly := True;
end;

destructor TBoldCustomRichEditCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fCanvas);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  FreeAndNil(fStringStream);
  FreeAndNil(fStringStream);
  inherited Destroy;
end;

function TBoldCustomRichEditCom.GetFormattedText: string;
begin
  if not Assigned(fStringStream) then
    fStringStream := TStringStream.Create('');
  fStringSTream.Seek(0, soFromBeginning);
  Lines.SaveToStream(fStringStream);
  Result := fStringStream.DataString;
end;
(*
procedure TBoldCustomRichEditCom.SetFormattedText(const Value: string);
begin
  if not Assigned(fStringStream) then
    fStringStream := TStringStream.Create('');
  fStringSTream.Seek(0, soFromBeginning);
  Lines.LoadFromStream(fStringStream);
end;
 *)
procedure TBoldCustomRichEditCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomRichEditCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomRichEditCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomRichEditCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomRichEditCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomRichEditCom.SetBoldDisplay(value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomRichEditCom.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomRichEditCom.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomRichEditCom.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomRichEditCom._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomRichEditCom.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomRichEditCom.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomRichEditCom.SetColor(value: TColor);
begin
  if not (csLoading in ComponentState) then
    if fMyColor <> value then
    begin
      fMyColor := value;
      Follower.MarkValueOutOfDate;
    end;
end;

function TBoldCustomRichEditCom.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomRichEditCom.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomRichEditCom.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomRichEditCom.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise EBold.Create('TBoldEditCom.Text: Not modifiable');
end;

function TBoldCustomRichEditCom.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomRichEditCom.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldCustomRichEditCom.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and
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

procedure TBoldCustomRichEditCom.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomRichEditCom.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(Text, Follower);
end;

procedure TBoldCustomRichEditCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
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


procedure TBoldCustomRichEditCom.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomRichEditCom.CMExit(var Message: TCMExit);
begin
  if (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  SetFocused(False);
  DoExit;
end;

function TBoldCustomRichEditCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomRichEditCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

end.
