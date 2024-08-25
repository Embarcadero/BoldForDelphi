
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldMemoCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  Controls,
  Graphics,
  Menus,
  StdCtrls,
  Windows,

  // Bold
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldHandlesCom,
  BoldStringControlPackCom;

type
  TBoldCustomMemoCom = class;
  TBoldMemoCom = class;

  {---TBoldCustomMemoCom---}
  TBoldCustomMemoCom = class(TCustomMemo, IBoldOCLComponentCom)
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
    fMaxLength: integer;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;

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
    function GetText: string;
    procedure SetBoldDisplay(Value: TBoldStringFollowerControllerCom);
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    procedure SetColor(value: TColor);
    procedure SetEffectiveColor(v: TColor);
    procedure SetFocused(Value: Boolean);
    procedure SetFont(value: TFont);
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
    procedure Loaded; override;
    property Alignment: TAlignment read fAlignment write fAlignment;
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldDisplay;
    property Color: TColor read GetColor write SetColor default clWindow;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read GetEffectiveReadOnly;
    property Follower: TBoldFollowerCom read GetFollower;
    property Font: TFont read GetFont write SetFont;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property MaxLength: integer read fMaxLength write fMaxLength;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  {---TBoldMemoCom---}
  TBoldMemoCom = class(TBoldCustomMemoCom)
  public
    {$IFNDEF T2H}
    property Text;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BoldHandle;
    property BoldProperties;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property WantReturns;
    property WantTabs;
    property Visible;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldControlPackDefs;

{---TBoldCustomMemoCom---}
constructor TBoldCustomMemoCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  Color := clWindow;
  inherited ReadOnly := True;
end;

destructor TBoldCustomMemoCom.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange,
    bapExit: try
      Follower.Apply;
    except
      Follower.DiscardChange;
    end;
    bapDemand: Follower.DiscardChange;
  end;
  
  FreeAndNil(fHandleFollower);
  FreeAndNil(fCanvas);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  inherited Destroy;
end;

procedure TBoldCustomMemoCom.Loaded;
begin
  inherited;
  EffectiveColor := Color;
end;

procedure TBoldCustomMemoCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomMemoCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomMemoCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomMemoCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomMemoCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomMemoCom.SetBoldDisplay(value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomMemoCom.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomMemoCom.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomMemoCom.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomMemoCom._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomMemoCom.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomMemoCom.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomMemoCom.SetColor(value: TColor);
begin
  if fMyColor <> value then
    fMyColor := value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomMemoCom.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomMemoCom.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomMemoCom.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomMemoCom.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise Exception.CreateFmt('%s.Text: Not modifiable', [ClassName]);
end;

function TBoldCustomMemoCom.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomMemoCom.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldCustomMemoCom.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and
    not BoldProperties.ValidateCharacter(AnsiChar(Key), Follower) then
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

procedure TBoldCustomMemoCom.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then
      Invalidate;
  end;
end;

procedure TBoldCustomMemoCom.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(text, Follower);
end;

procedure TBoldCustomMemoCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  newText: string;
  ec    : TColor;
  EffectiveMaxLength: integer;
  RendererDataMaxLength: integer;
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
  EffectiveMaxLength := 0;
  RendererDataMaxLength := (Follower.RendererData as TBoldStringRendererDataCom).MaxStringLength;

  if RendererDataMaxLength <> -1 then
    EffectiveMaxLength := RendererDataMaxLength
  else
    EffectiveMaxLength := MaxLength;

  if (MaxLength > 0) and (MaxLength < EffectiveMaxLength) then
    EffectiveMaxLength := MaxLength;

  inherited MaxLength := EffectiveMaxLength;
end;

procedure TBoldCustomMemoCom.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomMemoCom.CMExit(var Message: TCMExit);
begin
  if (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply; 
  SetFocused(False);
  DoExit;
end;

function TBoldCustomMemoCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomMemoCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomMemoCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    Result := nil;
end;

function TBoldCustomMemoCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomMemoCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldCustomMemoCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
