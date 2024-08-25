
{ Global compiler directives }
{$include bold.inc}
unit BoldNavigatorDefs;

interface

uses
  Classes,
  Controls,
  Extctrls,
  Buttons,
  Windows;

type
  TBoldNavButton = class;
  TBoldNavigateBtnImageIndex = class;
  TBoldNavigateBtnImageIndexOwner = class;

  TBoldNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbMoveUp, nbMoveDown);
  TBoldButtonSet = set of TBoldNavigateBtn;
  TBoldDeleteMode = (dmDefault, dmRemoveFromList, dmDelete, dmUnlinkAllAndDelete);
  TBoldNavButtonStyle = set of (nsAllowTimer, nsFocusRect);
  TBoldNavClick = procedure (Sender: TObject; Button: TBoldNavigateBtn) of object;

  TBoldNavigateBtnImageIndexOwner = class(TCustomPanel)
  private
    fFocusedButton: TBoldNavigateBtn;
  protected
    procedure FixButtonGlyphs; virtual; abstract;
    property FocusedButton: TBoldNavigateBtn read fFocusedButton write fFocusedButton;
  end;


    { TBoldNavigateBtnImageIndex }
  TBoldNavigateBtnImageIndex = class(TPersistent)
  private
    FnbNext: integer;
    FnbInsert: integer;
    FnbFirst: integer;
    FnbPrior: integer;
    FnbDelete: integer;
    FnbLast: integer;
    fnbMoveUp: integer;
    fnbMoveDown: integer;
    fOwner: TBoldNavigateBtnImageIndexOwner;
    procedure SetnbDelete(const Value: integer);
    procedure SetnbFirst(const Value: integer);
    procedure SetnbInsert(const Value: integer);
    procedure SetnbLast(const Value: integer);
    procedure SetnbNext(const Value: integer);
    procedure SetnbPrior(const Value: integer);
    procedure SetnbMoveUp(const Value: integer);
    procedure SetnbMoveDown(const Value: integer);
  public
    constructor Create(Owner: TBoldNavigateBtnImageIndexOwner);
    procedure Assign(Source: TPersistent); override;
  published
    property nbFirst: integer read FnbFirst write SetnbFirst;
    property nbPrior: integer read FnbPrior write SetnbPrior;
    property nbNext: integer read FnbNext write SetnbNext;
    property nbLast: integer read FnbLast write SetnbLast;
    property nbInsert: integer read FnbInsert write SetnbInsert;
    property nbDelete: integer read FnbDelete write SetnbDelete;
    property nbMoveUp: integer read FnbMoveUp write SetnbMoveUp;
    property nbMoveDown: integer read FnbMoveDown write SetnbMoveDown;
  end;

  { TBoldNavButton }
  TBoldNavButton = class(TSpeedButton)
  private
    FIndex: TBoldNavigateBtn;
    FNavStyle: TBoldNavButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    destructor Destroy; override;
    property Index: TBoldNavigateBtn read FIndex write FIndex;
    property NavStyle: TBoldNavButtonStyle read FNavStyle write FNavStyle;
  end;


implementation

uses
  SysUtils;

const
  InitRepeatPause = 400; { pause before repeat timer (ms)}
  RepeatPause = 100; { pause before hint window displays (ms)}


{ TBoldNavigateBtnImageIndex }

procedure TBoldNavigateBtnImageIndex.Assign(Source: TPersistent);
begin
  if source is TBoldNavigateBtnImageIndex then
  begin
    FnbNext := (source as TBoldNavigateBtnImageIndex).nbNext;
    FnbInsert := (source as TBoldNavigateBtnImageIndex).nbINsert;
    FnbFirst := (source as TBoldNavigateBtnImageIndex).nbFirst;
    FnbPrior := (source as TBoldNavigateBtnImageIndex).nbPrior;
    FnbDelete := (source as TBoldNavigateBtnImageIndex).nbDelete;
    FnbLast := (source as TBoldNavigateBtnImageIndex).nbLast;
    FnbMoveUp := (source as TBoldNavigateBtnImageIndex).nbMoveUp;
    FnbMoveDown := (source as TBoldNavigateBtnImageIndex).nbMoveDown;
  end
  else
    inherited Assign(source);
end;

constructor TBoldNavigateBtnImageIndex.Create(Owner: TBoldNavigateBtnImageIndexOwner);
begin
  fOwner := Owner;
  inherited Create;
end;

procedure TBoldNavigateBtnImageIndex.SetnbDelete(const Value: integer);
begin
  FnbDelete := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbFirst(const Value: integer);
begin
  FnbFirst := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbInsert(const Value: integer);
begin
  FnbInsert := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbLast(const Value: integer);
begin
  FnbLast := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbMoveUp(const Value: integer);
begin
  FnbMoveUp := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbNext(const Value: integer);
begin
  FnbNext := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbPrior(const Value: integer);
begin
  FnbPrior := Value;
  fOwner.FixButtonGlyphs;
end;

procedure TBoldNavigateBtnImageIndex.SetnbMoveDown(const Value: integer);
begin
  FnbMoveDown := Value;
  fOwner.FixButtonGlyphs;
end;


{---TBoldNavButton---}

destructor TBoldNavButton.Destroy;
begin
  FreeAndNil(FRepeatTimer);
  inherited Destroy;
end;

procedure TBoldNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if nsAllowTimer in FNavStyle then
  begin
    if not Assigned(FRepeatTimer) then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
  end;
end;

procedure TBoldNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(FRepeatTimer) then
    FRepeatTimer.Enabled := False;
end;

procedure TBoldNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (fState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TBoldNavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
    (FIndex = TBoldNavigateBtnImageIndexOwner(Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, - 3, - 3);
    if fState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

end.
