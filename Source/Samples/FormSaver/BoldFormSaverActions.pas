
{ Global compiler directives }
{$include bold.inc}
unit BoldFormSaverActions;

interface

uses
  classes,
  BoldFormSaver,
  BoldHandleAction;

type
  TBoldFormSaverAction = class;
  TBoldFormSaverApplyAction = class;
  TBoldFormSaverCancelAction = class;
  TBoldFormSaverOkAction = class;

  TBoldFormSaverAction = class(TBoldHandleAction)
  private
    function GetBoldFormSaver: TBoldFormSaver;
  protected
    procedure SetBoldFormSaver(const Value: TBoldFormSaver); virtual;
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BoldFormSaver: TBoldFormSaver read GetBoldFormSaver write SetBoldFormSaver;
  end;

  TBoldFormSaverApplyAction = class(TBoldFormSaverAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldFormSaverCancelAction = class(TBoldFormSaverAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldFormSaverOkAction = class(TBoldFormSaverAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

implementation

uses
  BoldSystem,
  ActnList,
  SysUtils,
  Menus; // for TextToShortCut

{ TBoldFormSaverAction }

procedure TBoldFormSaverAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := Assigned(BoldFormSaver);
end;

constructor TBoldFormSaverAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'FormSaver';
  ShortCut := TextToShortCut('Ctrl+Z');
end;

function TBoldFormSaverAction.GetBoldFormSaver: TBoldFormSaver;
begin
  result := BoldElementHandle as TBoldFormSaver;
end;

procedure TBoldFormSaverAction.SetBoldFormSaver(const Value: TBoldFormSaver);
begin
  BoldElementHandle := Value;
end;

{ TBoldFormSaverApplyAction }

procedure TBoldFormSaverApplyAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and not BoldFormSaver.DirtyObjects.Empty;
end;

constructor TBoldFormSaverApplyAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '&Apply';
end;

procedure TBoldFormSaverApplyAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldFormSaver.Apply;
end;

{ TBoldFormSaverCancelAction }

procedure TBoldFormSaverCancelAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
  begin
    if BoldFormSaver.DirtyObjects.Empty then
      Caption := '&Close'
    else
      Caption := '&Cancel'
  end;
end;

constructor TBoldFormSaverCancelAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '&Cancel';
  ShortCut := TextToShortCut('Escape');
end;

procedure TBoldFormSaverCancelAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldFormSaver.Cancel;
end;

{ TBoldFormSaverOkAction }

procedure TBoldFormSaverOkAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and not BoldFormSaver.DirtyObjects.Empty;
end;

constructor TBoldFormSaverOkAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := '&Ok';
end;

procedure TBoldFormSaverOkAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldFormSaver.OK;
end;

end.
