
{ Global compiler directives }
{$include bold.inc}
unit BoldUndoActions;

interface

uses
  BoldActions,
  classes;

type
  TBoldSetCheckPointAction = class;
  TBoldUndoAction = class;
  TBoldRedoAction = class;
  TBoldClearUndoAction = class;

  TBoldSetCheckPointAction = class(TBoldSystemHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldUndoAction = class(TBoldSystemHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldRedoAction = class(TBoldSystemHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldClearUndoAction = class(TBoldSystemHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

implementation

uses
  ActnList,
  Menus, // for TextToShortCut
  SysUtils,

  BoldCoreConsts,
  BoldSystem,
  BoldUndoInterfaces,
  BoldUndoHandler;


{ TBoldSetCheckPointAction }

procedure TBoldSetCheckPointAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and BoldSystemHandle.Active and BoldSystemHandle.System.UndoHandlerInterface.Enabled
      and BoldSystemHandle.System.UndoHandlerInterface.CurrentUndoBlockHasChanges;
end;

constructor TBoldSetCheckPointAction.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := false;
  Caption := sSetCheckPoint;
end;

procedure TBoldSetCheckPointAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldSystemHandle.System.UndoHandlerInterface.SetCheckPoint;
end;

{ TBoldUndoAction }

procedure TBoldUndoAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and BoldSystemHandle.Active and BoldSystemHandle.System.UndoHandlerInterface.Enabled
                  and (BoldSystemHandle.System.UndoHandlerInterface.CurrentUndoBlockHasChanges
                  or (BoldSystemHandle.System.UndoHandlerInterface.UndoList.Count > 1));
  if EnableAction then
  begin
    Caption := Format('Undo %s', [TBoldUndoHandler(BoldSystemHandle.System.UndoHandler).UndoBlocks.GetFirstNonEmptyBlock.Caption]);
    Hint := caption;
  end
  else
  begin
    Caption := 'Undo';
    Hint := '';
  end;
end;

constructor TBoldUndoAction.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := false;
  Caption := sUndo;
  ShortCut := TextToShortCut('Ctrl+Z');
end;

procedure TBoldUndoAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldSystemHandle.System.UndoHandlerInterface.UndoLatest;
end;

{ TBoldRedoAction }

procedure TBoldRedoAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldSystemHandle.Active and BoldSystemHandle.System.UndoHandlerInterface.Enabled and
                    (BoldSystemHandle.System.UndoHandlerInterface.RedoList.Count > 0);
  if EnableAction then
  begin
    Caption := Format('Redo %s', [TBoldUndoHandler(BoldSystemHandle.System.UndoHandler).RedoBlocks.CurrentBlock.Caption]);
    Hint := Caption;
  end
  else
  begin
    Caption := 'Redo';
    Hint := '';
  end;
end;

constructor TBoldRedoAction.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := false;
  Caption := sRedo;
  ShortCut := TextToShortCut('Shift+Ctrl+Z');
end;

procedure TBoldRedoAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldSystemHandle.System.UndoHandlerInterface.RedoLatest;
end;

{ TBoldClearUndoAction }

procedure TBoldClearUndoAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    with BoldSystemHandle.System.UndoHandlerInterface do
      EnableAction := BoldSystemHandle.Active and Enabled and UndoList.ContainsChanges;
end;

constructor TBoldClearUndoAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Clear';
end;

procedure TBoldClearUndoAction.ExecuteTarget(Target: TObject);
begin
  BoldSystemHandle.System.UndoHandlerInterface.ClearAllUndoBlocks;
end;

end.
