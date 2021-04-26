
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


implementation

uses
  BoldSystem,
  ActnList,
  BoldUndoInterfaces;


{ TBoldSetCheckPointAction }

procedure TBoldSetCheckPointAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldSystemHandle.Active;
end;

constructor TBoldSetCheckPointAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Set check point';
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
  if EnableAction then
    EnableAction := BoldSystemHandle.Active and
                    (BoldSystemHandle.System.UndoHandlerInterface.UndoList.Count > 0) and
                    ((BoldSystemHandle.System.UndoHandlerInterface.UndoList.Count > 1) or
                      BoldSystemHandle.System.UndoHandlerInterface.UndoList.TopBlock.ContainsChanges);
end;

constructor TBoldUndoAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Undo';
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
    EnableAction := BoldSystemHandle.Active and
                    (BoldSystemHandle.System.UndoHandlerInterface.RedoList.Count > 0);
end;

constructor TBoldRedoAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Redo';
end;

procedure TBoldRedoAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldSystemHandle.System.UndoHandlerInterface.RedoLatest;
end;

initialization

end.
