{ Global compiler directives }
{$include bold.inc}
unit BoldListActions;

interface

uses
  Classes,
  Controls,

  BoldCoreConsts,
  BoldDefs,
  BoldSubscription,
  BoldSystem,
  BoldAbstractListHandle,
  BoldHandleAction,
  BoldNavigatorDefs;

type
  TBoldListHandleAction = class;
  TBoldListHandleNextAction = class;
  TBoldListHandlePrevAction = class;
  TBoldListHandleFirstAction = class;
  TBoldListHandleLastAction = class;
  TBoldListHandleDeleteAction = class;
  TBoldListHandleAddNewAction = class;
  TBoldListHandleMoveUpAction =  class;
  TBoldListHandleMoveDownAction =  class;

  TBoldListHandleAction = class(TBoldHandleAction)
  private
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(const Value: TBoldAbstractListHandle);
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  published
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
  end;

  TBoldListHandleNextAction = class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandlePrevAction = class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandleFirstAction = class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandleLastAction = class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandleDeleteAction = class(TBoldListHandleAction)
  private
    FDeleteMode: TBoldDeleteMode;
  protected
    function AllowDelete(CanDeleteObject: boolean): boolean;
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BoldDeleteMode: TBoldDeleteMode read FDeleteMode write FDeleteMode default dmDefault;
  end;

  TBoldListHandleAddNewAction = class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandleMoveUpAction =  class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TBoldListHandleMoveDownAction =  class(TBoldListHandleAction)
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldListHandleAction }

procedure TBoldListHandleAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := Assigned(BoldHandle.List);
end;

function TBoldListHandleAction.GetBoldHandle: TBoldAbstractListHandle;
begin
  Assert(not Assigned(BoldElementHandle) or (BoldElementHandle is TBoldAbstractListHandle));
  Result := BoldElementHandle as TBoldAbstractListHandle;
end;

procedure TBoldListHandleAction.SetBoldHandle(
  const Value: TBoldAbstractListHandle);
begin
  BoldElementHandle := Value;
end;

{ TBoldListHandleNextAction }

procedure TBoldListHandleNextAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.HasNext;
end;

constructor TBoldListHandleNextAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sNext;
end;

procedure TBoldListHandleNextAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.Next;
end;

{ TBoldListHandlePrevAction }

procedure TBoldListHandlePrevAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.HasPrior;
end;

constructor TBoldListHandlePrevAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sPrev;
end;

procedure TBoldListHandlePrevAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.Prior;
end;

{ TBoldListHandleFirstAction }

procedure TBoldListHandleFirstAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.Count > 0;
end;

constructor TBoldListHandleFirstAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sFirst;
end;

procedure TBoldListHandleFirstAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.First;
end;

{ TBoldListHandleLastAction }

procedure TBoldListHandleLastAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.Count > 0;
end;

constructor TBoldListHandleLastAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sLast;
end;

procedure TBoldListHandleLastAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.Last;
end;

{ TBoldListHandleDeleteAction }

function TBoldListHandleDeleteAction.AllowDelete(
  CanDeleteObject: boolean): boolean;
begin
  case BoldDeleteMode of
    dmDefault, dmRemoveFromList:
      Result := True;
    dmDelete:
      Result := CanDeleteObject;
    dmUnlinkAllAndDelete:
      Result := True;
    else
      raise EBold.CreateFmt(sUnknownDeleteMode, [ClassName, 'AllowDelete']);
  end;
end;

procedure TBoldListHandleDeleteAction.CheckAllowEnable(
  var EnableAction: boolean);
var
  BoldObject: TBoldObject;
begin
  inherited;
  if EnableAction then
  begin
    BoldObject := BoldHandle.CurrentBoldObject;
    EnableAction := Assigned(BoldObject) and
                    AllowDelete(BoldObject.CanDelete);
  end;
end;

constructor TBoldListHandleDeleteAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sDelete;
end;

procedure TBoldListHandleDeleteAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  case BoldDeleteMode of
    dmDefault, dmRemoveFromList:
      BoldHandle.RemoveCurrentElement;
    dmDelete:
      BoldHandle.CurrentBoldObject.Delete;
    dmUnlinkAllAndDelete:
      with BoldHandle.CurrentBoldObject do
      begin
        UnLinkAll;
        Delete;
      end;
  end;
end;

{ TBoldListHandleAddNewAction }

procedure TBoldListHandleAddNewAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.List.CanCreateNew;
end;

constructor TBoldListHandleAddNewAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Add New';
end;

procedure TBoldListHandleAddNewAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if assigned(BoldHandle.MutableList) then
    BoldHandle.MutableList.AddNew
  else
    BoldHandle.List.AddNew;
end;

{ TBoldListHandleMoveUpAction }

procedure TBoldListHandleMoveUpAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.List.CanMove(BoldHandle.CurrentIndex, BoldHandle.CurrentIndex-1)
end;

constructor TBoldListHandleMoveUpAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Move up';
end;

procedure TBoldListHandleMoveUpAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.List.Move(BoldHandle.CurrentIndex, BoldHandle.CurrentIndex-1);
end;

{ TBoldListHandleMoveDownAction }

procedure TBoldListHandleMoveDownAction.CheckAllowEnable(
  var EnableAction: boolean);
begin
  inherited;
  if EnableAction then
    EnableAction := BoldHandle.List.CanMove(BoldHandle.CurrentIndex, BoldHandle.CurrentIndex+1)
end;

constructor TBoldListHandleMoveDownAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Move down';
end;

procedure TBoldListHandleMoveDownAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldHandle.List.Move(BoldHandle.CurrentIndex, BoldHandle.CurrentIndex+1);
end;

end.
