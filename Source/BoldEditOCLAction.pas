
{ Global compiler directives }
{$include bold.inc}
unit BoldEditOCLAction;

interface

uses
  Classes,
  Controls,
  ActnList,
  BoldElements,
  BoldControlPack,
  BoldHandles;

type
  { forward declaration }
  TBoldEditOCLAction = class;

  { TBoldEditOCLAction }
  TBoldEditOCLAction = class(TAction)
  private
    fBoldComponent: TComponent;
    fOCLCOmponent: IBoldOCLComponent;
    fOnPostExecute: TNotifyEvent;
    procedure SetBoldComponent(const Value: TComponent);
    function GetOCLExpression: string;
    procedure SetOCLExpression(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function EditOCL: string;
    property OCLComponent: IBoldOCLComponent read fOCLComponent;
  public
    constructor Create(aOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure PostExecute;
    property OCLExpression: string read GetOCLExpression write SetOCLExpression;
  published
    property BoldComponent: TComponent read fBoldCOmponent write SetBoldComponent;
    property OnPostExecute: TNotifyEvent read fOnPostExecute write fOnPostExecute;
  end;

implementation

uses
  SysUtils,
  BoldOclPropEditor,
  BoldDefs;

{ TBoldEditOCLAction }

constructor TBoldEditOCLAction.Create(aOwner: TComponent);
begin
  inherited;
  Caption := 'Edit OCL';
end;

function TBoldEditOCLAction.EditOCL: string;
var
  EditForm: TBoldOclPropEditForm;
begin
  EditForm := TBoldOclPropEditForm.Create(nil);
  EditForm.Context := OCLComponent.ContextType;
  EditForm.OclExpr := OCLComponent.Expression;
  if Editform.ShowModal = mrOK then
    result := editForm.OclExpr
  else
    result := OCLComponent.Expression;
  EditForm.Release;
end;

procedure TBoldEditOCLAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if HandlesTarget(Target) then
  begin
    OCLComponent.Expression := EditOCL;
    PostExecute;
  end;
end;

function TBoldEditOCLAction.GetOCLExpression: string;
begin
  if Assigned(OCLComponent) then
    Result := OCLComponent.Expression
  else
    Result := '';
end;

function TBoldEditOCLAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(OCLComponent) and
            (Target is TControl) and
            (TControl(Target).Action = self);
end;

procedure TBoldEditOCLAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fBoldComponent) and (Operation = opRemove) then
  begin
    fBoldComponent := nil;
    fOCLComponent := nil;
  end;
end;

procedure TBoldEditOCLAction.PostExecute;
begin
  if Assigned(fOnPostExecute) then fOnPostExecute(Self);
end;

procedure TBoldEditOCLAction.SetBoldComponent(const Value: TComponent);
begin
  if Value <> fBoldComponent then
  begin
    if Assigned(fBoldComponent) then
      fBoldComponent.RemoveFreeNotification(self);
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
      Value.GetInterface(IBoldOCLComponent, fOCLComponent);
    end
    else
      fOCLComponent := nil;
    fBoldComponent := Value;
  end;
end;

procedure TBoldEditOCLAction.SetOCLExpression(const Value: string);
begin
  if Assigned(OCLComponent) then
    OCLComponent.Expression := Value
  else
    raise Exception.CreateFmt('%s is not connected to an OCL Component', [Name]);
end;

end.
