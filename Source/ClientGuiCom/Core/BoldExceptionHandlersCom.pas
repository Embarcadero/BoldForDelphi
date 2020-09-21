unit BoldExceptionHandlersCom;

interface

uses
  Classes,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  SysUtils;

type
  { forward declarations }
  TBoldExceptionHandlerCom = class;

  { prototypes }
  TBoldApplyExceptionEventCom = procedure (E: Exception; Component: TComponent; Elem: IBoldElement; var Discard: Boolean) of object;
  TBoldDisplayExceptionEventCom = procedure (E: Exception; Component: TComponent; Elem: IBoldElement) of object;

  { TBoldExceptionHandlerCom }
  TBoldExceptionHandlerCom = class(TComponent)
  private
    fOnApplyException: TBoldApplyExceptionEventCom;
    fOnDisplayException: TBoldDisplayExceptionEventCom;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function FindExceptionHandler(Component: TComponent): TBoldExceptionHandlerCom;
    procedure HandleApplyException(E: Exception; Component: TComponent; Elem: IBoldElement; var Discard: Boolean; var HandledByUser: boolean);
    procedure HandleDisplayException(E: Exception; Component: TComponent; Elem: IBoldElement; var HandledByUser: boolean);
  published
    property OnApplyException: TBoldApplyExceptionEventCom read fOnApplyException write fOnApplyException;
    property OnDisplayException: TBoldDisplayExceptionEventCom read fOnDisplayException write fOnDisplayException;
  end;

implementation

uses
  BoldRev,
  BoldUtils,
  Controls;

var
  G_BoldExceptionHandlers: TList = nil;

function GetBoldExceptionHandlers: TList;
begin
  if not assigned(G_BoldExceptionHandlers) then
    G_BoldExceptionHandlers := TList.Create;
  result := G_BoldExceptionHandlers;
end;

{ TBoldExceptionHandlerCom }

constructor TBoldExceptionHandlerCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetBoldExceptionHandlers.Add(self);
end;

destructor TBoldExceptionHandlerCom.Destroy;
begin
  if assigned(G_BoldExceptionHandlers) then
    G_BoldExceptionHandlers.Remove(self);
  inherited;
end;

class function TBoldExceptionHandlerCom.FindExceptionHandler(Component: TComponent): TBoldExceptionHandlerCom;
function OwningComponent(Component: TComponent): TComponent;
begin
//Find topmost owning component
  Result := Component;
  while Assigned(Result) and Assigned(Result.Owner) do
    Result := Result.Owner;
end;
function ParentControl(Component: TComponent): TWinControl;
begin
//Find topmost parent control
  Result := nil;
  if Component is TWinControl then
  begin
    Result := TWinControl(Component);
    while Assigned(Result.Parent) do
      Result := Result.Parent;
  end;
end;
var
  i: integer;
  ExceptionHandlerOwner: TComponent;
begin
  //Find matching exception handler
  result := nil;
  if assigned(G_BoldExceptionHandlers) then
    for i := 0 to G_BoldExceptionHandlers.Count - 1 do
    begin
      ExceptionHandlerOwner := TBoldExceptionHandlerCom(G_BoldExceptionHandlers[i]).Owner;
      if (ExceptionHandlerOwner = OwningComponent(Component)) or
         (ExceptionHandlerOwner = ParentControl(Component)) then
      begin
        result := TBoldExceptionHandlerCom(G_BoldExceptionHandlers[i]);
        exit;
      end;
    end;
end;

procedure TBoldExceptionHandlerCom.HandleApplyException(E: Exception;
  Component: TComponent; Elem: IBoldElement; var Discard: Boolean; var HandledByUser: boolean);
begin
  // Note: Discard must be set by caller, as there might be no matching exception handler
  // to set discard!
  HandledByUser := Assigned(fOnApplyException);
  if HandledByUser then
    OnApplyException(E, Component, Elem, Discard);
end;

procedure  TBoldExceptionHandlerCom.HandleDisplayException(E: Exception;
  Component: TComponent; Elem: IBoldElement; var HandledByUser: boolean);
begin
  HandledByUser := Assigned(fOnDisplayException);
  if HandledByUser then
    OnDisplayException(E, Component, Elem);
end;

initialization

finalization
  FreeAndNil(G_BoldExceptionHandlers);

end.
