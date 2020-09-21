unit BoldExceptionHandlers;

interface

uses
  Classes,
  BoldElements,
  SysUtils;

type
  { forward declarations }
  TBoldExceptionHandler = class;

  { prototypes }
  TBoldApplyExceptionEvent = procedure (E: Exception; Component: TComponent; Elem: TBoldElement; var Discard: Boolean) of object;
  TBoldDisplayExceptionEvent = procedure (E: Exception; Component: TComponent; Elem: TBoldElement) of object;

  { TBoldExceptionHandler }
  TBoldExceptionHandler = class(TComponent)
  private
    fOnApplyException: TBoldApplyExceptionEvent;
    fOnDisplayException: TBoldDisplayExceptionEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function FindExceptionHandler(Component: TComponent): TBoldExceptionHandler;
    procedure HandleApplyException(E: Exception; Component: TComponent; Elem: TBoldElement; var Discard: Boolean; var HandledByUser: boolean);
    procedure HandleDisplayException(E: Exception; Component: TComponent; Elem: TBoldElement; var HandledByUser: boolean);
  published
    property OnApplyException: TBoldApplyExceptionEvent read fOnApplyException write fOnApplyException;
    property OnDisplayException: TBoldDisplayExceptionEvent read fOnDisplayException write fOnDisplayException;
  end;

implementation


uses
  Controls,
  BoldUtils;

var
  G_BoldExceptionHandlers: TList = nil;

function GetBoldExceptionHandlers: TList;
begin
  if not assigned(G_BoldExceptionHandlers) then
    G_BoldExceptionHandlers := TList.Create;
  result := G_BoldExceptionHandlers;
end;

{ TBoldExceptionHandler }

constructor TBoldExceptionHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetBoldExceptionHandlers.Add(self);
end;

destructor TBoldExceptionHandler.Destroy;
begin
  if assigned(G_BoldExceptionHandlers) then
    G_BoldExceptionHandlers.Remove(self);
  inherited;
end;

class function TBoldExceptionHandler.FindExceptionHandler(Component: TComponent): TBoldExceptionHandler;
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
      ExceptionHandlerOwner := TBoldExceptionHandler(G_BoldExceptionHandlers[i]).Owner;
      if (ExceptionHandlerOwner = OwningComponent(Component)) or
         (ExceptionHandlerOwner = ParentControl(Component)) then
      begin
        result := TBoldExceptionHandler(G_BoldExceptionHandlers[i]);
        exit;
      end;
    end;
end;

procedure TBoldExceptionHandler.HandleApplyException(E: Exception;
  Component: TComponent; Elem: TBoldElement; var Discard: Boolean; var HandledByUser: boolean);
begin
  // Note: Discard must be set by caller, as there might be no matching exception handler
  // to set discard!
  HandledByUser := Assigned(fOnApplyException);
  if HandledByUser then
    OnApplyException(E, Component, Elem, Discard);
end;

procedure  TBoldExceptionHandler.HandleDisplayException(E: Exception;
  Component: TComponent; Elem: TBoldElement; var HandledByUser: boolean);
begin
  HandledByUser := Assigned(fOnDisplayException);
  if HandledByUser then
    OnDisplayException(E, Component, Elem);
end;

initialization

finalization
  FreeAndNil(G_BoldExceptionHandlers);

end.
