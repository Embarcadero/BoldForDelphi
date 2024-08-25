{ Global compiler directives }
{$include bold.inc}
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
  TBoldApplyExceptionEvent = procedure (E: Exception; Component: TComponent; Elem: TBoldElement; var Discard: Boolean; var HandledByUser: boolean) of object;
  TBoldDisplayExceptionEvent = procedure (E: Exception; Component: TComponent; Elem: TBoldElement) of object;

  { TBoldExceptionHandler }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldExceptionHandler = class(TComponent)
  private
    fOnApplyException: TBoldApplyExceptionEvent;
    fOnDisplayException: TBoldDisplayExceptionEvent;
    fGlobal: boolean;
    procedure SetGlobal(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function FindExceptionHandler(Component: TComponent): TBoldExceptionHandler;
    procedure HandleApplyException(E: Exception; Component: TComponent; Elem: TBoldElement; var Discard: Boolean; var HandledByUser: boolean);
    procedure HandleDisplayException(E: Exception; Component: TComponent; Elem: TBoldElement; var HandledByUser: boolean);
  published
    property OnApplyException: TBoldApplyExceptionEvent read fOnApplyException write fOnApplyException;
    property OnDisplayException: TBoldDisplayExceptionEvent read fOnDisplayException write fOnDisplayException;
    property Global: boolean read fGlobal write SetGlobal default false;
  end;      

implementation

uses
  System.Types;

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
var
  i: integer;
begin
  if assigned(G_BoldExceptionHandlers) and (G_BoldExceptionHandlers.Count > 0) then
  begin
    for i := 0 to G_BoldExceptionHandlers.Count - 1 do
    begin
      result := TBoldExceptionHandler(G_BoldExceptionHandlers[i]);
      if (result.Owner = Component.Owner) then
        exit;
    end;
    for i := 0 to G_BoldExceptionHandlers.Count - 1 do
    begin
      result := TBoldExceptionHandler(G_BoldExceptionHandlers[i]);
      if Result.Global then
        exit;
    end;
  end;
  result := nil;
end;

procedure TBoldExceptionHandler.HandleApplyException(E: Exception;
  Component: TComponent; Elem: TBoldElement; var Discard: Boolean; var HandledByUser: boolean);
begin
  if Assigned(fOnApplyException) then
    OnApplyException(E, Component, Elem, Discard, HandledByUser);
end;

procedure TBoldExceptionHandler.HandleDisplayException(E: Exception;
  Component: TComponent; Elem: TBoldElement; var HandledByUser: boolean);
begin
  HandledByUser := Assigned(fOnDisplayException);
  if HandledByUser then
    OnDisplayException(E, Component, Elem);
end;

procedure TBoldExceptionHandler.SetGlobal(const Value: boolean);
var
  i: integer;
begin
  if Value = Global then
    exit;
  if value and assigned(G_BoldExceptionHandlers) then
  for i := 0 to G_BoldExceptionHandlers.Count - 1 do
    if TBoldExceptionHandler(G_BoldExceptionHandlers[i]).Global then
      exit;
  fGlobal := Value;
end;

initialization

finalization
  FreeAndNil(G_BoldExceptionHandlers);

end.