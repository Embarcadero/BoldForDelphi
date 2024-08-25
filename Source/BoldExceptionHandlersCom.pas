
{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionHandlersCom;

interface

uses
  Classes,
  BoldComObjectSpace_TLB,
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
var
  i: integer;
begin
  result := nil;
  if assigned(G_BoldExceptionHandlers) then
    for i := 0 to G_BoldExceptionHandlers.Count - 1 do
      if TBoldExceptionHandlerCom(G_BoldExceptionHandlers[i]).Owner = Component.Owner then
      begin
        result := TBoldExceptionHandlerCom(G_BoldExceptionHandlers[i]);
        exit;
      end;
end;

procedure TBoldExceptionHandlerCom.HandleApplyException(E: Exception;
  Component: TComponent; Elem: IBoldElement; var Discard: Boolean; var HandledByUser: boolean);
begin

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
