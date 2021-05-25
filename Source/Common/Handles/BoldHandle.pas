
{ Global compiler directives }
{$include bold.inc}
unit BoldHandle;

interface

uses
  Classes,
  BoldContainers,
  BoldSubscription
  ;

type
  { forward declarations }
  TBoldHandle = class;

  {-- TBoldHandle --}
  TBoldHandle = class(TBoldSubscribableComponent)
  protected
    function GetHandledObject: TObject; virtual; abstract;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property HandledObject: TObject read GetHandledObject;
  end;

var
  BoldHandleList: TBoldObjectArray;

implementation

uses
  SysUtils;

{ TBoldHandle }

constructor TBoldHandle.Create(owner: TComponent);
begin
  inherited;
  if assigned(BoldHandleList) then
    BoldHandleList.Add(self);
end;

destructor TBoldHandle.Destroy;
begin
  FreePublisher;
  if assigned(BoldHandleList) then
    BoldHandleList.Remove(self);
  inherited;
end;

initialization
  BoldHandleList := TBoldObjectArray.Create(10, []);

finalization
  FreeAndNil(BoldHandleList);

end.
