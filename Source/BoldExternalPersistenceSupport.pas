
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceSupport;

interface

uses
  BoldBase,
  BoldMeta,
  BoldValueSpaceInterfaces;

type
  IPersistentBoldObject = interface;
  TBoldObjectPersistenceAdapter = class;
  TBoldObjectPersistenceAdapterClass = class of TBoldObjectPersistenceAdapter;

  IPersistentBoldObject = interface
    function GetAsObjectContents: IBoldObjectContents;
    property AsObjectContents: IBoldObjectContents read GetAsObjectContents;
  end;

  TBoldObjectPersistenceAdapter = class(TBoldRefCountedObject, IPersistentBoldObject)
  private
    fAdaptedInterface: IBoldObjectContents;
    fValueSpace: IBoldValueSpace;
    fMoldClass: TMoldClass;
  protected
    function GetAsObjectContents: IBoldObjectContents;
  public
    constructor Create(AdaptedInterface: IBoldObjectContents; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass);
    class procedure RegisterPersistenceInterface(Adapter: TBoldObjectPersistenceAdapterClass; const ExpressionName: String);
    class function FindRegisteredPersistenceInterface(const ExpressionName: String): TBoldObjectPersistenceAdapterClass;
    property ValueSpace: IBoldValueSpace read fValueSpace;
    property AdaptedInterface: IBoldObjectContents read fAdaptedInterface;
    property MoldClass: TMoldClass read fMoldClass;
  end;

implementation

uses
  Classes;

var
  G_RegisteredPersistenceAdapters: TStringList;

{ TBoldPersistentObjectAdapter }

constructor TBoldObjectPersistenceAdapter.Create(AdaptedInterface: IBoldObjectContents; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass);
begin
  inherited create;
  fAdaptedInterface := AdaptedInterface;
  fValueSpace := ValueSpace;
  fMoldClass := MoldClass;
end;

class function TBoldObjectPersistenceAdapter.FindRegisteredPersistenceInterface(
  const ExpressionName: String): TBoldObjectPersistenceAdapterClass;
var
  i: integer;
begin
  result := nil;
  if assigned(G_RegisteredPersistenceAdapters) then
  begin
    I := G_RegisteredPersistenceAdapters.IndexOf(ExpressionName);
    if i <> -1 then
      result := TBoldObjectPersistenceAdapterClass(G_RegisteredPersistenceAdapters.Objects[i]);
  end;
end;

function TBoldObjectPersistenceAdapter.GetAsObjectContents: IBoldObjectContents;
begin
  result := AdaptedInterface;
end;

class procedure TBoldObjectPersistenceAdapter.RegisterPersistenceInterface(
  Adapter: TBoldObjectPersistenceAdapterClass;
  const ExpressionName: String);
begin
  if not assigned(G_RegisteredPersistenceAdapters) then
  begin
    G_RegisteredPersistenceAdapters := TStringList.Create;
    G_RegisteredPersistenceAdapters.Sorted := true;
  end;
  G_RegisteredPersistenceAdapters.AddObject(ExpressionName, TObject(Adapter));
end;

end.
