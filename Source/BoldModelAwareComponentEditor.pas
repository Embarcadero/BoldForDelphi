
{ Global compiler directives }
{$include bold.inc}
unit BoldModelAwareComponentEditor;

interface

uses
  DesignEditors,
  Classes,
  BoldAbstractModel;

type
  { forward declarations }
  TBoldModelAwareComponentEditor = class;

  { TBoldModelAwareComponentEditor }
  TBoldModelAwareComponentEditor = class(TComponentEditor)
  private
    function FollowUplinkToModel(aComponent: TComponent): TBoldAbstractModel;
  public
    function GetEditModelMenuCaption: string;
    procedure EditModel;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils,
  TypInfo,

  BoldCoreConsts,
  BoldDefs,
  BoldDefsDT;

const
  PropNames: array[0..5] of string =
              ('RootHandle',
               'StaticSystemHandle',
               'BoldSystemHandle',
               'SystemTypeInfoHandle',
               'BoldModel',
               'BoldHandle');

procedure TBoldModelAwareComponentEditor.EditModel;
var
  Model: TBoldAbstractModel;
begin
  Model := FollowUplinkToModel(Component);
  if Assigned(Model) then
    // create and show the model editor
    GetComponentEditor(Model, Designer).Edit
  else
    raise EBold.CreateFmt(sNoModel, [Component.Name]);
end;

function TBoldModelAwareComponentEditor.GetEditModelMenuCaption: string;
begin
  Result := BOLD_OPENUMLEDITOR;
end;

procedure TBoldModelAwareComponentEditor.ExecuteVerb(Index: Integer);
begin
  EditModel;
end;

function TBoldModelAwareComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := GetEditModelMenuCaption;
end;

function TBoldModelAwareComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TBoldModelAwareComponentEditor.FollowUplinkToModel(
  aComponent: TComponent): TBoldAbstractModel;
var
  i: integer;
  PropName: string;
  PropHandle: TObject;
  PropInfo: PPropInfo;
  Found: Boolean;
begin
  PropHandle := nil;
  Found := false;
  if Assigned(aComponent) then
    for i := 0 to High(PropNames) do
    begin
      PropName := PropNames[i];
      PropInfo := GetPropInfo(aComponent.ClassInfo, PropName, [tkClass]);
      if  PropInfo <> nil then
        PropHandle := GetObjectProp(aComponent, PropName);
      if Assigned(PropHandle) then
      begin
        Found := true;
        Break;
      end;
    end;
  if Found then
  begin
    if PropHandle is TBoldAbstractModel then
      Result := PropHandle as TBoldAbstractModel
    else
      Result := FollowUplinkToModel(PropHandle as TComponent);
  end
  else
    Result := nil;
end;

end.
