
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldDataSetPropertyEditors;

interface

uses
  DesignEditors,
  DesignIntf;

type
  { forward declarations }
  TBoldDataSetEditor = class;

  { TBoldDataSetEditor }
  TBoldDataSetEditor = class(TComponentEditor)
  private
    procedure GetPropEditProc(const Prop: IProperty);
  protected
    procedure EditFields;
    procedure ClearFields;
    procedure CreateDefaultFields;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  BoldDataset,
  TypInfo;

type
  TBoldExposedAbstractDataSet = class(TBoldAbstractDataset);

{ TBoldDataSetEditor }

procedure TBoldDataSetEditor.CreateDefaultFields;
begin
  if Component is TBoldAbstractDataSet and
      Assigned(TBoldExposedAbstractDataSet(Component).BoldHandle) then
    with TBoldAbstractDataSet(Component) do
      CreateDefaultFields;
end;

procedure TBoldDataSetEditor.ClearFields;
begin
  if Component is TBoldAbstractDataSet then
    with TBoldExposedAbstractDataSet(Component) do
      DeleteAllFields;
end;

procedure TBoldDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: EditFields;
    1: CreateDefaultFields;
    2: ClearFields;
  end;
end;

function TBoldDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Fields Editor...';
    1: Result := 'Create default fields';
    2: Result := 'Clear all columns';
  end;
end;

function TBoldDataSetEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

procedure TBoldDataSetEditor.EditFields;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  Components.Add(Component);

  GetComponentProperties(Components,
                         [tkClass],
                         Designer,
                         GetPropEditProc, nil);
end;

procedure TBoldDataSetEditor.GetPropEditProc(const Prop: IProperty);
begin
  if Prop.GetName = 'FieldDescriptions' then
    Prop.Edit;
end;

end.