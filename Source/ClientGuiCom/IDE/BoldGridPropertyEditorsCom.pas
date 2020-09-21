// MANUALLY UPDATED
unit BoldGridPropertyEditorsCom;

interface

uses
  DesignIntf,
  DesignEditors,
  TypInfo;

type
  { TBoldColumnsEditor }
  TBoldColumnsEditorCom = class(TComponentEditor)
  private
    procedure EditPropertyColumns(const PropertyEditor: IProperty);
  public
    procedure EmptyColumns;
    procedure CreateDefaultColumns;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

implementation

uses
  SysUtils,
  BoldRev,
  BoldUtils,
  BoldGridCom;

type
  TBoldExposedCustomGridCom = class(TBoldCustomGridCom);


{---TBoldColumnsEditor---}
procedure TBoldColumnsEditorCom.EditPropertyColumns(const PropertyEditor: IProperty);
begin
  if SameText(PropertyEditor.GetName, 'Columns') then
    PropertyEditor.Edit;
end;

procedure TBoldColumnsEditorCom.Edit;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  try
    Components.Add(Component);
    GetComponentProperties(Components,
                           tkProperties,
                           Designer,
                           EditPropertyColumns,
                           nil);
  finally
    Components := nil;
  end;
end;

procedure TBoldColumnsEditorCom.CreateDefaultColumns;
begin
  if Component is TBoldCustomGridCom and
      Assigned(TBoldExposedCustomGridCom(Component).BoldHandle) then
    with TBoldExposedCustomGridCom(Component) do
      DefaultColumns;
end;

procedure TBoldColumnsEditorCom.EmptyColumns;
begin
  with TBoldExposedCustomGridCom(Component) do
  begin
    DeleteAllColumns;
    AddColumn;
    // This is done because the grid screws up the column widths
    AddColumn;
    Columns[1].Free;
  end;
end;

procedure TBoldColumnsEditorCom.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1:{ CreateDefaultColumns;
    2: }EmptyColumns;  // manual fix
  end;
end;

function TBoldColumnsEditorCom.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Edit Columns';
    1:{ Result := 'Create Default Columns';
    2: }Result := 'Clear all Columns';  // manual fix
  end;
end;

function TBoldColumnsEditorCom.GetVerbCount: Integer;
begin
  Result := 2;     // manual fix
end;

initialization
end.
