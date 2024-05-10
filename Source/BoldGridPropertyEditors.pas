
{ Global compiler directives }
{$include bold.inc}
unit BoldGridPropertyEditors;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  DesignEditors,
  DesignIntf;

type
  { forward declarations }
  TBoldColumnsEditor = class;

  { TBoldColumnsEditor }
  TBoldColumnsEditor = class(TComponentEditor)
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
  BoldGrid,
  TypInfo;

type
  TBoldExposedCustomGrid = class(TBoldCustomGrid);

{---TBoldColumnsEditor---}
procedure TBoldColumnsEditor.EditPropertyColumns(const PropertyEditor: IProperty);
begin
  if SameText(PropertyEditor.GetName, 'Columns') then
    PropertyEditor.Edit;
end;

procedure TBoldColumnsEditor.Edit;
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

procedure TBoldColumnsEditor.CreateDefaultColumns;
begin
  if Component is TBoldCustomGrid and
      Assigned(TBoldExposedCustomGrid(Component).BoldHandle) then
    with TBoldExposedCustomGrid(Component) do
      DefaultColumns;
end;

procedure TBoldColumnsEditor.EmptyColumns;
begin
  with TBoldExposedCustomGrid(Component) do
  begin
    DeleteAllColumns;
    AddColumn;
    AddColumn;
    Columns[1].Free;
  end;
end;

procedure TBoldColumnsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: CreateDefaultColumns;
    2: EmptyColumns;
  end;
end;

function TBoldColumnsEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Edit Columns';
    1: Result := 'Create Default Columns';
    2: Result := 'Clear all Columns';
  end;
end;

function TBoldColumnsEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;


end.
