
{ Global compiler directives }
{$include bold.inc}
unit BoldGridPropertyEditorsCom;

interface

uses
  {$IFDEF BOLD_DELPHI5_OR_LATER}
  Contnrs,
  {$ENDIF}
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  TypInfo;

type
  { TBoldColumnsEditor }
  TBoldColumnsEditorCom = class(TComponentEditor)
  private
    {$IFDEF BOLD_DELPHI6_OR_LATER}
    procedure EditPropertyColumns(const PropertyEditor: IProperty);
    {$ELSE}
    procedure EditPropertyColumns(PropertyEditor: TPropertyEditor);
    {$ENDIF}
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
  BoldUtils,
  BoldGridCom;

type
  TBoldExposedCustomGridCom = class(TBoldCustomGridCom);


{---TBoldColumnsEditor---}
{$IFDEF BOLD_DELPHI6_OR_LATER}
procedure TBoldColumnsEditorCom.EditPropertyColumns(const PropertyEditor: IProperty);
{$ELSE}
procedure TBoldColumnsEditorCom.EditPropertyColumns(PropertyEditor: TPropertyEditor);
{$ENDIF}
begin
  if SameText(PropertyEditor.GetName, 'Columns') then
    PropertyEditor.Edit;
end;

procedure TBoldColumnsEditorCom.Edit;
var
{$IFDEF BOLD_DELPHI6_OR_LATER}
  Components: IDesignerSelections;
{$ELSE}
  Components: TDesignerSelectionList;
{$ENDIF}
begin
{$IFDEF BOLD_DELPHI6_OR_LATER}
  Components := TDesignerSelections.Create;
{$ELSE}
  Components := TDesignerSelectionList.Create;
{$ENDIF}
  try
    Components.Add(Component);
    GetComponentProperties(Components,
                           tkProperties,
                           Designer,
                           EditPropertyColumns
                           {$IFDEF BOLD_DELPHI6_OR_LATER}, nil{$ENDIF});
  finally
    {$IFDEF BOLD_DELPHI6_OR_LATER}
    Components := nil;
    {$ELSE}
    Components.Free;
    {$ENDIF}
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
    AddColumn;
    Columns[1].Free;
  end;
end;

procedure TBoldColumnsEditorCom.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1:{ CreateDefaultColumns;
    2: }EmptyColumns;
  end;
end;

function TBoldColumnsEditorCom.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Edit Columns';
    1:{ Result := 'Create Default Columns';
    2: }Result := 'Clear all Columns';
  end;
end;

function TBoldColumnsEditorCom.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
