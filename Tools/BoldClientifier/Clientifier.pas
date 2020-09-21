unit Clientifier;

interface
uses
  Inifiles,
  Classes,
  BoldUtils,
  sysutils,
  BoldMeta,
  FileReplacer;

type
  TBoldClientifier = class( TIniFile )
  private
    function GetExcludeFiles: TStringList;
    function GetRenderers: TStringList;
  private
    fExcludeFiles: TStringList;
    fNewPacknames: TStringList;
    fControlPacknames: TStringList;
    fComponentNames: TStringList;
    fBoldNames: TStringList;
    fUnitNames: TStringList;
    fRenderers: TStringList;
    function GetUnitNames: TStringList;
    function GetBoldNames: TStringList;
    function GetComponentNames: TStringList;
    function InitSection(var StringList: TStringList;
      Section: String): TStringList;
    function GetControlPackNames: TStringList;
    function GetNewNames: TStringList;
  protected
    property NewNames: TStringList read GetNewNames;
    property ComponentNames: TStringList read GetComponentNames;
    property BoldNames: TStringList read GetBoldNames;
    procedure UpdateWords( FileContents: TStringList; ReplacementDefinitions: TStringList );
    property ControlPackNames: TStringList read GetControlPackNames;
  public
    destructor destroy; override;
    procedure UpdateDfm( FileContents: TStringList);
    procedure UpdatePascal( FileContents: TStringList);
    procedure UpdateModelStuff( FileContents: TStringList; MoldModel: TMoldModel );
    function ExcludeFile( FileName: String ): Boolean;
    property UnitNames: TStringList read GetUnitNames;
    property Renderers: TStringList read GetRenderers;
    property ExcludeFiles: TStringList read GetExcludeFiles;
  end;

implementation


{ TBoldClientifier }

destructor TBoldClientifier.destroy;
begin
  inherited;
  freeAndNil( fBoldNames );
  freeAndNil( fUnitNames );
  freeAndNil( fComponentNames );
end;

function TBoldClientifier.InitSection( var StringList: TStringList; Section: String ): TStringList;
var
  i: integer;
  source, dest: string;
begin
  if not assigned( StringList ) then
  begin
    StringList := TStringList.Create;

    ReadSectionValues(Section, StringList);
    for i := 0 to StringList.Count-1 do begin
      Source := StringList.Names[i];
      Dest:= stringList.Values[Source];
      if CompareText( source, dest ) = 0 then
        raise Exception.CreateFmt( 'Illegal substitution: %s=%s in section %s', [source, dest, Section] )
    end;
    StringList.Text := ReplaceString(StringList.Text, '%datetime%', formatDateTime('yyyy-mm-dd hh:nn:ss', now), true);
  end;
  result := StringList;
end;


function TBoldClientifier.GetBoldNames: TStringList;
begin
  result := InitSection( fBoldNames, 'BoldNames' );
end;

function TBoldClientifier.GetComponentNames: TStringList;
begin
  result := InitSection( fComponentNames, 'ComponentNames' );
end;

function TBoldClientifier.GetUnitNames: TStringList;
begin
  result := InitSection( fUnitNames, 'UnitNames' );
end;



procedure TBoldClientifier.UpdateDfm(FileContents: TStringList);
begin
  UpdateWords( FileContents, ComponentNames );
  UpdateWords( FileContents, Renderers );
end;

procedure TBoldClientifier.UpdatePascal(FileContents: TStringList);
begin
  UpdateWords( FileContents, UnitNames );
  UpdateWords( FileContents, ComponentNames );
  UpdateWords( FileContents, BoldNames );
  UpdateWords( FileContents, ControlPackNames );
  UpdateWords( FileContents, Renderers );
  UpdateWords( FileContents, NewNames );
end;

procedure TBoldClientifier.UpdateWords(FileContents, ReplacementDefinitions: TStringList);
var
  Source, Dest: String;
  i, j: integer;
begin
  for i := 0 to FileContents.Count-1 do
    for j := 0 to ReplacementDefinitions.Count-1 do
    begin
      Source := ReplacementDefinitions.Names[j];
      Dest:= ReplacementDefinitions.Values[Source];
      FileContents[i] := ReplaceString(
          FileContents[i], Source, dest, true )
    end;
end;

function TBoldClientifier.GetControlPackNames: TStringList;
begin
  result := InitSection( fControlPacknames, 'ControlPackNames' );
end;

function TBoldClientifier.GetNewNames: TStringList;
begin
  result := InitSection( fNewPacknames, 'New' );
end;

function TBoldClientifier.GetExcludeFiles: TStringList;
begin
  result := InitSection( fExcludeFiles, 'Exclude' );
end;

function TBoldClientifier.ExcludeFile(FileName: String): Boolean;
begin
  result := ExcludeFiles.Values[ChangeFileExt( FileName, '' )] = '1'; 
end;

procedure TBoldClientifier.UpdateModelStuff(FileContents: TStringList;
  MoldModel: TMoldModel);
var
  TypeNames: TStringList;
  i: integer;
begin
  TypeNames := tStringList.Create;
  for i := 0 to MoldModel.Classes.Count-1 do
    TypeNames.Add( format( '%s=%s', [MoldModel.Classes[i].ExpandedDelphiName, MoldModel.Classes[i].ExpandedInterfaceName]));

  UpdateWords( FileContents, TypeNames );
  TypeNames.Free;
end;

function TBoldClientifier.GetRenderers: TStringList;
begin
  result := InitSection( fRenderers, 'Renderers' );
end;

end.

