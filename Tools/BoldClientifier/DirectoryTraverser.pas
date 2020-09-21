unit DirectoryTraverser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  FileCtrl;

type
  TOnDirectoryEvent = procedure( CurrentDirectory : String ) of object;
  TOnFileEvent = procedure( CurrentDirectory : string; CurrentFile : TSearchRec ) of object;
  TTraverseAlgorithm = (taPreOrder, taPostOrder);

  TDirectoryTraverser = class(TComponent)
  private
    FPath: String;
    FonDirectory: TOnDirectoryEvent;
    FonFile: TOnFileEvent;
    FMask: string;
    FTraverseAlgorithm: TTraverseAlgorithm;
    procedure SetPath(const Value: String);
    procedure SetonDirectory(const Value: TOnDirectoryEvent);
    procedure SetonFile(const Value: TOnFileEvent);
    procedure SetMask(const Value: string);
    procedure SetTraverseAlgorithm(const Value: TTraverseAlgorithm);
    { Private declarations }
  protected
    { Protected declarations }
    procedure ExecuteRecursive( Path : String );
    procedure TraverseFiles( path : String );
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    { Published declarations }
    property Path : String read FPath write SetPath;
    property Mask : string read FMask write SetMask;
    property onDirectory : TOnDirectoryEvent read FonDirectory write SetonDirectory;
    property onFile : TOnFileEvent read FonFile write SetonFile;
    property TraverseAlgorithm : TTraverseAlgorithm read FTraverseAlgorithm write SetTraverseAlgorithm;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Bold', [TDirectoryTraverser]);
end;

{ TDirectoryTraverser }

constructor TDirectoryTraverser.Create(AOwner: TComponent);
begin
  inherited;
  fMask := '*.*';
end;

procedure TDirectoryTraverser.ExecuteRecursive(Path: String);
var
  res : integer;
  SearchRec : TSearchRec;
begin
  if TraverseAlgorithm = taPreOrder then
    TraverseFiles( path );
  if assigned( onDirectory ) then
    OnDirectory( path );

  res := findfirst( path+'*', faDirectory, SearchRec );
  while res = 0 do begin
    if (searchRec.Name <> '.' ) and
      ( searchRec.name <> '..' ) and
      ( SearchRec.Attr and faDirectory = faDirectory) then
      ExecuteRecursive( Path + SearchRec.Name+'\' );
    res := FindNext( SearchRec );
  end;
  if TraverseAlgorithm = taPostOrder then
    TraverseFiles( Path )
end;

procedure TDirectoryTraverser.Execute;
begin
  if fPAth[Length( fPath ) ] <> '\' then
    fPath := fPath + '\';

  if DirectoryExists( fPAth ) then
    ExecuteRecursive( fPath )
  else
    raise Exception.CreateFmt( 'TDirectoryTraverser.Execute: Path does not exist (%s)', [fPath] );
end;


procedure TDirectoryTraverser.SetMask(const Value: string);
begin
  FMask := Value;
end;

procedure TDirectoryTraverser.SetonDirectory(
  const Value: TOnDirectoryEvent);
begin
  FonDirectory := Value;
end;

procedure TDirectoryTraverser.SetonFile(const Value: TOnFileEvent);
begin
  FonFile := Value;
end;

procedure TDirectoryTraverser.SetPath(const Value: String);
begin
  FPath := Value;
end;

procedure TDirectoryTraverser.SetTraverseAlgorithm(
  const Value: TTraverseAlgorithm);
begin
  FTraverseAlgorithm := Value;
end;

procedure TDirectoryTraverser.TraverseFiles(path: String);
var
  res : integer;
  SearchRec : TSearchRec;
begin
  res := FindFirst( path + fMask, 0, SearchRec );
  while res = 0 do begin
    if assigned( OnFile ) then
      OnFile( Path, searchRec );
    res := FindNext( SearchRec );
  end;
end;

end.
