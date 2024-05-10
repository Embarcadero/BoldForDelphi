unit BoldDirectoryTraverser;

interface

uses
  SysUtils,
  Classes;

type
  TBoldOnDirectoryEvent = procedure(CurrentDirectory: String) of object;
  TBoldOnFileEvent = procedure(CurrentDirectory: string; CurrentFile: TSearchRec) of object;
  TBoldTraverseAlgorithm = (taPreOrder, taPostOrder);

  TBoldDirectoryTraverser = class(TComponent)
  private
    fPath: String;
    fOnDirectory: TBoldOnDirectoryEvent;
    fOnFile: TBoldOnFileEvent;
    fMask: string;
    fTraverseAlgorithm: TBoldTraverseAlgorithm;
  protected
    procedure ExecuteRecursive(Path: String);
    procedure TraverseFiles(path: String);
    procedure ExecuteFile(CurrentDirectory: string; CurrentFile: TSearchRec); virtual;
    procedure ExecuteDirectory(CurrentDirectory: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    property Path: String read fPath write fPath;
    property Mask: string read fMask write fMask;
    property OnDirectory: TBoldOnDirectoryEvent read fOnDirectory write fOnDirectory;
    property OnFile: TBoldOnFileEvent read fOnFile write fOnFile;
    property TraverseAlgorithm: TBoldTraverseAlgorithm read FTraverseAlgorithm write fTraverseAlgorithm;
  end;

implementation

uses
  BoldCoreConsts,
  BoldDefs,
  BoldUtils;

{ TBoldDirectoryTraverser }

constructor TBoldDirectoryTraverser.Create(AOwner: TComponent);
begin
  inherited;
  fMask := '*.*';
end;

procedure TBoldDirectoryTraverser.ExecuteRecursive(Path: String);
var
  res: integer;
  SearchRec: TSearchRec;
begin
  if TraverseAlgorithm = taPreOrder then
    TraverseFiles(path);
  if assigned(onDirectory) then
    OnDirectory(path);
  ExecuteDirectory(path);

  res := findfirst(path+'*', faDirectory, SearchRec);
  while res = 0 do
  begin
    if (searchRec.Name <> '.') and
       (searchRec.name <> '..') and
       (SearchRec.Attr and faDirectory = faDirectory) then
      ExecuteRecursive(Path + SearchRec.Name + PathDelim);
    res := FindNext(SearchRec);
  end;
  if TraverseAlgorithm = taPostOrder then
    TraverseFiles(Path)
end;

procedure TBoldDirectoryTraverser.Execute;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  if (pos(DriveDelim, Path) = 2) and (DiskSize(ord(upcase(path[1])) - 64) = -1) then
    raise EBold.CreateFmt(sInvalidDrive, [classname, Path]);
  if not DirectoryExists(Path) then
    raise EBold.CreateFmt(sPathDoesNotExist, [ClassName, Path]);

  ExecuteRecursive(Path)
end;

procedure TBoldDirectoryTraverser.TraverseFiles(Path: String);
var
  res: integer;
  SearchRec: TSearchRec;
begin
  res := FindFirst(Path + Mask, 0, SearchRec);
  while res = 0 do
  begin
    if assigned(OnFile) then
      OnFile(Path, SearchRec);
    ExecuteFile(path, SearchRec);
    res := FindNext(SearchRec);
  end;
end;

procedure TBoldDirectoryTraverser.ExecuteDirectory(CurrentDirectory: string);
begin
  // for subclassing
end;

procedure TBoldDirectoryTraverser.ExecuteFile(CurrentDirectory: string; CurrentFile: TSearchRec);
begin
  // for subclassing
end;

end.
