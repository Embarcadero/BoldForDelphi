
{ Global compiler directives }
{$include bold.inc}
unit BoldImageBitmap;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Classes,
  Graphics,
  Clipbrd,
  BoldViewerControlPack;

type
  { forward declaration }
  TBoldViewBitmapAdapter = class;

  { TBoldViewBitmapAdapter }
  TBoldViewBitmapAdapter = class(TBoldAbstractViewAdapter)
  private
    FBitmap: TBitmap;
    procedure EnsureBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    {Content}
    function Empty: Boolean; override;
    procedure Clear; override;
    function HasChanged: Boolean; override;
    class function CanReadContent(const ContentType: string): Boolean; override;
    function ContentType: string; override;
    class function Description: string; override;
    {Clipboard}
    procedure CopyToClipboard; override;
    class function CanPasteFromClipboard(const AcceptedContentType: string): Boolean; override;
    procedure PasteFromClipboard; override;
    {Streams}
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    {Files}
    class function DefaultExtension: string; override;
    class function FileFilter: string; override;
    class function CanLoadFromFile(const Filename: string): Boolean; override;
    procedure LoadFromFile(const Filename: string); override;
    procedure SaveToFile(const Filename: string); override;
    {Canvas}
    procedure Paint(Canvas: TCanvas; Rect: TRect); override;
    function GetPalette: HPALETTE; override;
    function Width: Integer; override;
    function Height: Integer; override;
  end;

implementation

uses
  SysUtils;

{-- TBoldViewBitmapAdapter --}

constructor TBoldViewBitmapAdapter.Create;
begin
  inherited Create;
end;

destructor TBoldViewBitmapAdapter.Destroy;
begin
  FreeAndNil(fBitMap);
  inherited Destroy;
end;

{Content}
procedure TBoldViewBitmapAdapter.EnsureBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
end;

function TBoldViewBitmapAdapter.Empty: Boolean;
begin
  Result := not Assigned(FBitmap) or FBitmap.Empty;
end;

procedure TBoldViewBitmapAdapter.Clear;
begin
  FreeAndNil(FBitmap);
end;

function TBoldViewBitmapAdapter.HasChanged: Boolean;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Modified
  else
    Result := False;
end;

class function TBoldViewBitmapAdapter.CanReadContent(const ContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(ContentType);
  Result := (S = '') or
            (S = 'image/bitmap') or
            (S = 'image/bmp');
end;

function TBoldViewBitmapAdapter.ContentType: string;
begin
  if Empty then
    Result := ''
  else
    Result := 'image/bitmap'
end;

class function TBoldViewBitmapAdapter.Description: string;
begin
  Result := 'Bitmap image'
end;

{Clipboard}
procedure TBoldViewBitmapAdapter.CopyToClipboard;
begin
  if not Empty then
    Clipboard.Assign(FBitmap);
end;

class function TBoldViewBitmapAdapter.CanPasteFromClipboard(const AcceptedContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(AcceptedContentType);
  Result := Clipboard.HasFormat(CF_BITMAP) and
            ((S = '') or
             (S = 'image/*') or
             (S = 'image/bitmap'));
end;

procedure TBoldViewBitmapAdapter.PasteFromClipboard;
begin
  EnsureBitmap;
  FBitmap.Assign(Clipboard);
end;

{Streams}
procedure TBoldViewBitmapAdapter.LoadFromStream(Stream: TStream);
begin
  EnsureBitmap;
  FBitmap.LoadFromStream(Stream);
end;

procedure TBoldViewBitmapAdapter.SaveToStream(Stream: TStream);
begin
  EnsureBitmap;
  FBitmap.SaveToStream(Stream);
end;

{Files}
class function TBoldViewBitmapAdapter.DefaultExtension: string;
begin
  Result := 'bmp';
end;

class function TBoldViewBitmapAdapter.FileFilter: string;
begin
  Result := Format('%s (*.bmp)|*.bmp', [Description]);
end;

class function TBoldViewBitmapAdapter.CanLoadFromFile(const Filename: string): Boolean;
var
  Extension: string;
begin
  Extension := ExtractFileExt(FileName);
  Extension := Copy(Extension, 2, Length(Extension));
  Result := CompareText(Extension, 'bmp') = 0;
end;

procedure TBoldViewBitmapAdapter.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBoldViewBitmapAdapter.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{Canvas}
procedure TBoldViewBitmapAdapter.Paint(Canvas: TCanvas; Rect: TRect);
begin
  Canvas.StretchDraw(Rect, FBitmap);
end;

function TBoldViewBitmapAdapter.GetPalette: HPALETTE;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Palette
  else
    Result := 0;
end;

function TBoldViewBitmapAdapter.Width: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Width
  else
    Result := 0;
end;

function TBoldViewBitmapAdapter.Height: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Height
  else
    Result := 0;
end;

initialization
  TBoldViewBitmapAdapter.RegisterViewAdapter(TBoldViewBitmapAdapter);
  
end.
