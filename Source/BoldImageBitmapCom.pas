
{ Global compiler directives }
{$include bold.inc}
unit BoldImageBitmapCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Windows,
  Classes,
  Graphics,
  Clipbrd,
  BoldViewerControlPackCom;

type
  { forward declaration }
  TBoldViewBitmapAdapterCom = class;

  { TBoldViewBitmapAdapterCom }
  TBoldViewBitmapAdapterCom = class(TBoldAbstractViewAdapterCom)
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

{-- TBoldViewBitmapAdapterCom --}

constructor TBoldViewBitmapAdapterCom.Create;
begin
  inherited Create;
end;

destructor TBoldViewBitmapAdapterCom.Destroy;
begin
  FreeAndNil(fBitMap);
  inherited Destroy;
end;

{Content}
procedure TBoldViewBitmapAdapterCom.EnsureBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
end;

function TBoldViewBitmapAdapterCom.Empty: Boolean;
begin
  Result := not Assigned(FBitmap) or FBitmap.Empty;
end;

procedure TBoldViewBitmapAdapterCom.Clear;
begin
  FreeAndNil(FBitmap);
end;

function TBoldViewBitmapAdapterCom.HasChanged: Boolean;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Modified
  else
    Result := False;
end;

class function TBoldViewBitmapAdapterCom.CanReadContent(const ContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(ContentType);
  Result := (S = '') or
            (S = 'image/bitmap') or
            (S = 'image/bmp');
end;

function TBoldViewBitmapAdapterCom.ContentType: string;
begin
  if Empty then
    Result := ''
  else
    Result := 'image/bitmap'
end;

class function TBoldViewBitmapAdapterCom.Description: string;
begin
  Result := 'Bitmap image'
end;

{Clipboard}
procedure TBoldViewBitmapAdapterCom.CopyToClipboard;
begin
  if not Empty then
    Clipboard.Assign(FBitmap);
end;

class function TBoldViewBitmapAdapterCom.CanPasteFromClipboard(const AcceptedContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(AcceptedContentType);
  Result := Clipboard.HasFormat(CF_BITMAP) and
            ((S = '') or
             (S = 'image/*') or
             (S = 'image/bitmap'));
end;

procedure TBoldViewBitmapAdapterCom.PasteFromClipboard;
begin
  EnsureBitmap;
  FBitmap.Assign(Clipboard);
end;

{Streams}
procedure TBoldViewBitmapAdapterCom.LoadFromStream(Stream: TStream);
begin
  EnsureBitmap;
  FBitmap.LoadFromStream(Stream);
end;

procedure TBoldViewBitmapAdapterCom.SaveToStream(Stream: TStream);
begin
  EnsureBitmap;
  FBitmap.SaveToStream(Stream);
end;

{Files}
class function TBoldViewBitmapAdapterCom.DefaultExtension: string;
begin
  Result := 'bmp';
end;

class function TBoldViewBitmapAdapterCom.FileFilter: string;
begin
  Result := Format('%s (*.bmp)|*.bmp', [Description]);
end;

class function TBoldViewBitmapAdapterCom.CanLoadFromFile(const Filename: string): Boolean;
var
  Extension: string;
begin
  Extension := ExtractFileExt(FileName);
  Extension := Copy(Extension, 2, Length(Extension));
  Result := CompareText(Extension, 'bmp') = 0;
end;

procedure TBoldViewBitmapAdapterCom.LoadFromFile(const Filename: string);
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

procedure TBoldViewBitmapAdapterCom.SaveToFile(const Filename: string);
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
procedure TBoldViewBitmapAdapterCom.Paint(Canvas: TCanvas; Rect: TRect);
begin
  Canvas.StretchDraw(Rect, FBitmap);
end;

function TBoldViewBitmapAdapterCom.GetPalette: HPALETTE;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Palette
  else
    Result := 0;
end;

function TBoldViewBitmapAdapterCom.Width: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Width
  else
    Result := 0;
end;

function TBoldViewBitmapAdapterCom.Height: Integer;
begin
  if Assigned(FBitmap) then
    Result := FBitmap.Height
  else
    Result := 0;
end;

initialization
  TBoldViewBitmapAdapterCom.RegisterViewAdapter(TBoldViewBitmapAdapterCom);
  
end.
