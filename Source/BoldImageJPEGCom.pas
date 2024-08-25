
{ Global compiler directives }
{$include bold.inc}
unit BoldImageJPEGCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Windows,
  Classes,
  Graphics,
  Clipbrd,
  Jpeg,
  BoldViewerControlPackCom;

type
  { forward declarations }
  TBoldViewJPEGAdapterCom = class;

  { TBoldViewBitmapAdapterCom }
  TBoldViewJPEGAdapterCom = class(TBoldAbstractViewAdapterCom)
  private
    FJPEGImage: TJPEGImage;
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
  SysUtils

{-- TBoldViewJPEGAdapterCom --}

constructor TBoldViewJPEGAdapterCom.Create;
begin
  inherited Create;
end;

destructor TBoldViewJPEGAdapterCom.Destroy;
begin
  FreeAndNil(FJPEGImage);
  inherited Destroy;
end;

{Content}
procedure TBoldViewJPEGAdapterCom.EnsureBitmap;
begin
  if not Assigned(FJPEGImage) then
    FJPEGImage := TJPEGImage.Create;
end;

function TBoldViewJPEGAdapterCom.Empty: Boolean;
begin
  Result := not Assigned(FJPEGImage) or FJPEGImage.Empty;
end;

procedure TBoldViewJPEGAdapterCom.Clear;
begin
  FreeAndNil(FJPEGImage);
end;

function TBoldViewJPEGAdapterCom.HasChanged: Boolean;
begin
  if Assigned(FJPEGImage) then
    Result := FJPEGImage.Modified
  else
    Result := False;
end;

class function TBoldViewJPEGAdapterCom.CanReadContent(const ContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(ContentType);
  Result := (S = 'image/jpeg');
end;

function TBoldViewJPEGAdapterCom.ContentType: string;
begin
  if Empty then
    Result := ''
  else
    Result := 'image/jpeg'
end;

class function TBoldViewJPEGAdapterCom.Description: string;
begin
  Result := 'JPEG image'
end;

{Clipboard}
procedure TBoldViewJPEGAdapterCom.CopyToClipboard;
begin
  if not Empty then
    Clipboard.Assign(FJPEGImage);
end;

class function TBoldViewJPEGAdapterCom.CanPasteFromClipboard(const AcceptedContentType: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(AcceptedContentType);
  Result := Clipboard.HasFormat(CF_BITMAP) and
            ((S = '') or (S = 'image/*') or (S = 'image/jpeg'));
end;

type
  THack = class(TJPEGImage)
  end;

procedure TBoldViewJPEGAdapterCom.PasteFromClipboard;
var
  Data: THandle;
  Palette: HPALETTE;
begin
  EnsureBitmap;
  Clipboard.Open;
  try
    Data := GetClipboardData(CF_BITMAP);
    Palette := GetClipboardData(CF_PALETTE);
    THack(FJPEGImage).NewBitmap;
    FJPEGImage.LoadFromClipboardFormat(CF_BITMAP, Data, Palette);
  finally
    Clipboard.Close;
  end;
end;


{Streams}
procedure TBoldViewJPEGAdapterCom.LoadFromStream(Stream: TStream);
begin
  EnsureBitmap;
  FJPEGImage.LoadFromStream(Stream);
end;

procedure TBoldViewJPEGAdapterCom.SaveToStream(Stream: TStream);
begin
  EnsureBitmap;
  FJPEGImage.SaveToStream(Stream);
end;

{Files}
class function TBoldViewJPEGAdapterCom.DefaultExtension: string;
begin
  Result := 'jpg';
end;

class function TBoldViewJPEGAdapterCom.FileFilter: string;
begin
  Result := Format('%s (*.jpg, *.jpeg)|*.jpg;*.jpeg', [Description]);
end;

class function TBoldViewJPEGAdapterCom.CanLoadFromFile(const Filename: string): Boolean;
var
  Extension: string;
begin
  Extension := ExtractFileExt(FileName);
  Extension := Copy(Extension, 2, Length(Extension));
  Result := (CompareText(Extension, 'jpg') = 0) or
            (CompareText(Extension, 'jpeg') = 0);
end;

procedure TBoldViewJPEGAdapterCom.LoadFromFile(const Filename: string);
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

procedure TBoldViewJPEGAdapterCom.SaveToFile(const Filename: string);
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
procedure TBoldViewJPEGAdapterCom.Paint(Canvas: TCanvas; Rect: TRect);
begin
  Canvas.StretchDraw(Rect, FJPEGImage);
end;

function TBoldViewJPEGAdapterCom.GetPalette: HPALETTE;
begin
  if Assigned(FJPEGImage) then
    Result := FJPEGImage.Palette
  else
    Result := 0;
end;

function TBoldViewJPEGAdapterCom.Width: Integer;
begin
  if Assigned(FJPEGImage) then
    Result := FJPEGImage.Width
  else
    Result := 0;
end;

function TBoldViewJPEGAdapterCom.Height: Integer;
begin
  if Assigned(FJPEGImage) then
    Result := FJPEGImage.Height
  else
    Result := 0;
end;

initialization
  TBoldViewJPEGAdapterCom.RegisterViewAdapter(TBoldViewJPEGAdapterCom);
end.
