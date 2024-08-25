
{ Global compiler directives }
{$include bold.inc}
unit BoldTextStream;

interface

uses
  Classes,
  IStreams;

{ Derive a class from TStringStream to make it easier to create
  a project expert. }
const
  ADDNEWLINE = True;
  NONEWLINE = False;

type
  { forward declarations }
  TBoldTextStream = class;
  TITextStream = class;
  TTextStream = TBoldTextStream;

  { TBoldTextStream }
  TBoldTextStream = class(TStringStream)
  private
    fIndentLevel: integer;
    function IndentSpace: string;
    function GetIsEmpty: boolean;
  public
    procedure AddFormatted(const Fmt: string; Args: array of const);
    procedure AddString(const s: string);
    procedure Clear;
    procedure Dedent;
    procedure EndBlock(const AddNewLine: boolean);
    procedure FormatLn(const Fmt: string; Args: array of const);
    procedure Indent;
    procedure NewLine;
    procedure SaveToFile(const FileName: string);
    procedure StartBlock;
    procedure WriteLn(const Str: string);
    property IsEmpty: boolean read GetIsEmpty;
  end;

  { Declare an interface stream class for TextStreams. }
  { TITextStream }
  TITextStream = class(TIStreamAdapter)
  private
    function GetTextStream: TBoldTextStream;
  public
    constructor Create(BoldTextStream: TBoldTextStream);
    property TextStream: TBoldTextStream read GetTextStream;
  end;

function AdaptStream(Stream: TStream): TIStreamAdapter;

implementation

uses
  SysUtils,
  BoldDefs;

const
  INDENTSIZE = 2;

function AdaptStream(Stream: TStream): TIStreamAdapter;
begin
  try
    Stream.Position := 0;
    Result := TIStreamAdapter.Create(Stream, soOwned);
  except
    Stream.Free;
    raise;
  end;
end;

{---TBoldTextStream---}
function TBoldTextStream.IndentSpace: string;
begin
  Result := StringOfChar(' ', fIndentLevel * INDENTSIZE);
end;

procedure TBoldTextStream.AddString(const s: string);
begin
  WriteString(IndentSpace + s);
end;

procedure TBoldTextStream.StartBlock;
begin
  Writeln('begin');
  Indent;
end;

procedure TBoldTextStream.EndBlock(const AddNewLine: boolean);
begin
  Dedent;
  Writeln('end;');
  if AddNewLine then
    NewLine;
end;

procedure TBoldTextStream.NewLine;
begin
  WriteString(BOLDCRLF);
end;

procedure TBoldTextStream.WriteLn(const Str: string);
begin
  AddString(Str);
  NewLine;
end;

procedure TBoldTextStream.Indent;
begin
  Inc(fIndentLevel);
end;

procedure TBoldTextStream.Dedent;
begin
  Dec(fIndentLevel);
  if fIndentLevel < 0 then
    fIndentLevel := 0;
end;

procedure TBoldTextStream.FormatLn(const Fmt: string; Args: array of const);
begin
  WriteLn(Format(Fmt, Args))
end;

procedure TBoldTextStream.AddFormatted(const Fmt: string; Args: array of const);
begin
  AddString(Format(Fmt, Args))
end;

{---TITextStream---}
{ Create a new interface text stream object. Pass a nil
  pointer for TextStream to create a new TTextStream that
  will also be freed when this TITextStream is freed.
  Pass a non-nil stream object if the caller wants to
  manage the lifetime of the TextStream object. }
constructor TITextStream.Create(BoldTextStream: TBoldTextStream);
begin
  if BoldTextStream = nil then
    inherited Create(TBoldTextStream.Create(''), soOwned)
  else
    inherited Create(BoldTextStream, soReference);
end;

{ Return the stream as a TTextStream object. }
function TITextStream.GetTextStream: TBoldTextStream;
begin
  Result := Stream as TBoldTextStream;
end;

procedure TBoldTextStream.SaveToFile(const FileName: string);
begin
  with TStringList.Create do
  try
    Text := DataString;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

function TBoldTextStream.GetIsEmpty: boolean;
begin
  Result := Size = 0;
end;

procedure TBoldTextStream.Clear;
begin
  Size := 0;
end;

end.
