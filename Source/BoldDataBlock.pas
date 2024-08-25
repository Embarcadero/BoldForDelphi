
{ Global compiler directives }
{$include bold.inc}
unit BoldDataBlock;

interface

uses
  Classes,
  BoldBase;

resourcestring
  SInvalidDataPacket = 'Invalid data packet';

type
  {forward declarations}
  TBoldDataBlock = class;

  { TBoldDataBlock }
  TBoldDataBlock = class(TBoldMemoryManagedObject)
  private
    FStream: TMemoryStream;
    FReadPos: Integer;
    FWritePos: Integer;
  protected
    function GetSize: Integer;
    procedure SetSize(Value: Integer);
    function GetStream: TStream;
    function GetMemory: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Write(const Buffer; Count: Integer): Integer;
    function Read(var Buffer; Count: Integer): Integer;
    property Size: Integer read GetSize write SetSize;
    property Stream: TStream read GetStream;
    property Memory: Pointer read GetMemory;
  end;

implementation

uses
  SysUtils;

{ TBoldDataBlock}

function TBoldDataBlock.GetSize: Integer;
begin
  Result := FStream.Size;
end;

procedure TBoldDataBlock.SetSize(Value: Integer);
begin
  FStream.Size := Value;
end;

function TBoldDataBlock.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TBoldDataBlock.Clear;
begin
  FStream.size := 0;
  FReadPos := 0;
  FWritePos := 0;
end;

constructor TBoldDataBlock.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  Clear;
end;

destructor TBoldDataBlock.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TBoldDataBlock.Read(var Buffer; Count: Integer): Integer;
begin
  FStream.Position := FReadPos;
  Result := FStream.Read(Buffer, Count);
  FReadPos := FStream.Position;
end;

function TBoldDataBlock.Write(const Buffer; Count: Integer): Integer;
begin
  FStream.Position := FWritePos;
  Result := FStream.Write(Buffer, Count);
  FWritePos := FStream.Position;
end;

function TBoldDataBlock.GetMemory: Pointer;
begin
  Result := FStream.Memory;
end;

end.
