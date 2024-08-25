
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectMarshaler;

interface

uses
  syncobjs,
  BoldThreadSafeLog;

type

  TBoldObjectMarshaler = class;

  TBoldObjectMarshaler = class
  protected
    FStream: Pointer;
    FMarshalIID: TGuid;
    function MarshalObject (const obj: IUnknown): boolean;
  public
    constructor Create(const IID: TGUID; const obj: IUnknown);
    destructor Destroy; override;
    function UnMarshalObject (out Obj): boolean;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  comobj,
  ActiveX
  ;

{ TBoldObjectMarshaler }

constructor TBoldObjectMarshaler.Create(const IID: TGUID;
  const obj: IUnknown);
begin
  inherited Create;
  FMarshalIID := IID;
  MarshalObject(obj);
end;

destructor TBoldObjectMarshaler.Destroy;
begin
  try
    if Assigned(FStream) then
    begin
      CoReleaseMarshalData(IStream(FStream));
      IStream(FStream) := nil;
    end;
  except on E: Exception do
    BoldLogError('%s.Destroy: %s', [ClassName, E.Message]);
  end;
  inherited;
end;

function TBoldObjectMarshaler.MarshalObject(
  const obj: IUnknown): boolean;
begin
  Result := false;
  try
    CreateStreamOnHGlobal(0, True, IStream(FStream));
    CoMarshalInterface( IStream(FStream), FMarshalIID, obj,
                      MSHCTX_INPROC, nil, MSHLFLAGS_TABLEWEAK);
    Result := true;
  except on E: Exception do
      BoldLogError('%s.MarshalObject: %s', [ClassName, E.Message]);
  end;
end;

function TBoldObjectMarshaler.UnMarshalObject(out Obj): boolean;
var
  p: {$IFDEF BOLD_DELPHI13_OR_LATER}LargeUInt{$ELSE}int64{$ENDIF};
begin
  Result := false;
  try
    IStream(FStream).Seek(0, STREAM_SEEK_SET, p);
    OleCheck(CoUnmarshalInterface(IStream(fStream), FMarshalIID, Obj));
    Result := True;
  except on E: Exception do
      BoldLogError('%s.UnMarshalObject: %s', [ClassName, E.Message]);
  end;
end;

end.
