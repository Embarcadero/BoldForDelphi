
{ Global compiler directives }
{$include bold.inc}
unit BoldWCodeInformer;

interface

uses
  classes,
  BoldWAMethodInfo,
  BoldWAClassInfo,
  BoldWScanner;

const
  COLUMN = ':';

type
  {Token types for keeping track of state information while parsing a source file}
  TCodeType = (ctNone, ctProcedure, ctIdentifier, ctEqual, ctClass,
               ctOpenParan, ctCloseParan, ctReturnType);
  TClassParser = class
  private
    fCurrentClass: TClassInfo;
    fInImplementation, fInInterface: Boolean;
    fMethodType: TMethodType;
    fCodeType: TCodeType;
    fVisibility: TVisibility;
    fClassToken: string;
    fTokenPos: Integer;
    fStream: TStringStream;
    fParam: string;
    fReturnType: string;
    fProcName: string;
    fClasses: TClassInfoList;
  public
    constructor Create(SourceStream: TStringStream); reintroduce; overload;
    destructor Destroy; override;
    function getClasses(out Item: TClassInfo;var I: integer): Boolean;
    function getClassbyName(ClassName: string): TClassInfo;
    function getMethodbyTreeIndex(MethodIndex: integer): TMethodInfo;
    function IsProcedure(Scanner: TScanner): Boolean;
    procedure ScannerToken(Sender: TObject);
    procedure Start;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

                      {TClassParser}
constructor TClassParser.Create(SourceStream: TStringStream);
begin
  inherited Create;
  fStream := SourceStream;
  fClasses := TClassInfoList.Create;
  fCurrentClass := nil;
end;

destructor TClassParser.Destroy;
begin
  FreeAndNil(fStream);
  FreeAndNil(fClasses);
  inherited;
end;

function TClassParser.getClasses(out Item: TClassInfo;var I: integer): Boolean;
begin
  if (I < fClasses.Count) and (I >= 0) then
  begin
    Item := fClasses.Items[I];
    Inc(i);
    Result := true;
  end
  else
    Result := false;
end;

function TClassParser.getClassbyName(ClassName: string): TClassInfo;
var
  counter: integer;
  ClassInfo: TClassInfo;
begin
  counter:= 0;
  while getClasses(ClassInfo, counter) do
    if Assigned(ClassInfo) then
      if ClassInfo.DelphiName = ClassName then Break;
  Result := ClassInfo;
end;

function TClassParser.getMethodbyTreeIndex(MethodIndex: integer): TMethodInfo;
var
  i,
  Offset,
  RelativeIndex: integer;
begin
  {Returns first method in code fragment with name MethodName}
  Offset := 0;
  Result := nil;
  for i:= 0 to fClasses.Count - 1 do
  begin
    RelativeIndex := methodIndex - Offset ;
    if relativeIndex < fClasses[i].Methods.Count then
    begin
      Result := fClasses[i].Methods[RelativeIndex];
      Break;
    end;
    Inc(Offset, fClasses[i].Methods.Count);
  end;
end;

function TClassParser.IsProcedure(Scanner: TScanner): Boolean;
begin
  Result := True;
  if Scanner.IsIdentifier('procedure') then
    fMethodType := mtProcedure
  else if Scanner.IsIdentifier('function') then
    fMethodType := mtFunction
  else if Scanner.IsIdentifier('constructor') then
    fMethodType := mtConstructor
  else if Scanner.IsIdentifier('destructor') then
    fMethodType := mtDestructor
  else
    Result := false;
end;

{
 Scan a single token in a source. Look for procedure, function and class
 declarations, and add a node for each one.
 The following fields keep track of state information:
 fsImplementation: true when parsing implementation section;
 fCodeType: kind of code entity currently being scanned;
 fClassToken: the potential name in a class declaration;
 fTokenPos: the starting position of the code entity;
}
procedure TClassParser.ScannerToken(Sender: TObject);
var
  str: string;

  procedure AddMethod;
  begin
    with Sender as TScanner do
    begin
      if Trim(fParam) = COLUMN then fParam := '';
      if (TokenType <> ':') then
      begin
        if (fMethodType = mtFunction)  then
          fReturnType := Token
        else
          fReturnType := '';
        fCurrentClass := fClasses.Items[fClasses.Count - 1];
        fCurrentClass.addMethod(TMethodInfo.Create(fMethodType,fProcName, fParam, fReturnType, fVisibility, Pointer(fCurrentClass))) ;
        fCodeType := ctNone;
      end
    end;  
  end;

begin
  with Sender as TScanner do
  begin
    str := token;
    if IsIdentifier('implementation') then
    begin
      fInImplementation := true;
      fInInterface := false;
      fCodeType := ctNone;
    end
    else if IsIdentifier('interface') then
    begin
      fInImplementation := false;
      fInInterface := true;
      fCodeType := ctNone;
    end
    else if fInInterface then
    begin
      if IsIdentifier('public') then
        fVisibility := stPublic
      else if IsIdentifier('protected') then
        fVisibility := stProtected
      else if IsIdentifier('published') then
        fVisibility := stPublished
      else if IsIdentifier('private') then
        fVisibility := stPrivate
      else if IsIdentifier('end') then
        fCodeType := ctIdentifier
      else if {NewLine and }IsProcedure(TScanner(Sender)) then
      begin
        fTokenPos := Position;
        fCodeType := ctProcedure;
      end
      else if fCodeType = ctProcedure then
      begin
        fCodeType := ctOpenParan;
        fProcName := Token;
        fParam := '';
        fReturnType := '';
      end
      else if (fCodeType = ctOpenParan) then
      begin
        if (Token = ';') then
        begin
          fCodeType := ctReturnType;
          AddMethod;
        end
        else
        begin
          fParam := fParam + Token;
          if (TokenType = '(') then
            fCodeType := ctCloseParan
          else
          begin
            fCodeType := ctReturnType;
            if (fMethodType = mtFunction) then fReturnType:= Token;
          end
        end;
      end
      else if (fCodeType = ctCloseParan) then
      begin
        if ((Token <> ';') and (Token <> ')') and (Token <> ':')) then
          fParam := fParam + ' ';
        fParam := fParam + Token;
        if (TokenType = ')') then
          fCodeType := ctReturnType;
      end
      else if (fCodeType = ctReturnType) then
        AddMethod
      else if (fCodeType = ctClass) and IsIdentifier('of') then
        fCodeType := ctNone
      else if (fCodeType = ctIdentifier) and (TokenType = '=') then
        fCodeType := ctEqual
      else if (fCodeType = ctEqual) and IsIdentifier('class') then
        fCodeType := ctClass
      else if (fCodeType = ctClass) and (TokenType <> ';') then
      begin
        fClasses.Add(TClassInfo.Create(fClassToken));
        fCodeType := ctNone;
      end
      else if NewLine and (TokenType = ttIdentifier) then
      begin
        fClassToken := Token;
        fTokenPos := Position;
        fCodeType := ctIdentifier;
      end
    end
    else
      fCodeType := ctNone;
  end;
end;

procedure TClassParser.Start;
var
  Scanner: TScanner;
begin
  Scanner := TScanner.Create;
  Scanner.OnToken := ScannerToken;
  try
    Scanner.Scan(fStream);
  finally
    Scanner.Free;
  end;
end;

end.
