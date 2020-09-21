unit BoldUMLDelphiSupport;

interface

uses
  BoldDefs,
  BoldUMLTypes;

type
  TBoldUMLDelphiSupport = class
  public
    class function ExtractType(var ParameterName: string): string;
    class function ExtractIsConst(var ParameterName: string): Boolean;
    class function ExtractKind(var ParameterName: string): TBoldParameterDirectionKind;
    class function ParameterModifier(Kind: TBoldParameterDirectionKind; IsConst: Boolean): string;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldUMLDelphiSupport }

class function TBoldUMLDelphiSupport.ExtractType(
  var ParameterName: string): string;
var
  ColonPos: integer;
begin
  ColonPos := Pos(':', ParameterName);
  if ColonPos > 0 then
  begin
    Result := BoldTrim(Copy(ParameterName, ColonPos+1, MaxInt));
    Delete(ParameterName, ColonPos, Maxint);
  end;
end;

class function TBoldUMLDelphiSupport.ExtractIsConst(
  var ParameterName: string): Boolean;
var
  Index: integer;
begin
  Index := Pos('const ', LowerCase(ParameterName)); // do not localize
  Result := Index > 0;
  if Result then
    Delete(ParameterName, Index, 6);  // 6 = length('const ');
end;

class function TBoldUMLDelphiSupport.ExtractKind(
  var ParameterName: string): TBoldParameterDirectionKind;
var
  Index: integer;
begin
  Index := Pos('var ', LowerCase(ParameterName)); // do not localize
  if Index > 0 then
  begin
    Result := pdInOut;
    Delete(ParameterName, Index, 4);  // 4 = length('var ');
  end
  else
  begin
    Index := Pos('out ', LowerCase(ParameterName)); // do not localize
    if Index > 0 then
    begin
      Result := pdOut;
      Delete(ParameterName, Index, 4);  // 4 = length('out ');
    end
    else
      Result := pdIn;
  end;
end;

class function TBoldUMLDelphiSupport.ParameterModifier(
  Kind: TBoldParameterDirectionKind; IsConst: Boolean): string;
begin
  if IsConst then
    Result := 'const ' // do not localize
  else if Kind = pdInOut then
    Result := 'var ' // do not localize
  else if Kind = pdOut then
    Result := 'out ' // do not localize
  else
    Result := '';
end;

end.
