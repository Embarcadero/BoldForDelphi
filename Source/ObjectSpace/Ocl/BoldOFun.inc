function ALexClass.TokenToString( TheToken : Longint) : string;
begin
  case TheToken of
    ALexINTEGER: Result := 'INTEGER';
    ALexFLOAT: Result := 'FLOAT';
    ALexNAME: Result := 'NAME';
    ALexTYPENAME: Result := 'TYPENAME';
    ALexDATE: Result := 'DATE';
    ALexTIME: Result := 'TIME';
    ALexStringSt: Result := 'STRINGstart';
    ALexLBrack: Result := '[';
    ALexRBrack: Result := ']';
    ALexLParen: Result := '(';
    ALexRParen: Result := ')';
    ALexLBrace: Result := '{';
    ALexRBrace: Result := '}';
    ALexGreater: Result := '>';
    ALexLess: Result := '<';
    ALexEqual: Result := '=';
    ALexGTEqual: Result := '>=';
    ALexLTEqual: Result := '<=';
    ALexNEqual: Result := '<>';
    ALexPlus: Result := '+';
    ALexMinus: Result := '-';
    ALexMult: Result := '*';
    ALexDiv: Result := '/';
    ALexDot: Result := '.';
    ALexArrow: Result := '->';
    ALexComma: Result := ',';
    ALexCommercialAt: Result := '@';
    ALexIF: Result := 'if';
    ALexTHEN: Result := 'then';
    ALexELSE: Result := 'else';
    ALexENDIF: Result := 'endif';
    ALexENUM: Result := 'enum';
    ALexHASH: Result := '#';
    ALexBAR: Result := '|';
    ALexDOUBLECOLON: Result := '::';
    ALexDOUBLEDOT: Result := '..';
    ALexCOLON: Result := ':';
    ALexBAG: Result := 'Bag';
    ALexSET: Result := 'Set';
    ALexSEQUENCE: Result := 'Sequence';
    ALexCOLLECTION: Result := 'Collection';
    ALexIntDIV: Result := 'IntDIV';
    ALexMOD: Result := 'MOD';
    ALexAND: Result := 'AND';
    ALexOR: Result := 'OR';
    ALexNOT: Result := 'NOT';
    ALexXOR: Result := 'XOR';
    ALexIMPLIES: Result := 'IMPLIES';
    ALexString: Result := 'STRINGliteral';
    ALexStringEnd: Result := 'STRINGend';
    -2: Result := '%error';
    -1: Result := 'eof';
  else
    Result := 'Token Not Found';
  end;

end;

procedure AYaccStackElement.FreeAllOwnedElements;
var
  i: integer;
begin
  FreeAndNil(Node);
  if assigned(list) then
  begin
    for i := 0 to LIst.Count-1 do
    begin
      AYaccStackElement(List[i]).freeallOwnedElements;
      AYaccStackElement(List[i]).Free;
    end;
    FreeAndNil(List);
  end;
end;

