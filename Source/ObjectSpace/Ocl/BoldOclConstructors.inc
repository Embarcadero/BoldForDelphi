// BoldORed.pas

function MakeList( First : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( offs );
   result.List := TList.Create;
   Result.List.Add( First.Clone );
end;

function PushList( First, List : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( Offs );
   result.list := list.list;
   result.list.insert( 0, first.clone );
end;

function MakeEmptyList : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( 0 );
   result.List := TList.Create;
end;

procedure ReleaseList( List : AYaccStackElement );
// Since all lists contain cloned elements, the lists them selves must make sure
// the elements are removed completely when they are used.
// the elements must be cloned since they come from the parser stack where they will
// be overwritten
var
  i : integer;
begin
   if assigned( LIst.LIst ) then begin
     for i := 0 to list.list.count-1 do begin
        AYaccStackElement( List.List[i] ).free;
     end;
     list.list.free;
   end;
end;

procedure RetrieveNodeList( ParseList : AYaccStackElement; var NodeList : TBoldOCLNodeList );
var
   i : integer;
begin
   if not assigned( ParseList ) then exit;
   if assigned( ParseList.List ) then begin
      if not assigned( Nodelist ) then
         Nodelist := TBoldOCLNodeList.create;
      for i := 0 to ParseList.List.Count-1 do
         Nodelist.Add( AYaccStackElement( ParseList.List[i] ).Node );
      ReleaseList( ParseList );
   end;
end;


function Make1Operation( const OpName : String; arg1 : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( Arg1.Offset );
   result.Node := TBoldOclOperation.Create;
   Result.Node.Position := Offs;
   TBoldOclOperation( result.Node ).args := TBoldOCLNodeList.create;
   TBoldOclOperation( result.Node ).args.add( arg1.Node );
   TBoldOclOperation( result.Node ).OperationName := OpName;
end;

function Make2Operation(const OpName : String; arg1, arg2 : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( offs );
   result.Node := TBoldOclOperation.Create;
   Result.Node.Position := Offs;
   TBoldOclOperation( result.Node ).args := TBoldOClNodeList.create;
   TBoldOclOperation( result.Node ).args.add( arg1.Node );
   TBoldOclOperation( result.Node ).args.add( arg2.Node );
   TBoldOclOperation( result.Node ).OperationName := OpName;
end;

function Make3Operation(const OpName : String; arg1, arg2, arg3 : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( offs );
   result.Node := TBoldOclOperation.Create;
   Result.Node.Position := Offs;

   TBoldOclOperation( result.Node ).args := TBoldOCLNodeList.create;
   TBoldOclOperation( result.Node ).args.add( arg1.Node );
   TBoldOclOperation( result.Node ).args.add( arg2.Node );
   TBoldOclOperation( result.Node ).args.add( arg3.Node );
   TBoldOclOperation( result.Node ).OperationName := OpName;
end;

function MakeUnaryMinus(const OpName : String; arg1 : AYaccStackElement; Offs : integer ) : AYaccStackElement;
//var
//  Zero : TBoldOCLIntLiteral;
//  ZeroNode : AYaccStackElement;
//  Value : TBAInteger;
begin
// ZeroNode := AYaccStackElement.CreateOffs( offs );
//  Zero := TBoldOCLIntLiteral.create;
//  value := TBAInteger.Create(nil);
//  Zero.fIntValue := 0;
//  ZeroNode.node := Zero;
//  ZeroNode.node.SetOwnedValue( value );
//  ZeroNode.Node.Position := Offs;
  Result := Make1Operation( 'unary-', Arg1, Offs );
end;


function MakeCollectionKind( Kind : TBOCollectionKind; Offs : integer ) : AYaccStackElement;
begin
  result := AYaccStackElement.CreateOffs( offs );
  result.CollectionKind := Kind;
  result.Node := TBoldOCLCollectionLiteral.Create;
  Result.Node.Position := Offs;
end;


function MakeTimeExpression( Kind : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
  raise EBoldOCLAbort.CreateFmt( boeTimeExpressionsUnImpl, [offs]);
  result := AYaccStackElement.CreateOffs( offs );
  result.Name := Kind.Name;
end;

function MakeName( Name : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs( Offs );
   Result.Name := BoldSharedStringManager.GetSharedString(Name.Lexeme.Buffer);
   if Result.Name[1] in ['A'..'Z'] then Result.NameIsType := true;
end;

function MakeDeclarator( VarName, VarType : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
  result := AYaccStackElement.CreateOffs( Offs );
  if assigned( varName ) then
    result.Name := BoldSharedStringManager.GetSharedString(VarName.Lexeme.Buffer);
  if assigned( varType ) then begin
    result.DeclaratorTypeList := VarType.List;
    result.SimpleTypeSpecifierIsEnum := VarType.SimpleTypeSpecifierIsEnum;
  end;
end;

function MakeRange( start, Stop : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
   result := AYaccStackElement.CreateOffs(Offs);
   result.Start := Start.Node;
   result.Stop := Stop.Node;
end;

function MakeLiteralCollection( CollectionType, ExprListOrRange : AYaccStackElement; Offs : integer ) : AYaccStackElement;
var
  LiteralCollection : TBoldOClCollectionLiteral;
begin
  result := AYaccStackElement.CreateOffs( offs );
  LiteralCollection := TBoldOClCollectionLiteral.Create;
  LiteralCollection.CollectionKind := CollectionType.CollectionKind;
  LiteralCollection.Position := Offs;
  if assigned( ExprListOrRange.Start ) then begin
    LiteralCollection.IsRange := true;
    LiteralCollection.RangeStart := ExprListOrRange.Start;
    LiteralCollection.RangeStop := ExprListOrRange.Stop;
  end else begin
    LiteralCollection.IsRange := false;
    LiteralCollection.Elements := TBoldOCLNodeList.Create;
    RetrieveNodeList( ExprListOrRange, LiteralCollection.Elements );
  end;

  Result.Node := LiteralCollection;
end;

function MakeSimpleEnumType( Enumeration : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
  raise EBoldOCLAbort.CreateFmt( boeUserSpecifiedEnumsUnImpl, [offs]);
  result := AYaccStackElement.CreateOffs( offs );
end;

function MakeSimplePath( Path : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
  if Path.List.Count <> 1 then
    raise EBoldOCLAbort.CreateFmt( boePackageSpecUnImpl, [offs]);
  result := AYaccStackElement.CreateOffs(offs);
  result.SimpleTypeSpecifierIsEnum := false;
  result.List := Path.List;
end;

function MakeLiteralName( Name : AYaccStackElement; Offs : integer ) : AYaccStackElement;
begin
  result := AYaccStackElement.CreateOffs( offs );
  result.Node := TBoldOCLEnumLiteral.create;
  Result.Node.Position := Offs;
  TBoldOCLEnumLiteral( result.Node ).Name := Name.Name;
end;

function MakeLiteralString( Str : AYaccStackElement; Offs : integer ) : AYaccStackElement;
var
  node : TBoldOCLStrLiteral;
  s: String;
//  value : TBAString;
begin
  result := AYaccStackElement.CreateOffs( offs );
  node := TBoldOCLStrLiteral.create;
//  value := TBAString.Create(nil);
  if assigned( Str ) then
  begin
    s := Str.Lexeme.Buffer;
    s := StringReplace(s, '\''', '''', [rfReplaceAll]); 
    s := StringReplace(s, '\\', '\', [rfReplaceAll]); 
    Node.StrValue := BoldSharedStringManager.GetSharedString(s);
  end
  else
    Node.StrValue := '';
  result.node := node;
  Result.Node.Position := Offs;
end;

function MakeLiteralInt( Int : AYaccStackElement; Offs : integer ) : AYaccStackElement;
var
  node : TBoldOCLIntLiteral;
//  value : TBAInteger;
begin
  result := AYaccStackElement.CreateOffs( offs );
  node := TBoldOCLIntLiteral.create;
//  value := TBAInteger.Create(nil);
  try
    Node.IntValue := StrToInt( Int.Lexeme.Buffer );
//    value.MakeImmutable;
  except
    on eConvertError do
       raise EBoldOCLAbort.CreateFmt( boeConversionError, [offs, Int.Lexeme.Buffer, 'integer']);
  end;
  result.node := node;
//  result.node.SetOwnedValue( value );
  Result.Node.Position := Offs;
end;

function MakeLiteralDate(Date: AYaccStackElement; Offs: integer ) : AYaccStackElement;
var
  node : TBoldOCLDateLiteral;
  y, m, d: integer;
begin
  result := AYaccStackElement.CreateOffs( offs );
  node := TBoldOCLDateLiteral.create;
  y := StrToInt(copy(Date.lexeme.buffer, 2, 4));
  m := StrToInt(copy(Date.lexeme.buffer, 7, 2));
  d := StrToInt(copy(Date.lexeme.buffer, 10, 2));
  try
    Node.DateValue := EncodeDate(y, m, d);
  except
    on eConvertError do
       raise EBoldOCLAbort.CreateFmt( boeConversionError, [offs, Date.Lexeme.Buffer, 'Date']);
  end;
  result.node := node;
  Result.Node.Position := Offs;
end;

function MakeLiteralTime(Time: AYaccStackElement; Offs: integer ) : AYaccStackElement;
var
  node : TBoldOCLDateLiteral;
  h, m, s: integer;
begin
  result := AYaccStackElement.CreateOffs( offs );
  node := TBoldOCLDateLiteral.create;
  h := StrToInt(copy(Time.lexeme.buffer, 2, 2));
  m := StrToInt(copy(Time.lexeme.buffer, 5, 2));
  s := StrToIntDef(copy(Time.lexeme.buffer, 8, 2), 0);
  try
    Node.DateValue := EncodeTime(h, m, s, 0);
  except
    on eConvertError do
       raise EBoldOCLAbort.CreateFmt( boeConversionError, [offs, Time.Lexeme.Buffer, 'Time']);
  end;
  result.node := node;
  Result.Node.Position := Offs;
end;



function MakeLiteralfloat( Float : AYaccStackElement; Offs : integer ) : AYaccStackElement;
var
  node : TBoldOCLNumericLiteral;
//  value : TBAFloat;
  tempfloatString : String;
begin
  result := AYaccStackElement.CreateOffs( offs );
  node := TBoldOCLNumericLiteral.create;
//  value := TBAFloat.Create(nil);
  try
    TempFloatString := Float.Lexeme.Buffer;
    if FormatSettings.DecimalSeparator <> '.' then TempFloatString[POs('.', TempFloatString)] := FormatSettings.DecimalSeparator;
    Node.FloatValue := StrTofloat( TempFloatString );
  except
    on eConvertError do begin
       raise EBoldOCLAbort.CreateFmt( boeConversionError, [offs, Float.Lexeme.Buffer, 'real']);
    end;
  end;
  result.node := node;
//  result.node.SetOwnedValue( value );
  Result.Node.Position := Offs;
end;


function MakeFeatureCall( Declaratorname, DeclaratorType, paramList : AYaccStackElement; Offs : integer ) : AYaccStackElement;
var
  Declarator : AYaccStackElement;
begin
  declarator := nil;
  if assigned( declaratorName ) then
    Declarator := MakeDeclarator( DeclaratorName, DeclaratorType, DeclaratorName.Offset );
  result := AYaccStackElement.CreateOffs( offs );
  if assigned( Declarator ) then
  begin
   result.Name := Declarator.Name;
   result.DeclaratorTypeList := Declarator.DeclaratorTypeList;
   result.SimpleTypeSpecifierIsEnum := Declarator.SimpleTypeSpecifierIsEnum;
   Declarator.Free;
  end;
  if assigned( ParamList ) then begin
   result.List := ParamList.List;
  end;
end;


function MakeMember( Context, Name, Time, Qual : AYaccStackElement; Offs : integer ) : AYaccStackElement;
    { postFixExpression -> postFixExpression . pathName timeExpression qualifier }
begin
  result := AYaccStackElement.CreateOffs( offs );
  if not assigned( context ) and aYaccStackElement(Name.List[0]).NameIsType then begin
    if name.list.count <> 1 then
      raise EBoldOCLAbort.CreateFmt( boeInvalidTypeNameName, [offs]);
    result.Node := TBoldOclTypeNode.Create;
    Result.Node.Position := Offs;
    (result.Node as TBoldOClTypeNode).TypeName := aYaccStackElement(Name.List[0]).Name;
    ReleaseList( Name );

  end else begin
    result.Node := TBoldOCLMember.Create;
    Result.Node.Position := Offs;
    RetrieveNodeList( Qual, TBoldOCLMember( result.Node ).Qualifier );
    // Take Care of Time in the future
    if Name.List.Count <> 1 then begin
      ReleaseList( Name );
      raise EBoldOCLAbort.CreateFmt( boeInvalidMemberName, [offs]);
    end else begin
      TBoldOclMember( Result.Node ).MemberName := AYaccStackElement( Name.List[0] ).Name;
      ReleaseList( Name );
    end;

    if assigned( Context ) then begin
      TBoldOclMember( Result.Node ).MemberOf := Context.Node;
    end;
  end;
end;

function MakeMethod( Context, Name, Time, Qual, Params : AYaccStackElement; Offs : integer ) : AYaccStackElement;
    { postFixExpression -> postFixExpression . pathName timeExpression qualifier featureCallParameters }
begin
  result := AYaccStackElement.CreateOffs( offs );
  result.Node := TBoldOCLMethod.Create;
  Result.Node.Position := Offs;
  if assigned( Qual.list ) then begin
    Qual.List.Free;
    raise EBoldOCLAbort.CreateFmt( boeInvalidQualifier, [offs]);
  end;
  if Time.Name <> '' then begin
    raise EBoldOCLAbort.CreateFmt( boeInvalidTimeSpecifier, [offs]);
  end;
  if Name.List.Count <> 1 then begin
    ReleaseList( Name );
    raise EBoldOCLAbort.CreateFmt( boeInvalidMethodName, [offs]);

   end else begin
    TBoldOclMethod( Result.Node ).OperationName := AYaccStackElement( Name.List[0] ).Name;
    TBoldOclMethod( Result.Node ).IsMethod := True;
    ReleaseList( Name );
  end;

  if assigned( Context ) then begin
    TBoldOclMethod( Result.Node ).MethodOf := Context.Node;
  end;


  RetrieveNodeList( Params, TBoldOclMethod( result.Node ).Args );

end;


function MakeLoopVar( Params : AYaccStackElement ) : TBoldOCLVariableBinding;
var
   i : integer;
begin
  result := TBoldOClVariableBinding.Create;
  Result.TypeNameList := TStringList.Create;

  if assigned( Params ) then begin
    Result.Position := Params.Offset ;
    result.VariableName := Params.Name;
    if assigned( Params.DeclaratorTypeList ) then begin
      for i := 0 to Params.DeclaratorTypeList.Count-1 do begin
         Result.TypeNameList.Add( AYaccStackElement( Params.DeclaratorTypeList[i] ).Name );
         AYaccStackElement(Params.DeclaratorTypeList[i]).free;
      end;
      Params.DeclaratorTypeList.free;
    end;
  end;
end;

function MakeOperation( Context, Name, Time, Qual, Params : AYaccStackElement; Offs : integer ) : AYaccStackElement;
    { postFixExpression -> postFixExpression -> pathName timeExpression qualifier params }
var
  upcaseName : String;
  IteratorSpecifier : TBoldOclIteratorSpecifier;
begin
  result := AYaccStackElement.CreateOffs( Offs );
  IteratorSpecifier := OclNoIterator;
  UpcaseName := UpperCase( AYaccStackElement( Name.List[0] ).Name );

  if ( UpCaseName = 'SELECT' ) then IteratorSpecifier := OclSelect
  else if ( UpCaseName = 'REJECT' ) then IteratorSpecifier := OclReject
  else if ( UpCaseName = 'COLLECT' ) then IteratorSpecifier := OclCollect
  else if ( UpCaseName = 'FORALL' ) then IteratorSpecifier := OclForAll
  else if ( UpCaseName = 'EXISTS' ) then IteratorSpecifier := OclExists
  else if ( UpCaseName = 'ITERATE' ) then IteratorSpecifier := OclIterate
  else if ( UpCaseName = 'ORDERBY' ) then IteratorSpecifier := OclOrderBy
  else if ( UpCaseName = 'ORDERDESCENDING' ) then IteratorSpecifier := OclOrderDescending
  else if ( UpCaseName = 'UNIQUE' ) then IteratorSpecifier := OclUnique;

  if IteratorSpecifier <> OclNoIterator then begin
     result.Node := TBoldOCLIteration.Create;
     TBoldOclIteration( result.Node ).LoopVar := MakeLoopVar( Params );
     TBoldOclIteration( result.Node ).IteratorSpecifier := IteratorSpecifier;
  end else begin
     result.Node := TBoldOclOperation.Create;
     TBoldOclOperation( result.Node ).IteratorSpecifier := IteratorSpecifier;

     if assigned( params ) and (Params.Name <> '' ) then
       raise EBoldOCLAbort.CreateFmt( boeInvalidLoopVariable, [offs, AYaccStackElement( Name.List[0] ).Name]);
  end;
  Result.Node.Position := Offs;

  if assigned( Qual.list ) then begin
    Qual.List.Free;
    raise EBoldOCLAbort.CreateFmt( boeInvalidQualifier, [offs]);
  end;
  if Time.Name <> '' then
    raise EBoldOCLAbort.CreateFmt( boeInvalidTimeSpecifier, [offs]);

  if Name.List.Count <> 1 then begin
    ReleaseList( Name );
    raise EBoldOCLAbort.CreateFmt( boeInvalidMemberName, [offs]);

  end else begin
    TBoldOclOperation( Result.Node ).OperationName := AYaccStackElement( Name.List[0] ).Name;
    ReleaseList( Name );
  end;

  TBoldOclOperation( Result.Node ).args := TBoldOCLNodeList.Create;

  if assigned( Context ) then
    TBoldoclOperation( Result.Node ).Args.Add( Context.Node )
  else
    raise EBoldOCLAbort.CreateFmt( boeOperationWithOutContext, [offs]);

  RetrieveNodeList( Params, TBoldOclOperation( result.Node ).Args );
end;



