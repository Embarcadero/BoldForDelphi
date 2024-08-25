
{ Global compiler directives }
{$include bold.inc}
unit BoldORed;

interface

uses
  Classes,
  BoldSSLexU,
  BoldSSYaccU,
  BoldOclClasses;

{$I BoldOCon.inc}
{$I BoldOCls.inc}

implementation

uses
  SysUtils,
  BoldSharedStrings,
  BoldOclError;

type
  TBoldOclCollectionType = (tboNoCollection, tboBag, tboSet, tboSequence, tboCollection, tboCopyArg1, tboMinCollection);


{$I BoldOclConstructors.inc}

constructor AYaccStackElement.Create;
begin
   inherited Create;
   Node := nil;
   List := nil;
   start := nil;
   Stop := nil;
   Name := '';
   Offset := -1;
   NameIsType := False;
   SimpleTypeSpecifierIsEnum := false;
   DeclaratorTypeList := nil;
end;

constructor AYaccStackElement.CreateOffs(offs: integer);
begin
  inherited create;
  offset := offs;
end;


function AYaccStackElement.Clone: AYaccStackElement;
begin
  result := AYaccStackElement.Create;
  result.Node := Node;
  Result.List := List;
  result.Start := Start;
  Result.Stop := Stop;
  Result.Name := Name;
  Result.NameIsType := NameIsType;
  Result.SimpleTypeSpecifierIsEnum := SimpleTypeSpecifierIsEnum;
  result.DeclaratorTypeList := DeclaratorTypeList;
end;

function AYaccClass.StackElement: SSYaccStackElement;
begin
 Result := AYaccStackElement.Create;
end;


{$I BoldOFun.inc}

{
Note: In order not to confuse the auto-merge parser,
it's very important to keep this function clean. To
avoid any problems, just make function calls to other
functions that do the real work. NEVER include nested
case statements in this function.
}
function AYaccClass.Reduce(TheProduction, TheSize: Longint): SSYaccStackElement;
   function Stack(i: integer): AYaccStackElement;
   begin
      result := AYaccStackElement(ElementFromProduction(i));
   end;

begin
   Result := nil;
   case TheProduction of
    AYaccGoal:
    { Goal -> expression } begin
    FinalValue := Stack(0);
   end;


    AYaccexpressionPost:
    { expression -> postFixExpression }
    result := stack(0);

    AYaccexpressionAnd:
    { expression -> expression AND expression }
    result := Make2Operation('and', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionOr:
    { expression -> expression OR expression }
    result := Make2Operation('or', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionXor:
    { expression -> expression XOR expression }
    result := Make2Operation('xor', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionImp:
    { expression -> expression IMPLIES expression }
    result := Make2Operation('implies', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionEQ:
    { expression -> expression = expression }
    result := Make2Operation('=', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionLT:
    { expression -> expression < expression }
    result := Make2Operation('<', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionGT:
    { expression -> expression > expression }
    result := Make2Operation('>', stack(0), stack(2), stack(1).lexeme.offset);


    AYaccexpressionLE:
    { expression -> expression <= expression }
    result := Make2Operation('<=', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionGE:
    { expression -> expression >= expression }
    result := Make2Operation('>=', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionNE:
    { expression -> expression <> expression }
    result := Make2Operation('<>', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionAdd:
    { expression -> expression + expression }
    result := Make2Operation('+', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionSub:
    { expression -> expression - expression }
    result := Make2Operation('-', stack(0), stack(2), stack(1).lexeme.offset);


    AYaccexpressionDiv:
    { expression -> expression / expression }
    result := Make2Operation('/', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionMul:
    { expression -> expression * expression }
    result := Make2Operation('*', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionNeg:
    { expression -> - expression }
    result := MakeUnaryMinus('-', stack(1), stack(0).lexeme.offset);

    AYaccexpressionNOT:
    { expression -> NOT expression }
    result := Make1Operation('not', stack(1), stack(0).lexeme.offset);

    AYaccifExpression:
    { ifExpression -> if expression then expression else expression endif }
    result := Make3Operation('if', stack(1), stack(3), stack(5), stack(0).lexeme.offset);

    AYaccpostFixFirst:
    { postFixExpression -> primaryExpression }
    result := stack(0);

    AYaccpostFixMember:
    { postFixExpression -> postFixExpression . pathName timeExpression qualifier }
    Result := MakeMember(stack(0), stack(2), stack(3), stack(4), stack(2).offset);

    AYaccpostFixMethod:
    { postFixExpression -> postFixExpression . pathName timeExpression qualifier featureCallParameters }
    Result := MakeMethod(stack(0), stack(2), stack(3), stack(4), stack(5), stack(2).offset);

    AYaccpostFixOperation:
    { postFixExpression -> postFixExpression -> pathName timeExpression qualifier }
    Result := MakeOperation(stack(0), stack(2), stack(3), stack(4), nil, stack(2).offset);

    AYaccpostFixOperationParams:
    { postFixExpression -> postFixExpression -> pathName timeExpression qualifier featureCallParameters }
    Result := MakeOperation(stack(0), stack(2), stack(3), stack(4), stack(5), stack(2).offset);

    AYaccprimaryExpLittCollection:
    { primaryExpression -> literalCollection }
    result := stack(0);

    AYaccprimaryExpLitt:
    { primaryExpression -> literal }
    result := stack(0);

    AYaccprimaryExpParen:
    { primaryExpression -> (expression) }
    result := stack(1);

    AYaccprimaryExpIf:
    { primaryExpression -> ifExpression }
    result := stack(0);

    AYaccPrimaryNoParams:
    { primaryExpression -> pathName timeExpression qualifier }
    result := MakeMember(nil, stack(0), stack(1), stack(2), stack(0).offset);

    AYaccPrimaryWithParams:
    { primaryExpression -> pathName timeExpression qualifier featureCallParameters }
    result := MakeMethod(nil, stack(0), stack(1), stack(2), stack(3), stack(0).offset);

    AYaccliteral_String:
    { literal -> STRINGstart STRINGliteral STRINGend }
    result := MakeLiteralString(stack(1), stack(0).lexeme.offset);

    AYaccliteral_Number:
    { literal -> INTEGER }
    result := MakeLiteralInt(stack(0), stack(0).lexeme.offset);

    AYaccliteral_Name:
    { literal -> # NAME }
    result := MakeLiteralName(stack(1), stack(1).offset);

    AYaccsimpleTypeSpecifier_Ety:
    { simpleTypeSpecifier -> }
    result := stackelement;

    AYaccsimpleTypeSpecifier_Path:
    { simpleTypeSpecifier -> pathTypeName }
    Result := MakeSimplePath(stack(0), stack(0).offset);

    AYaccsimpleTypeSpecifier_Enum:
    { simpleTypeSpecifier -> enumerationType }
    result := MakeSimpleEnumType(stack(0), stack(0).offset);

    AYaccliteralCollection:
    { literalCollection -> collectionKind { expressionListOrRange # }
    result := MakeLiteralCollection(stack(0), stack(2), stack(0).offset);

    AYaccexpressionListOrRange_List:
    { expressionListOrRange -> expressionList }
    result := stack(0);

    AYaccexpressionList:
    { expressionList -> expression }
    result := MakeList(stack(0), stack(0).offset);

    AYaccexpressionList_Rec:
    { expressionList -> expression , expressionList }
    result := PushList(Stack(0), stack(2), stack(0).offset);

    AYaccexpressionListOrRange_Range:
    { expressionListOrRange -> expression .. expression }
    result := MakeRange(stack(0), stack(2), stack(0).offset);

    AYaccenumerationType:
    { enumerationType -> enum { enumTypeList # }
    Result := stack(2);

    AYaccenumTypeList:
    { enumTypeList -> # NAME }
    Result := MakeList(stack(1), stack(1).lexeme.offset);

    AYaccenumTypeList_Rec:
    { enumTypeList -> # NAME , enumTypeList }
    result := PushList(stack(1), stack(3), stack(1).lexeme.offset);

    AYaccqualifiers_Ety:
    { qualifier -> }
    result := stackelement;

    AYaccqualifiers:
    { qualifier -> [ actualParameterList ] }
    result := stack(1);

    AYaccpathTypeName:
    { pathTypeName -> typeNameList }
    result := stack(0);

    AYacctypeNameList:
    { typeNameList -> TYPENAME }
    result := MakeList(stack(0), stack(0).lexeme.offset);

    AYacctypeNameList_Rec:
    { typeNameList -> TYPENAME :: typeNameList }
    result := PushList(Stack(0), stack(2), stack(0).lexeme.offset);

    AYaccpathName:
    { pathName -> anyName }
    result := MakeList(stack(0), stack(0).offset);

    AYaccpathName_Rec:
    { pathName -> anyName :: pathName }
    result := PushList(stack(0), stack(2), stack(0).offset);

    AYaccanyName_Type:
    { anyName -> TYPENAME }
    result := MakeName(stack(0), stack(0).lexeme.offset);

    AYaccanyName_Name:
    { anyName -> NAME }
    result := MakeName(stack(0), stack(0).lexeme.offset);

    AYacctimeExpression:
    { timeExpression -> @ NAME }
    Result := MakeTimeExpression(stack(1), stack(1).offset);

    AYacccollectionKind_Set:
    { collectionKind -> Set }
    Result := MakeCollectionKind(oclSet, stack(0).lexeme.offset);

    AYacccollectionKind_Bag:
    { collectionKind -> Bag }
    Result := MakeCollectionKind(oclBag, stack(0).lexeme.offset);

    AYacccollectionKind_Sequence:
    { collectionKind -> Sequence }
    Result := MakeCollectionKind(oclSequence, stack(0).lexeme.offset);

    AYacccollectionKind_Collection:
    { collectionKind -> Collection }
    raise EBoldOCLAbort.CreateFmt(boeCollectionNotValidLiteral, [stack(0).lexeme.offset]);


    AYacctimeExpression_Ety:
    { timeExpression ->  }
    Result := stackelement;

    AYaccliteral_float:
    { literal -> FLOAT  }
    result := MakeLiteralFloat(stack(0), stack(0).lexeme.offset);

    AYaccexpressionDivI:
    { expression -> expression IntDIV expression  }
    result := Make2Operation('div', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccexpressionMod:
    { expression -> expression MOD expression  }
     result := Make2Operation('mod', stack(0), stack(2), stack(1).lexeme.offset);

    AYaccfeatureCallParameters1:
    { featureCallParameters -> (actualParameterList)  }
    result := MakeFeatureCall(nil, nil, stack(1), stack(1).offset);


    AYaccfeatureCallParameters2:
    { featureCallParameters -> (NAME | actualParameterList)  }
    result := MakeFeatureCall(stack(1), nil, stack(3), stack(3).offset);

    AYaccfeatureCallParameters3:
    { featureCallParameters -> (NAME: simpleTypeSpecifier | actualParameterList)  }
    result := MakeFeatureCall(stack(1), stack(3), stack(5), stack(5).offset);

    AYaccactualParameterList_ety:
    { actualParameterList ->  }
    result := MakeEmptyList;

    AYaccActualParameterlist_nonEty:
    { actualParameterList -> NonEmptyActualParameterList  }
    Result := stack(0);

    AYaccNonEmptyactualParameterList:
    { NonEmptyActualParameterList -> expression  }
    Result := MakeList(stack(0), stack(0).offset);

    AYaccNonEmptyactualParameterList_Rec:
    { NonEmptyActualParameterList -> expression , NonEmptyActualParameterList  }
    Result := PushList(stack(0), stack(2), stack(0).offset);

    AYaccliteral_EmptyString:
    { literal -> STRINGstart STRINGend  }
    result := MakeLiteralString(nil, stack(0).lexeme.offset);

    AYaccliteral_Date:
    { literal -> DATE  }
    Result := MakeLiteralDate(stack(0), Stack(0).lexeme.Offset);

    AYaccliteral_Time:
    { literal -> TIME  }
    Result := MakeLiteralTime(stack(0), Stack(0).lexeme.Offset);

  else{Reduce}
    { Bad Production }
    assert(true, 'Bad production in OCL-parser');
  end;
end;

end.
