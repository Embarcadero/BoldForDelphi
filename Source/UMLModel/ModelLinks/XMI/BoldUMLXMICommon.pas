unit BoldUMLXMICommon;

interface

uses
  BoldSystemRT;

function UnqualifiedName(const aText: String): String;
function RemoveBoldPackageNames(Name: string): string;
function QualifiedMemberName(MemberRT: TBoldMemberRTInfo): string;
function IsNameOfExpressionClass(Name: string): Boolean;
function FindDefiningClass(Member: TBoldMemberRTInfo): TBoldClassTypeInfo;

implementation

uses
  SysUtils;

function UnqualifiedName(const aText: String): String;
begin
  Result := Copy(aText, LastDelimiter(':.', aText) + 1, MAXINT);
end;

function RemoveBoldPackageNames(Name: string): string;
const
  BoldPackagePrefix = 'BoldUMLModel.UMLMeta.'; // do not localize
begin
  assert(Copy(Name, 1, Length(BoldPackagePrefix)) = BoldPackagePrefix);
  result := Copy(Name, Length(BoldPackagePrefix)+1, maxint);
end;

function RemoveTrailingUnderscore(Name: string): string;
begin
  if Name[Length(Name)] = '_' then
    result := Copy(Name, 1, Length(Name)-1)
  else
    result := Name;
end;

function FindDefiningClass(Member: TBoldMemberRTInfo): TBoldClassTypeInfo;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo := Member.ClassTypeInfo;
  while ClassTypeInfo.FirstOwnMemberIndex > Member.index do
    ClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
  result := ClassTypeInfo;
end;

function QualifiedMemberName(MemberRT: TBoldMemberRTInfo): string;
begin
  result := RemoveBoldPackageNames(FindDefiningClass(MemberRT).QualifiedName) +
    '.' + RemoveTrailingUnderscore(MemberRT.ModelName);
end;

function IsNameOfExpressionClass(Name: string): Boolean;
begin
  result :=(Name = 'Expression') or // do not localize
     (Name = 'ActionExpression') or // do not localize
     (Name = 'ArgListsExpression') or // do not localize
     (Name = 'BooleanExpression') or // do not localize
     (Name = 'IterationExpression') or // do not localize
     (Name = 'MappingExpression') or // do not localize
     (Name = 'ObjectSetExpression') or // do not localize
     (Name = 'ProcedureExpression') or // do not localize
     (Name = 'TimeExpression') or // do not localize
     (Name = 'TypeExpression'); // do not localize
end;

end.
