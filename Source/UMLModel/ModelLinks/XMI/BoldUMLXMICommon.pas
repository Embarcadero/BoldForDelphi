
{ Global compiler directives }
{$include bold.inc}
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
  BoldPackagePrefix = 'BoldUMLModel.UMLMeta.';
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
  result :=(Name = 'Expression') or
     (Name = 'ActionExpression') or
     (Name = 'ArgListsExpression') or
     (Name = 'BooleanExpression') or
     (Name = 'IterationExpression') or
     (Name = 'MappingExpression') or
     (Name = 'ObjectSetExpression') or
     (Name = 'ProcedureExpression') or
     (Name = 'TimeExpression') or
     (Name = 'TypeExpression');
end;

end.
