{ Global compiler directives }
{$include bold.inc}
unit BoldRegionDefinitionParser;

interface

uses
  SysUtils,
  classes,
  BoldBase,
  BoldDefs,
  BoldSystemRt,
  BoldRegionDefinitions;

type
  TBoldRegionParser = class;
  EBoldBadLockExpression = class (EBold)
  end;
  EBoldLockExpressionSyntaxError = class (EAbort)
  end;

  TBoldRegionDefinitionSymbolType = (stClassName, stMemberName, stRegionName);
  TBoldRegionParserErrorType = (petSyntax, petSemantics);

  TBoldRegionParser = class (TBoldMemoryManagedObject)
  private
    fExpression: string;
    fPosition: Integer;
    fRegionDefinitions: TBoldRegionDefinitions;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fErrors: TStringList;
    fLastSymbolPosition: integer;
    function GetEOS: Boolean;
    procedure ParseCurrentExpression;
    procedure ParseMembersAndSubregions(CoreDefinition: TBoldRegionCoreDefinition; ClassTypeInfo: TBoldClassTypeInfo);
    procedure ParseMembers(ConcreteRegionDefinition: TBoldConcreteRegionDefinition);
    procedure ParseSubregions(ConcreteRegionDefinition: TBoldConcreteRegionDefinition);
    function GetNextToken: Char;
    procedure AddError(pos: integer; msg: String; args: Array of const; ErrorType: TBoldRegionParserErrorType);
    procedure CheckRegionReferences;
  protected
    procedure Eat(s: string);
    function GetSymbol(Symboltype: TBoldRegionDefinitionSymbolType): string;
    procedure Skip;
    function TryToEat(s: string): Boolean;
    property EOS: Boolean read GetEOS;
    property NextToken: Char read GetNextToken;
    procedure GenerateDefaultRegions;
  public
    constructor Create(RegionDefinitions: TBoldRegionDefinitions; SystemTypeInfo: TBoldSystemTypeInfo);
    destructor Destroy; override;
    function Parse(RegionDefinitionList: TStrings): Boolean;
    property RegionDefinitions: TBoldRegionDefinitions read fRegionDefinitions;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property Errors: TStringList read fErrors;
  end;

implementation

uses
  BoldCoreConsts,
  BoldTaggedValueSupport,
  BoldUtils;

const
  SPACE = #32;
  TAB = #9;
  BOLDDEFAULTREGIONNAME = 'Default';
  BOLDDEFAULTEMPTYREGIONNAME = 'Exist';
  BOLDINDEPENDENTCASCADECOREREGIONPREFIX = 'IC_';

{ TBoldRegionParser }

{
****************************** TBoldRegionParser *******************************
}
constructor TBoldRegionParser.Create(RegionDefinitions: TBoldRegionDefinitions;
        SystemTypeInfo: TBoldSystemTypeInfo);
begin
  fRegionDefinitions := RegionDefinitions;
  fSystemTypeInfo := SystemTypeInfo;
  fErrors := TStringLIst.Create;
end;

procedure TBoldRegionParser.Eat(s: string);
var
  i: Integer;
  OrgPosition: integer;
begin
  OrgPosition := fPosition;
  for i := 1 to length(s) do
    if EOS or (NextToken <> s[i]) then
      AddError(OrgPosition, sExpectedToken, [s], petSyntax)
    else
      inc(fPosition);
  Skip;
end;

procedure TBoldRegionParser.GenerateDefaultRegions;
var
  ClassIx, MemberIx: integer;
  ClassTypeInfo: TBoldClassTypeInfo;
  DefaultCore: TBoldRegionCoreDefinition;
  DefaultRegion: TBoldConcreteRegionDefinition;
  DefaultEmptyCore: TBoldRegionCoreDefinition;
  MemberRTInfo: TBoldMemberRTInfo;
  RoleRTInfo: TBoldRoleRTInfo;
  existed: Boolean;
  IndependentCascadeCore: TBoldRegionCoreDefinition;
  IndependentCascadeRegion: TBoldConcreteRegionDefinition;

begin
  if SystemTypeInfo.GenerateDefaultRegions then
  begin
    DefaultCore := RegionDefinitions.EnsuredCoreDefinition(BOLDDEFAULTREGIONNAME);
    DefaultEmptyCore := RegionDefinitions.EnsuredCoreDefinition(BOLDDEFAULTEMPTYREGIONNAME);
    for ClassIx := 0 to SystemTypeInfo.TopSortedClasses.Count-1 do
    begin
      ClassTypeInfo := SystemTypeInfo.TopSortedClasses[ClassIx];
      DefaultEmptyCore.EnsuredConcreteDefinition(ClassTypeInfo, existed);

      if ClassTypeInfo.GenerateDefaultRegion then
      begin
        DefaultRegion := DefaultCore.EnsuredConcreteDefinition(ClassTypeInfo, existed);
        if not existed then
        begin
          for MemberIx := 0 to ClassTypeInfo.AllMembersCount-1 do
          begin
            MemberRTInfo := ClassTypeInfo.AllMembers[MemberIx];
            if MemberRTInfo.IsAttribute then
              TBoldRegionElementInclusion.Create(DefaultRegion, MemberRTInfo)
            else
            begin
              RoleRTInfo := MemberRTInfo as TBoldRoleRTInfo;

              if not RoleRTInfo.IsDerived then
              begin
                case RoleRTinfo.DefaultRegionMode of
                  aedrmExistence: begin
                    TBoldRegionElementInclusion.Create(DefaultRegion, MemberRTInfo);
                    TBoldSubregionReference.Create(DefaultRegion, DefaultEmptyCore, RoleRTInfo, true);
                  end;
                  aedrmCascade: begin
                    TBoldRegionElementInclusion.Create(DefaultRegion, MemberRTInfo);
                    TBoldSubregionReference.Create(DefaultRegion, DefaultCore, RoleRTInfo, true);
                  end;
                  aedrmIndependentCascade: begin
                    TBoldRegionElementInclusion.Create(DefaultRegion, MemberRTInfo);
                    IndependentCascadeCore := RegionDefinitions.EnsuredCoreDefinition(BOLDINDEPENDENTCASCADECOREREGIONPREFIX+RoleRTInfo.ExpressionName);
                    IndependentCascadeRegion := IndependentCascadeCore.EnsuredConcreteDefinition(ClassTypeInfo, existed);
                    if not existed then
                    begin
                      TBoldRegionElementInclusion.Create(IndependentCascadeRegion, MemberRTInfo);
                      TBoldSubregionReference.Create(IndependentCascadeRegion, DefaultCore, RoleRTInfo, true);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TBoldRegionParser.GetEOS: Boolean;
begin
  Result := fPosition > length(fExpression);
end;

function TBoldRegionParser.GetSymbol(Symboltype: TBoldRegionDefinitionSymbolType): String;
begin
  fLastSymbolPosition := fPosition;
  if EOS or not CharInSet(NextToken, ['a'..'z','A'..'Z','_']) then
  begin
    case SymbolType of
      stClassName: AddError(fPosition, sClassNameExpected, [], petSyntax);
      stMemberName: AddError(fPosition, sMemberNameExpected, [], petSyntax);
      stRegionName: AddError(fPosition, sRegionNameExpected, [], petSyntax);
    end;
  end;
  Result := NextToken;
  inc(fPosition);
  while (not EOS) and CharInSet(NextToken, ['a'..'z','A'..'Z','_', '0'..'9']) do
  begin
    Result := result + NextToken;
    inc(fPosition);
  end;
  Skip;
end;

function TBoldRegionParser.Parse(RegionDefinitionList: TStrings): Boolean;
var
  i: integer;
begin
  RegionDefinitions.Clear;
  for i := 0 to RegionDefinitionList.Count-1 do
  begin
    fExpression := RegionDefinitionList[i];
    fPosition := 1;
    Skip;
    try
      ParseCurrentExpression;
    except
      on e: EBoldLockExpressionSyntaxError do
    end;
  end;
  RegionDefinitions.ExpandDefinitions;
  GenerateDefaultRegions;
  CheckRegionReferences;
  result := fErrors.Count = 0;
end;

procedure TBoldRegionParser.ParseMembers(ConcreteRegionDefinition: TBoldConcreteRegionDefinition);
var
  Membername: string;
  MemberRTInfo: TBoldmemberRTInfo;
begin
  while not eos and (nextToken <> '|') do
  begin
    memberName := GetSymbol(stMemberName);
    MemberRTInfo := ConcreteRegionDefinition.RootClass.MemberRTInfoByExpressionName[MemberName];
    if assigned(MemberRTInfo) then
      TBoldRegionElementInclusion.Create(ConcreteRegionDefinition, MemberRTInfo)
    else
      AddError(fLastSymbolPosition, sXIsNotAMember, [MemberName, ConcreteRegionDefinition.RootClass.ExpressionName], petSemantics);
    tryToEat(',');
  end;
end;

procedure TBoldRegionParser.ParseSubregions(ConcreteRegionDefinition: TBoldConcreteRegionDefinition);
var
  Membername: string;
  regionName: String;
  MemberRTInfo: TBoldmemberRTInfo;
  Subregion: TBoldRegionCoreDefinition;
  IsDependent: Boolean;
begin
  while not EOS do
  begin
    regionName := GetSymbol(stRegionName);
    Eat('[');
    MemberName := GetSymbol(stMemberName);
    eat(']');
    IsDependent := not tryToEat('-');
    MemberRTInfo := ConcreteRegionDefinition.RootClass.MemberRTInfoByExpressionName[MemberName];
    if not assigned(MemberRTInfo) then
      AddError(fLastSymbolPosition, sXIsNotAMember, [MemberName, ConcreteRegionDefinition.RootClass.ExPressionName], petSemantics)
    else if not (MemberRTInfo is TBoldRoleRTInfo) then
      AddError(fLastSymbolPosition, sMemberIsNotARole, [ConcreteRegionDefinition.RootClass.ExPressionName, MemberName], petSemantics)
    else
    begin
      SubRegion := RegionDefinitions.EnsuredCoreDefinition(RegionName);
      TBoldSubregionReference.Create(ConcreteRegionDefinition, Subregion, MemberRTInfo as TBoldRoleRTInfo, IsDependent);
      TBoldRegionElementInclusion.Create(ConcreteRegionDefinition, MemberRTInfo);
    end;
    tryToEat(',');
  end;
end;

procedure TBoldRegionParser.ParseMembersAndSubregions(CoreDefinition: TBoldRegionCoreDefinition; ClassTypeInfo: TBoldClassTypeInfo);
var
  ConcreteRegionDefinition: TBoldConcreteRegionDefinition;
  AlreadyDefined: Boolean;
begin
  ConcreteRegionDefinition := CoreDefinition.EnsuredConcreteDefinition(ClassTypeInfo, AlreadyDefined);
  if AlreadyDefined then
    AddError(1, sMultipleDefinitions, [CoreDefinition.Name ,classTypeInfo.ExpressionName], petSemantics)
  else
  begin
    ParseMembers(ConcreteRegionDefinition);
    if trytoeat('|') then
      ParseSubregions(ConcreteRegionDefinition);
  end;
end;

procedure TBoldRegionParser.Skip;
begin
  while not EOS and CharInSet(NextToken, [SPACE, TAB]) do
    inc(fPosition);
end;

function TBoldRegionParser.TryToEat(s: string): Boolean;
var
  i: Integer;
begin
  result := True;
  for i := 1 to length(s) do
    if EOS or (fExpression[fPosition+i-1] <> s[i]) then
      begin
        result := False;
        break;
      end;
  if Result then
  begin
    fPosition := fPosition + length(s);
    Skip;
  end;
end;

function TBoldRegionParser.GetNextToken: Char;
begin
  result := fExpression[fPosition];
end;


procedure TBoldRegionParser.ParseCurrentExpression;
var
  ClassName: string;
  ClassTypeInfo: TBoldClassTypeInfo;
  CoreDefinition: TBoldRegionCoreDefinition;
  RegionName: string;
begin
  RegionName := GetSymbol(stRegionName);
  CoreDefinition := RegionDefinitions.EnsuredCoreDefinition(RegionName);
  Eat('[');
  Classname := GetSymbol(stClassName);
  Eat(']');
  Eat(':');
  ClassTypeInfo := fSystemTypeInfo.ClassTypeInfoByExpressionName[ClassName];
  if Assigned(ClassTypeInfo) then
    ParseMembersAndSubregions(CoreDefinition, ClassTypeInfo)
  else
    AddError(fLastSymbolPosition, sUnknownClassName, [ClassName], petSemantics);
end;

destructor TBoldRegionParser.Destroy;
begin
  FreeAndNil(fErrors);
  inherited;
end;

procedure TBoldRegionParser.AddError(pos: integer; msg: String; args: array of const; ErrorType: TBoldRegionParserErrorType);
var
  ErrorMsg: string;
begin
  ErrorMsg := '';
  if Pos <> -1 then
    ErrorMsg := format('"%s" (%d): ', [fExpression, pos]); // do not localize
  ErrorMsg := ErrorMsg + format(Msg, args);
  fErrors.Add(ErrorMsg);
  if ErrorType = petSyntax then
    raise EBoldLockExpressionSyntaxError.Create(ErrorMsg);
end;

procedure TBoldRegionParser.CheckRegionReferences;
var
  i, j: integer;
  CoreDef: TBoldRegionCoreDefinition;
begin
  for i := 0 to fRegionDefinitions.CoreDefinitions.Count-1 do
  begin
    CoreDef := TBoldRegionCoreDefinition(fRegionDefinitions.CoreDefinitions[i]);
    if CoreDef.ConcreteDefinitions.Count = 0 then
      for j := 0 to CoreDef.UsedBy.Count-1 do
        AddError(-1, sReferencedRegionNotDefined, [
          CoreDef.Name,
          CoreDef.UsedBy[j].ParentRegion.CoreDefinition.Name,
          CoreDef.UsedBy[j].ParentRegion.RootClass.ExpressionName,
          CoreDef.UsedBy[j].SubregionRootNavigation.ExpressionName], petSemantics);
  end;
end;

end.