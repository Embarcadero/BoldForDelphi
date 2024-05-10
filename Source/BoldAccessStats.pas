{ Global compiler directives }
{$include bold.inc}
unit BoldAccessStats;

interface

uses
  SysUtils, Classes, BoldSystemRT, BoldSystem, BoldHandles, BoldSystemHandle, BoldMeta, BoldDBInterfaces;

procedure GetMemberAccessStats(const BoldSystemHandle: TBoldSystemHandle; const Lines: TStrings; PrintHeader: Boolean = True; const Prefix: string = ''; const Postfix: string = '');
procedure GetAssociationAccessStats(const BoldSystemHandle: TBoldSystemHandle; const Lines: TStrings; PrintHeader: Boolean = True; const Prefix: string = ''; const Postfix: string = '');

procedure SaveToFile(const BoldSystemHandle: TBoldSystemHandle);

implementation

uses
  AttracsTraceLog,

  BoldCoreConsts,
  BoldGuard;

procedure GetMemberAccessStats(const BoldSystemHandle: TBoldSystemHandle; const Lines: TStrings; PrintHeader: Boolean = True; const Prefix: string = ''; const Postfix: string = '');
var
  AccessStats: TArrayOfArrayOfIntegers;
  DeriveStats: TArrayOfArrayOfIntegers;
  InvalidateStats: TArrayOfArrayOfIntegers;
  ModifyStats: TArrayOfArrayOfIntegers;
  I, J: Integer;
  SystemTypeInfo: TBoldSystemTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  DefiningClass: TBoldClassTypeInfo;
  Level: Integer;

  procedure FindDefiningClass(Member: TBoldMemberRTInfo);
  begin
    Level := 0;
    DefiningClass := Member.ClassTypeInfo;
    while DefiningClass.FirstOwnMemberIndex > Member.index do
    begin
      DefiningClass := DefiningClass.SuperClassTypeInfo;
      Inc(Level);
    end;
  end;

begin
  AccessStats := BoldSystemHandle.System.fAccessStats;
  DeriveStats := BoldSystemHandle.System.fDeriveStats;
  InvalidateStats := BoldSystemHandle.System.fInvalidateStats;
  ModifyStats := BoldSystemHandle.System.fModifyStats;

  if PrintHeader then
    Lines.Add('ClassName,MemberName,Depth,DefiningClassName,ClassAndMemberName,DefiningClassAndMemberName,IsAttribute,IsRole,IsSingleRole,IsMultiRole,Persistent,IsStoredInObject,IsDerived,IsReverseDerived,AccessCount,DeriveCount,InvalidateCount,ModifyCount');

  SystemTypeInfo := BoldSystemHandle.System.BoldSystemTypeInfo;
  for I := 0 to SystemTypeInfo.TopSortedClasses.Count-1 do
  begin
    ClassTypeInfo := SystemTypeInfo.TopSortedClasses[I];
    for J := 0 to ClassTypeInfo.AllMembersCount-1 do
    begin
      MemberRTInfo := ClassTypeInfo.AllMembers[J];
      FindDefiningClass(MemberRTInfo);
      Lines.Add(Format('%15:s''%0:s'',''%1:s'',%17:d,''%2:s'',''%0:s.%1:s'',''%2:s.%1:s'',%3:s,%4:s,%5:s,%6:s,%7:s,%8:s,%9:s,%10:s,%11:d,%12:d,%13:d,%14:d%16:s',[
        ClassTypeInfo.ExpressionName,
        MemberRTInfo.ExpressionName,
        DefiningClass.ExpressionName,
        BoolToStr(MemberRTInfo.IsAttribute,True),
        BoolToStr(MemberRTInfo.IsRole,True),
        BoolToStr(MemberRTInfo.IsSingleRole,True),
        BoolToStr(MemberRTInfo.IsMultiRole,True),
        BoolToStr(MemberRTInfo.Persistent,True),
        BoolToStr(MemberRTInfo.IsStoredInObject,True),
        BoolToStr(MemberRTInfo.IsDerived,True),
        BoolToStr(MemberRTInfo.IsReverseDerived,True),
        AccessStats[I,J],
        DeriveStats[I,J],
        InvalidateStats[I,J],
        ModifyStats[I,J],
        Prefix,
        Postfix,
        Level]));
    end;
  end;

end;


procedure GetAssociationAccessStats(const BoldSystemHandle: TBoldSystemHandle; const Lines: TStrings; PrintHeader: Boolean = True; const Prefix: string = ''; const Postfix: string = '');
var
  AccessStats: TArrayOfArrayOfIntegers;
  DeriveStats: TArrayOfArrayOfIntegers;
  InvalidateStats: TArrayOfArrayOfIntegers;
  ModifyStats: TArrayOfArrayOfIntegers;
  AccumulatedAccess: Integer;
  AccumulatedDerive: Integer;
  AccumulatedInvalidate: Integer;
  AccumulatedModify: Integer;
  I: Integer;
  SystemTypeInfo: TBoldSystemTypeInfo;
  MoldModel: TMoldModel;
  MoldAssociation: TMoldAssociation;
  Role1, Role2: TBoldRoleRTInfo;
  LinkClassRole1, LinkClassRole2: TBoldRoleRTInfo;
  LinkClass: TBoldClassTypeInfo;
  LinkObjectRole1, LinkObjectRole2: TBoldRoleRTInfo;
  Class1, Class2: TBoldClassTypeInfo;
  tempMember: TBoldMemberRTInfo;
  LinkClassStr: string;
  SubClassCount: Integer;

  procedure ClearVars;
  begin
    AccumulatedAccess := 0;
    AccumulatedDerive := 0;
    AccumulatedInvalidate := 0;
    AccumulatedModify := 0;
    Role1 := nil;
    Role2  := nil;
    LinkClassRole1 := nil;
    LinkClassRole2 := nil;
    LinkClass := nil;
    LinkObjectRole1 := nil;
    LinkObjectRole2 := nil;
    Class1 := nil;
    Class2 := nil;
    tempMember  := nil;
    LinkClassStr := '';
    SubClassCount := 0;
  end;

  procedure AccumulateSubClasses(aRole, aLinkObjectRole: TBoldRoleRTInfo; moldClass: TMoldClass);
  var
    I: Integer;
    SubClass: TMoldClass;
    SubClassTypeInfo: TBoldClassTypeInfo;
    SubRole,
    SubLinkClassRole: TBoldRoleRTInfo;
  begin
    if assigned(aRole) {and assigned(aRole.OtherEnd)} then
      for I := 0 to moldClass.Subclasses.Count - 1 do
      begin
        SubClass := moldClass.Subclasses[I];
        SubClassTypeInfo := SystemTypeInfo.TopSortedClasses.ItemsByModelName[SubClass.name];
        Subrole := SubClassTypeInfo.AllMembers.ItemsByModelName[aRole.ModelName] as TBoldRoleRTInfo;
        Inc(AccumulatedAccess, AccessStats[SubRole.ClassTypeInfo.TopSortedIndex, SubRole.index]);
        Inc(AccumulatedDerive, DeriveStats[SubRole.ClassTypeInfo.TopSortedIndex, SubRole.index]);
        Inc(AccumulatedInvalidate, InvalidateStats[SubRole.ClassTypeInfo.TopSortedIndex, SubRole.index]);
        Inc(AccumulatedModify, ModifyStats[SubRole.ClassTypeInfo.TopSortedIndex, SubRole.index]);
        if Assigned(aLinkObjectRole) then
        begin
          SubLinkClassRole := SystemTypeInfo.TopSortedClasses.ItemsByModelName[SubClass.name].AllMembers.ItemsByModelName[aLinkObjectRole.ModelName] as TBoldRoleRTInfo;
          Inc(AccumulatedAccess, AccessStats[SubLinkClassRole.ClassTypeInfo.TopSortedIndex, SubLinkClassRole.index]);
          Inc(AccumulatedDerive, DeriveStats[SubLinkClassRole.ClassTypeInfo.TopSortedIndex, SubLinkClassRole.index]);
          Inc(AccumulatedInvalidate, InvalidateStats[SubLinkClassRole.ClassTypeInfo.TopSortedIndex, SubLinkClassRole.index]);
          Inc(AccumulatedModify, ModifyStats[SubLinkClassRole.ClassTypeInfo.TopSortedIndex, SubLinkClassRole.index]);
        end;
        Inc(SubClassCount);
        AccumulateSubClasses(aRole, aLinkObjectRole, SubClass);
      end;
  end;

begin
  AccessStats := BoldSystemHandle.System.fAccessStats;
  DeriveStats := BoldSystemHandle.System.fDeriveStats;
  InvalidateStats := BoldSystemHandle.System.fInvalidateStats;
  ModifyStats := BoldSystemHandle.System.fModifyStats;

  if PrintHeader then
    Lines.Add('AssociationName,LinkClassName,Role1ClassAndMemberName,Role2ClassAndMemberName,Persistent,SubClassCount,AccessCount,DeriveCount,InvalidateCount,ModifyCount');

  MoldModel := BoldSystemHandle.SystemTypeInfoHandle.BoldModel.MoldModel;
  SystemTypeInfo := BoldSystemHandle.System.BoldSystemTypeInfo;

  for I := 0 to moldModel.Associations.Count - 1 do
  begin
    ClearVars;
    MoldAssociation := moldModel.Associations[I];

    Class1 := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.Roles[0].moldClass.name];
    TempMember := Class1.MemberRTInfoByModelName[moldAssociation.Roles[0].name];
    Role1 := TBoldRoleRTInfo(TempMember);
    if Assigned(Role1) then begin
      Inc(AccumulatedAccess, AccessStats[Role1.ClassTypeInfo.TopSortedIndex, Role1.index]);
      Inc(AccumulatedDerive, DeriveStats[Role1.ClassTypeInfo.TopSortedIndex, Role1.index]);
      Inc(AccumulatedInvalidate, InvalidateStats[Role1.ClassTypeInfo.TopSortedIndex, Role1.index]);
      Inc(AccumulatedModify, ModifyStats[Role1.ClassTypeInfo.TopSortedIndex, Role1.index]);
    end;

    Class2 := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.Roles[1].moldClass.name];
    TempMember := Class2.MemberRTInfoByModelName[moldAssociation.Roles[1].name];
    Role2 := TBoldRoleRTInfo(TempMember);
    if Assigned(Role2) then begin
      Inc(AccumulatedAccess, AccessStats[Role2.ClassTypeInfo.TopSortedIndex, Role2.index]);
      Inc(AccumulatedDerive, DeriveStats[Role2.ClassTypeInfo.TopSortedIndex, Role2.index]);
      Inc(AccumulatedInvalidate, InvalidateStats[Role2.ClassTypeInfo.TopSortedIndex, Role2.index]);
      Inc(AccumulatedModify, ModifyStats[Role2.ClassTypeInfo.TopSortedIndex, Role2.index]);
    end;

    if Assigned(moldAssociation.LinkClass) then
    begin
      LinkClass := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.LinkClass.name];
      LinkClassRole1 := LinkClass.MemberRTInfoByModelName[moldAssociation.Roles[0].name] as TBoldRoleRTInfo;
      LinkClassRole2 := LinkClass.MemberRTInfoByModelName[moldAssociation.Roles[1].name] as TBoldRoleRTInfo;
      Inc(AccumulatedAccess, AccessStats[LinkClassRole1.ClassTypeInfo.TopSortedIndex, LinkClassRole1.index]);
      Inc(AccumulatedDerive, DeriveStats[LinkClassRole1.ClassTypeInfo.TopSortedIndex, LinkClassRole1.index]);
      Inc(AccumulatedInvalidate, InvalidateStats[LinkClassRole1.ClassTypeInfo.TopSortedIndex, LinkClassRole1.index]);
      Inc(AccumulatedModify, ModifyStats[LinkClassRole1.ClassTypeInfo.TopSortedIndex, LinkClassRole1.index]);
      Inc(AccumulatedAccess, AccessStats[LinkClassRole2.ClassTypeInfo.TopSortedIndex, LinkClassRole2.index]);
      Inc(AccumulatedDerive, DeriveStats[LinkClassRole2.ClassTypeInfo.TopSortedIndex, LinkClassRole2.index]);
      Inc(AccumulatedInvalidate, InvalidateStats[LinkClassRole2.ClassTypeInfo.TopSortedIndex, LinkClassRole2.index]);
      Inc(AccumulatedModify, ModifyStats[LinkClassRole2.ClassTypeInfo.TopSortedIndex, LinkClassRole2.index]);

      LinkObjectRole1 := Role1.ClassTypeInfo.AllMembers[Role1.index + 1] as TBoldRoleRTInfo;
      if Assigned(LinkObjectRole1) then begin
        Inc(AccumulatedAccess, AccessStats[LinkObjectRole1.ClassTypeInfo.TopSortedIndex, LinkObjectRole1.index]);
        Inc(AccumulatedDerive, DeriveStats[LinkObjectRole1.ClassTypeInfo.TopSortedIndex, LinkObjectRole1.index]);
        Inc(AccumulatedInvalidate, InvalidateStats[LinkObjectRole1.ClassTypeInfo.TopSortedIndex, LinkObjectRole1.index]);
        Inc(AccumulatedModify, ModifyStats[LinkObjectRole1.ClassTypeInfo.TopSortedIndex, LinkObjectRole1.index]);
      end;
      LinkObjectRole2 := Role2.ClassTypeInfo.AllMembers[Role2.index + 1] as TBoldRoleRTInfo;
      if Assigned(LinkObjectRole2) then begin
        Inc(AccumulatedAccess, AccessStats[LinkObjectRole2.ClassTypeInfo.TopSortedIndex, LinkObjectRole2.index]);
        Inc(AccumulatedDerive, DeriveStats[LinkObjectRole2.ClassTypeInfo.TopSortedIndex, LinkObjectRole2.index]);
        Inc(AccumulatedInvalidate, InvalidateStats[LinkObjectRole2.ClassTypeInfo.TopSortedIndex, LinkObjectRole2.index]);
        Inc(AccumulatedModify, ModifyStats[LinkObjectRole2.ClassTypeInfo.TopSortedIndex, LinkObjectRole2.index]);
      end;

      LinkClassStr := LinkClass.ExpressionName;
    end;

    AccumulateSubClasses(Role1, LinkObjectRole1, moldAssociation.Roles[0].moldClass);
    AccumulateSubClasses(Role2, LinkObjectRole2, moldAssociation.Roles[1].moldClass);


    Lines.Add(Format('%s''%s'',''%s'',''%s.%s'',''%s.%s'',%s,%d,%d,%d,%d,%d%s',[
      Prefix,
      MoldAssociation.ExpandedExpressionName,
      LinkClassStr,
      moldAssociation.Roles[0].moldClass.ExpandedExpressionName, moldAssociation.Roles[0].ExpandedExpressionName,
      moldAssociation.Roles[1].moldClass.ExpandedExpressionName, moldAssociation.Roles[1].ExpandedExpressionName,
      BoolToStr(MoldAssociation.EffectivePersistent,True),
      SubClassCount,
      AccumulatedAccess,
      AccumulatedDerive,
      AccumulatedInvalidate,
      AccumulatedModify,
      Postfix]));
  end;

end;


procedure SaveToFile(const BoldSystemHandle: TBoldSystemHandle);
var
  vGuard: IBoldGuard;
  vMemberData: TStringList;
  vAssociationData: TStringList;
  ServerTimestamp: TDateTime;
  vTimeStampStrDB: string;
  vTimeStampStrFile: string;
  vQuerySnippet1, vQuerySnippet2: string;
  I: Integer;
begin
  TraceLog.SystemMessage('Storing Access Statistics on Members and Associations to file...', ekInfo);
  vGuard := BoldGuard.TBoldGuard.Create(vMemberData,vAssociationData);
  vMemberData := TStringList.Create;
  vAssociationData := TStringList.Create;
  ServerTimestamp := Now;
  DateTimeToString(vTimeStampStrDb, BoldDateTimeFormat, ServerTimestamp);
  DateTimeToString(vTimeStampStrFile,'yyyymmddhhnnsszzz',ServerTimestamp);
  vQuerySnippet1 := 'INSERT INTO StatisticOnMember VALUES ('''+vTimeStampStrDb+''','''+ExtractFileName(ParamStr(0))+''',';
  vQuerySnippet2 := 'INSERT INTO StatisticOnAssociation VALUES ('''+vTimeStampStrDb+''','''+ExtractFileName(ParamStr(0))+''',';
  GetMemberAccessStats(BoldSystemHandle, vMemberData, False, vQuerySnippet1, ');');
  vMemberData.SaveToFile(TraceLog.LogFileDir+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'_AccessStatMember_'+vTimeStampStrFile+'.sql' );
  GetAssociationAccessStats(BoldSystemHandle, vAssociationData, False, vQuerySnippet2, ');');
  vAssociationData.SaveToFile(TraceLog.LogFileDir+ChangeFileExt(ExtractFileName(ParamStr(0)),'')+'_AccessStatAssociation_'+vTimeStampStrFile+'.sql' );
  TraceLog.SystemMessage('Done!', ekInfo);
end;

(*

CREATE TABLE StatisticOnMember (
StorageTimeStamp TIMESTAMP,
Application VARCHAR(100),
ClassName VARCHAR(100),
MemberName VARCHAR(100),
Depth INTEGER,
DefiningClassName VARCHAR(100),
ClassAndMemberName VARCHAR(100),
DefiningClassAndMemberName VARCHAR(100),
IsAttribute BOOLEAN,
IsRole BOOLEAN,
IsSingleRole BOOLEAN,
IsMultiRole BOOLEAN,
Persistent BOOLEAN,
IsStoredInObject BOOLEAN,
IsDerived BOOLEAN,
IsReverseDerived BOOLEAN,
AccessCount INTEGER,
DeriveCount INTEGER,
InvalidateCount INTEGER,
ModifyCount INTEGER
);

CREATE TABLE StatisticOnAssociation (
StorageTimeStamp TIMESTAMP,
Application VARCHAR(100),
AssociationName VARCHAR(100),
LinkClassName VARCHAR(100),
Role1ClassAndMemberName VARCHAR(100),
Role2ClassAndMemberName VARCHAR(100),
Persistent BOOLEAN,
SubClassCount INTEGER,
AccessCount INTEGER,
DeriveCount INTEGER,
InvalidateCount INTEGER,
ModifyCount INTEGER
);

*)

end.
