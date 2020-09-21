unit Bold1To2Upgrader;

interface
uses
  Classes,
  FileReplacer;

procedure UpdatePackages( FileContents : TStringList );
procedure UpdateIdentifiers( FileContents : TStringList );
procedure UpdateComponents( FileContents : TStringList );
procedure UpdateUnitNames( FileContents : TStringList );
procedure UpdateRoothandle( FileContents : TStringList);

procedure UpdateDFMTo206( FileContents : TStringList);
procedure Update20To25( FileContents : TStringList);

procedure UpdateOptionalComponents( FileContents : TStringList; FileName: String );

procedure CleanUpComments( FileContents : TStringList );

procedure UpdateModelAware( FileContents : TStringList;
   M_Members, NonStringAttributes, AttributeNames, AttributeAccessors: TStrings );

implementation

procedure UpdatePackages( FileContents : TStringList );
begin
  UpdateWord( FileContents, 'Bold40', 'Bold20D4');
  UpdateWord( FileContents, 'Bold50', 'Bold20D5');
end;

procedure UpdateOptionalComponents( FileContents : TStringList; FileName: String );
begin
  ReTypeComponent( FileContents, FileName, 'TBoldExpressionHandle', 'TBoldReferenceHandle', ['BoldHandle', 'Expression'] );
end;


procedure UpdateIdentifiers( FileContents : TStringList );
begin
{Elements}

  UpdateWord( FileContents, 'TBoldDirectElement', 'TBoldElement');
  UpdateWord( FileContents, 'TBoldBusinessElement', 'TBoldDomainElement' );

  UpdateWord( FileContents, 'TBoldBusinessElementClass', 'TBoldDomainElementClass' );

  UpdateWord( FileContents, 'CurrentElement.Value', 'Element');
  UpdateWord( FileContents, 'Element.Value', 'Element');

  UpdateWord( FileContents, 'EnsureCurrent', 'EnsureContentsCurrent{(13)}');

  UpdateWord( FileContents, '.SubscribeToBoldChanges', '.{(16)Obsolete}SubscribeToBoldChanges');

  UpdateWord( FileContents, 'BoldCursorElement,', '{BoldCursorElement,}');
  UpdateWord( FileContents, 'BoldRootedElements,', '{BoldRootedElements,}');

  UpdateWord( FileContents, 'BoldCursorElementListControlPack,', 'BoldListListControlPack,');
  UpdateWord( FileContents, 'TBoldCursorElementAsFollowerListController,', 'TBoldAbstractListAsFollowerListController,');

{Followers}
  UpdateWord( FileContents, 'ElementValue', 'Elemnent' );
  UpdateWord( FileContents, 'bfsOutOfDate', 'bfsValueOutOfDate' );

{Constructors}
  UpdateWord( FileContents, 'CreateTypedList(nil,', 'CreateTypedList(');
  UpdateWord( FileContents, 'CreateTypedList( nil,', 'CreateTypedList(');

  UpdateWord( FileContents, 'TBAString.Create(nil)', 'TBAString.Create');
  UpdateWord( FileContents, 'TBAInteger.Create(nil)', 'TBAInteger.Create');
  UpdateWord( FileContents, 'TBADateTime.Create(nil)', 'TBADateTime.Create');
  UpdateWord( FileContents, 'TBAFloat.Create(nil)', 'TBAFloat.Create');

  UpdateWord( FileContents, 'TBAString.Create( nil )', 'TBAString.Create');
  UpdateWord( FileContents, 'TBAInteger.Create( nil )', 'TBAInteger.Create');
  UpdateWord( FileContents, 'TBADateTime.Create( nil )', 'TBADateTime.Create');
  UpdateWord( FileContents, 'TBAFloat.Create( nil )', 'TBAFloat.Create');

  UpdateWord( FileContents, 'TBoldObjectReference.Create(nil)', 'TBoldObjectReference.Create');
  UpdateWord( FileContents, 'TBoldObjectReference.Create( nil )', 'TBoldObjectReference.Create');

  UpdateWord( FileContents, 'TBoldObjectList.Create( nil )', 'TBoldObjectList.Create');
  UpdateWord( FileContents, 'TBoldObjectList.Create(nil)', 'TBoldObjectList.Create');

{Handles}

  UpdateWord( FileContents, 'Handle.Element', 'Handle.Value', false);

  UpdateWordInNeighbourhoodOf( FileContents, 'Value.Element', 'Value.Value', ['Value: TBoldElementHandle'], 10 );
  UpdateWord( FileContents, '.Element', '.{(1)FIX: Value}Element');
  UpdateWord( FileContents, '.Root', '.{(7)FIX: Value}Root');

  UpdateWord( FileContents, 'BoldHandles,', 'BoldHandles{(*)}, BoldExpressionHandle,'); // unitnames
  UpdateWord( FileContents, 'TrackBold,', 'Subscribe');
  UpdateWord( FileContents, 'BoldEvaluate', 'MarkValueOutOfDate');
  UpdateWord( FileContents, 'HandleValueType', 'StaticBoldType', false);
  UpdateWord( FileContents, 'BoldHandle.BoldSystemTypeInfo', 'BoldHandle.StaticSystemTypeInfo', false );

  UpdateWord( FileContents, '.open', '.{(10)active := true}open');
  UpdateWord( FileContents, '.close', '.{(10)active := false}close');


{ValueSetHandle}

  UpdateWord( FileContents, 'BoldValueSetHandle,', '{BoldValueSetHandle,}' );
  UpdateWord( FileContents, 'TBoldValueSetHandle', 'TBoldListHandle' );

{SQLHandle}

  UpdateWord( FileContents, 'TBoldSQLListHandle', 'TBoldSQLHandle' ); //renames
  UpdateWord( FileContents, '.SQLWhere', '.SQLWhereClause' );
  UpdateWord( FileContents, '.SQLOrderBy', '.SQLOrderByClause' );

{ObjectLists}

  UpdateWord( FileContents, 'Items[', 'Items{(8)FIX: BoldObjects}[');
  UpdateWord( FileContents, 'LinkObjectFor[', 'LinkObjectFor{Property has been removed}[');
  UpdateWord( FileContents, 'LinkObjects[', 'LinkObjects{Property has been removed}[');

{Derived attributes}

  UpdateWord( FileContents, '_MakeBoldCurrent(sender: TBoldElement; Resubscribe: Boolean);', '_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);', false);
  UpdateWordInNeighbourhoodOf( FileContents, 'if Resubscribe then', 'if {(9)}assigned(Subscriber) then', ['DerivedObject: TObject'], 100);
  UpdateWord( FileContents, '_*Subscriber', 'Subscriber' );

{BoldObject/Locators}

  UpdateWord( FileContents, 'MemberByExpressionName', 'BoldMemberByExpressionName');
  UpdateWord( FileContents, 'MemberIndexByExpressionName(', 'BoldMemberIndexByExpressionName[{Change to end bracket->}' );
  UpdateWord( FileContents, '.ID', '.{(5)FIX: BoldObjectLocator.BoldObjectId)}ID');

  UpdateWord( FileContents, 'TBoldPersistenceState', '{(4)Add uses BoldDefs}TBoldObjectPersistenceState_X');
  UpdateWord( FileContents, 'PersistenceState = bpsModified', 'BoldDirty');
  UpdateWord( FileContents, 'PersistenceState in [bpsModifiedNew,bpsModified]', 'BoldDirty');
  UpdateWord( FileContents, 'PersistenceState in [bpsModified,bpsModifiedNew]', 'BoldDirty');
  UpdateWord( FileContents, 'PersistenceState in bpsDirty', 'BoldDirty');
  UpdateWord( FileContents, '.PersistenceState', '.{(4)BoldObjectPersistenceState}PersistenceState');

  UpdateWord( FileContents, 'TBoldObjectID', 'TBoldObjectLocator');

{BoldSystem}

  UpdateWord( FileContents, '.System', '.{(17)BoldSystem}System' );
  UpdateWord( FileContents, '.RemoveDeletedObjects', '.{Obsolete}RemoveDeletedObjects' );
  UpdateWord( FileContents, 'ultimateOwner', 'BoldSystem' );

{SystemRT}

  UpdateWord( FileContents, 'TBoldValueTypeInfo', 'TBoldElementTypeInfo');
  UpdateWord( FileContents, 'TBoldClassRTInfo', 'TBoldClassTypeInfo');
  UpdateWord( FileContents, 'TBoldSystemRTInfo', 'TBoldSystemTypeInfo');
  UpdateWord( FileContents, 'BoldSystemRtInfo', 'BoldSystemTypeInfo');
  UpdateWord( FileContents, 'BoldClassRtInfo', 'BoldClassTypeInfo');
  UpdateWord( FileContents, 'BoldSystemTypeInfo.ClassByExpressionName', 'BoldSystemTypeInfo.ClassTypeInfoByExpressionName');
  UpdateWord( FileContents, 'BoldSystem.SystemTypeInfo', 'BoldSystem.BoldSystemTypeInfo', false );
  UpdateWord( FileContents, 'BoldClassTypeInfo.memberCount', 'BoldClassTypeInfo.AllMembers.Count');
  UpdateWord( FileContents, 'AllRTMembers', 'AllMembers');
  UpdateWord( FileContents, 'ClassTypeByExpressionName', 'ClassTypeInfoByExpressionName');
  UpdateWord( FileContents, 'ListTypeByElement', 'ListTypeInfoByElement');
  UpdateWord( FileContents, '.rtInfo.', '.');
  UpdateWord( FileContents, '.SuperClass', '.{(12)SuperClassTypeInfo}SuperClass' );
  UpdateWord( FileContents, 'ModelTypeInfo', 'SystemTypeInfo');


  UpdateWord( FileContents, 'BoldObjectClasses.EntryByModelName', 'ClassTypeInfoByModelName' );

  UpdateWord( FileContents, 'TBoldClassRTInfoList', 'TBoldClassTypeInfoList');
  UpdateWord( FileContents, 'AttributeTypeByExpressionName', 'AttributeTypeInfoByExpressionName');

  UpdateWord( FileContents, 'BoldElementRT,', '{BoldElementRT ,}');


{Other}

  UpdateWord( FileContents, 'TBoldSingleLink', 'TBoldObjectReference');
  UpdateWord( FileContents, 'TBoldMultiLink', 'TBoldObjectList');
  UpdateWord( FileContents, 'TBoldRose98Link', 'TBoldUMLRose98Link');

  UpdateWord( FileContents, '.NotifyComponentRemoved', '.{(2)Obsolete, remove!}NotifyComponentRemoved');

  UpdateWord( FileContents, 'TBoldLookupComboBox', 'TBoldComboBox' );

  UpdateWord( FileContents, 'BoldFormHandler.CreateDefaultFormForElement', 'AutoFormProviderRegistry.FormForElement');

  UpdateWord( FileContents, 'BoldCurrentLanguage', 'BoldPrimaryLanguage' );
  UpdateWord( FileContents, 'BoldDefaultLanguage', 'BoldSecondaryLanguage' );

{Attribute registration}
  UpdateWord( FileContents, 'AddMemberTypeDescriptor(', 'AddMemberTypeDescriptor{(6)}(');
  UpdateWord( FileContents, 'BoldMemberPersistenceMappers.AddDescriptor(', 'BoldMemberPersistenceMappers.AddDescriptor{(6)}(');
  UpdateWord( FileContents, 'removeDescriptorByName', 'RemoveDescriptorByClass{(6)}' );

  UpdateWord( FileContents, 'TBoldDbString', 'TBoldPMString');
  UpdateWord( FileContents, 'ValueFromField(', 'ValueFromField{(11)}(');
  UpdateWord( FileContents, 'ValueToParam(', 'ValueToParam{(11)}(');


end;

procedure UpdateComponents(FileContents : TStringList);
begin
  RenameProperty( FileContents, ['TBoldSQLListHandle'], 'SQLWhere', 'SQLWhereClause' );
  RenameProperty( FileContents, ['TBoldSQLListHandle'], 'SQLOrderBy', 'SQLOrderByClause' );
  RenameProperty( FileContents, ['TBoldSQLListHandle'], 'SQLClassName', 'ClassExpressionName' );

  RenameProperty( FileContents, ['TBoldExpressionHandle'], 'TrackBold', 'Subscribe' );
  RenameProperty( FileContents, ['TBoldListHandle'], 'TrackBold', 'Subscribe' );

  UpdateWord( FileContents, ': TBoldSQLListHandle', ': TBoldSQLHandle' );
  UpdateWord( FileContents, ': TBoldBDEDataBase', ': TDataBase' );
  UpdateWord( FileContents, ': TBoldRose98Link', ': TBoldUMLRose98Link');

  UpdateWord( FileContents, ': TBoldValueSetHandle', ': TBoldListHandle' );
  UpdateWord( FileContents, ': TBoldLookupComboBox', ': TBoldComboBox' );
end;



procedure UpdateUnitNames(FileContents : TStringList);
var
  OldNames : TStringList;
  NewNames : TStringList;

  procedure Add( Old, New : String );
  begin
    OldNames.add( Old );
    NewNames.Add( New );
  end;

begin
  OldNames := TStringList.Create;
  NewNames := TStringList.Create;

  Add( 'BoldSQLListHandle', 'BoldSQLHandle' );
  Add( 'BoldLookupComboBox', 'BoldComboBox' );
  Add( 'BoldBusinessElement', 'BoldDomainElement' );
  Add( 'BoldFormGen', 'BoldAFP' );
  Add( 'BoldFilt', 'BoldFilteredHandle|BoldSortedHandle' );
  UpdateusesNames( FileContents, Oldnames, NewNames );

  OldNames.Free;
  NewNames.Free;
end;

procedure CleanUpComments( FileContents : TStringList );
begin
  UpdateWord( FileContents, '{(1)FIX: Value}', '');
  UpdateWord( FileContents, '{(2)Obsolete, remove!}', '');

  UpdateWord( FileContents, '{.(3)AsCurrency}', '' );
  UpdateWord( FileContents, '{.(3)AsString}', '' );
  UpdateWord( FileContents, '{.(3)AsBoolean}', '' );
  UpdateWord( FileContents, '{.(3)AsInteger}', '' );
  UpdateWord( FileContents, '{.(3)AsWord}', '' );
  UpdateWord( FileContents, '{.(3)AsDate}', '' );
  UpdateWord( FileContents, '{.(3)AsTime}', '' );
  UpdateWord( FileContents, '{.(3)AsDateTime}', '' );
  UpdateWord( FileContents, '{.(3)AsFloat}', '' );

  UpdateWord( FileContents, '{(4)BoldObjectPersistenceState}', '');
  UpdateWord( FileContents, '{(4)Add uses BoldDefs}', '');
  UpdateWord( FileContents, '{(5)FIX: BoldObjectLocator.BoldObjectId)}', '');

  UpdateWord( FileContents, '{(6)}', '');
  UpdateWord( FileContents, '{(7)FIX: Value}', '');
  UpdateWord( FileContents, '{(8)FIX: BoldObjects}', '');
  UpdateWord( FileContents, '{(9)}', '');
  UpdateWord( FileContents, '{(10)active := true}', '');
  UpdateWord( FileContents, '{(10)active := false}', '');
  UpdateWord( FileContents, '{(11)}', '');

  UpdateWord( FileContents, '{(12)SuperClassTypeInfo}', '' );

  UpdateWord( FileContents, '{(13)}', '');

  UpdateWord( FileContents, '{(14)}', '');
  UpdateWord( FileContents, '{BoldValueSetHandle,}', '');
  UpdateWord( FileContents, '{BoldCursorElement,}', '');
  UpdateWord( FileContents, '{BoldRootedElements,}', '');
  UpdateWord( FileContents, '{BoldElementRT,}', '');

  UpdateWord( FileContents, '{(17)BoldSystem}', '');

  UpdateWord( FileContents, '{(*)}', '');

end;

procedure UpdateModelAware( FileContents : TStringList; M_Members, NonStringAttributes, AttributeNames, AttributeAccessors: TStrings );
var
  i : integer;
begin
  for i := 0 to M_Members.Count-1 do begin
    UpdateWord( FileContents, M_Members[i]+'.DefaultSubscribe', 'M_'+M_Members[i]+'.{(14)}DefaultSubscribe');
  end;
  for i := 0 to AttributeNames.Count-1 do begin
    UpdateWord( FileContents, AttributeNames[i]+'.'+AttributeAccessors[i], AttributeNames[i]+'{.(3)'+AttributeAccessors[i]+'}');
  end;
  for i := 0 to NonStringAttributes.Count-1 do begin
    UpdateWord( FileContents, NonStringAttributes[i]+'.AsString', 'M_'+NonStringAttributes[i]+'.{(3)}AsString');
  end;
end;

procedure UpdateRoothandle( FileContents : TStringList);
begin
  RenameProperty( FileContents, [
    'TBoldDerivedHandle',
    'TBoldExpressionHandle',
    'TBoldListHandle',
    'TBoldCursorHandle',
    'TBoldFilteredHandle',
    'TBoldSortedHandle',
    'TBoldCursorHandle' ], 'BoldHandle', 'RootHandle' );
end;

procedure UpdateDFMTo206( FileContents : TStringList);
begin
  UpdateWord( FileContents, ': TBoldVariableCollection', ': TBoldVariableDefinition' );
end;

procedure Update20To25( FileContents : TStringList);
begin
  RenameProperty( FileContents, ['TBoldVariableHandle'], 'ExpressionName', 'ValueTypeName' );
end;




end.
