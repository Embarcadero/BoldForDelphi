(*****************************************)
(*      This file is autogenerated       *)
(*   Any manual changes will be LOST!    *)
(*****************************************)
(* Generated 11-06-2019 15:36:30         *)
(*****************************************)
(* This file should be stored in the     *)
(* same directory as the form/datamodule *)
(* with the corresponding model          *)
(*****************************************)
(* Copyright notice:                     *)
(*                                       *)
(*****************************************)

unit UndoTestModelClasses;

{$DEFINE UndoTestModelClasses_unitheader}
{$INCLUDE UndoTestModelClasses_Interface.inc}

{ Includefile for methodimplementations }

{$INCLUDE UndoTestMode_impl.inc}

const
  BoldMemberAssertInvalidObjectType: string = 'Object of singlelink (%s.%s) is of wrong type (is %s, should be %s)';

{ TTestModelClassesRoot }

procedure TTestModelClassesRootList.Add(NewObject: TTestModelClassesRoot);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TTestModelClassesRootList.IndexOf(anObject: TTestModelClassesRoot): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TTestModelClassesRootList.Includes(anObject: TTestModelClassesRoot) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TTestModelClassesRootList.AddNew: TTestModelClassesRoot;
begin
  result := TTestModelClassesRoot(InternalAddNew);
end;

procedure TTestModelClassesRootList.Insert(index: Integer; NewObject: TTestModelClassesRoot);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TTestModelClassesRootList.GetBoldObject(index: Integer): TTestModelClassesRoot;
begin
  result := TTestModelClassesRoot(GetElement(index));
end;

procedure TTestModelClassesRootList.SetBoldObject(index: Integer; NewObject: TTestModelClassesRoot);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TTestModelClassesRootList.GetEnumerator: TTestModelClassesRootListEnumerator;
begin
 Result := TTestModelClassesRootListEnumerator.Create(Self);
end;

function TTestModelClassesRootListEnumerator.GetCurrent: TTestModelClassesRoot;
begin
 Result := List[Index] as TTestModelClassesRoot;
end;

{$ENDIF UseBoldListEnumerator}

{ TAPersistentClass }

function TAPersistentClass._Get_M_aString: TBAString;
begin
  assert(ValidateMember('TAPersistentClass', 'aString', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TAPersistentClass._GetaString: String;
begin
  Result := M_aString.AsString;
end;

procedure TAPersistentClass._SetaString(const NewValue: String);
begin
  M_aString.AsString := NewValue;
end;

function TAPersistentClass._Get_M_one: TBoldObjectReference;
begin
  assert(ValidateMember('TAPersistentClass', 'one', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TAPersistentClass._Getone: TATransientClass;
begin
  Result := TATransientClass(M_one.BoldObject);
  assert(not assigned(Result) or (Result is TATransientClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'one', Result.ClassName, 'TATransientClass']));
end;

procedure TAPersistentClass._Setone(const value: TATransientClass);
begin
  M_one.BoldObject := value;
end;

procedure TAPersistentClassList.Add(NewObject: TAPersistentClass);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TAPersistentClassList.IndexOf(anObject: TAPersistentClass): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TAPersistentClassList.Includes(anObject: TAPersistentClass) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TAPersistentClassList.AddNew: TAPersistentClass;
begin
  result := TAPersistentClass(InternalAddNew);
end;

procedure TAPersistentClassList.Insert(index: Integer; NewObject: TAPersistentClass);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TAPersistentClassList.GetBoldObject(index: Integer): TAPersistentClass;
begin
  result := TAPersistentClass(GetElement(index));
end;

procedure TAPersistentClassList.SetBoldObject(index: Integer; NewObject: TAPersistentClass);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TAPersistentClassList.GetEnumerator: TAPersistentClassListEnumerator;
begin
 Result := TAPersistentClassListEnumerator.Create(Self);
end;

function TAPersistentClassListEnumerator.GetCurrent: TAPersistentClass;
begin
 Result := List[Index] as TAPersistentClass;
end;

{$ENDIF UseBoldListEnumerator}

{ TATransientClass }

function TATransientClass._Get_M_aString: TBAString;
begin
  assert(ValidateMember('TATransientClass', 'aString', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TATransientClass._GetaString: String;
begin
  Result := M_aString.AsString;
end;

procedure TATransientClass._SetaString(const NewValue: String);
begin
  M_aString.AsString := NewValue;
end;

function TATransientClass._Getmany: TAPersistentClassList;
begin
  assert(ValidateMember('TATransientClass', 'many', 1, TAPersistentClassList));
  Result := TAPersistentClassList(BoldMembers[1]);
end;

procedure TATransientClassList.Add(NewObject: TATransientClass);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TATransientClassList.IndexOf(anObject: TATransientClass): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TATransientClassList.Includes(anObject: TATransientClass) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TATransientClassList.AddNew: TATransientClass;
begin
  result := TATransientClass(InternalAddNew);
end;

procedure TATransientClassList.Insert(index: Integer; NewObject: TATransientClass);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TATransientClassList.GetBoldObject(index: Integer): TATransientClass;
begin
  result := TATransientClass(GetElement(index));
end;

procedure TATransientClassList.SetBoldObject(index: Integer; NewObject: TATransientClass);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TATransientClassList.GetEnumerator: TATransientClassListEnumerator;
begin
 Result := TATransientClassListEnumerator.Create(Self);
end;

function TATransientClassListEnumerator.GetCurrent: TATransientClass;
begin
 Result := List[Index] as TATransientClass;
end;

{$ENDIF UseBoldListEnumerator}

{ TBook }

function TBook._Get_M_Title: TBAString;
begin
  assert(ValidateMember('TBook', 'Title', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TBook._GetTitle: String;
begin
  Result := M_Title.AsString;
end;

procedure TBook._SetTitle(const NewValue: String);
begin
  M_Title.AsString := NewValue;
end;

function TBook._GetTopic: TTopicList;
begin
  assert(ValidateMember('TBook', 'Topic', 1, TTopicList));
  Result := TTopicList(BoldMembers[1]);
end;

function TBook._Gettopicbook: TtopicbookList;
begin
  assert(ValidateMember('TBook', 'topicbook', 2, TtopicbookList));
  Result := TtopicbookList(BoldMembers[2]);
end;

procedure TBookList.Add(NewObject: TBook);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBookList.IndexOf(anObject: TBook): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TBookList.Includes(anObject: TBook) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TBookList.AddNew: TBook;
begin
  result := TBook(InternalAddNew);
end;

procedure TBookList.Insert(index: Integer; NewObject: TBook);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBookList.GetBoldObject(index: Integer): TBook;
begin
  result := TBook(GetElement(index));
end;

procedure TBookList.SetBoldObject(index: Integer; NewObject: TBook);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TBookList.GetEnumerator: TBookListEnumerator;
begin
 Result := TBookListEnumerator.Create(Self);
end;

function TBookListEnumerator.GetCurrent: TBook;
begin
 Result := List[Index] as TBook;
end;

{$ENDIF UseBoldListEnumerator}

{ TClassWithLink }

function TClassWithLink._Get_M_aString: TBAString;
begin
  assert(ValidateMember('TClassWithLink', 'aString', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TClassWithLink._GetaString: String;
begin
  Result := M_aString.AsString;
end;

procedure TClassWithLink._SetaString(const NewValue: String);
begin
  M_aString.AsString := NewValue;
end;

function TClassWithLink._Getmany: TClassWithLinkList;
begin
  assert(ValidateMember('TClassWithLink', 'many', 1, TClassWithLinkList));
  Result := TClassWithLinkList(BoldMembers[1]);
end;

function TClassWithLink._GetmanyLinkClass: TLinkClassList;
begin
  assert(ValidateMember('TClassWithLink', 'manyLinkClass', 2, TLinkClassList));
  Result := TLinkClassList(BoldMembers[2]);
end;

function TClassWithLink._Get_M_one: TBoldObjectReference;
begin
  assert(ValidateMember('TClassWithLink', 'one', 3, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[3]);
end;

function TClassWithLink._Getone: TClassWithLink;
begin
  Result := TClassWithLink(M_one.BoldObject);
  assert(not assigned(Result) or (Result is TClassWithLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'one', Result.ClassName, 'TClassWithLink']));
end;

procedure TClassWithLink._Setone(const value: TClassWithLink);
begin
  M_one.BoldObject := value;
end;

function TClassWithLink._Get_M_oneLinkClass: TBoldObjectReference;
begin
  assert(ValidateMember('TClassWithLink', 'oneLinkClass', 4, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[4]);
end;

function TClassWithLink._GetoneLinkClass: TLinkClass;
begin
  Result := TLinkClass(M_oneLinkClass.BoldObject);
  assert(not assigned(Result) or (Result is TLinkClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'oneLinkClass', Result.ClassName, 'TLinkClass']));
end;

procedure TClassWithLinkList.Add(NewObject: TClassWithLink);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TClassWithLinkList.IndexOf(anObject: TClassWithLink): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TClassWithLinkList.Includes(anObject: TClassWithLink) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TClassWithLinkList.AddNew: TClassWithLink;
begin
  result := TClassWithLink(InternalAddNew);
end;

procedure TClassWithLinkList.Insert(index: Integer; NewObject: TClassWithLink);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TClassWithLinkList.GetBoldObject(index: Integer): TClassWithLink;
begin
  result := TClassWithLink(GetElement(index));
end;

procedure TClassWithLinkList.SetBoldObject(index: Integer; NewObject: TClassWithLink);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TClassWithLinkList.GetEnumerator: TClassWithLinkListEnumerator;
begin
 Result := TClassWithLinkListEnumerator.Create(Self);
end;

function TClassWithLinkListEnumerator.GetCurrent: TClassWithLink;
begin
 Result := List[Index] as TClassWithLink;
end;

{$ENDIF UseBoldListEnumerator}

{ TLinkClass }

function TLinkClass._Get_M_Attribute1: TBAString;
begin
  assert(ValidateMember('TLinkClass', 'Attribute1', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TLinkClass._GetAttribute1: String;
begin
  Result := M_Attribute1.AsString;
end;

procedure TLinkClass._SetAttribute1(const NewValue: String);
begin
  M_Attribute1.AsString := NewValue;
end;

function TLinkClass._Get_M_one: TBoldObjectReference;
begin
  assert(ValidateMember('TLinkClass', 'one', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TLinkClass._Getone: TClassWithLink;
begin
  Result := TClassWithLink(M_one.BoldObject);
  assert(not assigned(Result) or (Result is TClassWithLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'one', Result.ClassName, 'TClassWithLink']));
end;

function TLinkClass._Get_M_many: TBoldObjectReference;
begin
  assert(ValidateMember('TLinkClass', 'many', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TLinkClass._Getmany: TClassWithLink;
begin
  Result := TClassWithLink(M_many.BoldObject);
  assert(not assigned(Result) or (Result is TClassWithLink), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'many', Result.ClassName, 'TClassWithLink']));
end;

procedure TLinkClassList.Add(NewObject: TLinkClass);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TLinkClassList.IndexOf(anObject: TLinkClass): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TLinkClassList.Includes(anObject: TLinkClass) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TLinkClassList.AddNew: TLinkClass;
begin
  result := TLinkClass(InternalAddNew);
end;

procedure TLinkClassList.Insert(index: Integer; NewObject: TLinkClass);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TLinkClassList.GetBoldObject(index: Integer): TLinkClass;
begin
  result := TLinkClass(GetElement(index));
end;

procedure TLinkClassList.SetBoldObject(index: Integer; NewObject: TLinkClass);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TLinkClassList.GetEnumerator: TLinkClassListEnumerator;
begin
 Result := TLinkClassListEnumerator.Create(Self);
end;

function TLinkClassListEnumerator.GetCurrent: TLinkClass;
begin
 Result := List[Index] as TLinkClass;
end;

{$ENDIF UseBoldListEnumerator}

{ TSomeClass }

function TSomeClass._Get_M_aString: TBAString;
begin
  assert(ValidateMember('TSomeClass', 'aString', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TSomeClass._GetaString: String;
begin
  Result := M_aString.AsString;
end;

procedure TSomeClass._SetaString(const NewValue: String);
begin
  M_aString.AsString := NewValue;
end;

function TSomeClass._Getpart: TSomeClassList;
begin
  assert(ValidateMember('TSomeClass', 'part', 1, TSomeClassList));
  Result := TSomeClassList(BoldMembers[1]);
end;

function TSomeClass._Getpartpartpartof: TpartpartofList;
begin
  assert(ValidateMember('TSomeClass', 'partpartpartof', 2, TpartpartofList));
  Result := TpartpartofList(BoldMembers[2]);
end;

function TSomeClass._Getpartof: TSomeClassList;
begin
  assert(ValidateMember('TSomeClass', 'partof', 3, TSomeClassList));
  Result := TSomeClassList(BoldMembers[3]);
end;

function TSomeClass._Getpartofpartpartof: TpartpartofList;
begin
  assert(ValidateMember('TSomeClass', 'partofpartpartof', 4, TpartpartofList));
  Result := TpartpartofList(BoldMembers[4]);
end;

function TSomeClass._Get_M_next: TBoldObjectReference;
begin
  assert(ValidateMember('TSomeClass', 'next', 5, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[5]);
end;

function TSomeClass._Getnext: TSomeClass;
begin
  Result := TSomeClass(M_next.BoldObject);
  assert(not assigned(Result) or (Result is TSomeClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'next', Result.ClassName, 'TSomeClass']));
end;

procedure TSomeClass._Setnext(const value: TSomeClass);
begin
  M_next.BoldObject := value;
end;

function TSomeClass._Get_M_previous: TBoldObjectReference;
begin
  assert(ValidateMember('TSomeClass', 'previous', 6, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[6]);
end;

function TSomeClass._Getprevious: TSomeClass;
begin
  Result := TSomeClass(M_previous.BoldObject);
  assert(not assigned(Result) or (Result is TSomeClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'previous', Result.ClassName, 'TSomeClass']));
end;

procedure TSomeClass._Setprevious(const value: TSomeClass);
begin
  M_previous.BoldObject := value;
end;

function TSomeClass._Getchild: TSomeClassList;
begin
  assert(ValidateMember('TSomeClass', 'child', 7, TSomeClassList));
  Result := TSomeClassList(BoldMembers[7]);
end;

function TSomeClass._Get_M_parent: TBoldObjectReference;
begin
  assert(ValidateMember('TSomeClass', 'parent', 8, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[8]);
end;

function TSomeClass._Getparent: TSomeClass;
begin
  Result := TSomeClass(M_parent.BoldObject);
  assert(not assigned(Result) or (Result is TSomeClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'parent', Result.ClassName, 'TSomeClass']));
end;

procedure TSomeClass._Setparent(const value: TSomeClass);
begin
  M_parent.BoldObject := value;
end;

procedure TSomeClassList.Add(NewObject: TSomeClass);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TSomeClassList.IndexOf(anObject: TSomeClass): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TSomeClassList.Includes(anObject: TSomeClass) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TSomeClassList.AddNew: TSomeClass;
begin
  result := TSomeClass(InternalAddNew);
end;

procedure TSomeClassList.Insert(index: Integer; NewObject: TSomeClass);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TSomeClassList.GetBoldObject(index: Integer): TSomeClass;
begin
  result := TSomeClass(GetElement(index));
end;

procedure TSomeClassList.SetBoldObject(index: Integer; NewObject: TSomeClass);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TSomeClassList.GetEnumerator: TSomeClassListEnumerator;
begin
 Result := TSomeClassListEnumerator.Create(Self);
end;

function TSomeClassListEnumerator.GetCurrent: TSomeClass;
begin
 Result := List[Index] as TSomeClass;
end;

{$ENDIF UseBoldListEnumerator}

{ TTopic }

function TTopic._Get_M_name: TBAString;
begin
  assert(ValidateMember('TTopic', 'name', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TTopic._Getname: String;
begin
  Result := M_name.AsString;
end;

procedure TTopic._Setname(const NewValue: String);
begin
  M_name.AsString := NewValue;
end;

function TTopic._GetBook: TBookList;
begin
  assert(ValidateMember('TTopic', 'Book', 1, TBookList));
  Result := TBookList(BoldMembers[1]);
end;

function TTopic._Gettopicbook: TtopicbookList;
begin
  assert(ValidateMember('TTopic', 'topicbook', 2, TtopicbookList));
  Result := TtopicbookList(BoldMembers[2]);
end;

procedure TTopicList.Add(NewObject: TTopic);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TTopicList.IndexOf(anObject: TTopic): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TTopicList.Includes(anObject: TTopic) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TTopicList.AddNew: TTopic;
begin
  result := TTopic(InternalAddNew);
end;

procedure TTopicList.Insert(index: Integer; NewObject: TTopic);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TTopicList.GetBoldObject(index: Integer): TTopic;
begin
  result := TTopic(GetElement(index));
end;

procedure TTopicList.SetBoldObject(index: Integer; NewObject: TTopic);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TTopicList.GetEnumerator: TTopicListEnumerator;
begin
 Result := TTopicListEnumerator.Create(Self);
end;

function TTopicListEnumerator.GetCurrent: TTopic;
begin
 Result := List[Index] as TTopic;
end;

{$ENDIF UseBoldListEnumerator}

{ Tpartpartof }

function Tpartpartof._Get_M_partof: TBoldObjectReference;
begin
  assert(ValidateMember('Tpartpartof', 'partof', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Tpartpartof._Getpartof: TSomeClass;
begin
  Result := TSomeClass(M_partof.BoldObject);
  assert(not assigned(Result) or (Result is TSomeClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'partof', Result.ClassName, 'TSomeClass']));
end;

function Tpartpartof._Get_M_part: TBoldObjectReference;
begin
  assert(ValidateMember('Tpartpartof', 'part', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Tpartpartof._Getpart: TSomeClass;
begin
  Result := TSomeClass(M_part.BoldObject);
  assert(not assigned(Result) or (Result is TSomeClass), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'part', Result.ClassName, 'TSomeClass']));
end;

procedure TpartpartofList.Add(NewObject: Tpartpartof);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TpartpartofList.IndexOf(anObject: Tpartpartof): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TpartpartofList.Includes(anObject: Tpartpartof) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TpartpartofList.AddNew: Tpartpartof;
begin
  result := Tpartpartof(InternalAddNew);
end;

procedure TpartpartofList.Insert(index: Integer; NewObject: Tpartpartof);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TpartpartofList.GetBoldObject(index: Integer): Tpartpartof;
begin
  result := Tpartpartof(GetElement(index));
end;

procedure TpartpartofList.SetBoldObject(index: Integer; NewObject: Tpartpartof);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TpartpartofList.GetEnumerator: TpartpartofListEnumerator;
begin
 Result := TpartpartofListEnumerator.Create(Self);
end;

function TpartpartofListEnumerator.GetCurrent: Tpartpartof;
begin
 Result := List[Index] as Tpartpartof;
end;

{$ENDIF UseBoldListEnumerator}

{ Ttopicbook }

function Ttopicbook._Get_M_Book: TBoldObjectReference;
begin
  assert(ValidateMember('Ttopicbook', 'Book', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function Ttopicbook._GetBook: TBook;
begin
  Result := TBook(M_Book.BoldObject);
  assert(not assigned(Result) or (Result is TBook), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Book', Result.ClassName, 'TBook']));
end;

function Ttopicbook._Get_M_Topic: TBoldObjectReference;
begin
  assert(ValidateMember('Ttopicbook', 'Topic', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function Ttopicbook._GetTopic: TTopic;
begin
  Result := TTopic(M_Topic.BoldObject);
  assert(not assigned(Result) or (Result is TTopic), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Topic', Result.ClassName, 'TTopic']));
end;

procedure TtopicbookList.Add(NewObject: Ttopicbook);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TtopicbookList.IndexOf(anObject: Ttopicbook): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TtopicbookList.Includes(anObject: Ttopicbook) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TtopicbookList.AddNew: Ttopicbook;
begin
  result := Ttopicbook(InternalAddNew);
end;

procedure TtopicbookList.Insert(index: Integer; NewObject: Ttopicbook);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TtopicbookList.GetBoldObject(index: Integer): Ttopicbook;
begin
  result := Ttopicbook(GetElement(index));
end;

procedure TtopicbookList.SetBoldObject(index: Integer; NewObject: Ttopicbook);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TtopicbookList.GetEnumerator: TtopicbookListEnumerator;
begin
 Result := TtopicbookListEnumerator.Create(Self);
end;

function TtopicbookListEnumerator.GetCurrent: Ttopicbook;
begin
 Result := List[Index] as Ttopicbook;
end;

{$ENDIF UseBoldListEnumerator}

function GeneratedCodeCRC: String;
begin
  result := '156520488';
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('TestModelClassesRoot', TTestModelClassesRootList);
  BoldObjectListClasses.AddObjectEntry('APersistentClass', TAPersistentClassList);
  BoldObjectListClasses.AddObjectEntry('ATransientClass', TATransientClassList);
  BoldObjectListClasses.AddObjectEntry('Book', TBookList);
  BoldObjectListClasses.AddObjectEntry('ClassWithLink', TClassWithLinkList);
  BoldObjectListClasses.AddObjectEntry('LinkClass', TLinkClassList);
  BoldObjectListClasses.AddObjectEntry('SomeClass', TSomeClassList);
  BoldObjectListClasses.AddObjectEntry('Topic', TTopicList);
  BoldObjectListClasses.AddObjectEntry('Partpartof', TpartpartofList);
  BoldObjectListClasses.AddObjectEntry('Topicbook', TtopicbookList);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('TestModelClassesRoot', TTestModelClassesRoot);
  BoldObjectClasses.AddObjectEntry('APersistentClass', TAPersistentClass);
  BoldObjectClasses.AddObjectEntry('ATransientClass', TATransientClass);
  BoldObjectClasses.AddObjectEntry('Book', TBook);
  BoldObjectClasses.AddObjectEntry('ClassWithLink', TClassWithLink);
  BoldObjectClasses.AddObjectEntry('LinkClass', TLinkClass);
  BoldObjectClasses.AddObjectEntry('SomeClass', TSomeClass);
  BoldObjectClasses.AddObjectEntry('Topic', TTopic);
  BoldObjectClasses.AddObjectEntry('Partpartof', Tpartpartof);
  BoldObjectClasses.AddObjectEntry('Topicbook', Ttopicbook);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc('BusinessClasses', InstallBusinessClasses, InstallObjectListClasses, GeneratedCodeCRC);
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.