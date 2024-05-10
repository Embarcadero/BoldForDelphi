(*****************************************)
(*      This file is autogenerated       *)
(*   Any manual changes will be LOST!    *)
(*****************************************)
(* Generated 11-06-2019 15:41:18         *)
(*****************************************)
(* This file should be stored in the     *)
(* same directory as the form/datamodule *)
(* with the corresponding model          *)
(*****************************************)
(* Copyright notice:                     *)
(*                                       *)
(*****************************************)

unit ModelEv;

{$DEFINE ModelEv_unitheader}
{$INCLUDE ModelEv_Interface.inc}

{ Includefile for methodimplementations }


const
  BoldMemberAssertInvalidObjectType: string = 'Object of singlelink (%s.%s) is of wrong type (is %s, should be %s)';

{ TBusinessClassesRoot }

procedure TBusinessClassesRootList.Add(NewObject: TBusinessClassesRoot);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBusinessClassesRootList.IndexOf(anObject: TBusinessClassesRoot): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TBusinessClassesRootList.Includes(anObject: TBusinessClassesRoot) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TBusinessClassesRootList.AddNew: TBusinessClassesRoot;
begin
  result := TBusinessClassesRoot(InternalAddNew);
end;

procedure TBusinessClassesRootList.Insert(index: Integer; NewObject: TBusinessClassesRoot);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBusinessClassesRootList.GetBoldObject(index: Integer): TBusinessClassesRoot;
begin
  result := TBusinessClassesRoot(GetElement(index));
end;

procedure TBusinessClassesRootList.SetBoldObject(index: Integer; NewObject: TBusinessClassesRoot);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TBusinessClassesRootList.GetEnumerator: TBusinessClassesRootListEnumerator;
begin
 Result := TBusinessClassesRootListEnumerator.Create(Self);
end;

function TBusinessClassesRootListEnumerator.GetCurrent: TBusinessClassesRoot;
begin
 Result := List[Index] as TBusinessClassesRoot;
end;

{$ENDIF UseBoldListEnumerator}

{ TA }

function TA._Get_M_a1: TBAString;
begin
  assert(ValidateMember('TA', 'a1', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TA._Geta1: String;
begin
  Result := M_a1.AsString;
end;

procedure TA._Seta1(const NewValue: String);
begin
  M_a1.AsString := NewValue;
end;

function TA._Get_M_a2: TBAInteger;
begin
  assert(ValidateMember('TA', 'a2', 1, TBAInteger));
  Result := TBAInteger(BoldMembers[1]);
end;

function TA._Geta2: Integer;
begin
  Result := M_a2.AsInteger;
end;

procedure TA._Seta2(const NewValue: Integer);
begin
  M_a2.AsInteger := NewValue;
end;

function TA._GetB: TBList;
begin
  assert(ValidateMember('TA', 'B', 2, TBList));
  Result := TBList(BoldMembers[2]);
end;

function TA._GetBaRole: TBaRoleList;
begin
  assert(ValidateMember('TA', 'BaRole', 3, TBaRoleList));
  Result := TBaRoleList(BoldMembers[3]);
end;

procedure TAList.Add(NewObject: TA);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TAList.IndexOf(anObject: TA): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TAList.Includes(anObject: TA) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TAList.AddNew: TA;
begin
  result := TA(InternalAddNew);
end;

procedure TAList.Insert(index: Integer; NewObject: TA);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TAList.GetBoldObject(index: Integer): TA;
begin
  result := TA(GetElement(index));
end;

procedure TAList.SetBoldObject(index: Integer; NewObject: TA);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TAList.GetEnumerator: TAListEnumerator;
begin
 Result := TAListEnumerator.Create(Self);
end;

function TAListEnumerator.GetCurrent: TA;
begin
 Result := List[Index] as TA;
end;

{$ENDIF UseBoldListEnumerator}

{ TB }

function TB._Get_M_b1: TBAString;
begin
  assert(ValidateMember('TB', 'b1', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TB._Getb1: String;
begin
  Result := M_b1.AsString;
end;

procedure TB._Setb1(const NewValue: String);
begin
  M_b1.AsString := NewValue;
end;

function TB._Get_M_b2: TBAInteger;
begin
  assert(ValidateMember('TB', 'b2', 1, TBAInteger));
  Result := TBAInteger(BoldMembers[1]);
end;

function TB._Getb2: Integer;
begin
  Result := M_b2.AsInteger;
end;

procedure TB._Setb2(const NewValue: Integer);
begin
  M_b2.AsInteger := NewValue;
end;

function TB._GetaRole: TAList;
begin
  assert(ValidateMember('TB', 'aRole', 2, TAList));
  Result := TAList(BoldMembers[2]);
end;

function TB._GetBaRole: TBaRoleList;
begin
  assert(ValidateMember('TB', 'BaRole', 3, TBaRoleList));
  Result := TBaRoleList(BoldMembers[3]);
end;

function TB._GetC: TCList;
begin
  assert(ValidateMember('TB', 'C', 4, TCList));
  Result := TCList(BoldMembers[4]);
end;

procedure TBList.Add(NewObject: TB);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBList.IndexOf(anObject: TB): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TBList.Includes(anObject: TB) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TBList.AddNew: TB;
begin
  result := TB(InternalAddNew);
end;

procedure TBList.Insert(index: Integer; NewObject: TB);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBList.GetBoldObject(index: Integer): TB;
begin
  result := TB(GetElement(index));
end;

procedure TBList.SetBoldObject(index: Integer; NewObject: TB);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TBList.GetEnumerator: TBListEnumerator;
begin
 Result := TBListEnumerator.Create(Self);
end;

function TBListEnumerator.GetCurrent: TB;
begin
 Result := List[Index] as TB;
end;

{$ENDIF UseBoldListEnumerator}

{ TBaRole }

function TBaRole._Get_M_aRole: TBoldObjectReference;
begin
  assert(ValidateMember('TBaRole', 'aRole', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TBaRole._GetaRole: TA;
begin
  Result := TA(M_aRole.BoldObject);
  assert(not assigned(Result) or (Result is TA), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'aRole', Result.ClassName, 'TA']));
end;

function TBaRole._Get_M_B: TBoldObjectReference;
begin
  assert(ValidateMember('TBaRole', 'B', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TBaRole._GetB: TB;
begin
  Result := TB(M_B.BoldObject);
  assert(not assigned(Result) or (Result is TB), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'B', Result.ClassName, 'TB']));
end;

procedure TBaRoleList.Add(NewObject: TBaRole);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBaRoleList.IndexOf(anObject: TBaRole): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TBaRoleList.Includes(anObject: TBaRole) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TBaRoleList.AddNew: TBaRole;
begin
  result := TBaRole(InternalAddNew);
end;

procedure TBaRoleList.Insert(index: Integer; NewObject: TBaRole);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBaRoleList.GetBoldObject(index: Integer): TBaRole;
begin
  result := TBaRole(GetElement(index));
end;

procedure TBaRoleList.SetBoldObject(index: Integer; NewObject: TBaRole);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TBaRoleList.GetEnumerator: TBaRoleListEnumerator;
begin
 Result := TBaRoleListEnumerator.Create(Self);
end;

function TBaRoleListEnumerator.GetCurrent: TBaRole;
begin
 Result := List[Index] as TBaRole;
end;

{$ENDIF UseBoldListEnumerator}

{ TC }

function TC._Get_M_C1: TBAString;
begin
  assert(ValidateMember('TC', 'C1', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TC._GetC1: String;
begin
  Result := M_C1.AsString;
end;

procedure TC._SetC1(const NewValue: String);
begin
  M_C1.AsString := NewValue;
end;

function TC._Get_M_B: TBoldObjectReference;
begin
  assert(ValidateMember('TC', 'B', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TC._GetB: TB;
begin
  Result := TB(M_B.BoldObject);
  assert(not assigned(Result) or (Result is TB), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'B', Result.ClassName, 'TB']));
end;

procedure TC._SetB(const value: TB);
begin
  M_B.BoldObject := value;
end;

procedure TCList.Add(NewObject: TC);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCList.IndexOf(anObject: TC): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCList.Includes(anObject: TC) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCList.AddNew: TC;
begin
  result := TC(InternalAddNew);
end;

procedure TCList.Insert(index: Integer; NewObject: TC);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCList.GetBoldObject(index: Integer): TC;
begin
  result := TC(GetElement(index));
end;

procedure TCList.SetBoldObject(index: Integer; NewObject: TC);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TCList.GetEnumerator: TCListEnumerator;
begin
 Result := TCListEnumerator.Create(Self);
end;

function TCListEnumerator.GetCurrent: TC;
begin
 Result := List[Index] as TC;
end;

{$ENDIF UseBoldListEnumerator}

{ TSubB }

function TSubB._Get_M_SubB1: TBAString;
begin
  assert(ValidateMember('TSubB', 'SubB1', 5, TBAString));
  Result := TBAString(BoldMembers[5]);
end;

function TSubB._GetSubB1: String;
begin
  Result := M_SubB1.AsString;
end;

procedure TSubB._SetSubB1(const NewValue: String);
begin
  M_SubB1.AsString := NewValue;
end;

procedure TSubBList.Add(NewObject: TSubB);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TSubBList.IndexOf(anObject: TSubB): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TSubBList.Includes(anObject: TSubB) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TSubBList.AddNew: TSubB;
begin
  result := TSubB(InternalAddNew);
end;

procedure TSubBList.Insert(index: Integer; NewObject: TSubB);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TSubBList.GetBoldObject(index: Integer): TSubB;
begin
  result := TSubB(GetElement(index));
end;

procedure TSubBList.SetBoldObject(index: Integer; NewObject: TSubB);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TSubBList.GetEnumerator: TSubBListEnumerator;
begin
 Result := TSubBListEnumerator.Create(Self);
end;

function TSubBListEnumerator.GetCurrent: TSubB;
begin
 Result := List[Index] as TSubB;
end;

{$ENDIF UseBoldListEnumerator}

{ TCSub }

function TCSub._Get_M_CSub1: TBAString;
begin
  assert(ValidateMember('TCSub', 'CSub1', 2, TBAString));
  Result := TBAString(BoldMembers[2]);
end;

function TCSub._GetCSub1: String;
begin
  Result := M_CSub1.AsString;
end;

procedure TCSub._SetCSub1(const NewValue: String);
begin
  M_CSub1.AsString := NewValue;
end;

procedure TCSubList.Add(NewObject: TCSub);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCSubList.IndexOf(anObject: TCSub): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCSubList.Includes(anObject: TCSub) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCSubList.AddNew: TCSub;
begin
  result := TCSub(InternalAddNew);
end;

procedure TCSubList.Insert(index: Integer; NewObject: TCSub);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCSubList.GetBoldObject(index: Integer): TCSub;
begin
  result := TCSub(GetElement(index));
end;

procedure TCSubList.SetBoldObject(index: Integer; NewObject: TCSub);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TCSubList.GetEnumerator: TCSubListEnumerator;
begin
 Result := TCSubListEnumerator.Create(Self);
end;

function TCSubListEnumerator.GetCurrent: TCSub;
begin
 Result := List[Index] as TCSub;
end;

{$ENDIF UseBoldListEnumerator}

{ TCSubSub1 }

procedure TCSubSub1List.Add(NewObject: TCSubSub1);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCSubSub1List.IndexOf(anObject: TCSubSub1): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCSubSub1List.Includes(anObject: TCSubSub1) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCSubSub1List.AddNew: TCSubSub1;
begin
  result := TCSubSub1(InternalAddNew);
end;

procedure TCSubSub1List.Insert(index: Integer; NewObject: TCSubSub1);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCSubSub1List.GetBoldObject(index: Integer): TCSubSub1;
begin
  result := TCSubSub1(GetElement(index));
end;

procedure TCSubSub1List.SetBoldObject(index: Integer; NewObject: TCSubSub1);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TCSubSub1List.GetEnumerator: TCSubSub1ListEnumerator;
begin
 Result := TCSubSub1ListEnumerator.Create(Self);
end;

function TCSubSub1ListEnumerator.GetCurrent: TCSubSub1;
begin
 Result := List[Index] as TCSubSub1;
end;

{$ENDIF UseBoldListEnumerator}

{ TCSubSub2 }

function TCSubSub2._Get_M_CSubSub1: TBAString;
begin
  assert(ValidateMember('TCSubSub2', 'CSubSub1', 3, TBAString));
  Result := TBAString(BoldMembers[3]);
end;

function TCSubSub2._GetCSubSub1: String;
begin
  Result := M_CSubSub1.AsString;
end;

procedure TCSubSub2._SetCSubSub1(const NewValue: String);
begin
  M_CSubSub1.AsString := NewValue;
end;

procedure TCSubSub2List.Add(NewObject: TCSubSub2);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCSubSub2List.IndexOf(anObject: TCSubSub2): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCSubSub2List.Includes(anObject: TCSubSub2) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCSubSub2List.AddNew: TCSubSub2;
begin
  result := TCSubSub2(InternalAddNew);
end;

procedure TCSubSub2List.Insert(index: Integer; NewObject: TCSubSub2);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCSubSub2List.GetBoldObject(index: Integer): TCSubSub2;
begin
  result := TCSubSub2(GetElement(index));
end;

procedure TCSubSub2List.SetBoldObject(index: Integer; NewObject: TCSubSub2);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TCSubSub2List.GetEnumerator: TCSubSub2ListEnumerator;
begin
 Result := TCSubSub2ListEnumerator.Create(Self);
end;

function TCSubSub2ListEnumerator.GetCurrent: TCSubSub2;
begin
 Result := List[Index] as TCSubSub2;
end;

{$ENDIF UseBoldListEnumerator}

function GeneratedCodeCRC: String;
begin
  result := '959243213';
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRootList);
  BoldObjectListClasses.AddObjectEntry('A', TAList);
  BoldObjectListClasses.AddObjectEntry('B', TBList);
  BoldObjectListClasses.AddObjectEntry('BaRole', TBaRoleList);
  BoldObjectListClasses.AddObjectEntry('C', TCList);
  BoldObjectListClasses.AddObjectEntry('SubB', TSubBList);
  BoldObjectListClasses.AddObjectEntry('CSub', TCSubList);
  BoldObjectListClasses.AddObjectEntry('CSubSub1', TCSubSub1List);
  BoldObjectListClasses.AddObjectEntry('CSubSub2', TCSubSub2List);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRoot);
  BoldObjectClasses.AddObjectEntry('A', TA);
  BoldObjectClasses.AddObjectEntry('B', TB);
  BoldObjectClasses.AddObjectEntry('BaRole', TBaRole);
  BoldObjectClasses.AddObjectEntry('C', TC);
  BoldObjectClasses.AddObjectEntry('SubB', TSubB);
  BoldObjectClasses.AddObjectEntry('CSub', TCSub);
  BoldObjectClasses.AddObjectEntry('CSubSub1', TCSubSub1);
  BoldObjectClasses.AddObjectEntry('CSubSub2', TCSubSub2);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc('ModelEv', InstallBusinessClasses, InstallObjectListClasses, GeneratedCodeCRC);
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.
