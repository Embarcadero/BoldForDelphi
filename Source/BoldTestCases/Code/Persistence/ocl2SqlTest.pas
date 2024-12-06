(*****************************************)
(*      This file is autogenerated       *)
(*   Any manual changes will be LOST!    *)
(*****************************************)
(* Generated 11-06-2019 15:42:39         *)
(*****************************************)
(* This file should be stored in the     *)
(* same directory as the form/datamodule *)
(* with the corresponding model          *)
(*****************************************)
(* Copyright notice:                     *)
(*                                       *)
(*****************************************)

unit ocl2SqlTest;

{$DEFINE ocl2SqlTest_unitheader}
{$INCLUDE ocl2SqlTest_Interface.inc}

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

{ TBuilding }

function TBuilding._Get_M_Address: TBAString;
begin
  assert(ValidateMember('TBuilding', 'Address', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TBuilding._GetAddress: String;
begin
  Result := M_Address.AsString;
end;

procedure TBuilding._SetAddress(const NewValue: String);
begin
  M_Address.AsString := NewValue;
end;

function TBuilding._GetOwners: TPersonList;
begin
  assert(ValidateMember('TBuilding', 'Owners', 1, TPersonList));
  Result := TPersonList(BoldMembers[1]);
end;

function TBuilding._GetOwnership: TOwnershipList;
begin
  assert(ValidateMember('TBuilding', 'Ownership', 2, TOwnershipList));
  Result := TOwnershipList(BoldMembers[2]);
end;

procedure TBuildingList.Add(NewObject: TBuilding);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBuildingList.IndexOf(anObject: TBuilding): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TBuildingList.Includes(anObject: TBuilding) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TBuildingList.AddNew: TBuilding;
begin
  result := TBuilding(InternalAddNew);
end;

procedure TBuildingList.Insert(index: Integer; NewObject: TBuilding);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBuildingList.GetBoldObject(index: Integer): TBuilding;
begin
  result := TBuilding(GetElement(index));
end;

procedure TBuildingList.SetBoldObject(index: Integer; NewObject: TBuilding);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TBuildingList.GetEnumerator: TBuildingListEnumerator;
begin
 Result := TBuildingListEnumerator.Create(Self);
end;

function TBuildingListEnumerator.GetCurrent: TBuilding;
begin
 Result := List[Index] as TBuilding;
end;

{$ENDIF UseBoldListEnumerator}

{ TOwnership }

function TOwnership._Get_M_Owners: TBoldObjectReference;
begin
  assert(ValidateMember('TOwnership', 'Owners', 0, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[0]);
end;

function TOwnership._GetOwners: TPerson;
begin
  Result := TPerson(M_Owners.BoldObject);
  assert(not assigned(Result) or (Result is TPerson), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Owners', Result.ClassName, 'TPerson']));
end;

function TOwnership._Get_M_OwnedBuildings: TBoldObjectReference;
begin
  assert(ValidateMember('TOwnership', 'OwnedBuildings', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TOwnership._GetOwnedBuildings: TBuilding;
begin
  Result := TBuilding(M_OwnedBuildings.BoldObject);
  assert(not assigned(Result) or (Result is TBuilding), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'OwnedBuildings', Result.ClassName, 'TBuilding']));
end;

procedure TOwnershipList.Add(NewObject: TOwnership);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TOwnershipList.IndexOf(anObject: TOwnership): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TOwnershipList.Includes(anObject: TOwnership) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TOwnershipList.AddNew: TOwnership;
begin
  result := TOwnership(InternalAddNew);
end;

procedure TOwnershipList.Insert(index: Integer; NewObject: TOwnership);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TOwnershipList.GetBoldObject(index: Integer): TOwnership;
begin
  result := TOwnership(GetElement(index));
end;

procedure TOwnershipList.SetBoldObject(index: Integer; NewObject: TOwnership);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TOwnershipList.GetEnumerator: TOwnershipListEnumerator;
begin
 Result := TOwnershipListEnumerator.Create(Self);
end;

function TOwnershipListEnumerator.GetCurrent: TOwnership;
begin
 Result := List[Index] as TOwnership;
end;

{$ENDIF UseBoldListEnumerator}

{ TPerson }

function TPerson._Get_M_Assets: TBACurrency;
begin
  assert(ValidateMember('TPerson', 'Assets', 0, TBACurrency));
  Result := TBACurrency(BoldMembers[0]);
end;

function TPerson._GetAssets: Currency;
begin
  Result := M_Assets.AsCurrency;
end;

procedure TPerson._SetAssets(const NewValue: Currency);
begin
  M_Assets.AsCurrency := NewValue;
end;

function TPerson._Get_M_FirstName: TBAString;
begin
  assert(ValidateMember('TPerson', 'FirstName', 1, TBAString));
  Result := TBAString(BoldMembers[1]);
end;

function TPerson._GetFirstName: String;
begin
  Result := M_FirstName.AsString;
end;

procedure TPerson._SetFirstName(const NewValue: String);
begin
  M_FirstName.AsString := NewValue;
end;

function TPerson._Get_M_LastName: TBAString;
begin
  assert(ValidateMember('TPerson', 'LastName', 2, TBAString));
  Result := TBAString(BoldMembers[2]);
end;

function TPerson._GetLastName: String;
begin
  Result := M_LastName.AsString;
end;

procedure TPerson._SetLastName(const NewValue: String);
begin
  M_LastName.AsString := NewValue;
end;

function TPerson._Get_M_isMarried: TBABoolean;
begin
  assert(ValidateMember('TPerson', 'isMarried', 3, TBABoolean));
  Result := TBABoolean(BoldMembers[3]);
end;

function TPerson._GetisMarried: Boolean;
begin
  Result := M_isMarried.AsBoolean;
end;

procedure TPerson._SetisMarried(const NewValue: Boolean);
begin
  M_isMarried.AsBoolean := NewValue;
end;

function TPerson._GetOwnedBuildings: TBuildingList;
begin
  assert(ValidateMember('TPerson', 'OwnedBuildings', 4, TBuildingList));
  Result := TBuildingList(BoldMembers[4]);
end;

function TPerson._GetOwnership: TOwnershipList;
begin
  assert(ValidateMember('TPerson', 'Ownership', 5, TOwnershipList));
  Result := TOwnershipList(BoldMembers[5]);
end;

function TPerson._Get_M_Home: TBoldObjectReference;
begin
  assert(ValidateMember('TPerson', 'Home', 6, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[6]);
end;

function TPerson._GetHome: TResidentialBuilding;
begin
  Result := TResidentialBuilding(M_Home.BoldObject);
  assert(not assigned(Result) or (Result is TResidentialBuilding), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Home', Result.ClassName, 'TResidentialBuilding']));
end;

procedure TPerson._SetHome(const value: TResidentialBuilding);
begin
  M_Home.BoldObject := value;
end;

function TPerson._Get_M_Mother: TBoldObjectReference;
begin
  assert(ValidateMember('TPerson', 'Mother', 7, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[7]);
end;

function TPerson._GetMother: TPerson;
begin
  Result := TPerson(M_Mother.BoldObject);
  assert(not assigned(Result) or (Result is TPerson), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Mother', Result.ClassName, 'TPerson']));
end;

procedure TPerson._SetMother(const value: TPerson);
begin
  M_Mother.BoldObject := value;
end;

function TPerson._GetChildren: TPersonList;
begin
  assert(ValidateMember('TPerson', 'Children', 8, TPersonList));
  Result := TPersonList(BoldMembers[8]);
end;

procedure TPersonList.Add(NewObject: TPerson);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TPersonList.IndexOf(anObject: TPerson): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TPersonList.Includes(anObject: TPerson) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TPersonList.AddNew: TPerson;
begin
  result := TPerson(InternalAddNew);
end;

procedure TPersonList.Insert(index: Integer; NewObject: TPerson);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TPersonList.GetBoldObject(index: Integer): TPerson;
begin
  result := TPerson(GetElement(index));
end;

procedure TPersonList.SetBoldObject(index: Integer; NewObject: TPerson);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TPersonList.GetEnumerator: TPersonListEnumerator;
begin
 Result := TPersonListEnumerator.Create(Self);
end;

function TPersonListEnumerator.GetCurrent: TPerson;
begin
 Result := List[Index] as TPerson;
end;

{$ENDIF UseBoldListEnumerator}

{ TResidentialBuilding }

function TResidentialBuilding._Get_M_TotalRent: TBACurrency;
begin
  assert(ValidateMember('TResidentialBuilding', 'TotalRent', 3, TBACurrency));
  Result := TBACurrency(BoldMembers[3]);
end;

function TResidentialBuilding._GetTotalRent: Currency;
begin
  Result := M_TotalRent.AsCurrency;
end;

procedure TResidentialBuilding._SetTotalRent(const NewValue: Currency);
begin
  M_TotalRent.AsCurrency := NewValue;
end;

function TResidentialBuilding._GetResidents: TPersonList;
begin
  assert(ValidateMember('TResidentialBuilding', 'Residents', 4, TPersonList));
  Result := TPersonList(BoldMembers[4]);
end;

procedure TResidentialBuildingList.Add(NewObject: TResidentialBuilding);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TResidentialBuildingList.IndexOf(anObject: TResidentialBuilding): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TResidentialBuildingList.Includes(anObject: TResidentialBuilding) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TResidentialBuildingList.AddNew: TResidentialBuilding;
begin
  result := TResidentialBuilding(InternalAddNew);
end;

procedure TResidentialBuildingList.Insert(index: Integer; NewObject: TResidentialBuilding);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TResidentialBuildingList.GetBoldObject(index: Integer): TResidentialBuilding;
begin
  result := TResidentialBuilding(GetElement(index));
end;

procedure TResidentialBuildingList.SetBoldObject(index: Integer; NewObject: TResidentialBuilding);
begin;
  SetElement(index, NewObject);
end;

{$IFDEF UseBoldListEnumerator}
function TResidentialBuildingList.GetEnumerator: TResidentialBuildingListEnumerator;
begin
 Result := TResidentialBuildingListEnumerator.Create(Self);
end;

function TResidentialBuildingListEnumerator.GetCurrent: TResidentialBuilding;
begin
 Result := List[Index] as TResidentialBuilding;
end;

{$ENDIF UseBoldListEnumerator}

function GeneratedCodeCRC: String;
begin
  result := '114419253';
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRootList);
  BoldObjectListClasses.AddObjectEntry('Building', TBuildingList);
  BoldObjectListClasses.AddObjectEntry('Ownership', TOwnershipList);
  BoldObjectListClasses.AddObjectEntry('Person', TPersonList);
  BoldObjectListClasses.AddObjectEntry('ResidentialBuilding', TResidentialBuildingList);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRoot);
  BoldObjectClasses.AddObjectEntry('Building', TBuilding);
  BoldObjectClasses.AddObjectEntry('Ownership', TOwnership);
  BoldObjectClasses.AddObjectEntry('Person', TPerson);
  BoldObjectClasses.AddObjectEntry('ResidentialBuilding', TResidentialBuilding);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc('ocl2SqlTest', InstallBusinessClasses, InstallObjectListClasses, GeneratedCodeCRC);
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.