
{ Global compiler directives }
{$include bold.inc}
unit DistributableInfo;

{$DEFINE DistributableInfo_unitheader}
{$INCLUDE DistributableInfo_Interface.inc}

uses
  BoldDefaultId,

  BoldGeneratedCodeDictionary;

{ Includefile for methodimplementations }

{$INCLUDE DistributableInfo.inc}

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

{ TCheckedOutObjectInfo }

function TCheckedOutObjectInfo._Get_M_Holder: TBoldObjectReference;
begin
  assert(ValidateMember('TCheckedOutObjectInfo', 'Holder', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TCheckedOutObjectInfo._GetHolder: TForeignPSInfo;
begin
  assert(not assigned(M_Holder.BoldObject) or (M_Holder.BoldObject is TForeignPSInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Holder', M_Holder.BoldObject.ClassName, 'TForeignPSInfo']));
  Result := TForeignPSInfo(M_Holder.BoldObject);
end;

procedure TCheckedOutObjectInfo._SetHolder(value: TForeignPSInfo);
begin
  M_Holder.BoldObject := value;
end;

procedure TCheckedOutObjectInfoList.Add(NewObject: TCheckedOutObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCheckedOutObjectInfoList.IndexOf(anObject: TCheckedOutObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCheckedOutObjectInfoList.Includes(anObject: TCheckedOutObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCheckedOutObjectInfoList.AddNew: TCheckedOutObjectInfo;
begin
  result := TCheckedOutObjectInfo(InternalAddNew);
end;

procedure TCheckedOutObjectInfoList.Insert(index: Integer; NewObject: TCheckedOutObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCheckedOutObjectInfoList.GetBoldObject(index: Integer): TCheckedOutObjectInfo;
begin
  result := TCheckedOutObjectInfo(GetElement(index));
end;

procedure TCheckedOutObjectInfoList.SetBoldObject(index: Integer; NewObject: TCheckedOutObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ TCheckingInObjectInfo }

function TCheckingInObjectInfo._Get_M_KeepHold: TBABoolean;
begin
  assert(ValidateMember('TCheckingInObjectInfo', 'KeepHold', 0, TBABoolean));
  Result := TBABoolean(BoldMembers[0]);
end;

function TCheckingInObjectInfo._GetKeepHold: Boolean;
begin
  Result := M_KeepHold.AsBoolean;
end;

procedure TCheckingInObjectInfo._SetKeepHold(NewValue: Boolean);
begin
  M_KeepHold.AsBoolean := NewValue;
end;

function TCheckingInObjectInfo._Get_M_LocalTimeStampAtCheckIn: TBAInteger;
begin
  assert(ValidateMember('TCheckingInObjectInfo', 'LocalTimeStampAtCheckIn', 1, TBAInteger));
  Result := TBAInteger(BoldMembers[1]);
end;

function TCheckingInObjectInfo._GetLocalTimeStampAtCheckIn: Integer;
begin
  Result := M_LocalTimeStampAtCheckIn.AsInteger;
end;

procedure TCheckingInObjectInfo._SetLocalTimeStampAtCheckIn(NewValue: Integer);
begin
  M_LocalTimeStampAtCheckIn.AsInteger := NewValue;
end;

function TCheckingInObjectInfo._Get_M_HeldObjectInfo: TBoldObjectReference;
begin
  assert(ValidateMember('TCheckingInObjectInfo', 'HeldObjectInfo', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TCheckingInObjectInfo._GetHeldObjectInfo: THeldObjectInfo;
begin
  assert(not assigned(M_HeldObjectInfo.BoldObject) or (M_HeldObjectInfo.BoldObject is THeldObjectInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'HeldObjectInfo', M_HeldObjectInfo.BoldObject.ClassName, 'THeldObjectInfo']));
  Result := THeldObjectInfo(M_HeldObjectInfo.BoldObject);
end;

procedure TCheckingInObjectInfo._SetHeldObjectInfo(value: THeldObjectInfo);
begin
  M_HeldObjectInfo.BoldObject := value;
end;

procedure TCheckingInObjectInfoList.Add(NewObject: TCheckingInObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TCheckingInObjectInfoList.IndexOf(anObject: TCheckingInObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TCheckingInObjectInfoList.Includes(anObject: TCheckingInObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TCheckingInObjectInfoList.AddNew: TCheckingInObjectInfo;
begin
  result := TCheckingInObjectInfo(InternalAddNew);
end;

procedure TCheckingInObjectInfoList.Insert(index: Integer; NewObject: TCheckingInObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TCheckingInObjectInfoList.GetBoldObject(index: Integer): TCheckingInObjectInfo;
begin
  result := TCheckingInObjectInfo(GetElement(index));
end;

procedure TCheckingInObjectInfoList.SetBoldObject(index: Integer; NewObject: TCheckingInObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ TDistributableObjectInfo }

function TDistributableObjectInfo._Get_M_LocalId: TBAInteger;
begin
  assert(ValidateMember('TDistributableObjectInfo', 'LocalId', 0, TBAInteger));
  Result := TBAInteger(BoldMembers[0]);
end;

function TDistributableObjectInfo._GetLocalId: Integer;
begin
  Result := M_LocalId.AsInteger;
end;

procedure TDistributableObjectInfo._SetLocalId(NewValue: Integer);
begin
  M_LocalId.AsInteger := NewValue;
end;

function TDistributableObjectInfo._Get_M_mapping: TBoldObjectReference;
begin
  assert(ValidateMember('TDistributableObjectInfo', 'mapping', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function TDistributableObjectInfo._Getmapping: TMapping;
begin
  assert(not assigned(M_mapping.BoldObject) or (M_mapping.BoldObject is TMapping), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'mapping', M_mapping.BoldObject.ClassName, 'TMapping']));
  Result := TMapping(M_mapping.BoldObject);
end;

procedure TDistributableObjectInfo._Setmapping(value: TMapping);
begin
  M_mapping.BoldObject := value;
end;

procedure TDistributableObjectInfoList.Add(NewObject: TDistributableObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TDistributableObjectInfoList.IndexOf(anObject: TDistributableObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TDistributableObjectInfoList.Includes(anObject: TDistributableObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TDistributableObjectInfoList.AddNew: TDistributableObjectInfo;
begin
  result := TDistributableObjectInfo(InternalAddNew);
end;

procedure TDistributableObjectInfoList.Insert(index: Integer; NewObject: TDistributableObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TDistributableObjectInfoList.GetBoldObject(index: Integer): TDistributableObjectInfo;
begin
  result := TDistributableObjectInfo(GetElement(index));
end;

procedure TDistributableObjectInfoList.SetBoldObject(index: Integer; NewObject: TDistributableObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ THeldObjectInfo }

function THeldObjectInfo._Get_M_OriginalLocalTimeStamp: TBAInteger;
begin
  assert(ValidateMember('THeldObjectInfo', 'OriginalLocalTimeStamp', 0, TBAInteger));
  Result := TBAInteger(BoldMembers[0]);
end;

function THeldObjectInfo._GetOriginalLocalTimeStamp: Integer;
begin
  Result := M_OriginalLocalTimeStamp.AsInteger;
end;

procedure THeldObjectInfo._SetOriginalLocalTimeStamp(NewValue: Integer);
begin
  M_OriginalLocalTimeStamp.AsInteger := NewValue;
end;

function THeldObjectInfo._Get_M_ForeignObjectInfo: TBoldObjectReference;
begin
  assert(ValidateMember('THeldObjectInfo', 'ForeignObjectInfo', 1, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[1]);
end;

function THeldObjectInfo._GetForeignObjectInfo: TForeignObjectInfo;
begin
  assert(not assigned(M_ForeignObjectInfo.BoldObject) or (M_ForeignObjectInfo.BoldObject is TForeignObjectInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'ForeignObjectInfo', M_ForeignObjectInfo.BoldObject.ClassName, 'TForeignObjectInfo']));
  Result := TForeignObjectInfo(M_ForeignObjectInfo.BoldObject);
end;

procedure THeldObjectInfo._SetForeignObjectInfo(value: TForeignObjectInfo);
begin
  M_ForeignObjectInfo.BoldObject := value;
end;

function THeldObjectInfo._Get_M_CheckingInObjectInfo: TBoldObjectReference;
begin
  assert(ValidateMember('THeldObjectInfo', 'CheckingInObjectInfo', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function THeldObjectInfo._GetCheckingInObjectInfo: TCheckingInObjectInfo;
begin
  assert(not assigned(M_CheckingInObjectInfo.BoldObject) or (M_CheckingInObjectInfo.BoldObject is TCheckingInObjectInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'CheckingInObjectInfo', M_CheckingInObjectInfo.BoldObject.ClassName, 'TCheckingInObjectInfo']));
  Result := TCheckingInObjectInfo(M_CheckingInObjectInfo.BoldObject);
end;

procedure THeldObjectInfo._SetCheckingInObjectInfo(value: TCheckingInObjectInfo);
begin
  M_CheckingInObjectInfo.BoldObject := value;
end;

procedure THeldObjectInfoList.Add(NewObject: THeldObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function THeldObjectInfoList.IndexOf(anObject: THeldObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function THeldObjectInfoList.Includes(anObject: THeldObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function THeldObjectInfoList.AddNew: THeldObjectInfo;
begin
  result := THeldObjectInfo(InternalAddNew);
end;

procedure THeldObjectInfoList.Insert(index: Integer; NewObject: THeldObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function THeldObjectInfoList.GetBoldObject(index: Integer): THeldObjectInfo;
begin
  result := THeldObjectInfo(GetElement(index));
end;

procedure THeldObjectInfoList.SetBoldObject(index: Integer; NewObject: THeldObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ TMapping }

function TMapping._GetObjectInfo: TDistributableObjectInfoList;
begin
  assert(ValidateMember('TMapping', 'ObjectInfo', 0, TDistributableObjectInfoList));
  Result := TDistributableObjectInfoList(BoldMembers[0]);
end;

function TMapping._Get_Q_ObjectInfo(LocalId: Integer): TDistributableObjectInfo;
var
  TempResult: TBoldObject;
  TempList: TBoldMemberList;
  Q_LocalId: TBAInteger;
begin
  TempList := TBoldMemberList.Create;
  TempList.CloneMembers := false;
  Q_LocalId := TBAInteger.Create;
  try
    Q_LocalId.AsInteger := LocalId;
    TempList.add(Q_LocalId);
    TempResult := M_ObjectInfo.GetByIndex(TempList);
    assert(not assigned(TempResult) or (TempResult is TDistributableObjectInfo), 'Illegal object in multilink');
    result := TDistributableObjectInfo(TempResult);
  finally
    TempList.Free;
    Q_LocalId.Free;
  end;
end;

procedure TMappingList.Add(NewObject: TMapping);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TMappingList.IndexOf(anObject: TMapping): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TMappingList.Includes(anObject: TMapping) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TMappingList.AddNew: TMapping;
begin
  result := TMapping(InternalAddNew);
end;

procedure TMappingList.Insert(index: Integer; NewObject: TMapping);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TMappingList.GetBoldObject(index: Integer): TMapping;
begin
  result := TMapping(GetElement(index));
end;

procedure TMappingList.SetBoldObject(index: Integer; NewObject: TMapping);
begin;
  SetElement(index, NewObject);
end;

{ TPSInfo }

function TPSInfo._Get_M_GlobalID: TBAString;
begin
  assert(ValidateMember('TPSInfo', 'GlobalID', 0, TBAString));
  Result := TBAString(BoldMembers[0]);
end;

function TPSInfo._GetGlobalID: String;
begin
  Result := M_GlobalID.AsString;
end;

procedure TPSInfo._SetGlobalID(NewValue: String);
begin
  M_GlobalID.AsString := NewValue;
end;

procedure TPSInfoList.Add(NewObject: TPSInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TPSInfoList.IndexOf(anObject: TPSInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TPSInfoList.Includes(anObject: TPSInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TPSInfoList.AddNew: TPSInfo;
begin
  result := TPSInfo(InternalAddNew);
end;

procedure TPSInfoList.Insert(index: Integer; NewObject: TPSInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TPSInfoList.GetBoldObject(index: Integer): TPSInfo;
begin
  result := TPSInfo(GetElement(index));
end;

procedure TPSInfoList.SetBoldObject(index: Integer; NewObject: TPSInfo);
begin;
  SetElement(index, NewObject);
end;

{ TForeignObjectInfo }

function TForeignObjectInfo._Get_M_ForeignTimeStamp: TBAInteger;
begin
  assert(ValidateMember('TForeignObjectInfo', 'ForeignTimeStamp', 2, TBAInteger));
  Result := TBAInteger(BoldMembers[2]);
end;

function TForeignObjectInfo._GetForeignTimeStamp: Integer;
begin
  Result := M_ForeignTimeStamp.AsInteger;
end;

procedure TForeignObjectInfo._SetForeignTimeStamp(NewValue: Integer);
begin
  M_ForeignTimeStamp.AsInteger := NewValue;
end;

function TForeignObjectInfo._Get_M_HeldObjectInfo: TBoldObjectReference;
begin
  assert(ValidateMember('TForeignObjectInfo', 'HeldObjectInfo', 3, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[3]);
end;

function TForeignObjectInfo._GetHeldObjectInfo: THeldObjectInfo;
begin
  assert(not assigned(M_HeldObjectInfo.BoldObject) or (M_HeldObjectInfo.BoldObject is THeldObjectInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'HeldObjectInfo', M_HeldObjectInfo.BoldObject.ClassName, 'THeldObjectInfo']));
  Result := THeldObjectInfo(M_HeldObjectInfo.BoldObject);
end;

procedure TForeignObjectInfo._SetHeldObjectInfo(value: THeldObjectInfo);
begin
  M_HeldObjectInfo.BoldObject := value;
end;

function TForeignObjectInfo._Get_M_Owner: TBoldObjectReference;
begin
  assert(ValidateMember('TForeignObjectInfo', 'Owner', 4, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[4]);
end;

function TForeignObjectInfo._GetOwner: TForeignPSInfo;
begin
  assert(not assigned(M_Owner.BoldObject) or (M_Owner.BoldObject is TForeignPSInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'Owner', M_Owner.BoldObject.ClassName, 'TForeignPSInfo']));
  Result := TForeignPSInfo(M_Owner.BoldObject);
end;

procedure TForeignObjectInfo._SetOwner(value: TForeignPSInfo);
begin
  M_Owner.BoldObject := value;
end;

procedure TForeignObjectInfoList.Add(NewObject: TForeignObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TForeignObjectInfoList.IndexOf(anObject: TForeignObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TForeignObjectInfoList.Includes(anObject: TForeignObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TForeignObjectInfoList.AddNew: TForeignObjectInfo;
begin
  result := TForeignObjectInfo(InternalAddNew);
end;

procedure TForeignObjectInfoList.Insert(index: Integer; NewObject: TForeignObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TForeignObjectInfoList.GetBoldObject(index: Integer): TForeignObjectInfo;
begin
  result := TForeignObjectInfo(GetElement(index));
end;

procedure TForeignObjectInfoList.SetBoldObject(index: Integer; NewObject: TForeignObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ TOwnObjectInfo }

function TOwnObjectInfo._Get_M_CheckedOutObjectInfo: TBoldObjectReference;
begin
  assert(ValidateMember('TOwnObjectInfo', 'CheckedOutObjectInfo', 2, TBoldObjectReference));
  Result := TBoldObjectReference(BoldMembers[2]);
end;

function TOwnObjectInfo._GetCheckedOutObjectInfo: TCheckedOutObjectInfo;
begin
  assert(not assigned(M_CheckedOutObjectInfo.BoldObject) or (M_CheckedOutObjectInfo.BoldObject is TCheckedOutObjectInfo), SysUtils.format(BoldMemberAssertInvalidObjectType, [ClassName, 'CheckedOutObjectInfo', M_CheckedOutObjectInfo.BoldObject.ClassName, 'TCheckedOutObjectInfo']));
  Result := TCheckedOutObjectInfo(M_CheckedOutObjectInfo.BoldObject);
end;

procedure TOwnObjectInfo._SetCheckedOutObjectInfo(value: TCheckedOutObjectInfo);
begin
  M_CheckedOutObjectInfo.BoldObject := value;
end;

procedure TOwnObjectInfoList.Add(NewObject: TOwnObjectInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TOwnObjectInfoList.IndexOf(anObject: TOwnObjectInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TOwnObjectInfoList.Includes(anObject: TOwnObjectInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TOwnObjectInfoList.AddNew: TOwnObjectInfo;
begin
  result := TOwnObjectInfo(InternalAddNew);
end;

procedure TOwnObjectInfoList.Insert(index: Integer; NewObject: TOwnObjectInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TOwnObjectInfoList.GetBoldObject(index: Integer): TOwnObjectInfo;
begin
  result := TOwnObjectInfo(GetElement(index));
end;

procedure TOwnObjectInfoList.SetBoldObject(index: Integer; NewObject: TOwnObjectInfo);
begin;
  SetElement(index, NewObject);
end;

{ TForeignPSInfo }

function TForeignPSInfo._Get_M_LastSynchTimestamp: TBAInteger;
begin
  assert(ValidateMember('TForeignPSInfo', 'LastSynchTimestamp', 1, TBAInteger));
  Result := TBAInteger(BoldMembers[1]);
end;

function TForeignPSInfo._GetLastSynchTimestamp: Integer;
begin
  Result := M_LastSynchTimestamp.AsInteger;
end;

procedure TForeignPSInfo._SetLastSynchTimestamp(NewValue: Integer);
begin
  M_LastSynchTimestamp.AsInteger := NewValue;
end;

function TForeignPSInfo._Get_M_OngoingSynchTimestamp: TBAInteger;
begin
  assert(ValidateMember('TForeignPSInfo', 'OngoingSynchTimestamp', 2, TBAInteger));
  Result := TBAInteger(BoldMembers[2]);
end;

function TForeignPSInfo._GetOngoingSynchTimestamp: Integer;
begin
  Result := M_OngoingSynchTimestamp.AsInteger;
end;

procedure TForeignPSInfo._SetOngoingSynchTimestamp(NewValue: Integer);
begin
  M_OngoingSynchTimestamp.AsInteger := NewValue;
end;

function TForeignPSInfo._GetOwnedObjectInfos: TForeignObjectInfoList;
begin
  assert(ValidateMember('TForeignPSInfo', 'OwnedObjectInfos', 3, TForeignObjectInfoList));
  Result := TForeignObjectInfoList(BoldMembers[3]);
end;

procedure TForeignPSInfoList.Add(NewObject: TForeignPSInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TForeignPSInfoList.IndexOf(anObject: TForeignPSInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TForeignPSInfoList.Includes(anObject: TForeignPSInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TForeignPSInfoList.AddNew: TForeignPSInfo;
begin
  result := TForeignPSInfo(InternalAddNew);
end;

procedure TForeignPSInfoList.Insert(index: Integer; NewObject: TForeignPSInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TForeignPSInfoList.GetBoldObject(index: Integer): TForeignPSInfo;
begin
  result := TForeignPSInfo(GetElement(index));
end;

procedure TForeignPSInfoList.SetBoldObject(index: Integer; NewObject: TForeignPSInfo);
begin;
  SetElement(index, NewObject);
end;

{ TOwnPSInfo }

procedure TOwnPSInfoList.Add(NewObject: TOwnPSInfo);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TOwnPSInfoList.IndexOf(anObject: TOwnPSInfo): Integer;
begin
  result := IndexOfElement(anObject);
end;

function TOwnPSInfoList.Includes(anObject: TOwnPSInfo) : Boolean;
begin
  result := IncludesElement(anObject);
end;

function TOwnPSInfoList.AddNew: TOwnPSInfo;
begin
  result := TOwnPSInfo(InternalAddNew);
end;

procedure TOwnPSInfoList.Insert(index: Integer; NewObject: TOwnPSInfo);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TOwnPSInfoList.GetBoldObject(index: Integer): TOwnPSInfo;
begin
  result := TOwnPSInfo(GetElement(index));
end;

procedure TOwnPSInfoList.SetBoldObject(index: Integer; NewObject: TOwnPSInfo);
begin;
  SetElement(index, NewObject);
end;

function GeneratedCodeCRC: String;
begin
  result := '287685748';
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRootList);
  BoldObjectListClasses.AddObjectEntry('CheckedOutObjectInfo', TCheckedOutObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('CheckingInObjectInfo', TCheckingInObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('DistributableObjectInfo', TDistributableObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('HeldObjectInfo', THeldObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('Mapping', TMappingList);
  BoldObjectListClasses.AddObjectEntry('PSInfo', TPSInfoList);
  BoldObjectListClasses.AddObjectEntry('ForeignObjectInfo', TForeignObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('OwnObjectInfo', TOwnObjectInfoList);
  BoldObjectListClasses.AddObjectEntry('ForeignPSInfo', TForeignPSInfoList);
  BoldObjectListClasses.AddObjectEntry('OwnPSInfo', TOwnPSInfoList);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRoot);
  BoldObjectClasses.AddObjectEntry('CheckedOutObjectInfo', TCheckedOutObjectInfo);
  BoldObjectClasses.AddObjectEntry('CheckingInObjectInfo', TCheckingInObjectInfo);
  BoldObjectClasses.AddObjectEntry('DistributableObjectInfo', TDistributableObjectInfo);
  BoldObjectClasses.AddObjectEntry('HeldObjectInfo', THeldObjectInfo);
  BoldObjectClasses.AddObjectEntry('Mapping', TMapping);
  BoldObjectClasses.AddObjectEntry('PSInfo', TPSInfo);
  BoldObjectClasses.AddObjectEntry('ForeignObjectInfo', TForeignObjectInfo);
  BoldObjectClasses.AddObjectEntry('OwnObjectInfo', TOwnObjectInfo);
  BoldObjectClasses.AddObjectEntry('ForeignPSInfo', TForeignPSInfo);
  BoldObjectClasses.AddObjectEntry('OwnPSInfo', TOwnPSInfo);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc('DistributableInfo', InstallBusinessClasses, InstallObjectListClasses, GeneratedCodeCRC);
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.
