(************************************)
(*    This file is autogenerated    *)
(* Any manual changes will be LOST! *)
(************************************)

(* This file should be stored in the same directory    *)
(* as the form/datamodule with the corresponding model *)

// 

{ File generated  }

unit ModelEvolTest;

{$DEFINE ModelEvolTest_unitheader}
{$INCLUDE ModelEvolTest_Interface.inc}

uses
  {ImplementationUses}
  {ImplementationDependancies}
  BoldGeneratedCodeDictionary;

{ Includefile for methodimplementations }


const
  BoldMemberAssertSelfAssigned: string = 'Unable to access member %s.%s: Object is not assigned!)';
  BoldMemberAssertInvalidType: string = 'Invalid membertype for %s.%s. Expected %s (code might be out of sync with model)';
  BoldMemberAssertInvalidObjectType: string = 'Object of singlelink (%s.%s) is of wrong type (is %s, should be %s)';
  BoldMemberAssertInvalidDelphiName: string = '%s.%s: Member is incorrect. it is called %s in meta info (code might be out of sync with model)';

{ TBusinessClassesRoot }


procedure TBusinessClassesRootList.Add(NewObject: TBusinessClassesRoot);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TBusinessClassesRootList.IndexOf(anObject: TBusinessClassesRoot): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TBusinessClassesRootList.Includes( anObject: TBusinessClassesRoot ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TBusinessClassesRootList.Insert(index: Integer; NewObject: TBusinessClassesRoot);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TBusinessClassesRootList.GetBoldObject(index: Integer): TBusinessClassesRoot;
begin
  result := GetElement(index) as TBusinessClassesRoot;
end;

procedure TBusinessClassesRootList.SetBoldObject(index: Integer; NewObject: TBusinessClassesRoot);
begin;
  SetElement( index, NewObject );
end;


{ TAssoc }

function TAssoc._Get_M_ChildMapped: TBoldObjectReference;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TAssoc', 'ChildMapped'] ));
  assert( BoldMembers[0] is TBoldObjectReference, format( BoldMemberAssertInvalidType, [ClassName, 'ChildMapped', 'TBoldObjectReference'] ));
  assert( BoldMembers[0].BoldMemberRTInfo.DelphiName = 'ChildMapped', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'ChildMapped', BoldMembers[0].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[0] as TBoldObjectReference;
end;

function TAssoc._GetChildMapped: TChildMapped;
begin
  assert( not assigned(M_ChildMapped.BoldObject ) or (M_ChildMapped.BoldObject is TChildMapped), format( BoldMemberAssertInvalidObjectType, [ClassName, ChildMapped, M_ChildMapped.BoldObject.ClassName, TChildMapped]));
  Result := M_ChildMapped.BoldObject as TChildMapped;
end;

function TAssoc._Get_M_ParentMapped: TBoldObjectReference;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TAssoc', 'ParentMapped'] ));
  assert( BoldMembers[1] is TBoldObjectReference, format( BoldMemberAssertInvalidType, [ClassName, 'ParentMapped', 'TBoldObjectReference'] ));
  assert( BoldMembers[1].BoldMemberRTInfo.DelphiName = 'ParentMapped', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'ParentMapped', BoldMembers[1].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[1] as TBoldObjectReference;
end;

function TAssoc._GetParentMapped: TParentMapped;
begin
  assert( not assigned(M_ParentMapped.BoldObject ) or (M_ParentMapped.BoldObject is TParentMapped), format( BoldMemberAssertInvalidObjectType, [ClassName, ParentMapped, M_ParentMapped.BoldObject.ClassName, TParentMapped]));
  Result := M_ParentMapped.BoldObject as TParentMapped;
end;


procedure TAssocList.Add(NewObject: TAssoc);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TAssocList.IndexOf(anObject: TAssoc): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TAssocList.Includes( anObject: TAssoc ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TAssocList.Insert(index: Integer; NewObject: TAssoc);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TAssocList.GetBoldObject(index: Integer): TAssoc;
begin
  result := GetElement(index) as TAssoc;
end;

procedure TAssocList.SetBoldObject(index: Integer; NewObject: TAssoc);
begin;
  SetElement( index, NewObject );
end;


{ TChildMapped }

function TChildMapped._Get_M_name: TBAString;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TChildMapped', 'name'] ));
  assert( BoldMembers[0] is TBAString, format( BoldMemberAssertInvalidType, [ClassName, 'name', 'TBAString'] ));
  assert( BoldMembers[0].BoldMemberRTInfo.DelphiName = 'name', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'name', BoldMembers[0].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[0] as TBAString;
end;

function TChildMapped._Getname: String;
begin
  Result := M_name.AsString;
end;

procedure TChildMapped._Setname(NewValue: String);
begin
  M_name.AsString := NewValue;
end;

function TChildMapped._GetParentMapped: TParentMappedList;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TChildMapped', 'ParentMapped'] ));
  assert( BoldMembers[1] is TParentMappedList, format( BoldMemberAssertInvalidType, [ClassName, 'ParentMapped', 'TParentMappedList'] ));
  assert( BoldMembers[1].BoldMemberRTInfo.DelphiName = 'ParentMapped', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'ParentMapped', BoldMembers[1].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[1] as TParentMappedList;
end;

function TChildMapped._GetAssoc: TAssocList;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TChildMapped', 'Assoc'] ));
  assert( BoldMembers[2] is TAssocList, format( BoldMemberAssertInvalidType, [ClassName, 'Assoc', 'TAssocList'] ));
  assert( BoldMembers[2].BoldMemberRTInfo.DelphiName = 'Assoc', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'Assoc', BoldMembers[2].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[2] as TAssocList;
end;


procedure TChildMappedList.Add(NewObject: TChildMapped);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TChildMappedList.IndexOf(anObject: TChildMapped): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TChildMappedList.Includes( anObject: TChildMapped ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TChildMappedList.Insert(index: Integer; NewObject: TChildMapped);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TChildMappedList.GetBoldObject(index: Integer): TChildMapped;
begin
  result := GetElement(index) as TChildMapped;
end;

procedure TChildMappedList.SetBoldObject(index: Integer; NewObject: TChildMapped);
begin;
  SetElement( index, NewObject );
end;


{ TParent }


procedure TParentList.Add(NewObject: TParent);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TParentList.IndexOf(anObject: TParent): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TParentList.Includes( anObject: TParent ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TParentList.Insert(index: Integer; NewObject: TParent);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TParentList.GetBoldObject(index: Integer): TParent;
begin
  result := GetElement(index) as TParent;
end;

procedure TParentList.SetBoldObject(index: Integer; NewObject: TParent);
begin;
  SetElement( index, NewObject );
end;


{ TChildA }


procedure TChildAList.Add(NewObject: TChildA);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TChildAList.IndexOf(anObject: TChildA): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TChildAList.Includes( anObject: TChildA ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TChildAList.Insert(index: Integer; NewObject: TChildA);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TChildAList.GetBoldObject(index: Integer): TChildA;
begin
  result := GetElement(index) as TChildA;
end;

procedure TChildAList.SetBoldObject(index: Integer; NewObject: TChildA);
begin;
  SetElement( index, NewObject );
end;


{ TChildB }


procedure TChildBList.Add(NewObject: TChildB);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TChildBList.IndexOf(anObject: TChildB): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TChildBList.Includes( anObject: TChildB ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TChildBList.Insert(index: Integer; NewObject: TChildB);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TChildBList.GetBoldObject(index: Integer): TChildB;
begin
  result := GetElement(index) as TChildB;
end;

procedure TChildBList.SetBoldObject(index: Integer; NewObject: TChildB);
begin;
  SetElement( index, NewObject );
end;


{ TParentMapped }

function TParentMapped._Get_M_name: TBAString;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TParentMapped', 'name'] ));
  assert( BoldMembers[0] is TBAString, format( BoldMemberAssertInvalidType, [ClassName, 'name', 'TBAString'] ));
  assert( BoldMembers[0].BoldMemberRTInfo.DelphiName = 'name', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'name', BoldMembers[0].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[0] as TBAString;
end;

function TParentMapped._Getname: String;
begin
  Result := M_name.AsString;
end;

procedure TParentMapped._Setname(NewValue: String);
begin
  M_name.AsString := NewValue;
end;

function TParentMapped._GetChildMapped: TChildMappedList;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TParentMapped', 'ChildMapped'] ));
  assert( BoldMembers[1] is TChildMappedList, format( BoldMemberAssertInvalidType, [ClassName, 'ChildMapped', 'TChildMappedList'] ));
  assert( BoldMembers[1].BoldMemberRTInfo.DelphiName = 'ChildMapped', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'ChildMapped', BoldMembers[1].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[1] as TChildMappedList;
end;

function TParentMapped._GetAssoc: TAssocList;
begin
  assert( assigned( self ), format( BoldMemberAssertSelfAssigned, ['TParentMapped', 'Assoc'] ));
  assert( BoldMembers[2] is TAssocList, format( BoldMemberAssertInvalidType, [ClassName, 'Assoc', 'TAssocList'] ));
  assert( BoldMembers[2].BoldMemberRTInfo.DelphiName = 'Assoc', format( BoldMemberAssertInvalidDelphiName, [ClassName, 'Assoc', BoldMembers[2].BoldMemberRTInfo.DelphiName] ));
  Result := BoldMembers[2] as TAssocList;
end;


procedure TParentMappedList.Add(NewObject: TParentMapped);
begin
  if Assigned(NewObject) then
    AddElement(NewObject);
end;

function TParentMappedList.IndexOf(anObject: TParentMapped): Integer;
begin
  result := IndexOfElement( anObject );
end;

function TParentMappedList.Includes( anObject: TParentMapped ) : Boolean;
begin
  result := IncludesElement( anObject );
end;

procedure TParentMappedList.Insert(index: Integer; NewObject: TParentMapped);
begin
  if assigned(NewObject) then
    InsertElement(index, NewObject);
end;

function TParentMappedList.GetBoldObject(index: Integer): TParentMapped;
begin
  result := GetElement(index) as TParentMapped;
end;

procedure TParentMappedList.SetBoldObject(index: Integer; NewObject: TParentMapped);
begin;
  SetElement( index, NewObject );
end;



function GeneratedCodeCRC: integer;
begin
  result := 0;
end;

procedure InstallObjectListClasses(BoldObjectListClasses: TBoldGeneratedClassList);
begin
  BoldObjectListClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRootList);
  BoldObjectListClasses.AddObjectEntry('Assoc', TAssocList);
  BoldObjectListClasses.AddObjectEntry('ChildMapped', TChildMappedList);
  BoldObjectListClasses.AddObjectEntry('Parent', TParentList);
  BoldObjectListClasses.AddObjectEntry('ChildA', TChildAList);
  BoldObjectListClasses.AddObjectEntry('ChildB', TChildBList);
  BoldObjectListClasses.AddObjectEntry('ParentMapped', TParentMappedList);
end;

procedure InstallBusinessClasses(BoldObjectClasses: TBoldGeneratedClassList);
begin
  BoldObjectClasses.AddObjectEntry('BusinessClassesRoot', TBusinessClassesRoot);
  BoldObjectClasses.AddObjectEntry('Assoc', TAssoc);
  BoldObjectClasses.AddObjectEntry('ChildMapped', TChildMapped);
  BoldObjectClasses.AddObjectEntry('Parent', TParent);
  BoldObjectClasses.AddObjectEntry('ChildA', TChildA);
  BoldObjectClasses.AddObjectEntry('ChildB', TChildB);
  BoldObjectClasses.AddObjectEntry('ParentMapped', TParentMapped);
end;

var
  CodeDescriptor: TBoldGeneratedCodeDescriptor;

initialization
  CodeDescriptor := GeneratedCodes.AddGeneratedCodeDescriptorWithFunc( 'ModelEvolTest', InstallBusinessClasses, InstallObjectListClasses );
finalization
  GeneratedCodes.Remove(CodeDescriptor);
end.



 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 