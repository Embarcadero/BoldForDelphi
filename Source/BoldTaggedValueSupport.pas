
{ Global compiler directives }
{$include bold.inc}
unit BoldTaggedValueSupport;

interface

uses
  Classes,
  BoldDefaultTaggedValues,
  BoldDefs;

type
  { forward declarations }
  TBoldTaggedValueSupport = class;

  TBoldOptimisticLockingMode = (bolmDefault, bolmOff, bolmModifiedMembers, bolmAllMembers, bolmTimeStamp);
  TBoldAttributeKind = (bastBold, bastDelphi);

  TDeleteAction = (daDefault, daAllow, daProhibit, daCascade);
  TDelphiFunctionType = (dfNormal, dfVirtual, dfOverride, dfDynamic, dfAbstractVirtual);
  TTableMapping = (tmOwn, tmParent, tmChildren, tmImported);

  TDelphiPropertyAccessKind = (pkNone, pkField, pkPrivateMethod, pkProtectedVirtualMethod);

  TBoldEvolutionState = (esNormal, esToBeRemoved, esRemoved);
  TBoldNationalCharConversion = (nccDefault, nccFalse, nccTrue);
  TBoldAssociationEndDefaultRegionMode = (aedrmDefault, aedrmNone, aedrmCascade, aedrmIndependentCascade, aedrmExistence);

  { TBoldTaggedValueSupport }
  TBoldTaggedValueSupport = class
  public
    class procedure AddEvolutionStates(Strings: TStrings);
    class procedure AddNationalCharConversions(Strings: TStrings);
    class procedure AddDefaultRegionModes(Strings: TStrings);
    class procedure AddOptimisticLockingModes(Strings: TStrings);
    class procedure AddDeleteActions(Strings: TStrings);
    class procedure AddDelphiFunctionTypes(Strings: TStrings);
    class procedure AddPropertyAccessKinds(Strings: TStrings);
    class procedure AddAttributeKinds(Strings: TStrings);
    class procedure AddTableMappings(Strings: TStrings);
    class function StringToDelphiFunctionType(const Value: String): TDelphiFunctionType;
    class function StringToNationalCharConversion(const value: String): TBoldNationalCharConversion;
    class function NationalCharConversionToString(Value: TBoldNationalCharConversion): String;
    class function StringToStorage(const value: String): TBoldStorage;
    class function DelphiFunctionTypeToString(Value: TDelphiFunctionType): String;
    class function StringToTableMapping(const Value: String): TTableMapping;
    class function TableMappingToString(Value: TTableMapping): String;
    class function StringToAttributeKind(const Value: String): TBoldAttributeKind;
    class function AttributeKindToString(Value: TBoldAttributeKind): String;
    class function StringToDelphiPropertyAccessKind(const Value: String): TDelphiPropertyAccessKind;
    class function StringToOptimisticLockingMode(const Value: String): TBoldOptimisticLockingMode;
    class function OptimisticLockingModeToString(Value: TBoldOptimisticLockingMode): String;
    class function DelphiPropertyAccessKindToString(Value: TDelphiPropertyAccessKind): String;
    class function StringToDeleteAction(const Value: String): TDeleteAction;
    class function DeleteActionToString(Value: TDeleteAction): String;
    class function StringToEvolutionState(const Value: String): TBoldEvolutionState;
    class function EvolutionStateToString(Value: TBoldEvolutionState): String;
    class function StringToDefaultRegionMode(const value: String): TBoldAssociationEndDefaultRegionMode;
    class function DefaultRegionModeToString(Value: TBoldAssociationEndDefaultRegionMode): String;
    class function StringToBoolean(const Value: String): Boolean;
   end;

implementation

uses
  SysUtils;

class procedure TBoldTaggedValueSupport.AddTableMappings(
  Strings: TStrings);
var
  i: TTableMapping;
begin
  for i := Low(i) to High(i) do
    Strings.Add(TableMappingToString(i));
end;

class procedure TBoldTaggedValueSupport.AddAttributeKinds(
  Strings: TStrings);
var
  i: TBoldAttributeKind;
begin
  for i := Low(i) to High(i) do
    Strings.Add(AttributeKindToString(i));
end;

class procedure TBoldTaggedValueSupport.AddOptimisticLockingModes(
  Strings: TStrings);
var
  i: TBoldOptimisticLockingMode;
begin
  for i := Low(i) to High(i) do
    Strings.Add(OptimisticLockingModeToString(i));
end;

class procedure TBoldTaggedValueSupport.AddDeleteActions(
  Strings: TStrings);
var
  i: TDeleteAction;
begin
  for i := Low(i) to High(i) do
    Strings.Add(DeleteActionToString(i));
end;

class procedure TBoldTaggedValueSupport.AddDelphiFunctionTypes(
  Strings: TStrings);
var
  i: TDelphiFunctionType;
begin
  for i := Low(i) to High(i) do
    Strings.Add(DelphiFunctionTypeToString(i));
end;

class procedure TBoldTaggedValueSupport.AddPropertyAccessKinds(
  Strings: TStrings);
var
  i: TDelphiPropertyAccessKind;
begin
  for i := Low(i) to High(i) do
    Strings.Add(DelphiPropertyAccessKindToString(i));
end;

class procedure TBoldTaggedValueSupport.AddEvolutionStates(
  Strings: TStrings);
var
  i: TBoldEvolutionState;
begin
  for i := Low(i) to High(i) do
    Strings.Add(EvolutionStateToString(i));
end;

class procedure TBoldTaggedValueSupport.AddNationalCharConversions(
  Strings: TStrings);
var
  i: TBoldNationalCharConversion;
begin
  for i := Low(i) to High(i) do
    Strings.Add(NationalCharConversionToString(i));
end;

class procedure TBoldTaggedValueSupport.AddDefaultRegionModes(
  Strings: TStrings);
var
  i: TBoldAssociationEndDefaultRegionMode;
begin
  for i := Low(i) to High(i) do
    Strings.Add(DefaultRegionModeToString(i));
end;


class function TBoldTaggedValueSupport.AttributeKindToString(Value: TBoldAttributeKind): String;
begin
  case Value of
    bastBold: Result := TV_ATTRIBUTEKIND_BOLD;
    bastDelphi: Result := TV_ATTRIBUTEKIND_DELPHI;
    else
      raise EBold.CreateFmt('%s.AttributeKindToString: Unknown TBoldAttributeKind', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.DeleteActionToString(Value: TDeleteAction): String;
begin
  case Value of
    daDefault: Result := TV_DELETEACTION_DEFAULT;
    daAllow: Result := TV_DELETEACTION_ALLOW;
    daProhibit: Result := TV_DELETEACTION_PROHIBIT;
    daCascade: Result := TV_DELETEACTION_CASCADE;
    else
      raise EBold.CreateFmt('%s.DeleteActionToString: Unknown TDeleteAction', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.DelphiFunctionTypeToString(Value: TDelphiFunctionType): String;
begin
  case Value of
    dfNormal: Result := TV_DELPHIOPERATIONKIND_NORMAL;
    dfVirtual: Result := TV_DELPHIOPERATIONKIND_VIRTUAL;
    dfOverride: Result := TV_DELPHIOPERATIONKIND_OVERRIDE;
    dfDynamic: Result := TV_DELPHIOPERATIONKIND_DYNAMIC;
    dfAbstractVirtual: Result := TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL;
    else
      raise EBold.CreateFmt('%s.DelphiFunctionTypeToString: Unknown TDelphiFunctionType', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.DelphiPropertyAccessKindToString(Value: TDelphiPropertyAccessKind): String;
begin
  case Value of
    pkNone: Result := TV_DPNONE;
    pkField: Result := TV_DPFIELD;
    pkPrivateMethod: Result := TV_DPPRIVATEMETHOD;
    pkProtectedVirtualMethod: Result := TV_DPPROTECTEDVIRTUALMETHOD;
    else
      raise EBold.CreateFmt('%s.DelphiPropertyAccessKindToString: Unknown TDelphiPropertyAccessKind', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.StringToAttributeKind(const Value: String): TBoldAttributeKind;
begin
  if Value = TV_ATTRIBUTEKIND_BOLD then
    Result := bastBold
  else if Value = TV_ATTRIBUTEKIND_DELPHI then
    Result := bastDelphi
  else
    result := bastBold;
end;

class function TBoldTaggedValueSupport.StringToBoolean(const Value: String): Boolean;
begin
  if SameText(Value, TV_TRUE) then
    Result := True
  else if SameText(Value, TV_FALSE) then
    Result := False
  else
    raise EBold.CreateFmt('%s is not a valid string for a Boolean', [Value]);
end;

class function TBoldTaggedValueSupport.StringToDefaultRegionMode(const value: String): TBoldAssociationEndDefaultRegionMode;
begin
  if Value = TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT then
    result := aedrmDefault
  else if value = TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE then
    result := aedrmNone
  else if value = TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE then
    result := aedrmExistence
  else if value = TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE then
    result := aedrmIndependentCascade
  else if value = TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE then
    result := aedrmCascade
  else
    result := aedrmDefault;
end;

class function TBoldTaggedValueSupport.StringToDeleteAction(const Value: String): TDeleteAction;
begin
  if Value = TV_DELETEACTION_DEFAULT then
    Result := daDefault
  else if Value = TV_DELETEACTION_ALLOW then
    Result := daAllow
  else if Value = TV_DELETEACTION_PROHIBIT then
    Result := daProhibit
  else if Value = TV_DELETEACTION_CASCADE then
    Result := daCascade
  else
    result := daDefault;
end;

class function TBoldTaggedValueSupport.StringToDelphiFunctionType(const Value: String): TDelphiFunctionType;
begin
  if Value = TV_DELPHIOPERATIONKIND_NORMAL then
    Result := dfNormal
  else if Value = TV_DELPHIOPERATIONKIND_VIRTUAL then
    Result := dfVirtual
  else if Value = TV_DELPHIOPERATIONKIND_OVERRIDE then
    Result := dfOverride
  else if Value = TV_DELPHIOPERATIONKIND_DYNAMIC then
    Result := dfDynamic
  else if Value = TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL then
    Result := dfAbstractVirtual
  else if Value = 'AbstractVirtual' then
    Result := dfAbstractVirtual
  else
    result := dfNormal;
end;

class function TBoldTaggedValueSupport.StringToDelphiPropertyAccessKind(const Value: String): TDelphiPropertyAccessKind;
begin
  if Value = TV_DPNONE then
    Result := pkNone
  else if Value = TV_DPFIELD then
    Result := pkField
  else if Value = TV_DPPRIVATEMETHOD then
    Result := pkPrivateMethod
  else if Value = TV_DPPROTECTEDVIRTUALMETHOD then
    Result := pkProtectedVirtualMethod
  else
    result := pkNone;
end;

class function TBoldTaggedValueSupport.StringToNationalCharConversion(
  const Value: String): TBoldNationalCharConversion;
begin
  if Value = DEFAULTNAME then
    result := nccDefault
  else if TVIsTrue(Value) then
    result := nccTrue
  else if TVIsFalse(Value) then
    result := nccFalse
  else
    result := nccDefault;
end;

class function TBoldTaggedValueSupport.StringToOptimisticLockingMode(
  const Value: String): TBoldOptimisticLockingMode;
begin
  if CompareText(value, DEFAULTNAME) = 0 then
    result := bolmDefault
  else if CompareText(value, TV_OPTIMISTICLOCKING_OFF) = 0 then
    result := bolmOff
  else if CompareText(value, TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS) = 0 then
    result := bolmModifiedMembers
  else if CompareText(value, TV_OPTIMISTICLOCKING_ALLMEMBERS) = 0 then
    result := bolmAllMembers
  else if CompareText(value, TV_OPTIMISTICLOCKING_TIMESTAMP) = 0 then
    result := bolmTimeStamp
  else if CompareText(value, TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS_OLDNAME) = 0 then
    result := bolmModifiedMembers
  else if CompareText(value, TV_OPTIMISTICLOCKING_ALLMEMBERS_OLDNAME) = 0 then
    result := bolmAllMembers
  else
    result := bolmDefault;
end;

class function TBoldTaggedValueSupport.StringToStorage(const value: String): TBoldStorage;
begin
  if Value = TV_STORAGE_INTERNAL then
    Result := bsInternal
  else if Value = TV_STORAGE_PARTIALLYEXTERNAL then
    Result := bsPartiallyExternal
  else if Value = TV_STORAGE_EXTERNAL then
    Result := bsExternal
  else if Value = TV_STORAGE_EXTERNALKEY then
    Result := bsExternalKey
  else
    result := bsInternal;
end;

class function TBoldTaggedValueSupport.StringToTableMapping(const Value: String): TTableMapping;
begin
  if Value = TV_TABLEMAPPING_OWN then
    Result := tmOwn
  else if Value = TV_TABLEMAPPING_PARENT then
    Result := tmParent
  else if Value = TV_TABLEMAPPING_CHILDREN then
    Result := tmChildren
  else if Value = TV_TABLEMAPPING_IMPORTED then
    Result := tmImported
  else
    result := tmOwn;
end;

class function TBoldTaggedValueSupport.TableMappingToString(Value: TTableMapping): String;
begin
  case Value of
    tmOwn: Result := TV_TABLEMAPPING_OWN;
    tmParent: Result := TV_TABLEMAPPING_PARENT;
    tmChildren: Result := TV_TABLEMAPPING_CHILDREN;
    tmImported: Result := TV_TABLEMAPPING_IMPORTED;
    else
      raise EBold.CreateFmt('%s.TableMappingToString: Unknown TTableMapping', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.OptimisticLockingModeToString(
  Value: TBoldOptimisticLockingMode): String;
begin
  case Value of
    bolmDefault: Result := DEFAULTNAME;
    bolmOff: Result := TV_OPTIMISTICLOCKING_OFF;
    bolmModifiedMembers: Result := TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS;
    bolmAllMembers: Result := TV_OPTIMISTICLOCKING_ALLMEMBERS;
    bolmTimeStamp: Result := TV_OPTIMISTICLOCKING_TIMESTAMP;
    else
      raise EBold.CreateFmt('%s.OptimisticLockingModeToString: Unknown TBoldOptimisticLockingMode', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.EvolutionStateToString(
  Value: TBoldEvolutionState): String;
begin
  case Value of
    esNormal: Result := TV_EVOLUTIONSTATE_NORMAL;
    esToBeRemoved: Result := TV_EVOLUTIONSTATE_TOBEREMOVED;
    esRemoved: Result := TV_EVOLUTIONSTATE_REMOVED;
    else
      raise EBold.CreateFmt('%s.EvolutionStateToString: Unknown TBoldEvolutionState', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.StringToEvolutionState(
  const Value: String): TBoldEvolutionState;
begin
  if SameText(Value, TV_EVOLUTIONSTATE_NORMAL) then
    result := esNormal
  else if SameText(Value, TV_EVOLUTIONSTATE_TOBEREMOVED) then
    result := esToBeRemoved
  else if SameText(Value, TV_EVOLUTIONSTATE_REMOVED) then
    result := esRemoved
  else
    result := esNormal;
end;

class function TBoldTaggedValueSupport.NationalCharConversionToString(
  Value: TBoldNationalCharConversion): String;
begin
  case Value of
    nccDefault: Result := DEFAULTNAME;
    nccTrue: Result := TV_TRUE;
    nccFalse: Result := TV_FALSE;
    else
      raise EBold.CreateFmt('%s.NationalCharConversionoString: Unknown TBoldNationalCharConversion', [ClassName]);
  end;
end;

class function TBoldTaggedValueSupport.DefaultRegionModeToString(
  Value: TBoldAssociationEndDefaultRegionMode): String;
begin
case Value of
    aedrmDefault: Result := TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT;
    aedrmNone: Result := TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE;
    aedrmExistence: Result := TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE;
    aedrmIndependentCascade: Result := TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE;
    aedrmCascade: Result := TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE;
    else
      raise EBold.CreateFmt('%s.DefaultRegionModeToString: Unknown TBoldAssociationEndDefaultRegionMode', [ClassName]);
  end;
end;

end.
