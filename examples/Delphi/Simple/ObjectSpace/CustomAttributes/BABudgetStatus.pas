{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2000-02-28 10:26:53      }
{*******************************}

unit BABudgetStatus;

interface

uses
  BoldAttributes,
  BoldMemberTypeDictionary,
  BoldSystem,
  BoldDefs;

// To install this attribute type in your application, add the following to your
// TypeNameHandle:
//
// ModelName:      BudgetStatus
// ExpressionName: BudgetStatus
// DelphiName:     TBABudgetStatus
// ContentName:    integer
// PMapper:        TBoldPMInteger
// Accessor:       As<Name>
// NativeType:     T<Name>Enum
// UnitName:       BABudgetStatus    // This unit's name, modify if necessary


type
 TBudgetStatusEnum = (bsPreliminary, bsCurrent, bsOld);

 TBABudgetStatus = class(TBAValueSet)
 private
   function GetAsBudgetStatus: TBudgetStatusEnum;
   procedure SetAsBudgetStatus(const Value: TBudgetStatusEnum);
 protected
   function GetValues: TBAValueSetValueList; override;
 public
   property AsBudgetStatus: TBudgetStatusEnum read GetAsBudgetStatus write SetAsBudgetStatus;
 end;

implementation
uses
  BoldUtils,
  SysUtils;
var
  _BudgetStatusValues: TBAValueSetValueList;

function TBABudgetStatus.GetAsBudgetStatus: TBudgetStatusEnum;
begin
  result := TBudgetStatusEnum(AsInteger);
end;

function TBABudgetStatus.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_BudgetStatusValues) then
  begin
    _BudgetStatusValues := TBAValueSetValueList.Create;
    _BudgetStatusValues.Add(integer(bsPreliminary), ['Preliminary', 'Prel', 'P']);
    _BudgetStatusValues.Add(integer(bsCurrent), ['Current', 'Curr', 'C']);
    _BudgetStatusValues.Add(integer(bsOld), ['Old', 'Old', 'O']);
  end;
  Result := _BudgetStatusValues;
end;

procedure TBABudgetStatus.SetAsBudgetStatus(const Value: TBudgetStatusEnum);
begin
  AsInteger := Integer(Value);
end;

initialization
  BoldmemberTypes.AddMemberTypeDescriptor(TBABudgetStatus, alConcrete);
finalization
  FreeAndNil(_BudgetStatusValues);
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBABudgetStatus);
end.









 
 
 
 
 
 
 
 
 