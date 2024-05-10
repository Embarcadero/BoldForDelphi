
{ Global compiler directives }
{$include bold.inc}
unit BoldComboBoxPropertyEditors;

interface

uses
  Classes,

  BoldCoreConsts,
  BoldElements,
  BoldPropertyEditors;

type
  TBoldOCLExpressionForComboBoxSetValueExpression = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldComboBox;

{ TBoldOCLExpressionForComboBox }

function TBoldOCLExpressionForComboBoxSetValueExpression.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
begin
  if component is TBoldComboBox then
    if Assigned(TBoldComboBox(component).BoldHandle) then
      Result := TBoldComboBox(component).BoldHandle.StaticBoldType
    else
      Result := nil
  else
    raise EBold.CreateFmt(sComponentNotComboBox, [ClassName]);
end;

end.
