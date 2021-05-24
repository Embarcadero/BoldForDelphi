
{ Global compiler directives }
{$include bold.inc}
unit BoldComboBoxPropertyEditors;

interface

uses
  Classes,
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
    raise EBold.CreateFmt('%s.GetContextType: Incoming component is not a BoldComboBox', [ClassName]);
end;

initialization
end.
