
{ Global compiler directives }
{$include bold.inc}
unit BoldHandleComReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  Classes,
  DesignIntf,
  BoldGuard,
  BoldSystemHandleCom,
  BoldDerivedHandleCom,
  BoldExpressionHandleCom,
  BoldCursorHandleCom,
  BoldListHandleCom,
  BoldReferenceHandleCom,
  BoldSQLHandleCom,
  BoldVariableDefinitionCom,
  BoldVariableHandleCom,
  BoldObjectNamePropertyEditor;

{$R BoldHandleComReg.res}

procedure Register;
begin

  begin
    RegisterComponents('Bold COM Handles',
      [
      TBoldSystemHandleCom,
      TBoldDerivedHandleCom,
      TBoldExpressionHandleCom,
      TBoldCursorHandleCom,
      TBoldListHandleCom,
      TBoldReferenceHandleCom,
      TBoldSQLHandleCom,
      TBoldVariableHandleCom
      ]);
    RegisterPropertyEditor(TypeInfo(String), TBoldSystemHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldDerivedHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldExpressionHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldCursorHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldListHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldReferenceHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldSQLHandleCom, 'Objectname', TBoldObjectNameProperty);
    RegisterPropertyEditor(TypeInfo(String), TBoldVariableHandleCom, 'Objectname', TBoldObjectNameProperty);
  end;
end;

end.