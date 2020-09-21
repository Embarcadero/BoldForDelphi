unit BoldHandleComReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  Classes,
  DesignIntf,
  BoldIDEConsts,
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

const
  prop_ObjectName = 'Objectname';

procedure Register;
begin
  RegisterComponents('Bold COM Handles', // do not localize
    [
    TBoldSystemHandleCom,
    TBoldDerivedHandleCom,
    TBoldExpressionHandleCom,
    TBoldCursorHandleCom,
    TBoldListHandleCom,
    TBoldReferenceHandleCom,
    TBoldSQLHandleCom,
    //TBoldVariableDefinitionCom,
    TBoldVariableHandleCom
    ]);
  RegisterPropertyEditor(TypeInfo(String), TBoldSystemHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldDerivedHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldExpressionHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldCursorHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldListHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldReferenceHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldSQLHandleCom, prop_ObjectName, TBoldObjectNameProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldVariableHandleCom, prop_ObjectName, TBoldObjectNameProperty);
end;

end.
