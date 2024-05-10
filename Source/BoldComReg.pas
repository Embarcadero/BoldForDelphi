
{ Global compiler directives }
{$include bold.inc}
unit BoldComReg;

interface

procedure Register;

implementation

{$R BoldComReg.Res}

uses
  Classes,
  DesignIntf,
  BoldComServerHandles,
  BoldComClientHandles,
  BoldComEditors,
  BoldIDEConsts,
  BoldObjectNamePropertyEditor
  ;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_COM,
      [TBoldComServerHandle,
       TBoldComServerObjectHandle,
       TBoldComConnectionHandle,
       TBoldComClientObjectHandle]);

    RegisterComponentEditor(TBoldComServerHandle, TBoldComServerHandleComponentEditor);

    RegisterPropertyEditor(TypeInfo(string), TBoldComExportHandle,
      'ServerClass', TBoldComServerClassPropertyEditor);
    RegisterPropertyEditor(TypeInfo(String), TBoldComClientObjectHandle, 'Objectname', TBoldObjectNameProperty);
end;

end.
