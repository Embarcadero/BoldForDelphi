unit BoldSamplesReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  Classes,
  DesignIntf,
  ActnList,
  BoldFormSaver,
  BoldPropertyEditors,
  BoldNewObjectInterceptor,
  BoldSortingGrid,
  BoldIDEConsts,
  BoldEditOCLAction,
  BoldEditOCLActionPropEditor,
  BoldDebugActions;

{$R *.res}

type
  TTextFileProperty = class(TBoldFileNameProperty)
  protected
    function FileFilter: string; override;
  end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_MISC,
                     [
                      TBoldNewObjectInterceptor,
                      TBoldFormSaver,
                      TBoldSortingGrid
                     ]);
end;

procedure RegisterBoldActions;
begin
  RegisterActions(BOLDACTIONGROUPNAME,
                  [
                   TBoldEditOCLAction,
                   TBoldSystemDebuggerAction,
                   TBoldLogOCLAction,
                   TBoldLogSQLAction,
                   TBoldLogPMAction,
                   TBoldLogFormAction
                  ],
                  nil);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(string), TBoldNewObjectInterceptor, 'Filename', TTextFileProperty); //do not localize
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldEditOCLAction, 'BoldComponent', TBoldOCLComponentEditor); //do not localize
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterBoldActions;
  RegisterEditors;
end;

{ TTextFileProperty }

function TTextFileProperty.FileFilter: string;
begin
  Result := Format('%s (*%s)|*%1:s', ['Text files', '.txt']);
end;

end.

