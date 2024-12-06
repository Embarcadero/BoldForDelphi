
{ Global compiler directives }
{$include bold.inc}
unit BoldSamplesReg;

interface

procedure Register;

implementation

{$R BoldSamplesReg.res}

uses
  SysUtils,
  Classes,
  DesignIntf,
  ActnList,
{$IFDEF BOLD_DELPHI16_OR_LATER}
  Actions,
{$ENDIF}
  BoldFormSaver,
  BoldGuard,
  BoldPropertyEditors,
  BoldNewObjectInterceptor,
  BoldIDEConsts,
  BoldEditOCLAction,
  BoldEditOCLActionPropEditor,
  BoldDebugActions,
  BoldFormSaverActions;

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
                      TBoldFormSaver
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
                   TBoldLogOSSAction,
                   TBoldLogQueueAction,
                   TBoldLogFormAction,
                   TBoldFormSaverApplyAction,
                   TBoldFormSaverCancelAction,
                   TBoldFormSaverOkAction
                  ],
                  nil);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(string), TBoldNewObjectInterceptor, 'Filename', TTextFileProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldEditOCLAction, 'BoldComponent', TBoldOCLComponentEditor);
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
