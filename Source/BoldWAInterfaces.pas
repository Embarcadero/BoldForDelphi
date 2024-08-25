
{ Global compiler directives }
{$include bold.inc}
unit BoldWAInterfaces;

interface

uses
  controls,
  BoldTemplateExpander;

const
  wfaIgnore = 0;
  wfaNext = 1;
  wfaFinish = 2;
  wfaCancel = 3;
  wfaBack = 4;
  wfaPrevious = 5;
  wfaLast = 6;
  wfaClose = 7;
type
  {forward declarations}
  IUnitGenerator = interface;
  TEnableNextEvent = procedure (const Enable: Boolean) of object;
  IWizardForm = interface
    function GetSteppedBack: Boolean;
    procedure AssignParent(aParent: TWinControl);
    procedure ClearParent;
    procedure Initialize;
    function Next: integer;
    function Back: integer;
    procedure Finish;
    procedure Cancel;
    function getUnitGeneratorIntf: IUnitGenerator;
    procedure setUnitGeneratorIntf(Intf: IUnitGenerator);
    procedure setEnableNext(Value: TEnableNextEvent);
    property SteppedBack: Boolean read GetSteppedBack;
    property UnitGenerator: IUnitGenerator read getUnitGeneratorIntf write setUnitGeneratorIntf;
    property EnableNext: TEnableNextEvent write setEnableNext;
  end;

  IUnitGenerator = interface
    procedure GenerateUnit(const UnitName: string; Template: TBoldTemplateHolder);
  end;

implementation

end.
