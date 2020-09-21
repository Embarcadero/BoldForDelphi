// $Header: /BfD/Source/Propagator/LowEnd/BoldLowEndPropagator.dpr 4     00-06-15 15:32 Joho $

program BoldLowEndPropagator;

uses
  Forms,
  BoldLowEndPropagatorMainForm in 'BoldLowEndPropagatorMainForm.pas' {BoldLEPropagatorMainForm},
  BoldLowEndPropagatorClasses in 'BoldLowEndPropagatorClasses.pas',
  BoldPropagatorInterfaces_TLB in '..\Common\BoldPropagatorInterfaces_TLB.pas';

{$R ..\common\BoldPropagatorInterfaces.tlb}
{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBoldLEPropagatorMainForm, BoldLEPropagatorMainForm);
  Application.Run;
end.
