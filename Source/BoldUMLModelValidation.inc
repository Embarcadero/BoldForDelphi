{$INCLUDE BoldUMLModel_Interface.inc}

procedure TValidator._HighestSeverity_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
var
  i: integer;
  Res: TSeverity;
begin
  res := sNone;
  Violation.DefaultSubscribe(Subscriber, breResubscribe);
  for i := 0 to Violation.count-1 do
  begin
    Violation[i].M_Severity.DefaultSubscribe(subscriber, breReEvaluate);
    if Violation[i].Severity > res then
      res := Violation[i].Severity;
  end;
  M_HighestSeverity.AsSeverity := res;
end;

