unit BoldAFPUser;

interface

uses
  Forms,
  BoldAFP;

type
  {---Forward declarations---}
  TBoldUserFormProvider = class;

  {---TBoldUserFormProvider---}
  TBoldUserFormProvider = class(TBoldAutoFormProvider)
  private
    fForm: TForm;
  protected
    function GetForm: TForm; override;
    procedure SetForm(Value: TForm); override;
    procedure EnsureComponents; override;
  end;

implementation

{---TBoldUserFormProvider---}
function TBoldUserFormProvider.GetForm: TForm;
begin
  Result := fForm;
end;

procedure TBoldUserFormProvider.SetForm(Value: TForm);
begin
  fForm := Value;
end;

procedure TBoldUserFormProvider.EnsureComponents;
begin
end;

end.
