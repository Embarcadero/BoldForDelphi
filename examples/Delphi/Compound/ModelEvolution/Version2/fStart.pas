unit fStart;

interface

uses
  Classes,
  Controls,
  Forms,
  BoldDbEvolutorForm,
  BoldDbEvolutor,
  StdCtrls;

type
  TfrmStart = class(TForm)
    btnCancel: TButton;
    btnCreateSchema: TButton;
    btnOpenSystem: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStart: TfrmStart;

implementation

uses
  dSystem, dPersistence;

{$R *.DFM}

procedure TfrmStart.Button1Click(Sender: TObject);
var
  Evolutor: TBoldDataBaseEvolutor;
  form: TfrmBoldDbEvolutor;
begin
  Evolutor := TBoldDataBaseEvolutor.Create(dmPersistence.BoldPersistenceHandleDB1);
  form := TfrmBoldDbEvolutor.Create(self);
  try
    Evolutor.GenericScript := true;
    Evolutor.CalculateScript;
    Evolutor.GenerateScript(form.SQLScript, form.MappingInfoScript);
    Evolutor.GenerateWarnings(form.Warnings);
    if form.ShowModal = mrOK then
    begin
      Evolutor.ExecuteScript;
      dmSystem.SystemHandle.Active := true;
      ModalResult := mrOK;
    end;
  finally
    Evolutor.Free;
  end;
end;

end.
