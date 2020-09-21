program BE_SimpleTreeView;

uses
  Forms,
  SimpleTreeViewForm in 'SimpleTreeViewForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
