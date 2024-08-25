
{ Global compiler directives }
{$include bold.inc}
unit BoldLowEndPropagatorMainForm;

interface

uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls, ComCtrls;

type

  TBoldLEPropagatorMainForm = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Label1: TLabel;
    Splitter1: TSplitter;
    Memo1: TMemo;
    Label2: TLabel;
    cbLogEvents: TCheckBox;
    btnClearEvents: TButton;
    procedure btnClearEventsClick(Sender: TObject);
  private
    function getLogEvents: Boolean ;
  public
    { Public declarations }
    procedure UpdateClientCount(const Count: Integer);
    procedure UpdateClientList(Clients: TList);
    procedure ClearEventsList;
    property DoLogEvents: Boolean read getLogEvents;
  end;

var
  BoldLEPropagatorMainForm: TBoldLEPropagatorMainForm;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

procedure TBoldLEPropagatorMainForm.ClearEventsList;
begin
  try
    Memo1.Lines.BeginUpdate;
    Memo1.Lines.Clear;  
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

function TBoldLEPropagatorMainForm.getLogEvents: Boolean;
begin
  Result := cbLogEvents.Checked;
end;

procedure TBoldLEPropagatorMainForm.UpdateClientCount(const Count: Integer);
begin
  StatusBar1.Panels[0].Text := Format('Connected Clients: %d', [Count]);
end;

procedure TBoldLEPropagatorMainForm.UpdateClientList(Clients: TList);
var
  i: integer;
  status: string;
begin
  if Assigned(Clients)then
  begin
    ListBox1.Items.BeginUpdate;
    try
      ListBox1.Items.Clear;
      for i:= 0 to Clients.Count - 1 do
      begin
        if Boolean(Clients[i]) = true then
          Status := 'Up'
        else
          Status := 'Down';
        ListBox1.Items.Add(Format('Client #%d: %s', [i+1, Status]));
      end;
    finally
      ListBox1.Items.EndUpdate;
    end;
  end;
end;

procedure TBoldLEPropagatorMainForm.btnClearEventsClick(Sender: TObject);
begin
  ClearEventsList;
end;

end.
