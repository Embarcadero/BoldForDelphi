unit ClientMainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  Forms,
  ClientDM,
  Grids,
  BoldGrid,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  StdCtrls,
  ExtCtrls,
  BoldNavigator,
  BoldListBox,
  BoldActions,
  ActnList,
  BoldHandleAction,
  BoldNavigatorDefs;

type
  TForm1 = class(TForm)
    blhPersons: TBoldListHandle;
    grdPersons: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    GroupBox1: TGroupBox;
    cmdServerSave: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtURLRoot: TEdit;
    Label4: TLabel;
    btnConnect: TButton;
    btnChargeRent: TButton;
    grdBuildings: TBoldGrid;
    blhBuildings: TBoldListHandle;
    lbxOwners: TBoldListBox;
    BoldNavigator2: TBoldNavigator;
    Label5: TLabel;
    Label6: TLabel;
    lbxResidents: TBoldListBox;
    blhOwners: TBoldListHandle;
    blhResidents: TBoldListHandle;
    Label7: TLabel;
    ActionList1: TActionList;
    cmdOpen: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    cmdSave: TButton;
    procedure cmdServerSaveClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnChargeRentClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BoldHTTPPersistenceControllerClient,
  BoldXMLRequests,
  BoldId;

{$R *.DFM}


procedure TForm1.cmdServerSaveClick(Sender: TObject);
var
  aSoapService: TBoldHTTPSOAPService;
  anXMLRequest: TBoldXMLRequest;
  aReply: WideString;
begin
  aSoapService := TBoldHTTPSOAPService.Create;
  aSoapService.WebConnection := dmClient.BoldWebConnection2;
  anXMLRequest := TBoldXMLRequest.CreateInitialized;
  try
    anXMLRequest.SetAction('SaveToDB');
    aSoapService.Get(anXMLRequest.DomDocument.xml, aReply);
  finally
    anXMLRequest.Free;
    aSoapService.Free;
  end;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  dmClient.BoldWebConnection1.URL := edtURLRoot.Text + '/persistence';
  dmClient.BoldWebConnection2.URL := edtURLRoot.Text + '/soapcalls';
  btnConnect.Enabled := false;
end;

procedure TForm1.btnChargeRentClick(Sender: TObject);
var
  aSoapService: TBoldHTTPSOAPService;
  anXMLRequest: TBoldXMLRequest;
  aReply: WideString;
  anObjectId: TBoldObjectId;
begin
  anObjectId := blhBuildings.CurrentBoldObject.BoldObjectLocator.BoldObjectID;
  if anObjectId.IsStorable then
  begin
    aSoapService := TBoldHTTPSOAPService.Create;
    aSoapService.WebConnection := dmClient.BoldWebConnection2;
    anXMLRequest := TBoldXMLRequest.CreateInitialized;
    try
      anXMLRequest.SetAction('ChargeRent');
      anXMLRequest.AddParam('Building', anObjectId.AsString);
      aSoapService.Get(anXMLRequest.DomDocument.xml, aReply);
      showmessage(aReply);
    finally
      anXMLRequest.Free;
      aSoapService.Free;
    end;
  end
  else
    showmessage('Cannot charge rent for this building. It does not exist on the server. You must save it first.');
end;

end.
