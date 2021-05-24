unit PersonAutoFormUnit;

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldSubscription,
  BoldHandles,
  BoldReferenceHandle,
  Grids,
  BoldGrid,
  StdCtrls,
  BoldCheckBox,
  BoldEdit, BoldCaptionController, BoldComboBox, BoldFormSaver,
  BoldFormSaverActions, System.Actions, Vcl.ActnList, BoldHandleAction;

type
  TPersonAutoForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    BoldEdit1: TBoldEdit;
    beFirstName: TBoldEdit;
    BoldEdit7: TBoldEdit;
    bcbIsMarried: TBoldCheckBox;
    bgrOwnedBuildings: TBoldGrid;
    brhPerson: TBoldReferenceHandle;
    blhOwnedBuildings: TBoldListHandle;
    Label5: TLabel;
    BoldCaptionController1: TBoldCaptionController;
    BoldComboBox1: TBoldComboBox;
    Label3: TLabel;
    Label6: TLabel;
    blhOwnedResidentialBuildings: TBoldListHandle;
    BoldFormSaver1: TBoldFormSaver;
    BoldGrid1: TBoldGrid;
    BoldCursorHandle1: TBoldCursorHandle;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ActionList1: TActionList;
    BoldFormSaverApplyAction1: TBoldFormSaverApplyAction;
    BoldFormSaverCancelAction1: TBoldFormSaverCancelAction;
    BoldFormSaverOkAction1: TBoldFormSaverOkAction;
    procedure brhPersonObjectDeleted(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TPersonAutoForm.brhPersonObjectDeleted(Sender: TObject);
begin
  // When the object is deleted or destroyed, we release the form
  TForm(TComponent(Sender).Owner).Release;
end;

procedure TPersonAutoForm.FormCreate(Sender: TObject);
begin
  // Links the form caption to person name
  BoldCaptionController1.TrackControl := self;
end;

end.
