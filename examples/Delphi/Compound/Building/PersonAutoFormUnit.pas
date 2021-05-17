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
  BoldEdit, BoldCaptionController;

type
  TPersonAutoForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    BoldEdit1: TBoldEdit;
    beFirstName: TBoldEdit;
    BoldEdit7: TBoldEdit;
    bcbIsMarried: TBoldCheckBox;
    bgrOwnedBuildings: TBoldGrid;
    brhPerson: TBoldReferenceHandle;
    blhOwnedBuildings: TBoldListHandle;
    Label5: TLabel;
    BoldCaptionController1: TBoldCaptionController;
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
