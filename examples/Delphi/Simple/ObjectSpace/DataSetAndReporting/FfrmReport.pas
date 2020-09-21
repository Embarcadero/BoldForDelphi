unit FfrmReport;

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  QuickRpt,
  Qrctrls,
  ExtCtrls,
  Db,
  BoldDataSet,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle;

type
  TfrmReport = class(TForm)
    qrMain: TQuickRep;
    bndTitle: TQRBand;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    blhBuildings: TBoldListHandle;
    blhResidents: TBoldListHandle;
    bdsResident: TBoldDataSet;
    bdsBuilding: TBoldDataSet;
    bndResidents: TQRSubDetail;
    QRDBText3: TQRDBText;
    QRDBText4: TQRDBText;
    bndBuildings: TQRSubDetail;
    QRDBText1: TQRDBText;
    QRDBText2: TQRDBText;
    bndResidentsHeader: TQRBand;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReport: TfrmReport;

implementation

uses fMain, dMain;

{$R *.DFM}

end.
