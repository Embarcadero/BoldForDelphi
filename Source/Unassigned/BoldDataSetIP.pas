unit BoldDataSetIP;

interface

uses
  classes,
  BoldDataset,
  wwTypes;

type
  TBoldDatasetIP = class(TBoldDataset)
  private
    FControlType: TStrings;
    FPictureMasks: TStrings;
    FUsePictureMask: Boolean;
    FOnInvalidValue: TwwInvalidValueEvent;
    function GetControlType: TStrings;
    procedure SetControlType(AControlType: TStrings);
    function GetPictureMasks: TStrings;
    procedure SetPictureMasks(APictureMasks: TStrings);
    function GetDatabaseName: String;
    procedure SetDatabaseName(ADatabaseName: String);
  protected
    destructor Destroy; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ControlType: TStrings read  GetControlType write SetControlType;
    property PictureMasks: TStrings read GetPictureMasks write SetPictureMasks;
    property ValidateWithMask: Boolean read FUsePictureMask write FUsePictureMask;
//    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property OnInvalidValue: TwwInvalidValueEvent read FOnInvalidValue write FOnInvalidValue;
  end;

  procedure Register;

implementation

uses
  SysUtils,
  BoldUtils;

destructor TBoldDatasetIP.Destroy;
begin
  FreeAndNil(FControlType);
  FreeAndNil(FPictureMasks);
  inherited Destroy;
end;

constructor TBoldDatasetIP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlType:= TStringList.Create;
  FPictureMasks:= TStringList.Create;
  FUsePictureMask:= True;
end;

function TBoldDatasetIP.GetControltype: TStrings;
begin
  Result:= FControlType;
end;

procedure TBoldDatasetIP.SetControlType(AControlType: TStrings);
begin
  FControlType.Assign(AControlType);
end;

function TBoldDatasetIP.GetPictureMasks: TStrings;
begin
  Result:= FPictureMasks;
end;

procedure TBoldDatasetIP.SetPictureMasks(APictureMasks: TStrings);
begin
  FPictureMasks.Assign(APictureMasks);
end;

function TBoldDatasetIP.GetDatabaseName: String;
begin
  Result:= '';
end;

procedure TBoldDatasetIP.SetDatabaseName(ADatabaseName: String);
begin
end;

procedure Register;
begin
  //RegisterComponents('Bold', [TBoldDatasetIP]);
end;

end.
