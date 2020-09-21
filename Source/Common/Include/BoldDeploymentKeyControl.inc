(******************************************************************************
 * CONFIDENTIAL! Do NOT distrubute to customer!!!
 * TBoldDeploymentKeyControl
 ******************************************************************************)

type
  TBoldDeploymentKeyStatus = (bdksMissing, bdksValid, bdksEvaluation, bdksInvalid, bdksExpired);

  TBoldDeploymentKeyControl = class
  private
    fMD5: TBoldMD5;
    fIssuedTo: string;
    fStartDate,
    fEndDate: Integer;
    fProduct: string;
    fRelease: string;
    fDeploymentKey: string;
    function GetStartDate: TDateTime;
    function GetEndDate: TDateTime;
    procedure SetFromString(Value: string);
    function Verify: TBoldDeploymentKeyStatus;
    function ReadResource: Boolean;
    function ReadRegistry: Boolean;
  protected
  public
    constructor Create(const Product, Release: string);
    destructor Destroy; override;
    class function CheckDeploymentKey(const Product, Release: string): TBoldDeploymentKeyStatus;
    property IssuedTo: string read fIssuedTo;
    property StartDate: TDateTime read GetStartDate;
    property EndDate: TDateTime read GetEndDate;
    property Product: string read fProduct;
    property Release: string read fRelease;
  end;

const
  ResourceNameFmt = 'BOLDSOFT-DEPLOYMENTKEY-%s-%s'; //Format with product and release
  DeploymentKeyRegistryPath = '\Software\BoldSoft\LicenseManager\DeploymentKeys';

constructor TBoldDeploymentKeyControl.Create(const Product, Release: string);
begin
  fMD5 := TBoldMD5.Create;
  fProduct := AnsiUpperCase(Product);
  fRelease := AnsiUpperCase(Release);
end;

destructor TBoldDeploymentKeyControl.Destroy;
begin
  FreeAndNil(fMD5);
end;

function TBoldDeploymentKeyControl.GetStartDate: TDateTime;
begin
  Result := fStartDate;
end;

function TBoldDeploymentKeyControl.GetEndDate: TDateTime;
begin
  Result := fEndDate;
end;

procedure TBoldDeploymentKeyControl.SetFromString(Value: string);
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    Values.CommaText := Value;
    if Values.Count>=4 then
    begin
      fIssuedTo := Values[0];
      fStartDate := StrToIntDef(Values[1], 0);
      fEndDate := StrToIntDef(Values[2], 0);
      fDeploymentKey := Values[3];
    end;
  finally
    FreeAndNil(Values);
  end;
end;

function TBoldDeploymentKeyControl.Verify: TBoldDeploymentKeyStatus;
var
  CurrentTimeStamp: TDateTime;
begin
  Result := bdksMissing;
  if fIssuedTo<>'' then
  begin
    fMD5.SelfTest;
    fMD5.Update(Classname); //Used as Magic cookie
    fMD5.Update(Product);
    fMD5.Update(Release);
    fMD5.Update(AnsiUpperCase(IssuedTo));
    fMD5.Update(fStartDate, SizeOf(fStartDate));
    fMD5.Update(fEndDate, SizeOf(fEndDate));
    if fMD5.DigestData.DigestStr = fDeploymentKey then
    begin
      CurrentTimeStamp := Now;
      if (fStartDate=0) and (fEndDate=0) then
      begin
        Result := bdksValid;
      end
      else
      begin
        if (CurrentTimeStamp<fStartDate) or (CurrentTimeStamp>fEndDate) then
          Result := bdksExpired
        else
          Result := bdksEvaluation;
      end;
    end
    else
      Result := bdksInvalid;
  end;
end;

function TBoldDeploymentKeyControl.ReadResource: Boolean;
var
  Buf: string;
  Len: Integer;
  ResourceName: string;
  Res: TResourceStream;
begin
  Result := False;
  ResourceName := Format(ResourceNameFmt, [fProduct, fRelease]);
  if FindResource(HInstance, PChar(ResourceName), RT_RCDATA) <> 0 then
  begin
    Res := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      SetLength(Buf, Res.Size);
      Len := Res.Read(PChar(Buf)^, Length(Buf));
      SetLength(Buf, Len);
      SetFromString(Buf);
      Result := True;
    finally
      FreeAndNil(Res);
    end;
  end;
end;

function TBoldDeploymentKeyControl.ReadRegistry: Boolean;
var
  Reg: TRegistry;
  ValueName: string;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    ValueName := Format('%s-%s', [fProduct, fRelease]);
    if Reg.OpenKey(DeploymentKeyRegistryPath, False) and Reg.ValueExists(ValueName) then
    begin
      SetFromString(Reg.ReadString(ValueName));
      Result := True;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

class function TBoldDeploymentKeyControl.CheckDeploymentKey(const Product, Release: string): TBoldDeploymentKeyStatus;
{$IFDEF BOLD_DEVELOPERIDE}
begin
  result := bdksValid;
end;
{$ELSE}
var
  DKC: TBoldDeploymentKeyControl;
begin
  DKC := Create(Product, Release);
  try
    if not DKC.ReadResource then
      DKC.ReadRegistry;
    Result := DKC.Verify;
  finally
    FreeAndNil(DKC)
  end;
end;
{$ENDIF}



(******************************************************************************)

