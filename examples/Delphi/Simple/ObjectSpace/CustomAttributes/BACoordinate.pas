{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{       2000-04-27 14:58:47             }
{*******************************}

unit BACoordinate;

interface

uses
  BoldDefs,
  ActiveX,
  BoldMemberTypeDictionary,
  BoldSubscription,
  BoldDomainElement,
  BoldSystem,
  BoldAttributes,
  BoldElements,
  BoldValueInterfaces,
  BACoordinateInterface;

// To install this attribute type in your application, add the following to your
// TypeNameHandle:
//
// ModelName:      Coordinate
// ExpressionName: Coordinate
// DelphiName:     TBACoordinate
// ContentName:    ContentName_Coordinate
// PMapper:        TBACoordinatePMapper
// Accessor:
// NativeType:
// UnitName:       BACoordinate    // This unit's name, modify if necessary

type
  TBACoordinate = class(TBoldAttribute)
  private
    {private declarations}
    fx: integer;
    fy: integer;
    fz: integer;
    function GetxContent: integer;
    procedure SetDataValuex(Value: integer);
    procedure SetxContent(NewValue: integer);
    function GetyContent: integer;
    procedure SetDataValuey(Value: integer);
    procedure SetyContent(NewValue: integer);
    function GetzContent: integer;
    procedure SetDataValuez(Value: integer);
    procedure SetzContent(NewValue: integer);
    function GetLength: Double;
  protected
    {protected declarations}
    function GetAsx: integer;
    procedure SetAsx(Value: integer);
    function MaySetx(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetAsy: integer;
    procedure SetAsy(Value: integer);
    function MaySety(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetAsz: integer;
    procedure SetAsz(Value: integer);
    function MaySetz(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    {public declarations}
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    procedure AssignContentValue(Source: IBoldValue); override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; override;
    function CanSetx(Value: integer; Subscriber: TBoldSubscriber): Boolean;
    property Asx: integer read GetAsx write SetAsx;
    function CanSety(Value: integer; Subscriber: TBoldSubscriber): Boolean;
    property Asy: integer read GetAsy write SetAsy;
    function CanSetz(Value: integer; Subscriber: TBoldSubscriber): Boolean;
    property Asz: integer read GetAsz write SetAsz;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property Length: Double read GetLength;
  end;

  {TBACoordinate_Proxy}
  TBACoordinate_Proxy = class(TBoldAttribute_Proxy, ICoordinate)
  private
    function GetProxedCoordinate: TBACoordinate;
  protected
    property ProxedCoordinate: TBACoordinate read GetProxedCoordinate implements ICoordinate;
  end;

const
  brXCoordinate = 2;
  brYCoordinate = 3;
  brZCoordinate = 4;
  brCoordinateLength = 5;

implementation

uses
  windows,
  SysUtils;

function TBACoordinate.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case representation of
    brDefault: result := format( 'x: %d y: %d z: %d', [Asx, Asy, Asz]); // your code here
    brXCoordinate: result := IntToStr( AsX );
    brYCoordinate: result := IntToStr( AsY );
    brZCoordinate: result := IntToStr( AsZ );
    brCoordinateLength: result := Format( 'Length: %4.2f', [length]);
    else result := inherited GetStringRepresentation( Representation );
  end;
end;

procedure TBACoordinate.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  case representation of
    brDefault: ; // Ignore
    brXCoordinate: AsX := StrToIntDef( Value, 0);
    brYCoordinate: AsY := StrToIntDef( Value, 0);
    brZCoordinate: AsZ := StrToIntDef( Value, 0);
    brCoordinateLength: ; // ignore
    else inherited SetStringRepresentation( Representation, value );
  end;
end;

function TBACoordinate.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  case representation of
    brDefault, brCoordinateLength: result := false; // not allowed to edit the default or the length
    brXCoordinate, brYCoordinate, brZCoordinate:
      result := StrToIntDef( value, MaxInt ) <> MaxInt;
    else result := inherited ValidateString( value, Representation );
  end;
end;

function TBACoordinate.ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean;
begin
  case representation of
    brDefault, brCoordinateLength: result := false; // Not allowed to edit the default or the length
    brXCoordinate, brYCoordinate, brZCoordinate: result := c in ['-', '0'..'9'];
    else result := inherited ValidateCharacter( c, Representation );
  end;
end;

function TBACoordinate.GetAsx: integer;
begin
  BoldClearLastFailure;
  if not canRead(nil) then
    BoldRaiseLastFailure( self, 'GetAsx', '');
  EnsureNotNull; {ensures current}
  Result := fx;
end;

function TBACoordinate.GetxContent: integer;
begin
  Result := fx;
end;

procedure TBACoordinate.SetAsx(Value: integer);
begin
  SetDataValuex(Value);
end;

function TBACoordinate.MaySetx(NewValue: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;

function TBACoordinate.CompareToAs(CompareType:TBoldCompareType;BoldElement:TBoldElement): Integer;
begin
  if BoldElement is TBACoordinate then
    result := Round( TBACoordinate(BoldElement).Length - Length)
  else
    result := inherited CompareToAs( CompareType, BoldElement );
end;

function TBACoordinate.IsEqualAs(CompareType:TBoldCompareType;BoldElement:TBoldElement): Boolean;
begin
  if BoldElement is TBACoordinate then
    result := (TBACoordinate( BoldElement ).AsX = AsX) and
    (TBACoordinate( BoldElement ).AsY = AsY) and
    (TBACoordinate( BoldElement ).AsZ = AsZ)
  else
    result := IsEqualAs( CompareType, BoldElement );
end;

procedure TBACoordinate.SetDataValuex(Value: integer);
begin
  BoldClearLastFailure;
  if not CanSetx(Value, nil) then
    BoldRaiseLastFailure( self, 'SetDataValuex', '');

  if IsNull or (fx <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValuex', '');
    try
      SetxContent(Value);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBACoordinate.SetxContent(NewValue: integer);
begin
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (Fx <> NewValue) then
  begin
    PreChange;
    Fx := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBACoordinate.CanSetx(Value: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetx(Value, Subscriber) and
            SendQuery(bqMaySetValue, [Value], Subscriber);
end;

function TBACoordinate.GetAsy: integer;
begin
  BoldClearLastFailure;
  if not canRead(nil) then
    BoldRaiseLastFailure( self, 'GetAsy', '');
  EnsureNotNull; {ensures current}
  Result := fy;
end;

function TBACoordinate.GetyContent: integer;
begin
  Result := fy;
end;

procedure TBACoordinate.SetAsy(Value: integer);
begin
  SetDataValuey(Value);
end;

function TBACoordinate.MaySety(NewValue: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;

procedure TBACoordinate.SetDataValuey(Value: integer);
begin
  BoldClearLastFailure;
  if not CanSety(Value, nil) then
    BoldRaiseLastFailure( self, 'SetDataValuey', '');

  if IsNull or (fy <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValuey', '');
    try
      SetyContent(Value);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBACoordinate.SetyContent(NewValue: integer);
begin
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (Fy <> NewValue) then
  begin
    PreChange;
    Fy := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBACoordinate.CanSety(Value: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySety(Value, Subscriber) and
            SendQuery(bqMaySetValue, [Value], Subscriber);
end;

function TBACoordinate.GetAsz: integer;
begin
  BoldClearLastFailure;
  if not canRead(nil) then
    BoldRaiseLastFailure( self, 'GetAsz', '');
  EnsureNotNull; {ensures current}
  Result := fz;
end;

function TBACoordinate.GetzContent: integer;
begin
  Result := fz;
end;

procedure TBACoordinate.SetAsz(Value: integer);
begin
  SetDataValuez(Value);
end;

function TBACoordinate.MaySetz(NewValue: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;

procedure TBACoordinate.SetDataValuez(Value: integer);
begin
  BoldClearLastFailure;
  if not CanSetz(Value, nil) then
    BoldRaiseLastFailure( self, 'SetDataValuez', '');

  if IsNull or (fz <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValuez', '');
    try
      SetzContent(Value);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBACoordinate.SetzContent(NewValue: integer);
begin
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (Fz <> NewValue) then
  begin
    PreChange;
    Fz := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBACoordinate.CanSetz(Value: integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetz(Value, Subscriber) and
            SendQuery(bqMaySetValue, [Value], Subscriber);
end;

procedure TBACoordinate.SetAsVariant(const Value: Variant);
begin
  AsString := Value; // your code here
end;

function TBACoordinate.GetAsVariant: Variant;
begin
  result := AsString; // your code here
end;


procedure TBACoordinate.AssignValue(Source: IBoldValue);
var
  s: ICoordinate;
begin
  if source.QueryInterface(ICoordinate, S) = S_OK then
  begin
    SetDataValuex(s.x);
    SetDataValuey(s.y);
    SetDataValuez(s.z);
  end
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBACoordinate.AssignContentValue(Source: IBoldValue);
var
  s: ICoordinate;
begin
  if source.QueryInterface(ICoordinate, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
    begin
      SetxContent(s.x);
      SetyContent(s.y);
      SetzContent(s.z);
    end
  end
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBACoordinate.Assign(Source: TBoldElement);
begin
  if Source is TBACoordinate then
  begin
    if TBACoordinate(Source).IsNull then
      SetToNull
    else
    begin
      Asx := TBACoordinate(Source).Asx;
      Asy := TBACoordinate(Source).Asy;
      Asz := TBACoordinate(Source).Asz;
    end
  end
  else
    AssignError(Source);
end;

function TBACoordinate.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBACoordinate_Proxy;
end;

function TBACoordinate.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, ICoordinate) then
  begin
    result := ProxyClass.create(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.Create( 'ProxyClass for %s did not implement ICoordinate');
  end else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;


  { TBACoordinate_Proxy }
function TBACoordinate_Proxy.GetProxedCoordinate: TBACoordinate;
begin
  result := ProxedElement as TBACoordinate;
end;

function TBACoordinate.GetLength: Double;
begin
  result := sqrt( sqr(AsX) + sqr(AsY) + sqr(AsZ));
end;

initialization
  BoldmemberTypes.AddMemberTypeDescriptor(TBACoordinate, alConcrete);

finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBACoordinate);
end.


































 
 
 
 
 
 
 
 
 
 