{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2001-08-27 14:28:47      }
{*******************************}

unit BACoordinateInterface;

interface

uses
  Windows,
  BoldDefs,
  BoldFreeStandingValues,
  BoldFreeStandingValueFactories,
  BoldValueInterfaces;

type
  ICoordinate = interface(IBoldNullableValue)
  ['{E4F60819-55B0-4265-BDBB-6907F2A3F7F9}']
    function GetxContent: integer;
    procedure SetxContent(Value: integer);
    function GetyContent: integer;
    procedure SetyContent(Value: integer);
    function GetzContent: integer;
    procedure SetzContent(Value: integer);
    property x: integer read GetxContent write SetxContent;
    property y: integer read GetyContent write SetyContent;
    property z: integer read GetzContent write SetzContent;
  end;

  TBFSCoordinate = class(TBoldFreeStandingNullableValue, ICoordinate)
  private
    fx : integer;
    fy : integer;
    fz : integer;
    function GetxContent: integer;
    procedure SetxContent(NewValue : integer);
    function GetyContent: integer;
    procedure SetyContent(NewValue : integer);
    function GetzContent: integer;
    procedure SetzContent(NewValue : integer);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property x: integer read GetxContent write SetxContent;
    property y: integer read GetyContent write SetyContent;
    property z: integer read GetzContent write SetzContent;
  end;

const
  ContentName_Coordinate = 'Coordinate';

implementation

{ TBFSCoordinate }

function TBFSCoordinate.GetStreamName: String;
begin
  result := ContentName_Coordinate;
end;

procedure TBFSCoordinate.AssignContentValue(Source: IBoldValue);
var
  s: ICoordinate;
begin
  if source.QueryInterface(ICoordinate, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
    begin
      x := s.x;
      y := s.y;
      z := s.z;
    end
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;


function TBFSCoordinate.GetxContent: integer;
begin
  result := fx;
end;

procedure TBFSCoordinate.SetxContent(NewValue: integer);
begin
  fx := NewValue;
end;


function TBFSCoordinate.GetyContent: integer;
begin
  result := fy;
end;

procedure TBFSCoordinate.SetyContent(NewValue: integer);
begin
  fy := NewValue;
end;


function TBFSCoordinate.GetzContent: integer;
begin
  result := fz;
end;

procedure TBFSCoordinate.SetzContent(NewValue: integer);
begin
  fz := NewValue;
end;

initialization
  With FreeStandingValueFactory do
    RegisterFreeStandingClass(ContentName_Coordinate, TBFSCoordinate);
end.

 
 
