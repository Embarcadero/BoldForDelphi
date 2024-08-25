
{ Global compiler directives }
{$include bold.inc}
unit BoldTypeNameHandle;

interface

uses
  Classes,
  BoldTypeNameDictionary;

type
  { Forward declaration of classes }
  TBoldTypeNameHandle = class;

  { TBoldTypeNameHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldTypeNameHandle = class(TComponent)
  private
    FDictionary: TBoldTypeNameDictionary;
    procedure SetDictionary(const Value: TBoldTypeNameDictionary);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Dictionary: TBoldTypeNameDictionary read FDictionary write SetDictionary;
  end;

implementation

uses
  SysUtils;

{ TBoldTypeNameHandle }

constructor TBoldTypeNameHandle.Create(AOwner: TComponent);
begin
  inherited;
  fDictionary := TBoldCurrentTypeNameDictionaryClass.Create(self);
  FDictionary.AddDefaultMappings;
end;

destructor TBoldTypeNameHandle.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited;
end;

procedure TBoldTypeNameHandle.SetDictionary(const Value: TBoldTypeNameDictionary);
begin
  FDictionary.Assign(Value);
end;

end.
