unit maan_PropagatorServer;

interface

uses
  BoldPropagatorServer, BoldAdvancedPropagator;

type
  TPropagatorServerTest = class(TBoldPropagatorServer)
  protected
    function getAdvancedPropagator: TBoldAdvancedPropagator; override;
  public
    fAdvancedPropagator: TBoldAdvancedPropagator;
  end;

implementation

uses
  SysUtils;

{ PropagatorServerTest }

function TPropagatorServerTest.getAdvancedPropagator: TBoldAdvancedPropagator;
begin
  Result := fAdvancedPropagator;
end;

end.
