
{ Global compiler directives }
{$include bold.inc}
unit BoldStreams;

interface

type
  { forward declarations }
  IBoldStreamable = interface;

  {-- IBoldStreamable --}
  IBoldStreamable = interface
    ['{CCDDD441-5790-11D2-B7DA-00600871B01B}']
    function GetStreamName: string;
    property StreamName: string read GetStreamName;
  end;

implementation

end.
