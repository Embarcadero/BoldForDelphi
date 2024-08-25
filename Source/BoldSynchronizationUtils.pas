
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldSynchronizationUtils;

interface


function GetPropagatorCLSID: TGuid;

implementation
uses
  BoldPropagatorInterfaces_TLB;

function GetPropagatorCLSID: TGuid;
begin
  Result := CLASS_BoldPropagator;
end;


end.