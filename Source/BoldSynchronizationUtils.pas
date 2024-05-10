
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
  BoldRev,
  BoldPropagatorInterfaces_TLB;

function GetPropagatorCLSID: TGuid;
begin
  Result := CLASS_BoldPropagator;
end;

initialization
   BoldRegisterModuleVersion('$Workfile: BoldSynchronizationUtils.pas $ $Revision: 2 $ $Date: 02-04-30 15:04 $');

end.