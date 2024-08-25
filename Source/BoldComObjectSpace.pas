
{ Global compiler directives }
{$include bold.inc}
unit BoldComObjectSpace;

interface

uses
  ActiveX;

const
  { DataFlag constants, used in IBoldElementHandle.GetData and SetData }
  DF_VALUE               = $00000001;
  DF_DYNAMICBOLDTYPE     = $00000002;
  DF_STATICBOLDTYPE      = $00000004;
  DF_STATICSYSTEMTYPE    = $00000008;
  DF_BOLDSYSTEM          = $00000010;
  DF_STATICROOTTYPE      = $00000020;
  DF_CURRENTBOLDOBJECT   = $00000040;
  DF_BOLDLIST            = $00000080;
  DF_LISTELEMENTTYPE     = $00000100;
  DF_HANDLEID            = $00000200;
  DF_ACTIVE              = $00000400;
  DF_PERSISTENT          = $00000800;
  DF_STATICSYSTEMHANDLE  = $00001000;
  DF_ENABLED             = $00002000;
  DF_ROOTHANDLE          = $00004000;
  DF_ROOTTYPENAME        = $00008000;
  DF_SUBSCRIBE           = $00010000;
  DF_EXPRESSION          = $00020000;
  DF_COUNT               = $00040000;
  DF_CURRENTINDEX        = $00080000;
  DF_AUTOFIRST           = $00100000;
  DF_STATICVALUETYPENAME = $00200000;
  DF_VALUETYPENAME       = $00400000;
  DF_INITIALVALUES       = $00800000;
  DF_CLASSEXPRESSIONNAME = $01000000;
  DF_CLEARBEFOREEXECUTE  = $02000000;
  DF_SQLORDERBYCLAUSE    = $04000000;
  DF_SQLWHERECLAUSE      = $08000000;
  DF_EVALUATEINPS        = $10000000;


function BoldComObjectSpaceTypeLibrary: ITypeLib;

implementation

uses
  SysUtils,
  BoldComUtils,
  BoldComObjectSpace_TLB;

var
  G_TypeLibrary: ITypeLib = nil;

function BoldComObjectSpaceTypeLibrary: ITypeLib;
begin
  if not Assigned(G_TypeLibrary) then
  begin
    if Failed(LoadRegTypeLib(LIBID_BoldComObjectSpace,1,0,0,G_TypeLibrary)) then
      raise EBoldCom.Create('Unable to load type library (BoldComObjectSpace)');
  end;
  Result := G_TypeLibrary;
end;

end.
