
{ Global compiler directives }
{$include bold.inc}
unit BoldSOAP2_TLB;






























{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL, 
  BoldSOAP_TLB;






const
  BoldSOAP2MajorVersion = 1;
  BoldSOAP2MinorVersion = 0;

  LIBID_BoldSOAP2: TGUID = '{430A7A46-55F8-49B7-82A2-539FA3580ABE}';

  IID_IBoldSOAPService2: TGUID = '{D349D000-EDDB-4F8C-886F-CB00F640DB6A}';
type


  IBoldSOAPService2 = interface;
  IBoldSOAPService2Disp = dispinterface;




  IBoldSOAPService2 = interface(IBoldSOAPService)
    ['{D349D000-EDDB-4F8C-886F-CB00F640DB6A}']
    function  Get2(const request: WideString): WideString; safecall;
  end;




  IBoldSOAPService2Disp = dispinterface
    ['{D349D000-EDDB-4F8C-886F-CB00F640DB6A}']
    function  Get2(const request: WideString): WideString; dispid 2;
    procedure Get(const request: WideString; out reply: WideString); dispid 1;
  end;

implementation

uses ComObj;

end.
