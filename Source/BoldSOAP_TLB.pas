
{ Global compiler directives }
{$include bold.inc}
unit BoldSOAP_TLB;






























{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;






const
  BoldSOAPMajorVersion = 1;
  BoldSOAPMinorVersion = 0;

  LIBID_BoldSOAP: TGUID = '{9BF07220-6C8A-11D4-BBAC-0010A4F9E114}';

  IID_IBoldSOAPService: TGUID = '{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}';
type


  IBoldSOAPService = interface;
  IBoldSOAPServiceDisp = dispinterface;




  IBoldSOAPService = interface(IDispatch)
    ['{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}']
    procedure Get(const request: WideString; out reply: WideString); safecall;
  end;




  IBoldSOAPServiceDisp = dispinterface
    ['{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}']
    procedure Get(const request: WideString; out reply: WideString); dispid 1;
  end;

implementation

uses ComObj;

end.
