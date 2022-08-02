
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorInterfaces_TLB;



















{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;






const
  BoldPropagatorInterfacesMajorVersion = 1;
  BoldPropagatorInterfacesMinorVersion = 0;

  LIBID_BoldPropagatorInterfaces: TGUID = '{DC6A300A-C8C2-4D26-9A8D-C8ECB164B54B}';

  IID_IBoldEventPropagator: TGUID = '{BB7ABB77-BCE2-44C8-9510-F760F507A298}';
  IID_IBoldClientHandler: TGUID = '{A86E36B1-5EA3-4961-8448-45FD822A271E}';
  IID_IBoldListener: TGUID = '{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}';
  CLASS_BoldPropagator: TGUID = '{97C5A9AD-B5F4-4350-A51D-028975782BDA}';
  IID_IBoldListenerAdmin: TGUID = '{7457EA48-E3B5-4E07-8496-87229ACA5E2D}';
  CLASS_BoldListener: TGUID = '{A1582C23-E7AB-451A-837F-2131B425E73B}';
type


  IBoldEventPropagator = interface;
  IBoldClientHandler = interface;
  IBoldListener = interface;
  IBoldListenerDisp = dispinterface;
  IBoldListenerAdmin = interface;
  IBoldListenerAdminDisp = dispinterface;



  BoldPropagator = IBoldClientHandler;
  BoldListener = IBoldListener;




  IBoldEventPropagator = interface(IUnknown)
    ['{BB7ABB77-BCE2-44C8-9510-F760F507A298}']
    function  SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function  AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
  end;




  IBoldClientHandler = interface(IUnknown)
    ['{A86E36B1-5EA3-4961-8448-45FD822A271E}']
    function  RegisterClient(LeaseDuration: Integer; PollingInterval: Integer; 
                             const BoldClientListener: IBoldListener; 
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
    function  ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
    function  UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
  end;




  IBoldListener = interface(IUnknown)
    ['{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}']
    function  ReceiveEvents(Events: OleVariant): Integer; safecall;
  end;




  IBoldListenerDisp = dispinterface
    ['{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}']
    function  ReceiveEvents(Events: OleVariant): Integer; dispid 1;
  end;




  IBoldListenerAdmin = interface(IUnknown)
    ['{7457EA48-E3B5-4E07-8496-87229ACA5E2D}']
    function  Ping: Integer; safecall;
    procedure DisconnectClient(const aMessage: WideString; RemainDisconnected: Integer); safecall;
  end;




  IBoldListenerAdminDisp = dispinterface
    ['{7457EA48-E3B5-4E07-8496-87229ACA5E2D}']
    function  Ping: Integer; dispid 1;
    procedure DisconnectClient(const aMessage: WideString; RemainDisconnected: Integer); dispid 2;
  end;






  CoBoldPropagator = class
    class function Create: IBoldClientHandler;
    class function CreateRemote(const MachineName: string): IBoldClientHandler;
  end;






  CoBoldListener = class
    class function Create: IBoldListener;
    class function CreateRemote(const MachineName: string): IBoldListener;
  end;

implementation

uses ComObj;

class function CoBoldPropagator.Create: IBoldClientHandler;
begin
  Result := CreateComObject(CLASS_BoldPropagator) as IBoldClientHandler;
end;

class function CoBoldPropagator.CreateRemote(const MachineName: string): IBoldClientHandler;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BoldPropagator) as IBoldClientHandler;
end;

class function CoBoldListener.Create: IBoldListener;
begin
  Result := CreateComObject(CLASS_BoldListener) as IBoldListener;
end;

class function CoBoldListener.CreateRemote(const MachineName: string): IBoldListener;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BoldListener) as IBoldListener;
end;

end.
