// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ComServ.pas' rev: 5.00

#ifndef ComServHPP
#define ComServHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ComObj.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Comserv
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TStartMode { smStandalone, smAutomation, smRegServer, smUnregServer };
#pragma option pop

typedef void __fastcall (__closure *TLastReleaseEvent)(bool &Shutdown);

class DELPHICLASS TComServer;
class PASCALIMPLEMENTATION TComServer : public Comobj::TComServerObject 
{
	typedef Comobj::TComServerObject inherited;
	
private:
	int FObjectCount;
	int FFactoryCount;
	_di_ITypeLib FTypeLib;
	AnsiString FServerName;
	AnsiString FHelpFileName;
	bool FIsInprocServer;
	TStartMode FStartMode;
	bool FStartSuspended;
	bool FRegister;
	bool FUIInteractive;
	TLastReleaseEvent FOnLastRelease;
	void __fastcall FactoryFree(TComObjectFactory* Factory);
	void __fastcall FactoryRegisterClassObject(TComObjectFactory* Factory);
	void __fastcall FactoryUpdateRegistry(TComObjectFactory* Factory);
	void __fastcall LastReleased(void);
	
protected:
	virtual int __fastcall CountObject(bool Created);
	virtual int __fastcall CountFactory(bool Created);
	virtual AnsiString __fastcall GetHelpFileName();
	virtual AnsiString __fastcall GetServerFileName();
	virtual AnsiString __fastcall GetServerKey();
	virtual AnsiString __fastcall GetServerName();
	virtual bool __fastcall GetStartSuspended(void);
	virtual _di_ITypeLib __fastcall GetTypeLib();
	virtual void __fastcall SetHelpFileName(const AnsiString Value);
	
public:
	__fastcall TComServer(void);
	__fastcall virtual ~TComServer(void);
	void __fastcall Initialize(void);
	void __fastcall LoadTypeLib(void);
	void __fastcall SetServerName(const AnsiString Name);
	void __fastcall UpdateRegistry(bool Register);
	__property bool IsInprocServer = {read=FIsInprocServer, write=FIsInprocServer, nodefault};
	__property int ObjectCount = {read=FObjectCount, nodefault};
	__property TStartMode StartMode = {read=FStartMode, nodefault};
	__property bool UIInteractive = {read=FUIInteractive, write=FUIInteractive, nodefault};
	__property TLastReleaseEvent OnLastRelease = {read=FOnLastRelease, write=FOnLastRelease};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TComServer* ComServer;
extern PACKAGE HRESULT __stdcall DllGetClassObject(const GUID &CLSID, const GUID &IID, void *Obj);
extern PACKAGE HRESULT __stdcall DllCanUnloadNow(void);
extern PACKAGE HRESULT __stdcall DllRegisterServer(void);
extern PACKAGE HRESULT __stdcall DllUnregisterServer(void);

}	/* namespace Comserv */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Comserv;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ComServ
