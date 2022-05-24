/*
 * Entry points needed to implement a COM inproc server.
 * The C wrappers defined here delegate to (library) methods
 * implemented in Haskell.
 *
 * Sigbjorn Finne, 1999
 */

#include "comPrim.h"
#include <windows.h>

/*
 * A Haskell in-proc server exposes a VTBL which mimics
 * the entry points that a self-registering inproc server
 * has to supply to the outside world.
 *
 */
typedef struct IComDll
{
    void (*dllUnload)();
    HRESULT (*dllCanUnloadNow)();
    HRESULT (*dllRegisterServer)();
    HRESULT (*dllUnregisterServer)();
    HRESULT (*dllGetClassObject)(CLSID *clsid, IID *iid, void **ppv);
} ComDll;

ComDll *comDll = NULL;

extern ComDll *newComDll(HANDLE hMod);

#if __GLASGOW_HASKELL__ >= 408
extern void startupHaskell(int argc, char *argv[], void *rootMod);
extern void *__stginit_ComDllMain;
extern void shutdownHaskell(void);
#else
extern void startupHaskell(int, char **);
extern void shutdownHaskell(void);
#endif

static char *args[] = {"ghcDll"};

BOOL STDCALL DllMain(HANDLE hModule, DWORD reason, void *reserved)
{
    if (reason == DLL_PROCESS_ATTACH)
    {
        /* By now, the RTS DLL should have been hoisted in, but we need to start
           it up.

       Note: for ghc-4.08 and later, you need to give the main / 'root module'
       of the Haskell module you want to start running. So, if this is something
       other than 'ComDllMain', you'll need to tweak the invocation below.
        */
#if __GLASGOW_HASKELL__ >= 408
        startupHaskell(sizeof(args) / sizeof(char *), args, &__stginit_ComDllMain);
#else
        startupHaskell(sizeof(args) / sizeof(char *), args);
#endif
        comDll = newComDll(hModule);
        return TRUE;
    }
    else
    {
        if (comDll && reason == DLL_PROCESS_DETACH)
        {
            (comDll)->dllUnload();
            shutdownHaskell();
            /* Not properly letting go of memory here is rude, but we're shutting down.. */
            comDll = NULL;
        }
        return TRUE;
    }
}

HRESULT
STDCALL
DllCanUnloadNow(void)
{
    if (comDll)
    {
        return (comDll)->dllCanUnloadNow();
    }
    else
    {
        return S_OK;
    }
}

HRESULT
STDCALL
DllRegisterServer(void)
{
    if (comDll)
    {
        return (comDll)->dllRegisterServer();
    }
    else
    {
        return E_FAIL;
    }
}

HRESULT
STDCALL
DllUnregisterServer(void)
{
    if (comDll)
    {
        return (comDll)->dllUnregisterServer();
    }
    else
    {
        return E_FAIL;
    }
}

HRESULT
STDCALL
DllGetClassObject(CLSID *rclsid, IID *riid, void **ppv)
{
    HRESULT hr;
    if (comDll)
    {
        hr = (comDll)->dllGetClassObject(rclsid, riid, ppv);
        return S_OK;
    }
    else
    {
        return E_FAIL;
    }
}
