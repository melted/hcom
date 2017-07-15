#ifndef __POINTERSRC_H__
#define __POINTERSRC_H__

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__CYGWIN__)
#define __BASTARDIZED_WIN32__ 1
#endif

#if defined(__BASTARDIZED_WIN32__) || defined(__MSVC__)
#define SUPPORT_COM 1
#endif

#ifndef COM
#define COM 1
#endif

extern void   primNoFree(void* p);
extern void   primPointerCheck();
extern void   primFinalise(void* f, void* a);
extern void*  primAllocMemory(int size);
extern void   primFreeMemory(void* p);
extern void*  finalFreeMemory();
extern void*  finalFreeObject();

/* Versions using COM allocator */
#if defined(SUPPORT_COM)
extern void*  primCoAllocMemory(int size);
extern void   primCoFreeMemory(void* p);
extern void   primCoFreeBSTR(void* p);
extern void*  finalCoFreeBSTR();
extern void   primFreeBSTR( void* p );
#endif

#endif /* __POINTERSRC_H__ */
