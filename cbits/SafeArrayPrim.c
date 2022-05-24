
#include "SafeArrayPrim.h"
#include "autoPrim.h"

/* Finaliser for SAFEARRAYs */
void primSafeArrayDestroy(void *p)
{
    SafeArrayDestroy((SAFEARRAY *)p);
}
