/* src/interfaces/ecpg/include/ecpg-pthread-win32.h */
/*
 * pthread support for ecpglib.  On Windows, this uses the partial pthread
 * emulation in src/port/pthread-win32.h, which is implemented in
 * src/interfaces/libpq/pthread-win32.c and also compiled into ecpglib.
 */
#ifndef _ECPG_PTHREAD_WIN32_H
#define _ECPG_PTHREAD_WIN32_H

#ifndef WIN32
#include <pthread.h>
#else
#include "pthread-win32.h"
#endif							/* WIN32 */

#endif							/* _ECPG_PTHREAD_WIN32_H */
