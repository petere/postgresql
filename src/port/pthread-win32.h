/*
 * src/port/pthread-win32.h
 *
 * Partial pthread emulation for Windows, used by libpq and ecpglib.  The
 * implementation is in src/interfaces/libpq/pthread-win32.c.
 */
#ifndef __PTHREAD_H
#define __PTHREAD_H

typedef ULONG pthread_key_t;

typedef SRWLOCK pthread_mutex_t;

#define PTHREAD_MUTEX_INITIALIZER	SRWLOCK_INIT

typedef INIT_ONCE pthread_once_t;

#define PTHREAD_ONCE_INIT	INIT_ONCE_STATIC_INIT

DWORD		pthread_self(void);

int			pthread_key_create(pthread_key_t *key, void (*destructor) (void *));
int			pthread_setspecific(pthread_key_t key, const void *value);
void	   *pthread_getspecific(pthread_key_t key);

int			pthread_mutex_init(pthread_mutex_t *, void *attr);
int			pthread_mutex_lock(pthread_mutex_t *);

/* blocking */
int			pthread_mutex_unlock(pthread_mutex_t *);

int			pthread_once(pthread_once_t *once, void (*init_routine) (void));

#endif
