/* src/include/port/cygwin.h */

#ifdef BUILDING_DLL
#define PGDLLIMPORT __declspec (dllexport)
#else
#define PGDLLIMPORT __declspec (dllimport)
#endif

#define PGDLLEXPORT
