// This file declares all 


typedef void FILE;
#if defined(_WIN32)
extern FILE* __acrt_iob_func(int idx);
    #define stdin  (__acrt_iob_func(0))

 #define stdout (__acrt_iob_func(1))
    #define stderr (__acrt_iob_func(2))
#else
    extern FILE* stderr;
#endif

#if defined(_WIN32)
#define PRI_size "zi"
#else
#define PRI_size "li"
#endif

extern void* memcpy( void *dest, const void *src, size_t count );

FILE* fopen( const char* filename, const char* mode );
size_t fread( void          *buffer, size_t size, size_t count,
              FILE          *stream );
int printf( const char *format, ... );
int fprintf( FILE* stream, const char *format, ... );
void* calloc( size_t num, size_t size );
void free( void* p );
void exit( int error_code );
