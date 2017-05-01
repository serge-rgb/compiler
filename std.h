
typedef void FILE;
extern FILE* stderr;

FILE* fopen( const char* filename, const char* mode );
size_t fread( void          *buffer, size_t size, size_t count,
              FILE          *stream );
int printf( const char *format, ... );
int fprintf( FILE* stream, const char *format, ... );
void* calloc( size_t num, size_t size );
void free( void* p );
void exit( int error_code );
