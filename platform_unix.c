#include "errno.h"
#include "unistd.h"
#include "dirent.h"

#define PRI_size "li"

int raise(int sig);

#if defined(__linux__)
#define SIGINT 5
#define PLATFORM_FORMAT_I64 "ld"
#define PLATFORM_FORMAT_U64 "lu"
#elif defined(__MACH__)
#define PLATFORM_FORMAT_I64 "lld"
#define PLATFORM_FORMAT_U64 "llu"
#else
#error Uknown UNIX platform
asdfasd
#endif

extern int    snprintf( char * buffer, unsigned long bufsz, const char * format, ... );
extern int    vsnprintf( char * buffer, unsigned long bufsz, const char * format, va_list vlist );


#define PlatformAssert(expr) do { if (!(expr)) { \
                                                printf("Assertion failed: %s\n", #expr);\
                                                __builtin_trap(); } } while(0)

#define PlatformPrintString(...) snprintf(__VA_ARGS__)
#define PlatformBreak __asm ("int $3")

void handleErrno(int error);

ErrorCode
platformRunProcess(char** args, sz n_args, i32 expected_return) {
   ErrorCode ret = Fail;
   int pid = fork();

   char** args_null_terminated = calloc((1 + n_args), sizeof(char*));
   memcpy(args_null_terminated, args, n_args * sizeof(char*));

   int status = 0;
   if (pid == 0) {
      execve(args_null_terminated[0], args_null_terminated, NULL);
      // Execution should not reach this point.
      handleErrno(errno);
      exit(-1);
   }
   else {
      pid_t wait_pid  = waitpid(pid, &status, 0);
      if (wait_pid != pid) {
         fprintf(stderr, "Child process exit error\n");
         handleErrno(errno);
         ret = Fail;
      }
      else {
         if (WIFEXITED(status)) {
            int exit_status = WEXITSTATUS(status);
            if (exit_status == expected_return) {
               ret = Ok;
            }
         }
         else if (WIFSIGNALED(status)) {
            fprintf(stderr, "Child process terminated with signal %d\n", WTERMSIG(status));
            ret = Fail;
         }
         else if (WIFSTOPPED(status)) {
            fprintf(stderr, "Child process was stopped\n", WSTOPSIG(status));
            ret = Fail;
         }
      }

   }
   return ret;
}

ErrorCode
platformCompileAndLinkAsmFile(char* filename_without_extension) {
   ErrorCode ret = Ok;
   // Call nasm from here.
   char asm_file[PathMax] = {0};
   snprintf(asm_file, PathMax, "%s.asm", filename_without_extension);
   char obj_file[PathMax] = {0};
   snprintf(obj_file, PathMax, "%s.o", filename_without_extension);
   printf("Running nasm\n");
   // TODO: Look in PATH for binary.
   char* nasm_args[] = { "/usr/local/bin/nasm", "-Znasm_output", "-f", "macho64", asm_file };
   if (Ok == platformRunProcess(nasm_args, ArrayCount(nasm_args), 0)) {
      printf("Running ld\n");
      char* ld_args[] = { "/usr/bin/ld", "-arch", "x86_64", "-e", "_start", obj_file, "/usr/lib/libSystem.dylib", "-o", filename_without_extension };
      if (Ok == platformRunProcess(ld_args, ArrayCount(ld_args), 0)) {
      }
      else {
         fprintf(stderr, "ld failed\n");
      }
   } else {
       fprintf(stderr, "nasm failed\n");
   }
   return ret;
}

ErrorCode
platformListDirectory(char*** s_out_files, char* dirname, b32 (*filter)(char*)) {
   ErrorCode err = Ok;
   DIR* dir = opendir(dirname);
   if (!dir) {
      err = CouldNotOpenDir;
   }
   else {
      struct dirent* entry = NULL;
      while ((entry = readdir(dir))) {
         char* name = entry->d_name;
         if (name[0] != '.') {
            if (filter(name)) {
               char file[PathMax] = Zero; {
                  strncat(file, dirname, PathMax);
                  strncat(file, "/", PathMax);
                  strncat(file, name, PathMax);
               }

               bufPush(*s_out_files, getString(file));
            }
         }
      }
      closedir(dir);
   }
   return err;
}

char*
platformOutputBinaryFilename(Arena* arena, char* fname_without_extension) {
   return fname_without_extension;
}


void
handleErrno(int error) {
   const char* str = NULL;
   switch ( error ) {
      case E2BIG:           str = "Argument list too long (POSIX.1)"; break;
      case EACCES:          str = "Permission denied (POSIX.1)"; break;
      case EADDRINUSE:      str = "Address already in use (POSIX.1)"; break;
      case EADDRNOTAVAIL:   str = "Address not available (POSIX.1)"; break;
      case EAFNOSUPPORT:    str = "Address family not supported (POSIX.1)"; break;
      case EAGAIN:          str = "Resource temporarily unavailable (may be the same value as EWOULDBLOCK) (POSIX.1)"; break;
      case EALREADY:        str = "Connection already in progress (POSIX.1)"; break;
           // case EBADE:           str = "Invalid exchange"; break;
      case EBADF:           str = "Bad file descriptor (POSIX.1)"; break;
           // case EBADFD:          str = "File descriptor in bad state"; break;
      case EBADMSG:         str = "Bad message (POSIX.1)"; break;
           // case EBADR:           str = "Invalid request descriptor"; break;
           // case EBADRQC:         str = "Invalid request code"; break;
           // case EBADSLT:         str = "Invalid slot"; break;
      case EBUSY:           str = "Device or resource busy (POSIX.1)"; break;
      case ECANCELED:       str = "Operation canceled (POSIX.1)"; break;
      case ECHILD:          str = "No child processes (POSIX.1)"; break;
           // case ECHRNG:          str = "Channel number out of range"; break;
           // case ECOMM:           str = "Communication error on send"; break;
      case ECONNABORTED:    str = "Connection aborted (POSIX.1)"; break;
      case ECONNREFUSED:    str = "Connection refused (POSIX.1)"; break;
      case ECONNRESET:      str = "Connection reset (POSIX.1)"; break;
      case EDEADLK:         str = "Resource deadlock avoided (POSIX.1)"; break;
          // case EDEADLOCK:       str = "Synonym for EDEADLK"; break;
      case EDESTADDRREQ:    str = "Destination address required (POSIX.1)"; break;
      case EDOM:            str = "Mathematics argument out of domain of function (POSIX.1, C99)"; break;
          // case EDQUOT:          str = "Disk quota exceeded (POSIX.1)"; break;
      case EEXIST:          str = "File exists (POSIX.1)"; break;
      case EFAULT:          str = "Bad address (POSIX.1)"; break;
      case EFBIG:           str = "File too large (POSIX.1)"; break;
          // case EHOSTDOWN:       str = "Host is down"; break;
      case EHOSTUNREACH:    str = "Host is unreachable (POSIX.1)"; break;
      case EIDRM:           str = "Identifier removed (POSIX.1)"; break;
      case EILSEQ:          str = "Illegal byte sequence (POSIX.1, C99)"; break;
      case EINPROGRESS:     str = "Operation in progress (POSIX.1)"; break;
      case EINTR:           str = "Interrupted function call (POSIX.1); see signal(7)."; break;
      case EINVAL:          str = "Invalid argument (POSIX.1)"; break;
      case EIO:             str = "Input/output error (POSIX.1)"; break;
      case EISCONN:         str = "Socket is connected (POSIX.1)"; break;
      case EISDIR:          str = "Is a directory (POSIX.1)"; break;
          // case EISNAM:          str = "Is a named type file"; break;
          // case EKEYEXPIRED:     str = "Key has expired"; break;
          // case EKEYREJECTED:    str = "Key was rejected by service"; break;
          // case EKEYREVOKED:     str = "Key has been revoked"; break;
          // case EL2HLT:          str = "Level 2 halted"; break;
          // case EL2NSYNC:        str = "Level 2 not synchronized"; break;
          // case EL3HLT:          str = "Level 3 halted"; break;
          // case EL3RST:          str = "Level 3 halted"; break;
          // case ELIBACC:         str = "Cannot access a needed shared library"; break;
          // case ELIBBAD:         str = "Accessing a corrupted shared library"; break;
          // case ELIBMAX:         str = "Attempting to link in too many shared libraries"; break;
          // case ELIBSCN:         str = "lib section in a.out corrupted"; break;
          // case ELIBEXEC:        str = "Cannot exec a shared library directly"; break;
      case ELOOP:           str = "Too many levels of symbolic links (POSIX.1)"; break;
          // case EMEDIUMTYPE:     str = "Wrong medium type"; break;
      case EMFILE:          str = "Too many open files (POSIX.1); commonly caused by exceeding the RLIMIT_NOFILE resource limit described in getrlimit(2)"; break;
      case EMLINK:          str = "Too many links (POSIX.1)"; break;
      case EMSGSIZE:        str = "Message too long (POSIX.1)"; break;
          // case EMULTIHOP:       str = "Multihop attempted (POSIX.1)"; break;
      case ENAMETOOLONG:    str = "Filename too long (POSIX.1)"; break;
      case ENETDOWN:        str = "Network is down (POSIX.1)"; break;
      case ENETRESET:       str = "Connection aborted by network (POSIX.1)"; break;
      case ENETUNREACH:     str = "Network unreachable (POSIX.1)"; break;
      case ENFILE:          str = "Too many open files in system (POSIX.1); on Linux, this is probably a result of encountering the /proc/sys/fs/file-max limit (see proc(5))."; break;
      case ENOBUFS:         str = "No buffer space available (POSIX.1 (XSI STREAMS option))"; break;
      case ENODATA:         str = "No message is available on the STREAM head read queue (POSIX.1)"; break;
      case ENODEV:          str = "No such device (POSIX.1)"; break;
      case ENOENT:          str = "No such file or directory (POSIX.1)"; break;
                          // Typically, this error results when a specified
                          // pathname does not exist, or one of the components in
                          // the directory prefix of a pathname does not exist, or
                          // the specified pathname is a dangling symbolic link.
      case ENOEXEC:         str = "Exec format error (POSIX.1)"; break;
          // case ENOKEY:          str = "Required key not available"; break;
      case ENOLCK:          str = "No locks available (POSIX.1)"; break;
      case ENOLINK:         str = "Link has been severed (POSIX.1)"; break;
          // case ENOMEDIUM:       str = "No medium found"; break;
      case ENOMEM:          str = "Not enough space (POSIX.1)"; break;
      case ENOMSG:          str = "No message of the desired type (POSIX.1)"; break;
          // case ENONET:          str = "Machine is not on the network"; break;
          // case ENOPKG:          str = "Package not installed"; break;
      case ENOPROTOOPT:     str = "Protocol not available (POSIX.1)"; break;
      case ENOSPC:          str = "No space left on device (POSIX.1)"; break;
      case ENOSR:           str = "No STREAM resources (POSIX.1 (XSI STREAMS option))"; break;
      case ENOSTR:          str = "Not a STREAM (POSIX.1 (XSI STREAMS option))"; break;
      case ENOSYS:          str = "Function not implemented (POSIX.1)"; break;
          // case ENOTBLK:         str = "Block device required"; break;
      case ENOTCONN:        str = "The socket is not connected (POSIX.1)"; break;
      case ENOTDIR:         str = "Not a directory (POSIX.1)"; break;
      case ENOTEMPTY:       str = "Directory not empty (POSIX.1)"; break;
      case ENOTSOCK:        str = "Not a socket (POSIX.1)"; break;
      case ENOTSUP:         str = "Operation not supported (POSIX.1)"; break;
      case ENOTTY:          str = "Inappropriate I/O control operation (POSIX.1)"; break;
          // case ENOTUNIQ:        str = "Name not unique on network"; break;
      case ENXIO:           str = "No such device or address (POSIX.1)"; break;
          // case EOPNOTSUPP:      str = "Operation not supported on socket (POSIX.1)"; break;
                          // (ENOTSUP and EOPNOTSUPP have the same value on Linux,
                          // but according to POSIX.1 these error values should be
                          // distinct.)
      case EOVERFLOW:       str = "Value too large to be stored in data type (POSIX.1)"; break;
      case EPERM:           str = "Operation not permitted (POSIX.1)"; break;
          // case EPFNOSUPPORT:    str = "Protocol family not supported"; break;
      case EPIPE:           str = "Broken pipe (POSIX.1)"; break;
      case EPROTO:          str = "Protocol error (POSIX.1)"; break;
      case EPROTONOSUPPORT: str = "Protocol not supported (POSIX.1)"; break;
      case EPROTOTYPE:      str = "Protocol wrong type for socket (POSIX.1)"; break;
      case ERANGE:          str = "Result too large (POSIX.1, C99)"; break;
          // case EREMCHG:         str = "Remote address changed"; break;
          // case EREMOTE:         str = "Object is remote"; break;
          // case EREMOTEIO:       str = "Remote I/O error"; break;
          // case ERESTART:        str = "Interrupted system call should be restarted"; break;
      case EROFS:           str = "Read-only filesystem (POSIX.1)"; break;
          // case ESHUTDOWN:       str = "Cannot send after transport endpoint shutdown"; break;
      case ESPIPE:          str = "Invalid seek (POSIX.1)"; break;
          // case ESOCKTNOSUPPORT: str = "Socket type not supported"; break;
      case ESRCH:           str = "No such process (POSIX.1)"; break;
          // case ESTALE:          str = "Stale file handle (POSIX.1)"; break;
                          // This error can occur for NFS and for other
                          // filesystems
          // case ESTRPIPE:        str = "Streams pipe error"; break;
      case ETIME:           str = "Timer expired (POSIX.1 (XSI STREAMS option))"; break;
                          // (POSIX.1 says "STREAM ioctl(2) timeout")
      case ETIMEDOUT:       str = "Connection timed out (POSIX.1)"; break;
      case ETXTBSY:         str = "Text file busy (POSIX.1)"; break;
          // case EUCLEAN:         str = "Structure needs cleaning"; break;
          // case EUNATCH:         str = "Protocol driver not attached"; break;
          // case EUSERS:          str = "Too many users"; break;
          // case EWOULDBLOCK:     str = "Operation would block (may be same value as EAGAIN) (POSIX.1)"; break;
      case EXDEV:           str = "Improper link (POSIX.1)"; break;
          // case EXFULL:          str = "Exchange full"; break;
   }
  if ( str ) {
    fprintf(stderr, "Errno is set to \"%s\"\n", str);
 }
}
