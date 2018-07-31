#pragma warning(push, 0)
#include <Windows.h>
#pragma warning(pop)

#define PlatformDefaultTarget Config_TARGET_WIN

#define PlatformAssert(expr) do { if (!(expr)) { MessageBox(0, \
                                                            "Assertion failed: " # expr, \
                                                            "Assertion",  \
                                                            MB_OK ); \
                                                 __debugbreak(); } }  while(0)
#define PlatformPrintString(...) sprintf_s(__VA_ARGS__)

#define PlatformBreak __debugbreak()

