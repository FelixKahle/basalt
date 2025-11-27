// Copyright (c) 2025 Felix Kahle.
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#if defined(__GNUC__) && defined(__linux__)
#include <unistd.h>
#include <stdio.h>
#endif
#if defined(__APPLE__) && defined(__GNUC__)  // MacOS
#include <mach/mach_init.h>
#include <mach/task.h>
#elif (defined(__FreeBSD__) || defined(__NetBSD__) || \
defined(__OpenBSD__))  // [Free,Net,Open]BSD
#include <sys/resource.h>
#include <sys/time.h>
// Windows
#elif defined(_MSC_VER) || defined(__MINGW32__) || defined(__MINGW64__)
#include <windows.h>
#include <psapi.h>
#endif

#include "basalt/base/system_info.h"

namespace bslt::system_info
{
#if defined(__APPLE__) && defined(__GNUC__)  // MacOS
    int64_t GetProcessMemoryUsage()
    {
        task_basic_info t_info;
        mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;

        if (KERN_SUCCESS != task_info(mach_task_self(), TASK_BASIC_INFO, reinterpret_cast<task_info_t>(&t_info),
                                      &t_info_count))
        {
            return -1;
        }
        return static_cast<int64_t>(t_info.resident_size);
    }

#elif defined(__GNUC__) && !defined(__FreeBSD__) && !defined(__NetBSD__) && \
!defined(__OpenBSD__) && !defined(__EMSCRIPTEN__) && \
!defined(_WIN32)  // Linux
    int64_t GetProcessMemoryUsage()
    {
        long rss = 0;
        char buf[64];
        snprintf(buf, sizeof(buf), "/proc/%u/statm", (unsigned)getpid());

        FILE* const pf = fopen(buf, "r");
        if (pf)
        {
            // The second value in statm is the RSS (Resident Set Size) in pages.
            // The first value is total program size (Virtual Memory).
            long total_program_size = 0;
            if (fscanf(pf, "%ld %ld", &total_program_size, &rss) != 2)
            {
                rss = 0; // Failed to read
            }
            fclose(pf);
        }

        // sysconf(_SC_PAGESIZE) returns the page size in bytes (usually 4096).
        return static_cast<int64_t>(rss) * sysconf(_SC_PAGESIZE);
    }

#elif (defined(__FreeBSD__) || defined(__NetBSD__) || \
defined(__OpenBSD__))  // [Free,Net,Open]BSD
    int64_t GetProcessMemoryUsage()
    {
        struct rusage rusage;
        getrusage(RUSAGE_SELF, &rusage);
        // BSD stores ru_maxrss in Kilobytes.
        return static_cast<int64_t>(rusage.ru_maxrss) * 1024;
    }

#elif defined(_MSC_VER) || defined(__MINGW32__) || \
defined(__MINGW64__)  // Windows
    int64_t GetProcessMemoryUsage()
    {
        PROCESS_MEMORY_COUNTERS pmc;
        // OpenProcess might fail if we don't have permissions, though unlikely for our own process.
        HANDLE hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());

        if (!hProcess)
        {
            return 0;
        }

        int64_t memory = 0;
        if (GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc)))
        {
            memory = static_cast<int64_t>(pmc.WorkingSetSize);
        }

        CloseHandle(hProcess);
        return memory;
    }

#else // Unknown, returning 0.
    int64_t GetProcessMemoryUsage() { return 0; }
#endif
}
