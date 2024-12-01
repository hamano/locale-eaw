#include <stdio.h>
#include <assert.h>
#include <dlfcn.h>
#include <locale.h>
#include <unistd.h>
#include <string.h>

#define DEBUG 0

// setlocale(LC_CTYPE, "<任意のロケール>")を無視する
// ただし、
// setlocale(LC_CTYPE, "")は実行する。
char *setlocale(int category, const char *locale) {
#if DEBUG
    FILE *fp = fopen("fixtmux.log", "a");
    fprintf(fp, "[%d]call setlocale: %d %s\n", getpid(), category, locale);
    fclose(fp);
#endif
    char* (*func)(int, const char *) = dlsym(RTLD_NEXT, "setlocale");
    assert(func);
    char* rc = NULL;
    if (category == LC_CTYPE) {
        if (locale == NULL || !strcmp(locale, "")) {
            rc = func(category, locale);
        }
    } else {
        rc = func(category, locale);
    }
    return rc;
}
