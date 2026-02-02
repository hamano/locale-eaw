/*
 * fixtui-proxy - TTY proxy to replace Unicode characters in terminal output.
 *
 * Creates a PTY pair, runs the target application on the slave side,
 * reads output from the master side, performs character replacements,
 * and writes to the real terminal.
 *
 * Usage:
 *   gcc -O2 -o fixtui-proxy fixtui-proxy.c
 *   ./fixtui-proxy <command> [args...]
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pty.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

/*
 * Replacement table: pairs of {"before", "after"} UTF-8 strings.
 * Before and after MUST have the same byte length.
 */
static const struct {
    const char *before;
    const char *after;
} table[] = {
    {"●", "⬤"}, /* U+25CF BLACK CIRCLE -> U+2B24 BLACK LARGE CIRCLE */
};

#define TABLE_SIZE (sizeof(table) / sizeof(table[0]))

static volatile sig_atomic_t child_exited = 0;
static volatile sig_atomic_t got_sigwinch = 0;
static int master_fd = -1;

static void sigchld_handler(int sig)
{
    (void)sig;
    child_exited = 1;
}

static void sigwinch_handler(int sig)
{
    (void)sig;
    got_sigwinch = 1;
}

static void replace_buf(unsigned char *buf, size_t count)
{
    for (size_t t = 0; t < TABLE_SIZE; t++) {
        const unsigned char *b = (const unsigned char *)table[t].before;
        const unsigned char *a = (const unsigned char *)table[t].after;
        size_t len = strlen(table[t].before);

        size_t i = 0;
        while (i + len <= count) {
            if (memcmp(buf + i, b, len) == 0) {
                memcpy(buf + i, a, len);
                i += len;
            } else {
                i++;
            }
        }
    }
}

static void forward_winsize(int src_fd, int dst_fd)
{
    struct winsize ws;
    if (ioctl(src_fd, TIOCGWINSZ, &ws) == 0)
        ioctl(dst_fd, TIOCSWINSZ, &ws);
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <command> [args...]\n", argv[0]);
        return 1;
    }

    /* Save original terminal settings */
    struct termios orig_termios;
    int is_tty = isatty(STDIN_FILENO);
    if (is_tty)
        tcgetattr(STDIN_FILENO, &orig_termios);

    /* Create PTY pair */
    struct winsize ws = {.ws_row = 24, .ws_col = 80};
    if (is_tty)
        ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);

    pid_t pid = forkpty(&master_fd, NULL, is_tty ? &orig_termios : NULL, &ws);
    if (pid < 0) {
        perror("forkpty");
        return 1;
    }

    if (pid == 0) {
        /* Child: exec the target command */
        execvp(argv[1], &argv[1]);
        perror("execvp");
        _exit(127);
    }

    /* Parent: set up signal handlers */
    struct sigaction sa;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);

    sa.sa_handler = sigchld_handler;
    sigaction(SIGCHLD, &sa, NULL);

    sa.sa_handler = sigwinch_handler;
    sigaction(SIGWINCH, &sa, NULL);

    /* Set terminal to raw mode */
    if (is_tty) {
        struct termios raw = orig_termios;
        cfmakeraw(&raw);
        tcsetattr(STDIN_FILENO, TCSANOW, &raw);
    }

    /* Main loop: proxy between stdin/stdout and PTY master */
    unsigned char buf[4096];
    struct pollfd fds[2];
    fds[0].fd = STDIN_FILENO;
    fds[0].events = POLLIN;
    fds[1].fd = master_fd;
    fds[1].events = POLLIN;

    while (!child_exited) {
        if (got_sigwinch) {
            got_sigwinch = 0;
            forward_winsize(STDIN_FILENO, master_fd);
        }

        int ret = poll(fds, 2, 100);
        if (ret < 0) {
            if (errno == EINTR)
                continue;
            break;
        }

        /* stdin -> PTY master (keyboard input) */
        if (fds[0].revents & POLLIN) {
            ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
            if (n > 0)
                write(master_fd, buf, n);
            else if (n == 0)
                break;
        }

        /* PTY master -> stdout (application output, with replacements) */
        if (fds[1].revents & POLLIN) {
            ssize_t n = read(master_fd, buf, sizeof(buf));
            if (n > 0) {
                replace_buf(buf, n);
                write(STDOUT_FILENO, buf, n);
            } else if (n == 0) {
                break;
            }
        }

        if (fds[1].revents & (POLLHUP | POLLERR))
            break;
    }

    /* Drain remaining output */
    for (;;) {
        ssize_t n = read(master_fd, buf, sizeof(buf));
        if (n <= 0)
            break;
        replace_buf(buf, n);
        write(STDOUT_FILENO, buf, n);
    }

    /* Restore terminal */
    if (is_tty)
        tcsetattr(STDIN_FILENO, TCSANOW, &orig_termios);

    /* Get child exit status */
    int status = 0;
    waitpid(pid, &status, 0);
    if (WIFEXITED(status))
        return WEXITSTATUS(status);
    return 1;
}
