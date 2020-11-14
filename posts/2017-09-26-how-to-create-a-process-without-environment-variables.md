---
title: How to create a process without environment variables
---
This is how you may create a process with no initial environment variables.

    % cat env-less-bash.c
        #include <unistd.h>

        int
        main(int argc __attribute__((unused)), char** argv)
        {
            char* envp[] = {0};
            return execve("/usr/bin/bash", argv, envp);
        }

    % gcc -Wall -Wextra -ggdb -o env-less-bash env-less-bash.c

    % env | wc -l
        50

    % ./env-less-bash
        [raitis@hostname l]$ env
            PWD=/home/s/code/c/l
            SHLVL=1
            _=/usr/bin/env
