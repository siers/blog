---
title: Dumping incorrect password entries from i3lock
---
For some reason I couldn't type the right password into `i3lock`. I always could in terminals though.
To check what `i3lock` thinks I'm writing, I wrote a patch to dump the passwords.
I also registered it in the package manager with AUR and made the `PKGBUILD` include the patch in the build,
though that's optional and it was for fun.

First, I downloaded the package itself(it's actually a fork of i3lock).

    % packer -G i3lock-lixxia-git
    % cd i3lock-lixxia-git

Then I opened up `PKGBUILD` in vim and added a line to include my patch,
which I made with `git format-patch`:

    % diff PKGBUILD /tmp/i3lock-lixxia-git/PKGBUILD
    27d26
    <   git apply ../../dump-pw.patch

Then `makepkg -i`nstalled it and concluded that `i3lock` didn't register that I pressed enter.
Weird… Anyway, here's the patchfile:

    % cat dump-pw.patch
    From 5f4becd40ff19b5d3c80490b2a4d2e0566ed4690 Mon Sep 17 00:00:00 2001
    Date: Tue, 13 Sep 2016 11:00:06 +0300
    Subject: [PATCH] d

    ---
     i3lock.c | 10 ++++++++++
     1 file changed, 10 insertions(+)

    diff --git a/i3lock.c b/i3lock.c
    index 66a1c4c..b7e558a 100644
    --- a/i3lock.c
    +++ b/i3lock.c
    @@ -1,3 +1,7 @@
    +#include <sys/types.h>
    +#include <sys/stat.h>
    +#include <fcntl.h>
    +
     /*
      * vim:ts=4:sw=4:expandtab
      *
    @@ -251,6 +255,12 @@ static void input_done(void) {

         pam_state = STATE_PAM_WRONG;
         failed_attempts += 1;
    +
    +    int fd = open("/tmp/sw", O_RDWR | O_APPEND | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    +    write(fd, password, strnlen(password, 512));
    +    write(fd, "\n", 1);
    +    close(fd);
    +
         clear_input();
         redraw_screen();

    --
    2.9.3

