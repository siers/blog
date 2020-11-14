---
title: Flushing stdin
---
I've often needed stdout of a program to be unbuffered and get pushed as fast as possible. I had been googling for "fflush(stdin)", which of course isn't the correct way to do it, because stdin is just a pipe and you can't flush something in, just out. The program on the other side of the pipe must flush.

Most of the times, it gets stuck because stdlib flushes the buffer after 8k of data can be sent at once. Other times, it flushes instantly, because upon `fprintf(f, "...");` it checks whether the fd contained within f [`isatty(fd)`](http://linux.die.net/man/3/isatty). I'm pretty sure of that, but I failed in tracing it in glibc's source.

I found a toolchain that has a tool that does just this! That toolchain is called ["expect"](http://expect.sourceforge.net/) and the tool -- [unbufffer](http://www.tcl.tk/man/expect5.31/unbuffer.1.html).

    $ unbuffer ./long-boring-job | tee /tmp/the-output

It suspends it in a [pty](https://en.wikipedia.org/wiki/Pseudo_terminal), so the libc is fooled into printing it on the fly.

The unbuffer's source file is also only 13 lines long, because it simply wraps some expect's functions and this has been mentioned in the manpage's BUGS section:

    BUGS
        The man page is longer than the program.

The other, cleaner option is using `stdbuf(1)`. Not exactly sure how it works under the hood as of right now, but `stdbuf -o0 your-long-job` should also make stdout unbuffered.
