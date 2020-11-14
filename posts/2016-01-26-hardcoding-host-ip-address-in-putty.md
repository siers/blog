---
title: Hardcoding host's IP address in PuTTY for windows
---
After having `putty.exe` downloaded onto some windows machine from my http server,
I decided I'd hack and patch its source a little to hardcode
my host's IP address into the "Host" field, just so I could be a teensy bit lazier. In the *second* part of this post, I remove the "hardcoded" from the equation.

In case you'ven't done that yet, download putty's source repo:

    git clone git://git.tartarus.org/simon/putty.git

Hardcoding works by providing `-DHARDCODED_ADDRESS` to the `make` through `XFLAGS`, which, in turn, is used
in `windows/window.c`'s `WinMain` procedure right after the command line argument handling.

    diff --git a/windows/window.c b/windows/window.c
    index fc51e57..77d24ee 100644
    --- a/windows/window.c
    +++ b/windows/window.c
    @@ -596,6 +596,10 @@ int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmdline, int show)
            }
        }

    +#ifdef HARDCODED_ADDRESS
    +    conf_set_str(conf, CONF_host, HARDCODED_ADDRESS);
    +#endif
    +
        cmdline_run_saved(conf);

        if (loaded_session || got_host)

To patch it,  
run `git apply`  
copy the patch above into the terminal while the command is waiting,  
press `enter`, `control + d`  
or use any other equivalent mechanism
and you should see this upon issuing `git status`.

    % git status
    On branch master
    Your branch is up-to-date with 'origin/master'.
    Changes not staged for commit:
    (use "git add <file>..." to update what will be committed)
    (use "git checkout -- <file>..." to discard changes in working directory)

            modified:   window.c

    no changes added to commit (use "git add" and/or "git commit -a")

As for building it, I consulted [blog.myconan.net/posts/3311](https://blog.myconan.net/posts/3311).
My build is cron-scheduled[^notanymore] and pay attention to the interesting bits in lines 1[^ip] and 8:

     1  externalipaddr() { dig +short myip.opendns.com @resolver1.opendns.com }
     2  perl mkfiles.pl
     3  cd windows
     4  sed -ri 's/-mno-cygwin//g' Makefile.cyg
     5  make \
     6     VER="-DSNAPSHOT=$(date '+%Y-%m-%d') -DSVN_REV='$(svnversion)' -DMODIFIED" \
     7     TOOLPATH=i486-mingw32- \
     8     XFLAGS="-DCOVERITY -DHARDCODED_ADDRESS=\"\\\"$(externalipaddr)\\\"\"" \
     9     -f Makefile.cyg \
    10     putty.exe

If you winced because of the slashes and quotes of `line 8`, let me assure you that some of them
are really needed. The `-D` passess the variable to C as a macro, which means that
it must have \" \" quotes to make C think it's a string. But you also need them to prevent bash
from splitting XFLAGS into multiple arguments.

If this doesn't work *or* you're very glad it does, feel free to try to contact me.

## Changing the host field to HTTP's Host value *<small>[2016-09-14]</small>*

Since the whole reason of doing this was to offer it for downloading from HTTP,
we can use it to make this work without any *hardcodes*.
The way it works for me is also a little complicated, but not too much if you're familiar with SSH.

We must then make the HTTP server, in my case lighttpd,
treat `putty.exe` as a script because otherwise it wouldn't change upon every changing host.
For me, the possible values are local, external adresses and `/etc/hosts` domains.

Now to do that, add this to `/etc/lighttpd/lighttpd.conf`

    cgi.assign = (
        # .. possibly something had there already
        "putty.exe"  => "/usr/bin/ruby"
    )

Now for the `putty.exe`, I use:

    CGI.new.out('Content-type': 'application/x-msdownload') do
      host = ENV['HTTP_HOST'].scan(/[a-zA-Z\.0-9]+/).join[0..127]
      %x{ssh 127.0.0.1 <<< "putty #{ host }"}
      File.read("#{ ENV['HOME'] }/code/cache/putty/windows/putty.exe")
    end

For security reasons, I use only a safe portion of the `HTTP_HOST` that CGI passed onto us.
I also didn't actually let the HTTP server build it —
I let it make a whitelisted call to my regular server user.
That works via `/etc/ssh/sshd_config`, `~http/.ssh/config`:

    % grep 'Match User raitis Host 127.0' /etc/ssh/sshd_config
    Match User raitis Host 127.0.0.1
        AuthorizedKeysFile ~raitis/.ssh/remote_authorized_keys
        ForceCommand ~raitis/code/desktop/remote/shell

    % cat .ssh/config
    Ciphers aes128-ctr,aes192-ctr,aes256-ctr,aes128-cbc,3des-cbc
    MACs hmac-md5,hmac-sha1,hmac-ripemd160
    PubKeyAcceptedKeyTypes ssh-rsa

    Host 127.0.0.1
        User raitis
        IdentityFile ~/.ssh/remote

That's the gist of it, but you've also to write your own shell
that'll correctly start the build process whilst taking the
`putty HOSTHERE`(from putty.exe CGI script) on the stdin into account.

[^notanymore]: Not any more since I've added the second part to this post.
[^ip]: [unix.stackexchange.com/../how-can-i-get-my-external-ip-address-in-bash](http://unix.stackexchange.com/questions/22615/..-my-external-ip-address-in-bash)
