---
title: What software do my computers run?
---
*This post is going to be about what software I run on my laptops,
how that's changed over time and how they synchronize. (This has omitted the bits about backing up.)*

## Prelude

I used to run Arch Linux, which was my first real distro after ubuntu.
It was always a pain to set up on new machines and reinstalling a manually modified
the package list from `pacman -Q` and moving around `/etc` files is no longer a my cup of tea.

I think it was Rob Pike whom I remember having written or said something about
treating his computers as just terminals with a cache (or something to that effect,
as I remember it anyway) and thus making changing them easy,
which he said he's been doing often.

While I'm not quite there and the meaning of those statements is debatable,
I've gotten closer now.

<center>❦</center>

## NixOS

I have two personal machines now, both currently run NixOS and syncthing,
who together give me a whole lot of control over both machines.

NixOS lets you create an OS from only the configuration files you give it,
so if you change a system file or add another package or do `$THING` to the OS,
you *must* do that through the configuration files, which means your OS's become
a whole lot more reproducible and your machine's no longer special.

For the record, here are mine: [siers/dotfiles](https://github.com/siers/dotfiles/)(see the `nixos/` tree).

## Syncthing

You could almost wipe the machine's disk carelessly, if not for your personal files.
That I've partially solved with syncthing on both machines and a remote server.

I've got a `~/.syncthing/shares/home`, that hosts all the top-level directories
of my `~`, which are bind-mount-bound to the initially empty mount directories
that actually do reside in `~`.

The alternative to this approach is storing everything in `$HOME` directly relieving
you of the 8 or so bind-mounts, which has the downsides of
1\. potentially storing absolutely everything in your `~` in case you mess
something with `.stignore`, which, in the long run, is highly likely or
2\. and walking over tons of cache files or boring local state,
which will make syncthing even slower than it often is.

For the record my syncthing folder currently has: 41588 files and 13073 directories totalling ~10.0 GiB of data.

## Some glue

There's also GNU `stow` that helps me manage symlinks for dotfiles,
also because the NixOS configuration files are stored in the syncthing repo,
which is being pointed to by the `/etc/nixos` symlink.

## Secrets

As the secrets are, of course, not included in the syncthing repo and
bootstrapping new computers without them wouldn't work,
I have a script([keys.bash](#footnote-keys.bash)) that creates a symmetrically GnuPG-encrypted tar archive
with a strong passpharse which contains my keys,
stored in a flash drive in a secure location.

This makes bootstrapping new computers very easy
(modulo downloading the 10GB of syncthing data).

# Improvements

Syncthing doesn't let you store the contents on a untrusted server for it lacks encryption.
Nor does it allow you to download anything partially or in any particular order.

I might switch to [perkeep](https://perkeep.org) someday,
which seemed to be way cooler and powerful. It also has a VFS.
One day, friends…

<br>
<br>
<br>

### Footnote: keys.bash

    #!/usr/bin/env bash

    set -eu

    sudo true

    SUDO_USER_HOME="$(getent passwd "${SUDO_USER:-$USER}" | cut -d: -f6)"

    export RECALL="$0"

    if [ -z "${UNSHARED:-}" ]; then
        exec sudo unshare -m bash <<-EOF
            export STORE="\$(mktemp -d /dev/shm/keys-tmp-XXX)"
            mount -t ramfs ramfs "\$STORE"
            UNSHARED=1 "$RECALL"
    EOF
    fi

    keys="$STORE/keys"
    keys_tar="$keys-$(date +%F).tar"
    mkdir "$keys"
    chown s "$STORE"

    if # if any fails: umount, rm
        cp -r "$SUDO_USER_HOME"/.{gnupg,ssh,histfile} "$keys" && \
        touch "$keys/created-at-$(date +%F)" && \
        tar -C "$STORE" -cf "$keys_tar" "$(basename "$keys")" && \
        sudo -u "$SUDO_USER" gpg -c "$keys_tar"
    then # sudo to SUDO_USER to talk to the gpg agent
        install -o "$SUDO_USER" "$keys_tar.gpg" .
    fi

    umount "$STORE"; rm -r "$STORE"
