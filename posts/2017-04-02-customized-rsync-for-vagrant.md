---
title: Customized rsync for vagrant
---
I needed more control over the paths that get transfered or even checked, since I was testing
chef on EC2 from the home link(~0.5M down), but
[vagrant rsync](https://www.vagrantup.com/docs/synced-folders/rsync.html)'s
only allows adding an exclude, which isn't even regex, so I wrote my own.

I didn't need to transfer non-chef paths or the cookbooks that I wasn't changing at the moment.
The script therefore lets you grep only the interesting (guest|host)dir pairs and add rsync's `--filter` commands.
(Took me a while to get those right.)

The paths are stored in `.vagrant/machines/aws/aws/synced_folders` and *ARE NOT* handled safely in this script,
so review your input before you submit via `CVR_RSYNC_COMMAND=echo`.

    % cat ~/work/scripts/chef-vagrant-rsync
    #!/bin/zsh

    dir_pairs() {
        jq -r < .vagrant/machines/aws/aws/synced_folders '.rsync | map("\(.hostpath) \(.guestpath)")[]'
    }

    usage() {
        echo "usage $1: <IP_ADDR> <SRC_GREP> <RSYNC_ARGS>*"
    }

    if [ "$#" -lt "2" ]; then
        usage "$(basename "$0")"
        exit
    fi

    #dump-args "$@"
    addr="$1"
    src_grep="$2"
    shift; shift

    dir_pairs | grep -E "$src_grep" | while read src dst; do
        "${CVR_RSYNC_COMMAND:-rsync}" -r "$src" "ubuntu@$addr:$dst" "$@"
    done

--

    s|work/chef master % cat notes/cvr
    #!/bin/sh

    ~/work/scripts/chef-vagrant-rsync 34.249.163.67 'work/chef/(cookbooks|roles)' "$@" -vnr \
        --filter='+ **/cookbooks' \
        --filter='+ **/cookbooks/specific_cookbook**' \
        --filter='+ **/roles**' \
        --exclude='*'
