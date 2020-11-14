---
title: Cloning setups
---
At my job, we've got a bunch of monitoring servers. I had set up one, that's quite minimal and simple. And since what's simple is also small it'd be quite easy to clone it.

How I cloned it through the network after spinning up a livecd of some linux:

1\. Clone the partition table:

    sfdisk -d /dev/sda > table # on the existing setup

2\. Creating a partition table:

    sfdisk /dev/sda < table # on remote host's live OS

3\. Make a filesystem:

    mkfs.ext4 /dev/sda1 # And any others, if needed.

4\. Mount the target partition on /mnt on the remote host

5\. Install and start ssh daemon.

6\. Copy the files:

    rsync -aAXv --compress --compress-level=9 root@10.6.6.6:/mnt

7\. Reset the boot loader's code:

    grub-install /dev/sda

(8.) reboot and you're done! You've got a cloned linux setup!
