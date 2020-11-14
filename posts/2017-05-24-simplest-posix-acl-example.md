---
title: Simplest POSIX ACL example
---
This is an exhibit of POSIX Access Control Lists.

    user|tmp/sand-PzO % # create an inaccessible file called "test" with "123" inside
    user|tmp/sand-PzO % # NOTE: I could've written 0 instead of 000.
    user|tmp/sand-PzO % install -m 000 <(echo 123) test

    user|tmp/sand-PzO % # which the "User nobody (can) Read"
    user|tmp/sand-PzO % setfacl -m u:nobody:r test

    user|tmp/sand-PzO % ls -l test
    ----r-----+ 1 user user 4 May 24 11:03 test

    user|tmp/sand-PzO % cat test
    cat: test: Permission denied
    user|tmp/sand-PzO % sudo -u nobody cat test
    [sudo] password for user:
    123
