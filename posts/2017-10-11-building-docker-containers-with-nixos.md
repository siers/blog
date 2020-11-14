---
title: Building docker containers with NixOS
tags: haskell docker nix
---
In this post I'll describe how I created a nixos docker container
with nix, the purely functional _package manager_,
and, **more importantly**, what I learned by doing it.

(In case you're in a hurry, here's [the link](#results-final-notes) to the finished `*.nix` file
section. Disclaimer: there's more to the journey than there is to the resulting docker images.)

Before that, let me get you up to speed on what nix is, for those familiar
with package managers like `apt-get` or `pacman`:

## Briefly about nix

1. The packages are referenced
not *only* by a name, but also by a hash of its inputs, thereby allowing multiple versions of
the same package coexist. (This does not really work well with `apt-get`s and `pacman`s.)
1. Undeclared build dependencies won't "just work". Since package names aren't authoritative,
there's no `/usr/lib/libc.so` nor is there one in the `LD_LIBRARY_PATH`,
so only a explicit requirement for a dependency will get you the path to the library.
1. Packages get rebuilt when their inputs change, which is not the case elsewhere.
On arch, if you update a minor ruby version, all the packages that depend on ruby, won't be rebuilt,
unless you explicitly request this. (A consequence of packages being referenced by their inputs, so
changing a dependency will make the cache be invalidated, because the cache key's changed.)
\
\
This ensures that if a user reports a bug in a version(hash + name) of the package, specifying the inputs,
it's much more likely that the other user will be able to reproduce it, since the version
number now specifies the FULL, not partial or approximate dependency tree up to glibc (in contrast to,
say, `gem` or `pip`).
In general, there still are, of course, other variables that might render a bug unreproducible
(e.g. runtime input data or code not controlled by nix)

## Gripes with dockerizing my haskell project

### Motivation

Initially I thought I should just use alpine, which is super light-weight, but somehow,
after being hearing about nix, being able to both compile the code and create the container
with a single tool(instead of `stack` + `docker build`) seemed better. I bet you it wasn't easier though.

A person in IRC showed me an example of the [`dockerTools`](https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools),
which has an example of creating a container, so it seemed like everything's going to be easy.

### Beginning

First off you have to convert the `.cabal` to a `.nix` that would compile the haskell application as such.
Here, [`cabal2nix`](https://github.com/NixOS/cabal2nix) helps. Then I copypasted the docker compilation code
from "ertes"(`#haskell`) [demo](https://www.youtube.com/watch?v=sR9p_aQncRo). I somehow managed to mix up the
docker commands thereby being unable to even load the image properly.
This led me to create [an issue](https://github.com/Gabriel439/haskell-nix/issues/18) on the Gabriel Gonzalez'
tutorial repo.

### UTF-8

I did manage to get it to compile the docker image, but the UTF-8 didn't work.
The solution was to add the `glibcLocales` package, which provides a `locale-archive` file, and adding a `LANG` and `LOCALE_ARCHIVE` environment variable.
By putting the `glibcLocales` package's path into `LOCALE_ARCHIVE`, the package also gets included as deps into `/nix/store`.
(Which isn't obvious to me after looking at the [image compiling code](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/default.nix). At the moment, master = `c4dbbbd890`.)

Now, how do I add env vars? I thought, by handing an `Environment` key to the
[`config`](https://github.com/NixOS/nixpkgs/blob/c4dbbbd890519fb791c51b9dc985a445df0806e2/pkgs/build-support/docker/examples.nix#L33)
attribute given to the `dockerTools.buildImage`, of course!

Okay, but the container doesn't have any environment variables when it's run. But the code seemed _so_ plausible!
After a week I started thinking about the implementation of the image builder whilst I wondered whether there are any documented uses of the `Environment` key.
I couldn't find any and realized I had just thought of it myself.
Then it dawned me that it probably gets cooked into the image verbatim and there's no key checking done by nix.
I inspected some docker image configs and I found what it should actually look like.

```
# docker_dir="$(docker info -f '{{.DockerRootDir}}')/image/overlay2/imagedb/content/sha256"
# find "$docker_dir" -type f | head -n1 | xargs -r cat | jq .config
{
  …
  "Env": [
    "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
  …
}
```

[Right then!](https://github.com/siers/zn/commit/4d204dbcdcbb0cc64833707b29afe6d973fdf525#diff-229f95a34283f35b010e59092f2297b7L22)
(I had even gotten the type of the value of the key wrong.)

### HTTPS

The haskell code wants to query a web server to fetch some `<title>`s. What happens at that point?
Amongst other, two things:

1. it looks up the port with which to connect to the endpoint — `/etc/services`,
2. it requires the root certificates for validation — `/etc/…/ca-certificates.crt`.

Surely, I ain't writing those myself!

NixOS should have those, so why can't I just copy them? Let's take it a step further,
why not — _drum roll!_ — include all of it, so I wouldn't have to cherry-pick?<sup>[[this is why]](#size)</sup>

This is where I started doing some serious digging around the
[`nixpkgs` repo](https://github.com/NixOS/nixpkgs) and about nix itself.
Reading all the manuals, looking at tons of code, fitting pieces together.
Still, from all the digging and yak-shaving around the problem, I hadn't
yet found how to recreate the required pieces of a NixOS for a container,
even though general breadth-wise researching was lots of fun.

Then I somehow stumbled upon
[puffnfresh's nixos-docker](https://github.com/puffnfresh/nix-files/blob/cf67ff0fdc0c1517b10693556a868610fdb02dde/nixos-docker/default.nix#L32)
example. This, finally, gives us ALL the `/etc` files via `(import eval-config.nix {}).config.system.etc`(!) and, at this point, my code more or less worked correctly.

NOTE: I could've gotten a hint from
[nixos/default.nix#L35](https://github.com/NixOS/nixpkgs/blob/982cd565cca6b31915442396505d6208ebe9ff81/nixos/default.nix#L35),
but it wasn't immediately clear when I looked at it.

## Problems

### NixOS & containers

This has a weird `boot.isContainer = true;` line. What's that? It's for NixOS declarative containers,
which are `systemd-nspawn` rather than `docker`-based.

Then there's also some weird
[profiles/docker-container.nix](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/docker-container.nix).
Initially I thought maybe building proper docker containers is actually something supported,
which prompted me to ask around in the
[nix-devel](https://groups.google.com/forum/#!topic/nix-devel/xg0lJKmaTHk/discussion)
mailing list, but that resulted in no responses. No idea what's up with that.
(Very discouraging, mind you, but we're all just having fun on our free time and resources, so I don't blame anyone.)

### Size

1\. There are two problems regarding the size. First, adding all of the `system.build.etc` and its
[closure](https://nixos.org/nix/manual/#idm140737315914544)
results in a whooping, massive 600M image(~130M packed).

The second issue's the fact that NixOS creates a recursive symlink `posix -> .`. That then gets packed into
the docker image by `rsync -ak`, which recursively includes the `zoneinfo` directory 40 times.
I filed a lengthy issue here: [NixOS/nixpkgs/issues/30432](https://github.com/NixOS/nixpkgs/issues/30432).

Note: the issue includes a clever double interpretation nix/bash script.
```
#! @bash@/bin/bash -e

"this is interpreted as nix"
/* &> /dev/null || :
# this is interpreted as bash
exit # */
+ "nix again"
```

2\. Does the container need all of the `/etc` files NixOS provides for a real machine? No, of course.
Let's derive a derivation(*hehe*) that takes only the files I want. Here's the
[code(zn/nix/docker.nix#L15)](https://github.com/siers/zn/blob/2f00bfce7dc97cf3f92304254ca12cf22a4fd13e/nix/docker.nix#L15)
and the juicy bits, extracted:
```
let
  system = (import <nixpkgs/nixos/lib/eval-config.nix> { … }).config.system;

  mini-system =
    runCommand "mini-system" {} "
      mkdir -p $out
      cd ${system.build.etc}
      ${rsync}/bin/rsync -aR etc/{services,protocols,ssl,nsswitch.conf} $out
    ";
```

3\. You should be able to just add the package that provides the `etc/` files as one of the output paths,
e.g. `iana-etc` or `ca-certificates`, though I haven't done that for zn.

## Results & final notes

Here are the resulting [`*.nix`](https://github.com/siers/zn/tree/fc787c5c2c7a3b19b9798e84d5877fe31f888712/nix/)
 files and the two images in [the docker hub/siers](https://hub.docker.com/r/siers/) —
`siers/zn` and `siers/caffe-open-nsfw-server`.
(It's the yahoo's
[open_nsfw](https://github.com/yahoo/open_nsfw)
neural network code with a
[TCP interface](https://github.com/siers/zn/blob/master/lib/opennsfw-caffe-server/Dockerfile)
that I added on top of another's packaging of it.)

In the process of solving this big problem of dockerizing my project,
I learned the nix language itself, its tools(oh boy), what a docker container consists of, about the data flow and the big picture of how NixOS code is organized in a nutshell, and
I read some of the [nix's thesis](https://nixos.org/~eelco/pubs/phd-thesis.pdf).

All of this, because not too much info available and I had some very specific questions, most of which couldn't be/weren't answered by `#nixos` IRC channel dwellers.
A large part of the in-depth learning took place at the national library during my unemployment, within a month or two until I was somewhat satisfied with the end result.
I'm still not totally satisfied with it, but it works.
