---
title: Portfolio
---
<div id='code'>
This portfolio is divided into two parts: projects written in Haskell and projects written in C.
Both are listed approximately in the order of importance.
Haskell projects are newer and, due to its nature and my skill at the time of writing, better.
C projects are older and their code quality is inversely proportional to their age.

<div class='sep'></div>

### [`#developerslv` IRC bot](https://github.com/siers/zn) haskell, stack, irc-client, 2016 mar
It's the «#developerslv» channel bot that is a great excuse to code haskell.

Uses regex-tdfa, lens, maybes and eithers, megaparsec, telegram-api, http-client-tls.  
Can be compiled with either nix or stack.

Currently deployed to a droplet inside a NixOS-built container[(read more)](https://raitis.veinbahs.lv/posts/2017-10-11-building-docker-containers-with-nixos.html),
together with the [`open_nsfw`](https://github.com/yahoo/open_nsfw) neural network, also wrapped in a [container](https://github.com/siers/zn/blob/master/lib/opennsfw-caffe-server/Dockerfile).

### [Violin pitchfork](https://github.com/siers/violin-pitchfork) haskell, fft, sox, 2015 jun
This program sounds a pitch for tuning tuning your violin
that listens and determines which string you're trying to play and
responds with the correct pitch. Feedback loops remain undetected.

### [Sudoku solver](https://github.com/siers/sudoku-solver) haskell, 2014 nov
For educational purposes, but the code is nice, so I'll include it.

### [Heat equation solver](https://github.com/siers/parabolic-pde#pretty-pictures) haskell, repa, imagemagick, 2014 dec
Main reason of creation was the generation of cool [gifs](https://github.com/siers/parabolic-pde#pretty-pictures).

### [Font random](https://github.com/siers/font-random) haskell, fc-query, 2017 jul
Generates random characters from the set of available glyphs found in fonts given as input.
The usual random text generation techniques are either overrepresenting unrenderable characters
or its output spans a very limited character set. Sample output: «ɷȣȥʔɜǜ¥ȷ¢řŲɜȸúĮǖ».

<div class='sep'></div>

### [ELF executable crypter POC](https://github.com/siers/stoical) linux, c, 2012 dec
Making a PE crypter was an old goal of mine, but having become a linux user,
PEs were no longer interesting. The code doesn't use libraries thus is a little messy,
but it works most of time.

### [BF language interpreter](https://github.com/siers/bf) linux, c, asm, 2011 aug
Created with the intent of learning about asm.

### [Simple motion sensor](https://github.com/siers/voculus) linux, c, pthread, ruby, 2012 jul
Plays sounds whenever detects changes in light above threshold.

### [SOCKS5 proxy server](https://github.com/siers/ss5) linux, c, tcp, 2010 jun
Implements some methods of the SOCKS5 protocol. Works in conjunction with firefox.

<div class='sep'></div>

#### [My gists](https://gist.github.com/siers) Little utilities, [sphere plot](https://gist.github.com/siers/1923564ca206c1b36f47) and other mentionable effort.

</div>
