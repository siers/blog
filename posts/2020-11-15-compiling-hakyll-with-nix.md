---
title: Compiling hakyll with nix
---
I found this post about "hakyll on nix"
[cs.yale.edu/homes/lucas.paul/posts/2017-04-10-hakyll-on-nix.html](https://www.cs.yale.edu/homes/lucas.paul/posts/2017-04-10-hakyll-on-nix.html)
and made it a little more straight-forward for my tastes. It's very short and this allowed me to deploy this blog through nginx via nixops.

`default.nix`:
```
{
  stdenv,
  haskellPackages,
  git,
  glibcLocales,
  nix-gitignore
}:

stdenv.mkDerivation {
  name = "blog";
  src = nix-gitignore.gitignoreSourcePure [".git" "_cache" "_site" "result"] ./.;
  phases = "unpackPhase buildPhase";
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [ hakyll ]))
    git
  ];
  buildPhase = ''
    ghc -O2 -dynamic --make site.hs -o generate-site

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive";
    export LANG=en_US.UTF-8
    ./generate-site build

    mkdir $out
    cp -r _site/* $out
  '';
}
```

`nixops` module:
```
{
  pkgs, lib, ...
}:

let
  nginxConf = ''
    server {
      listen       80 default_server;
      server_name  "";

      location / {
        root ${pkgs.callPackage (import ~/code/blog/default.nix) {}};
      }
    }
  '';
in {
  services = {
    nginx = {
      enable = true;
      appendHttpConfig = nginxConf;
    };
  };
}
```
