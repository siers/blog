{
  stdenv,
  haskellPackages,
  git,
  glibcLocales,
  nix-gitignore
}:

# https://www.cs.yale.edu/homes/lucas.paul/posts/2017-04-10-hakyll-on-nix.html

stdenv.mkDerivation {
  name = "blog";
  src = ./.;
  phases = "unpackPhase buildPhase";
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [ hakyll ]))
    git
  ];
  buildPhase = ''
    find
    ghc -O2 -dynamic --make site.hs -o generate-site

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive";
    export LANG=en_US.UTF-8
    ./generate-site build

    mkdir $out
    cp -r _site/* $out
  '';
}
