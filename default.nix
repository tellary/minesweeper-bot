with import
  ( fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/c58e6fbf258df1572b535ac1868ec42faf7675dd.tar.gz"
  ) { };
runCommand "dummy" {
  buildInputs = [
    (haskell.packages.ghc927.ghcWithPackages
      (pkgs: with pkgs; [ webdriver JuicyPixels-extra pretty-simple ])
    )
  ];
} ""
