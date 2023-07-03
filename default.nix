let
  pkgs =
    import ( fetchTarball
      "https://github.com/NixOS/nixpkgs/archive/c58e6fbf258df1572b535ac1868ec42faf7675dd.tar.gz"
    ) { };
  macosx-chromium = import ./macosx/chromium pkgs;
in
with pkgs;
stdenv.mkDerivation rec {
  name = "minesweeper-bot";
  src = ./.;
  buildInputs = [
    (haskell.packages.ghc927.ghcWithPackages
      (pkgs: with pkgs;
        [ webdriver
          JuicyPixels-extra
          pretty-simple
          extra
          lens
        ])
    )
  ];
  minesweeper-bot =
    writeScript "minesweeper-bot" (builtins.readFile ./minesweeper-bot);
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];
  # Copy only .hs
  unpackPhase = ''cp $src/*.hs .'';
  buildPhase = ''
    ghc GoogleMinesweeper.hs
  '';
  installPhase = ''
    mkdir -p $out
    cp GoogleMinesweeper $out/GoogleMinesweeper
    cp ${minesweeper-bot} $out/minesweeper-bot
    cp -pR ${macosx-chromium.chromium}/* $out/
    ln -s ${macosx-chromium.chromedriver}/chromedriver $out/chromedriver
  '';
}
