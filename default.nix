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
  chromium-no-sandbox =
    writeScript "chromium-no-sandbox"
      ( builtins.replaceStrings ["__CHROMIUM__"] ["${chromium.outPath}"]
        ( builtins.readFile ./chromium-no-sandbox)
      );
  installPhase
    = if stdenv.isDarwin
      then
        ''
        mkdir -p $out
        cp GoogleMinesweeper $out/GoogleMinesweeper
        cp ${minesweeper-bot} $out/minesweeper-bot
        cp -pR ${macosx-chromium.chromium}/* $out/
        ln -s ${macosx-chromium.chromedriver}/chromedriver $out/chromedriver
        ''
      else if stdenv.isLinux
        then
          ''
          mkdir -p $out
          cp GoogleMinesweeper $out/GoogleMinesweeper
          cp ${minesweeper-bot} $out/minesweeper-bot
          ln -s ${chromium}/bin/chromium $out/chromium
          ln -s ${chromedriver}/bin/chromedriver $out/chromedriver
          ''
        else assert false; "error";
  meta = {
    platforms = ["x86_64-darwin"] ++ lib.platforms.linux;
  };
}
