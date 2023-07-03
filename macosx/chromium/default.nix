pkgs:
with pkgs;
let
  chromium = stdenv.mkDerivation rec {
    name = "chromium";
    version = "117.0.5866.0";
    revision = "1165081";
    buildInputs =  [ unzip ];
    phases = [ "unpackPhase" "installPhase" ];
    src = builtins.fetchurl {
      url = "https://storage.googleapis.com/chromium-browser-snapshots/Mac/${revision}/chrome-mac.zip";
      sha256 = "08daa0f2c807425025f81c2bc95dbc4ec17982eef074a056239a7d14494d2c70";
    };
    chromiumRunner =
      writeScript "chromium" (builtins.readFile ./chromium);
    installPhase = ''
        mkdir -p "$out"
        cp -pR * "$out"
        cp ${chromiumRunner} "$out"/chromium
      '';
    description = "Chromium web browser";
    homepage = "https://chromium.org";
    meta = {
      platforms = ["x86_64-darwin"];
    };
  };

  chromedriver = stdenv.mkDerivation rec {
    name = "chromedriver";
    version = "117.0.5866.0";
    revision = "1165081";
    buildInputs =  [ unzip ];
    phases = [ "unpackPhase" "installPhase" ];
    src = builtins.fetchurl {
      url = "https://storage.googleapis.com/chromium-browser-snapshots/Mac/${revision}/chromedriver_mac64.zip";
      sha256 = "4710c568257739f260b5f52350a45c0a5f31dd2d9467018c7007e3c3d10da6e3";
    };
    description = "Chromium Driver";
    homepage = "https://chromium.org";
    installPhase = ''
        mkdir -p "$out"
        cp -pR chromedriver "$out"/chromedriver
      '';
    meta = {
      platforms = ["x86_64-darwin"];
    };
  };
in {
  inherit chromium;
  inherit chromedriver;
}
