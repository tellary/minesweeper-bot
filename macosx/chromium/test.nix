let
  pkgs = import <nixpkgs> {};
  chromium = import ./default.nix pkgs;
in chromium.chromedriver
  
