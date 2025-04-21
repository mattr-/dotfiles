{ pkgs, inputs, ... } :
{
  fonts = {
    packages = with pkgs; [
      # icon fonts
      material-symbols

      # normal fonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji

      inputs.self.packages.${pkgs.system}.SF-Pro
      inputs.self.packages.${pkgs.system}.SF-Mono

      # nerdfonts
      nerd-fonts.symbols-only
      nerd-fonts.departure-mono
      departure-mono
    ];

    # causes more issues than it solves
    enableDefaultPackages = false;

    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        autohint = false;
        style = "full";
      };
      subpixel = {
        lcdfilter = "default";
        rgba = "rgb";
      };
      defaultFonts = let
        addAll = builtins.mapAttrs (_: v: ["Symbols Nerd Font"] ++ v ++ ["Noto Color Emoji"]);
      in
        addAll {
          serif = ["Noto Sans Serif"];
          sansSerif = ["SF Pro"];
          monospace = ["SF Mono"];
          emoji = ["Noto Color Emoji"];
        };
    };
    fontDir = {
      enable = true;
      decompressFonts = true;
    };
  };
}
