{pkgs, ...}:
let
  onePasswordPath = "~/.1password/agent.sock";
in {
  programs.gpg = {
    enable = true;
    # Let me manage my own keys and trust settings
    mutableKeys = true;
    mutableTrust = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    enableExtraSocket = true;
    pinentryPackage = pkgs.pinentry-tty;
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
        IdentityAgent ${onePasswordPath}
    '';
  };
}
