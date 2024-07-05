{
  services = {
    openssh = {
      enable = true;
      settings = {
        # No root logins
        PermitRootLogin = "no";
        # Key based logins only
        PasswordAuthentication = false;
      };
    };
  };
  programs.ssh.startAgent = true;
}
