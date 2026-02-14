# Git configuration — home-manager aspect
#
# Manages git via home-manager. Provides a baseline git configuration that
# can be composed into any host or standalone home-manager configuration.
{ ... }:
{
  flake.modules.homeManager.git = {
    programs.git = {
      enable = true;
      userName = "Matt Rogers";
      userEmail = "mattr-@users.noreply.github.com";

      extraConfig = {
        init.defaultBranch = "main";
        push.autoSetupRemote = true;
        pull.rebase = true;
        rebase.autoStash = true;
        rerere.enabled = true;
      };

      delta = {
        enable = true;
        options = {
          navigate = true;
          side-by-side = true;
        };
      };
    };
  };
}
