{ ... }:
{
  flake.modules.homeManager.git = { pkgs, ... }: {
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
