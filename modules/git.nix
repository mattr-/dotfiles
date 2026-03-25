{ ... }:
{
  flake.modules.homeManager.git = { pkgs, ... }: {
    programs.git = {
      enable = true;
      package = pkgs.gitFull;
      settings = {
        user.name = "Matt Rogers";
        user.email = "mattr-@users.noreply.github.com";
        init.defaultBranch = "main";
        push.autoSetupRemote = true;
        pull.rebase = true;
        rebase.autoStash = true;
        rerere.enabled = true;
      };
    };
  };
}
