{ ... }:
{
  flake.modules.homeManager.git = {
    programs.git = {
      enable = true;
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
