{ ... }:
{
  flake.modules.homeManager.dotfiles = { ... }: {
    home.file = {
      ".config/ghostty/config".source = ../home/dot_config/ghostty/config;
      ".config/wezterm/wezterm.lua".source = ../home/dot_config/wezterm/wezterm.lua;
      ".config/lazygit/config.yml".source = ../home/dot_config/lazygit/config.yml;
      ".config/ripgrep/rc".source = ../home/dot_config/ripgrep/rc;
      ".config/waybar/config".source = ../home/dot_config/waybar/config;
      ".config/waybar/style.css".source = ../home/dot_config/waybar/style.css;
      ".config/niri/config.kdl".source = ../home/dot_config/niri/config.kdl;
      ".config/vicinae/vicinae.json".source = ../home/dot_config/vicinae/vicinae.json;

      ".config/nvim/init.lua".source = ../home/dot_config/nvim/init.lua;
      ".config/nvim/stylua.toml".source = ../home/dot_config/nvim/stylua.toml;
      ".config/nvim/.distro".source = ../home/dot_config/nvim/dot_distro;
      ".config/nvim/.neoconf.json".source = ../home/dot_config/nvim/dot_neoconf.json;
      ".config/nvim/lua".source = ../home/dot_config/nvim/lua;
      ".config/nvim/after".source = ../home/dot_config/nvim/after;

      ".config/nvim-dashvim/init.lua".source = ../home/dot_config/nvim-dashvim/init.lua;
      ".config/nvim-dashvim/stylua.toml".source = ../home/dot_config/nvim-dashvim/stylua.toml;
      ".config/nvim-dashvim/LICENSE".source = ../home/dot_config/nvim-dashvim/LICENSE;
      ".config/nvim-dashvim/.distro".source = ../home/dot_config/nvim-dashvim/dot_distro;
      ".config/nvim-dashvim/.gitignore".source = ../home/dot_config/nvim-dashvim/dot_gitignore;
      ".config/nvim-dashvim/.neoconf.json".source = ../home/dot_config/nvim-dashvim/dot_neoconf.json;

      ".config/nvim-lazyvim/init.lua".source = ../home/dot_config/nvim-lazyvim/init.lua;
      ".config/nvim-lazyvim/stylua.toml".source = ../home/dot_config/nvim-lazyvim/stylua.toml;
      ".config/nvim-lazyvim/lazyvim.json".source = ../home/dot_config/nvim-lazyvim/lazyvim.json;
      ".config/nvim-lazyvim/.distro".source = ../home/dot_config/nvim-lazyvim/dot_distro;

      ".zshenv".source = ../home/dot_zsh/vcsstub;
      ".zsh/sourcedir".source = ../home/dot_zsh/sourcedir;
      ".zsh/vcsstub".source = ../home/dot_zsh/vcsstub;
      ".zsh/.zshenv".source = ../home/dot_zsh/vcsstub;
      ".zsh/.zshrc".source = ../home/dot_zsh/vcsstub;
      ".zsh/.zlogout".source = ../home/dot_zsh/dot_zlogout;
      ".zsh/zshenv".source = ../home/dot_zsh/zshenv;
      ".zsh/zshrc".source = ../home/dot_zsh/zshrc;

      ".zsh/functions/_bundler".source = ../home/dot_zsh/functions/_bundler;
      ".zsh/functions/_docker".source = ../home/dot_zsh/functions/_docker;
      ".zsh/functions/_docker-compose".source = ../home/dot_zsh/functions/_docker-compose;
      ".zsh/functions/_gem".source = ../home/dot_zsh/functions/_gem;
      ".zsh/functions/_git-branch".source = ../home/dot_zsh/functions/_git-branch;
      ".zsh/functions/_git-remote".source = ../home/dot_zsh/functions/_git-remote;
      ".zsh/functions/_heroku".source = ../home/dot_zsh/functions/_heroku;
      ".zsh/functions/c".source = ../home/dot_zsh/functions/c;
      ".zsh/functions/cf".source = ../home/dot_zsh/functions/cf;
      ".zsh/functions/colors".source = ../home/dot_zsh/functions/colors;
      ".zsh/functions/prompt_mattr_setup".source = ../home/dot_zsh/functions/prompt_mattr_setup;
      ".zsh/functions/prompt_mattr2_setup".source = ../home/dot_zsh/functions/prompt_mattr2_setup;
      ".zsh/functions/vim".source = ../home/dot_zsh/functions/vim;
      ".zsh/functions/bounce_rails".source = ../home/dot_zsh/functions/executable_bounce_rails;
      ".zsh/functions/clone_starter_repo".source = ../home/dot_zsh/functions/executable_clone_starter_repo;
      ".zsh/functions/kdehomerm".source = ../home/dot_zsh/functions/executable_kdehomerm;
      ".zsh/functions/mdc".source = ../home/dot_zsh/functions/executable_mdc;
      ".zsh/functions/new_dashvim".source = ../home/dot_zsh/functions/executable_new_dashvim;
      ".zsh/functions/new_lazyvim".source = ../home/dot_zsh/functions/executable_new_lazyvim;
      ".zsh/functions/new_nvchad".source = ../home/dot_zsh/functions/executable_new_nvchad;
      ".zsh/functions/new_nvim".source = ../home/dot_zsh/functions/executable_new_nvim;
      ".zsh/functions/new-rails-app".source = ../home/dot_zsh/functions/executable_new-rails-app;
      ".zsh/functions/nvim_appname".source = ../home/dot_zsh/functions/executable_nvim_appname;
      ".zsh/functions/stitle".source = ../home/dot_zsh/functions/executable_stitle;
      ".zsh/functions/vcsh".source = ../home/dot_zsh/functions/executable_vcsh;
      ".zsh/functions/vv".source = ../home/dot_zsh/functions/executable_vv;
      ".zsh/functions/zgitinit".source = ../home/dot_zsh/functions/executable_zgitinit;

      ".p10k.zsh".source = ../home/dot_p10k.zsh;
      ".tmux.conf".source = ../home/dot_tmux.conf;
      ".githelpers".source = ../home/dot_githelpers;
    };
  };
}
