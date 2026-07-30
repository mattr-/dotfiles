{ ... }:
{
  flake.modules.homeManager.dotfiles = { lib, pkgs, ... }: {
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
      ".zsh/functions/bounce_rails" = { source = ../home/dot_zsh/functions/executable_bounce_rails; executable = true; };
      ".zsh/functions/clone_starter_repo" = { source = ../home/dot_zsh/functions/executable_clone_starter_repo; executable = true; };
      ".zsh/functions/kdehomerm" = { source = ../home/dot_zsh/functions/executable_kdehomerm; executable = true; };
      ".zsh/functions/mdc" = { source = ../home/dot_zsh/functions/executable_mdc; executable = true; };
      ".zsh/functions/new_dashvim" = { source = ../home/dot_zsh/functions/executable_new_dashvim; executable = true; };
      ".zsh/functions/new_lazyvim" = { source = ../home/dot_zsh/functions/executable_new_lazyvim; executable = true; };
      ".zsh/functions/new_nvchad" = { source = ../home/dot_zsh/functions/executable_new_nvchad; executable = true; };
      ".zsh/functions/new_nvim" = { source = ../home/dot_zsh/functions/executable_new_nvim; executable = true; };
      ".zsh/functions/new-rails-app" = { source = ../home/dot_zsh/functions/executable_new-rails-app; executable = true; };
      ".zsh/functions/nvim_appname" = { source = ../home/dot_zsh/functions/executable_nvim_appname; executable = true; };
      ".zsh/functions/stitle" = { source = ../home/dot_zsh/functions/executable_stitle; executable = true; };
      ".zsh/functions/vcsh" = { source = ../home/dot_zsh/functions/executable_vcsh; executable = true; };
      ".zsh/functions/vv" = { source = ../home/dot_zsh/functions/executable_vv; executable = true; };
      ".zsh/functions/zgitinit" = { source = ../home/dot_zsh/functions/executable_zgitinit; executable = true; };

      ".p10k.zsh".source = ../home/dot_p10k.zsh;
      ".tmux.conf".source = ../home/dot_tmux.conf;
      ".githelpers".source = ../home/dot_githelpers;

      ".ansible.cfg".source = ../home/dot_ansible.cfg;
      ".bash_profile".source = ../home/dot_bash_profile;
      ".bashrc".source = ../home/dot_bashrc;
      ".ctags".source = ../home/dot_ctags;
      ".cvsignore".source = ../home/dot_cvsignore;
      ".fonts.conf".source = ../home/dot_fonts.conf;
      ".gemrc".source = ../home/dot_gemrc;
      ".gtkrc-2.0".source = ../home/dot_gtkrc-2.0;
      ".i3status.conf".source = ../home/dot_i3status.conf;
      ".macos".source = ../home/dot_macos;
      ".mrconfig".source = ../home/dot_mrconfig;
      ".mrconfig.d/.keep".text = "";
      ".msmtprc".source = ../home/dot_msmtprc;
      ".muttrc".source = ../home/dot_muttrc;
      ".my.cnf".source = ../home/dot_my.cnf;
      ".notmuch-config".source = ../home/dot_notmuch-config;
      ".offlineimaprc".source = ../home/dot_offlineimaprc;
      ".pryrc".source = ../home/dot_pryrc;
      ".psqlrc".source = ../home/dot_psqlrc;
      ".reek".source = ../home/dot_reek;
      ".screenrc".source = ../home/dot_screenrc;
      ".slate".source = ../home/dot_slate;
      ".spacemacs".source = ../home/dot_spacemacs;
      ".todo.cfg".source = ../home/dot_todo.cfg;
      ".tool-versions".source = ../home/dot_tool-versions;
      ".urlview".source = ../home/dot_urlview;
      ".vimrc".source = ../home/dot_vimrc;
      ".Xdefaults".source = ../home/dot_Xdefaults;

      ".bundle/config".source = ../home/dot_bundle/config;

      ".emacs.d/init.el".source = ../home/dot_emacs.d/init.el;
      ".emacs.d/mattr.el".source = ../home/dot_emacs.d/mattr.el;
      ".emacs.d/mattr.org".source = ../home/dot_emacs.d/mattr.org;
      ".emacs.d/lisp".source = ../home/dot_emacs.d/lisp;

      ".i3/config".source = ../home/dot_i3/config;

      ".mutt/view_attachment.sh" = { source = ../home/dot_mutt/executable_view_attachment.sh; executable = true; };
      ".mutt/mailcap".source = ../home/dot_mutt/mailcap;
      ".mutt/offlineimap.py".source = ../home/dot_mutt/offlineimap.py;

      ".vim/after".source = ../home/dot_vim/after;
      ".vim/autoload".source = ../home/dot_vim/autoload;
      ".vim/colors".source = ../home/dot_vim/colors;
      ".vim/ftdetect".source = ../home/dot_vim/ftdetect;
      ".vim/snippets".source = ../home/dot_vim/snippets;
      ".vim/backup/.keep".text = "";

      ".local/share/nvim/site/markdown_preview.css".source = ../home/private_dot_local/private_share/nvim/site/markdown_preview.css;
    } // lib.optionalAttrs pkgs.stdenv.isDarwin {
      ".hammerspoon/init.lua".source = ../home/dot_hammerspoon/init.lua;
      ".hammerspoon/Spoons/AutoClick.spoon/docs_index.json".source = ../home/dot_hammerspoon/Spoons/AutoClick.spoon/docs_index.json;
      ".hammerspoon/Spoons/AutoClick.spoon/docs.json".source = ../home/dot_hammerspoon/Spoons/AutoClick.spoon/docs.json;
      ".hammerspoon/Spoons/AutoClick.spoon/init.lua".source = ../home/dot_hammerspoon/Spoons/AutoClick.spoon/init.lua;
      ".hammerspoon/Spoons/AutoClick.spoon/markdown/AutoClick.md".source = ../home/dot_hammerspoon/Spoons/AutoClick.spoon/markdown/AutoClick.md;
      ".hammerspoon/Spoons/AutoClick.spoon/markdown/index.md".source = ../home/dot_hammerspoon/Spoons/AutoClick.spoon/markdown/index.md;
      ".hammerspoon/Spoons/ControlEscape.spoon/Brewfile" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/executable_Brewfile; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/docs.json" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/executable_docs.json; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/init.lua" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/executable_init.lua; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/LICENSE" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/executable_LICENSE; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/README.md" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/executable_README.md; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/script/generate-docs" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/script/executable_generate-docs; executable = true; };
      ".hammerspoon/Spoons/ControlEscape.spoon/script/setup" = { source = ../home/dot_hammerspoon/Spoons/ControlEscape.spoon/script/executable_setup; executable = true; };
    };
  };
}
