{pkgs, ...}: {
  programs.mise = {
    enable = true;
    package = pkgs.unstable.mise;
    globalConfig = {
      tools = {
        elixir = "1.16.1";
        erlang = "26.2.2";
        elm = "0.19.1";
        go = "1.22.0";
        node = "lts";
        ruby = "3";
        rust = "1.76.0";
        shellcheck = "0.8.0";
      };
    };
  };
}
