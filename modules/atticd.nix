{ ... }:
{
  flake.modules.nixos.atticd = { pkgs, config, lib, ... }: {
    config = lib.mkIf config.atticd.enable {
      networking.firewall = { allowedTCPPorts = [ 8080 ]; };

      services = {
        atticd = {
          enable = true;
          settings = {
            listen = "127.0.0.1:8081";
          };
          environmentFile = "/root/.attic-env-file";
        };

        caddy = {
          enable = true;

          package = pkgs.caddy.withPlugins {
            plugins = [ "github.com/caddyserver/cache-handler@v0.16.0" ];
            hash = "sha256-9PJpfElltgLmP4mliIhJI35w/6/Lhj0HFT3/1l4xLbA=";
          };


          globalConfig = ''
            order cache before rewrite
            cache {
              # Global default cache duration (if not overridden below)
              ttl 1h
              log_level debug
            }
          '';

          virtualHosts.":8080" = {
            extraConfig = ''
              log {
                format console
              }

              # Nix cache info endpoint
              @nix_cache_info path /nix-cache-info
              handle @nix_cache_info {
                header Cache-Control "public, max-age=300"

                # 2. Tell Caddy's internal cache to hold this for 5 minutes
                cache {
                  ttl 300s
                }

                reverse_proxy https://cache.nixos.org {
                  header_up Host cache.nixos.org
                }
              }

              # NAR files (the actual packages)
              @nar path /nar/*
              handle @nar {
                header Cache-Control "public, max-age=31536000, immutable"

                # Cache the actual nar packages for a year
                cache {
                  ttl 8760h
                }

                reverse_proxy https://cache.nixos.org {
                  header_up Host cache.nixos.org
                }
              }

              # Narinfo files (metadata about packages)
              @narinfo path_regexp ^/[^/]+\.narinfo$
              handle @narinfo {
                header Cache-Control "public, max-age=86400"

                # Narinfo can change, so cache them locally for 24 hours
                cache {
                  ttl 24h
                }

                reverse_proxy https://cache.nixos.org {
                  header_up Host cache.nixos.org
                }
              }

              # Fallback for other requests
              handle {
                # We omit the `cache` directive here so Caddy doesn't interfere
                # with Attic's API operations or package pushing (PUT/POST requests).
                reverse_proxy 127.0.0.1:8081
              }
            '';
          };
        };
      };
    };
  };
}
