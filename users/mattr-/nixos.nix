{
  pkgs,
  config,
  ...
}: {
  programs.zsh.enable = true;

  users.users.mattr- = {
    isNormalUser = true;
    shell = pkgs.zsh;
    initialPassword = "1";
    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDtwYzNNb4WCrOSnIA6oC6LO4pagTo4jGzcR4W3fXCCfaRc14cZiuc3ZmHCMzGKTS3xo63Zz2cvFa8ZFF3IXgjXXOhF3l89xgiQZ8NgIemqNco5C8GPXzkWyvQF3yKXKWLSC1R8aYnI1gakpVA5iJ7CVwvZdTRVP01wK6fJDTM2N8uUIA2JuzYrDH6uH1t4RNBaGovG9G3yfwoYyR2w2UCXrQLGzOHFDSn/y+B6ClDuFngHazktaE4Fnh05AnKw+EQgii61b906XViWW0rcqQktcjiVlIrp8AgIK9yhi5ED0ZaIflFkWh+5KDH4tDMTymT47Cn+GlLTj6TZve+ywxRDr1HNIJTIvBICTFQAZjKL9ep5auKK1V8dmNCHAxfAfwCWqEnuimqupgR2tqZtY0fmZccboZAaac9cvJ0Na130FxTP5ThLusdDXrteKstx0etowL23KBGO6vZobaJkTG8QYjHnGhcvr8u3g7HeYRMWDs//9fgoaIQUv1D9Epe79y9Y0odUPdDhMt3ZerWuKoBfC7xJo9IWl4eHarrXf3yKk5JbAIfVXItn8K94WMpRD1eFdVMpufcnXbD1CnBEz52dL2a+hoH9O9CXpMxgRiQK05zQiYi0ybc+iHKEEyF9Vl1URrfiuzAZ5LpAPd73XcijWJViNgLi08ZtO+RkB0CLBQ=="
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJoNNZg/ZDdwjVukEuSjFsehBSlBXPqRKTlrDYGxUyZw"
    ];
  };

  # Enable 1password
  # TODO: Move this to a common directory. It's not
  # really specific to my user other than the policy kit owners thing.
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    # enable PolicyKit integration
    polkitPolicyOwners = [ "mattr-" ];
  };
}
