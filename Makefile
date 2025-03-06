# Connectivity info for the VM
NIXADDR ?= unset
NIXPORT ?= 22

NIXNAME ?= dev-vm

# Get the path to this Makefile and directory
MAKEFILE_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

MAKE_SSH_OPTIONS=-o PubkeyAuthentication=no -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no

define SSH_SCRIPT_BODY
nix --experimental-features 'nix-command flakes' run github:nix-community/disko/latest \
 -- --yes-wipe-all-disks --mode destroy,format,mount ./system/hosts/dev-vm/disko.nix; \
echo 'Disk configuration complete. Installing NixOS'; \
sleep 1; \
sudo nixos-install --no-channel-copy --flake './system#$(NIXNAME)'; \
echo 'NixOS installation complete. Please check for any errors.';
endef

# Bootstrap a brand new VM. The VM should have booted into a NixOS install
# image with the root password set.  After install, reboot and use
# the system like normal.
#
# Lots borrowed from https://github.com/mitchellh/nixos-config/blob/main/Makefile
.PHONY: vm/bootstrap
vm/bootstrap:
	rsync -av -e 'ssh $(MAKE_SSH_OPTIONS) -p$(NIXPORT)' \
		--exclude='.git/' \
		--rsync-path="sudo rsync" \
		$(MAKEFILE_DIR)/ root@$(NIXADDR):./system
	ssh $(MAKE_SSH_OPTIONS) -p$(NIXPORT) root@$(NIXADDR) "$(SSH_SCRIPT_BODY)"

macos/bootstrap:
	curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
		sh -s -- install
