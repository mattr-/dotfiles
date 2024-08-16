# Connectivity info for the VM
NIXADDR ?= unset
NIXPORT ?= 22
NIXUSER ?= mattr-

NIXNAME ?= dev-vm

# Get the path to this Makefile and directory
MAKEFILE_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

MAKE_SSH_OPTIONS=-o PubkeyAuthentication=no -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no

define SSH_SCRIPT_BODY
parted /dev/nvme0n1 -- mklabel gpt; \
parted /dev/nvme0n1 -- mkpart ESP fat32 1MB 512MB; \
parted /dev/nvme0n1 -- mkpart primary 512MB 100\%; \
parted /dev/nvme0n1 -- set 1 esp on; \
sleep 1; \
mkfs.fat -F 32 -n boot /dev/nvme0n1p1; \
mkfs.btrfs -f -L nixos /dev/nvme0n1p2; \
sleep 10; \
mkdir -p /mnt; \
mount /dev/nvme0n1p2 /mnt; \
btrfs subvolume create /mnt/root; \
btrfs subvolume create /mnt/home; \
btrfs subvolume create /mnt/nix; \
umount /mnt; \
sleep 5; \
mount -o compress=zstd,subvol=root /dev/nvme0n1p2 /mnt ; \
mkdir -p /mnt/boot ; \
mkdir -p /mnt/home ; \
mkdir -p /mnt/nix ; \
mount /dev/nvme0n1p1 /mnt/boot; \
mount -o compress=zstd,subvol=home /dev/nvme0n1p2 /mnt/home ; \
mount -o compress=zstd,noatime,subvol=nix /dev/nvme0n1p2 /mnt/nix ; \
sleep 5; \
nixos-generate-config --root /mnt; \
sed --in-place '/system\.stateVersion = .*/a \
	nix.package = pkgs.nixVersions.latest;\n \
	nix.extraOptions = \"experimental-features = nix-command flakes\";\n \
	services.openssh.enable = true;\n \
	services.openssh.settings.PasswordAuthentication = true;\n \
	services.openssh.settings.PermitRootLogin = \"yes\";\n \
	users.users.root.initialPassword = \"root\";\n \
' /mnt/etc/nixos/configuration.nix; \
nixos-install --no-root-passwd && reboot;
endef


# Bootstrap a brand new VM. The VM should have booted into a NixOS install
# image with the root password set to `root`.  After install, reboot and set
# the root password for the next step.
#
# Mostly borrowed from https://github.com/mitchellh/nixos-config/blob/main/Makefile
.PHONY: vm/pre-bootstrap
vm/pre-bootstrap:
	ssh $(MAKE_SSH_OPTIONS) -p$(NIXPORT) root@$(NIXADDR) "$(SSH_SCRIPT_BODY)"

# Post install, switch to the flake setup for the VM
.PHONY: vm/bootstrap
vm/bootstrap:
	rsync -av -e 'ssh $(MAKE_SSH_OPTIONS) -p$(NIXPORT)' \
		--exclude='.git/' \
		--rsync-path="sudo rsync" \
		$(MAKEFILE_DIR)/ root@$(NIXADDR):./nix-config
	ssh $(MAKE_SSH_OPTIONS) -p$(NIXPORT) root@$(NIXADDR) " \
		sudo nixos-rebuild switch --flake \"/root/nix-config#$(NIXNAME)\" \
	"
