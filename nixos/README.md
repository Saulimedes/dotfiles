# nixos

Flake for several machines. Dotfiles stay in chezmoi; this directory is ignored by `chezmoi apply`.

```
flake.nix                 mkHost, one line per machine
hosts/<name>/             default.nix (picks modules), hardware-configuration.nix, disko.nix
modules/base.nix          nix, boot, locale, shells, CLI packages
modules/desktop.nix       Hyprland, greetd, pipewire, fonts, GUI apps (imports emacs.nix)
modules/emacs.nix         Emacs 31 pgtk + vterm/pdf-tools/grammars, systemd user daemon
modules/laptop.nix        tlp, lid, backlight
modules/{podman,vpn,syncthing,pentest}.nix
install.sh                disko + nixos-install from the live ISO
```

## Install the notebook

On the live ISO (connect Wi-Fi first, e.g. `nmcli device wifi connect SSID --ask`):

```sh
rsync -a ~/.local/share/chezmoi/nixos/ nixos@LIVE_IP:nixos/      # from the old machine
printf '%s' 'LUKS_PASSPHRASE' > /tmp/luks-password               # on the ISO, RAM only
printf '%s' 'USER_PASSWORD'   > /tmp/user-password
cp ~/.config/chezmoi/chezmoi.toml /tmp/chezmoi.toml              # name/email for the first chezmoi init
cd nixos && sudo ./install.sh notebook /dev/nvme0n1
```

Without the password files the script prompts. All three files are shredded when it exits.

Afterwards copy `hosts/notebook/{disk-device,hardware-configuration}.nix` back into the repo.
The installed system has the flake in `/etc/nixos`, so `sudo nixos-rebuild switch` works as is.

## Dotfiles hook

`modules/home.nix` runs chezmoi from a Home Manager activation script: at first boot and after every
`nixos-rebuild switch`. It runs `chezmoi update` when `~/.local/share/chezmoi` exists, else
`chezmoi init --apply --branch nixos <repo>` (needs `~/.config/chezmoi/chezmoi.toml`, which install.sh seeds).
Failures never break the activation; a private externals repo that cannot be cloned is skipped.
Edit dotfiles in the chezmoi source and push, not in `~`: `apply` overwrites local changes.

## Add a machine

1. `hosts/<name>/default.nix` importing the modules it needs, plus `hardware-configuration.nix`
   from `nixos-generate-config`.
2. `<name> = mkHost "<name>" { };` in `flake.nix`.
3. `sudo nixos-rebuild switch --flake .#<name>`.
