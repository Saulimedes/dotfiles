# Custom Packages in Nix

## Directory Structure

```
pkgs/
├── default.nix          # Main package definitions
├── helium/              # Complex package in subdirectory
│   └── default.nix
└── ...

overlays/
└── default.nix          # Makes packages available as pkgs.custom.*
```

## Usage

After defining in `pkgs/default.nix`, use in your config:

```nix
# In home.packages or environment.systemPackages
home.packages = [
  pkgs.custom.my-script
  pkgs.custom.example-app
];
```

---

## Common Package Patterns

### 1. GitHub Release (source build)

```nix
my-app = pkgs.stdenv.mkDerivation rec {
  pname = "my-app";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "repo";
    rev = "v${version}";
    sha256 = "";  # Leave empty first, nix will tell you the hash
  };

  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = [ pkgs.openssl pkgs.zlib ];

  meta = with pkgs.lib; {
    description = "My application";
    homepage = "https://github.com/username/repo";
    license = licenses.mit;
  };
};
```

### 2. Pre-built Binary

```nix
my-binary = pkgs.stdenv.mkDerivation rec {
  pname = "my-binary";
  version = "2.0.0";

  src = pkgs.fetchurl {
    url = "https://github.com/user/repo/releases/download/v${version}/${pname}-linux-amd64.tar.gz";
    sha256 = "";
  };

  sourceRoot = ".";

  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = [ pkgs.stdenv.cc.cc.lib ];

  installPhase = ''
    mkdir -p $out/bin
    cp ${pname} $out/bin/
    chmod +x $out/bin/${pname}
  '';
};
```

### 3. AppImage

```nix
my-appimage = pkgs.appimageTools.wrapType2 {
  name = "my-app";
  src = pkgs.fetchurl {
    url = "https://example.com/MyApp.AppImage";
    sha256 = "";
  };
  extraPkgs = pkgs: [ pkgs.libudev-zero ];
};
```

### 4. Flatpak (via system)

```nix
# In system config
services.flatpak.enable = true;

# Then install manually:
# flatpak install flathub com.example.App
```

### 5. Simple Script

```nix
my-script = pkgs.writeShellScriptBin "my-script" ''
  #!/usr/bin/env bash
  echo "Hello!"
  ${pkgs.jq}/bin/jq --version
'';
```

### 6. Python Application

```nix
my-python-app = pkgs.python3Packages.buildPythonApplication rec {
  pname = "my-python-app";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "repo";
    rev = "v${version}";
    sha256 = "";
  };

  propagatedBuildInputs = with pkgs.python3Packages; [
    requests
    click
  ];
};
```

### 7. Node.js Application

```nix
my-node-app = pkgs.buildNpmPackage rec {
  pname = "my-node-app";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "repo";
    rev = "v${version}";
    sha256 = "";
  };

  npmDepsHash = "";
};
```

### 8. Rust Application

```nix
my-rust-app = pkgs.rustPlatform.buildRustPackage rec {
  pname = "my-rust-app";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "repo";
    rev = "v${version}";
    sha256 = "";
  };

  cargoSha256 = "";
};
```

### 9. Go Application

```nix
my-go-app = pkgs.buildGoModule rec {
  pname = "my-go-app";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "username";
    repo = "repo";
    rev = "v${version}";
    sha256 = "";
  };

  vendorSha256 = "";  # or null if vendor directory exists
};
```

---

## Getting SHA256 Hashes

### Method 1: Leave empty and build

```bash
nix build .#my-package
# Error will show: got sha256-XXXXX...
# Copy that hash
```

### Method 2: nix-prefetch-url

```bash
nix-prefetch-url https://example.com/file.tar.gz
```

### Method 3: nix-prefetch-github

```bash
nix-prefetch-github username repo --rev v1.0.0
```

---

## Example: Packaging Helium Browser

```nix
# pkgs/helium/default.nix
{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "helium";
  version = "1.0.0";  # Update to actual version

  src = pkgs.fetchurl {
    url = "https://github.com/nicoth-in/helium/releases/download/v${version}/helium-linux-x64.tar.gz";
    sha256 = "";  # Get from release
  };

  nativeBuildInputs = [
    pkgs.autoPatchelfHook
    pkgs.makeWrapper
  ];

  buildInputs = with pkgs; [
    stdenv.cc.cc.lib
    glib
    nss
    nspr
    atk
    cups
    dbus
    gtk3
    pango
    cairo
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    xorg.libxcb
    expat
    alsa-lib
  ];

  installPhase = ''
    mkdir -p $out/opt/helium
    cp -r * $out/opt/helium/

    mkdir -p $out/bin
    ln -s $out/opt/helium/helium $out/bin/helium

    mkdir -p $out/share/applications
    cat > $out/share/applications/helium.desktop << EOF
    [Desktop Entry]
    Name=Helium
    Exec=$out/bin/helium %U
    Icon=$out/opt/helium/resources/icon.png
    Type=Application
    Categories=Network;WebBrowser;
    EOF
  '';
}
```

Then in `pkgs/default.nix`:

```nix
{ pkgs }:
{
  helium = import ./helium { inherit pkgs; };
  # ... other packages
}
```

Use it:

```nix
home.packages = [ pkgs.custom.helium ];
```
