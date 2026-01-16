# Custom overlays
final: prev:

let
  customPkgs = import ../pkgs { pkgs = final; };
in
{
  # Add all custom packages under 'custom' namespace
  custom = customPkgs;

  # Or add them directly to pkgs (can override existing)
  # my-script = customPkgs.my-script;
}
