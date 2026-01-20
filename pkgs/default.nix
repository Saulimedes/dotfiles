# Custom packages
{ pkgs }:

{
  xcursor-thedot = pkgs.callPackage ./xcursor-thedot { };
  google-cursor = pkgs.callPackage ./google-cursor { };
  future-cursors = pkgs.callPackage ./future-cursors { };
}
