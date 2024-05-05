{config, pkgs, lib, ...}:
{
  mkOutOfStoreSymLinkRec =
    let
      ln = config.lib.file.mkOutOfStoreSymlink;
      lndir = path: link: builtins.listToAttrs (
        map (file: {
          name = "${link}/${lib.path.removePrefix (/. + path) (/. + file)}";
          value = { source = ln "${file}"; };
        }) (lib.filesystem.listFilesRecursive path)
      );
      rmopts = attrs: builtins.removeAttrs attrs
	["source" "recursive" "outOfStoreSymlink"];
    in
      fileAttrs: lib.attrsets.concatMapAttrs (name: value:         
        lib.attrsets.mapAttrs
	  (_: attrs: attrs // rmopts value) (lndir value.source name)  
      ) fileAttrs;
}
