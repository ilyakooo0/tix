{ config, pkgs, lib, ... }:

{
  config = lib.mkMerge [

    (lib.mkIf (lib.any (fs: fs == "unionfs-fuse") config.boot.initrd.supportedFilesystems) {
      boot.initrd.kernelModules = [ "fuse" ];

      boot.initrd.extraUtilsCommands = "a";

      boot.initrd.postDeviceCommands = "b";
    }
    )

    (lib.mkIf (lib.any (fs: fs == "unionfs-fuse") config.boot.supportedFilesystems) {
      system.fsPackages = [ pkgs.unionfs-fuse ];
    }
    )

  ];
}
