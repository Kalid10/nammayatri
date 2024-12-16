{ self, ... }:

let
  imageName = "ghcr.io/nammayatri/nammayatri";
  imageTag = builtins.substring 0 6 (self.rev or "dev");
in
{
  config = {
    perSystem = { self', pkgs, lib, ... }: {
      packages = lib.optionalAttrs (pkgs.stdenv.isLinux || pkgs.stdenv.isDarwin) {
        dockerImage = (pkgs.dockerTools.buildImage {
          name = imageName;
          created = "now";
          tag = imageTag;
          copyToRoot = pkgs.buildEnv {
            paths = with pkgs; [
              cacert
              awscli
              coreutils
              bash
              self'.packages.nammayatri
              gdal
              postgis
              curl
              htop
              wget
            ];
            name = "beckn-root";
            pathsToLink = [
              "/bin"
              "/opt"
            ];
          };
          config = {
            Env = [
              "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
            ];
            Cmd = [ "${self'.packages.nammayatri}/bin/rider-app-exe" ];
          };

          extraCommands = ''
            ls opt/app/rider-app-exe
            ls opt/app/swagger
          '';
        }).overrideAttrs (lib.addMetaAttrs {
          description = "Docker image for nammayatri backend";
          homepage = "https://github.com/nammayatri/nammayatri/pkgs/container/nammayatri";
        });
      };
    };
  };
}
