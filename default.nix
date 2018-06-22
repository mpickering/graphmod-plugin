let
  plugin-overlay = import /root/haskell-nix-plugin/overlay.nix ;
  normal-overlay = import /root/overlay.nix ;

  nixpkgs = import <nixpkgs> { overlays = [normal-overlay plugin-overlay]; };

  extension =
    sel: sup: {
      graphmod-plugin = sel.callCabal2nix "graphmod-plugin" ./graphmod-plugin {};
    };

  hp = nixpkgs.ghcHEAD.extend(extension);
  gv = nixpkgs.graphviz;

  graphmod = { pluginPackage = hp.graphmod-plugin;
               pluginName = "GraphMod";
               pluginOpts = (out-path: ["${out-path}/output"]);
               pluginDepends = [ nixpkgs.graphviz ];
               finalPhase = out-path: ''
                  ls ${out-path}
                  ls ${out-path}/output
                  graphmod-plugin --indir ${out-path}/output > ${out-path}/out.dot
                  cat ${out-path}/out.dot | tred | dot -Tpdf > ${out-path}/modules.pdf
                ''; } ;


in
  with nixpkgs.haskell.lib;
#  hp.graphmod-plugin
  (addPlugin graphmod hp.groups).GraphMod

