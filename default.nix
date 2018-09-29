let
  # Get the haskell-nix-plugin infrastructure
  plugin-overlay = (import <nixpkgs> {}).nur.repos.mpickering.overlays.haskell-plugins;

  # Use head.hackage to get ghc-8.6.1
  nixpkgs = import <nixpkgs> { overlays = [plugin-overlay]; };


  # Add the unreleased package to the package set
  extension =
    sel: sup: {
      graphmod-plugin = (sel.callCabal2nix "graphmod-plugin" ./graphmod-plugin {});
    };
  hp = nixpkgs.haskell.packages.ghc861.extend(extension);

  v3 = drv: nixpkgs.haskell.lib.appendConfigureFlag drv "-v2";

  gv = nixpkgs.graphviz;

  # Define the plugin
  graphmod = { pluginPackage = hp.graphmod-plugin;
               pluginName = "GraphMod";
               pluginOpts = (out-path: ["${out-path}/output"]);
               pluginDepends = [ nixpkgs.graphviz ];
               finalPhase = out-path: ''
                  ls ${out-path}
                  ls ${out-path}/output
                  graphmod-plugin --indir ${out-path}/output > ${out-path}/out.dot
                  cat ${out-path}/out.dot | tred | dot -Gdpi=600 -Tpng > ${out-path}/modules.png
                ''; } ;


in
  with nixpkgs.haskell.lib;
	# Disable library profiling for GHC bug
	# Add the plugin to the package we want to build.
  (addPlugin graphmod hp.aeson).GraphMod

