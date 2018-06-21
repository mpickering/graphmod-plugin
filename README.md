This is a reimplementation of `graphmod` using a source plugin.

The advantage is that the implementation is simpler and more robust as
it doesn't rely on lexing source files are finding files on disk. Instead,
we just serialise this information from the internal compiler state to
generate the graph.

The plugin is also an example of the design pattern of recording information
as the plugin runs and then collating information at the end.

`graphmod` was originally written by Thomas Halgren and Iavor Diatchki.

# Manual Usage

In order to run the plugin manually, pass the options appropiately to GHC when
compiling your package.

```
-fplugin=GraphMod -fplugin-opt GraphMod:output
```

Then, call the `graphmod-plugin` executable passing the `--indir` flag
to indicate where the plugin stored the information.

```
graphmod-plugin --indir output
```

You can this pass this output to `dot` in order to generate an image or pdf.

In addition, `graphmod-plugin` takes all the options that the old `graphmod`
executable accepted.

# Nix Usage

It is recomended to use the plugin with nix which will handle the finalisation
and generation steps appropiately. TODO
