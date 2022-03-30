# Hybrid Reasoning on Weighted Bipolar Argumentation Framework (WBAF)

## Advance preparation
Install haskell and make sure that you can use the `ghci` command on the console.
Also, install [graphvis](https://graphviz.org/) to use the `dot` command.

## Input Graph information
Write the information about the WBAF graph you want to create in the input_WBAF.hs file. This file already contains the information for the graph used in Figure 1 of the paper. You will need to rewrite this file.

## How to use
Type `ghci Arg.hs` and run the desired file in an interactive environment. Here are some of the functions you can use.

- `showGraph_PDF input_WBAF` - Outputs a graph in the form of a dot file and a diagram of it as a pdf file.

- `makeMetaWBAF input_WBAF` - Constructs a metaWBAF from the target WBAF file.

<!-- - `test_extracKeys input_WBAF` - Extract keys of input_WBAF. -->

- `sup_bundle input_WBAF "a"` - Extract sbundle (set supporter bundle) from the key "a".

- `value_att ["a","b"] input_WBAF` - Calculate the value of maximum attack path ["a","b"] from input_WBAF.

- `str_sup input_WBAF "a"` - Calculate the strength of an argument ("b" for example).

- `str_att input_WBAF "e"` - Calculate the strength of a-bundle ("e" for example).