{ inputs, cell }:

inputs.cells.toolchain.packages.todo-derivation
# TODO(std) needs agda

# cell.library.build-latex-doc {
#   name = "system-f-in-agda-paper";
#   src = inputs.self + /papers/system-f-in-agda;
#   description = "system-f in agda";
#   texFiles = [ "paper.tex" ];
#   withAgda = true;
#   agdaFile = "paper.lagda";
# }