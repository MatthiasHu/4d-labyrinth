nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [$(cat dependencies.txt | tr '\n' ' ')])"
