args@{ inNixShell, ... }: import ./. ({ asShell = inNixShell; } // builtins.removeAttrs args [ "inNixShell" ])
