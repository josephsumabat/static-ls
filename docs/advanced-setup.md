# Advanced setup

Here are some advacned setup setup options. These are generally intended for enterprise projects or for setting up an advanced "canned" configuration
for users in a project. If you're curious or want help setting any of these up please feel free to message me directly.

## Alloglot project configuration

The following snippet can be used in a project so that alloglot users don't need to manually set up their language client

```
{
  "languages": [
    {
      "languageId": "haskell",
      "serverCommand": ".bin/static-ls",
    }
  ]
}
```

## Indexing dependencies

The following snippet can used for indexing dependencies in hiedb allowing jump to dependency definition. The following snippet is
taken from Mercury's internal setup with permission to share:

```
{
  hiedb,
  lib,
  runCommand,
}: pkg: let
  dependencies = pkg.getCabalDeps;

  flattenedHaskellDependencies =
    (dependencies.executableHaskellDepends or [])
    ++ (dependencies.libraryHaskellDepends or [])
    ++ (dependencies.testHaskellDepends or [])
    ++ (dependencies.benchmarkHaskellDepends or [])
    ++ (dependencies.setupHaskellDepends or []);

  destination = dependency:
    lib.escapeShellArg dependency.name;

  unpackDependency = dependency:
    if dependency ? src && dependency.src != null
    then ''
      if [ ! -d ${destination dependency} ]; then
        sourceRoot=
        srcs= src=${dependency.src} unpackPhase
        pushd "$sourceRoot"
        patchPhase
        popd
      fi
      if [ ! -d ${destination dependency} ]; then
          mv "$sourceRoot" ${destination dependency}
      fi
    ''
    else "";

  indexHiedb = destinationFn: dependency:
    if dependency ? src && dependency.src != null && dependency ? hie
    then ''
      ${hiedb}/bin/hiedb -D .hiedb index ${lib.escapeShellArg dependency.hie} --src-base-dir ${destinationFn dependency} --skip-types
    ''
    else "";

  indexDeps = indexHiedb destination;
in
  runCommand "dependencies" {}
  ''
    mkdir "$out"
    cd "$out"

    ${lib.concatMapStrings unpackDependency flattenedHaskellDependencies }

    ${lib.concatMapStrings indexDeps flattenedHaskellDependencies }
  ''
```

Then can be called on your dependencies
```
{
  lib,
}:
lib.indexHaskellDependencies {your haskell packages}
```
say saved at `nix/packages/dependencies` for example.

Finally you can use the following command to copy an indexed version of your dependencies into .hiedb which will enable functionality such as go-to-definition on dependencies as well ask:

```
	nix build .#pkgs.dependencies --no-link --print-out-paths | xargs -I_ cp --remove-destination _/.hiedb ./.hiedb --no-preserve=mode,ownership,timestamps
```
