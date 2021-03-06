TodoMVC: Purescript on Native Android
=====================================

The repository contains a PoC implementation of TodoMVC on native android.

It is implemented in purescript using the
kotlin backend [pskt](https://github.com/csicar/pskt).

![screenshot.png](screenshot.png)

Structure
---------

```
.
├── android           -- android studio project
│   └── app
│       └── src
│           ├── main  -- kotlin source folder (containing the foreign files and generated files)
│           └── test
└── ps                -- spago project
    ├── output        -- purs compiler output
    │   ├── ...
    │   └── pskt      -- pskt transpiler output (containing .kt files)
    └── src           -- purscript source files
```


Building
--------

Build and copy kotlin files:

```bash
cd ps
spago build && cp -u -a output/pskt/. ../android/app/src/main/java/generated
```

Build & install the app:

```bash
cd android
git clone https://github.com/csicar/pskt-foreigns app/src/main/java/foreigns
gradle installDebug
```

or open the `/android` folder in Android Studio and build the app there.

Nix environment
---------------

nix-env -iE "_: with import <nixpkgs>{ config.android_sdk.accept_license = true;}; [androidenv.androidPkgs_9_0.androidsdk]"
./sdkmanager "platform;29"
./sdkmanager "build-tools;29.0.2"

for tools like ghcid
npm i -g pscid

run with:
pscid
