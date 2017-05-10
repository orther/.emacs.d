Bryan's emacs (based on the absolutely amazing [emacs configuration by hlissner](https://github.com/hlissner/.emacs.d))

## Installation

```bash
git clone https://github.com/gilbertw1/bmacs ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
make compile       # optional, may take a while
make compile-lite  # optional (lighter alternative to compile)
```

Run `make` after making changes, which is the equivalent of:

```bash
make install       # or (doom/packages-install)
make autoloads     # or (doom/reload-autoloads)
```

## Keybindings

```
modules/private/gilbertw1/+bindings.el
```
