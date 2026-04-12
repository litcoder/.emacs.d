# .emacs.d
My personal emacs init file and configurations.

```
git clone --recurse-submodules git@github.com:litcoder/.emacs.d.git
```

Or
```
git clone git@github.com:litcoder/.emacs.d.git
cd .emacs.d
git submodule update --init --recursive
```

# Dependencies

* Ubuntu
```
sudo apt install black clang-format clang-format-15
```


## Refresh package contents (ELPA, MELPA)
```
M-x package-refresh-contents
```

## Install packages described in init.el
```
M-x package-install-selected-packages
```

## Install packages
```
M-x package-install
```

Type package names.
```
tramp
clang-format
copilot
```

## Copilot
```
M-x copilot-install-server
M-x copilot-login

M-x global-copilot-mode
```
