# emacs.d

Personal Emacs configuration

## Install

### Node.js dependencies

Install `tern` globally:

```
npm install -g tern
# cd /usr/bin && ln -s /Users/sgmonda/.nvm/versions/node/v10.6.0/bin/tern
```

Install `eslint_d` globally:

```
npm install -g eslint_d
```

### Emacs

Clone this repository and link it from `~/.emacs.d`.

Open Emacs and let it install and prepare everything. You'll need to confirm some network connections. Just respond `a` (always) if asked.

Install font icons

```
M-x all-the-icons-install-fonts
```

# Features

#### Multiple region-replacing

Move cursor to a word and press `C-,`. All occurrences of that word are highlighted and can be modified at once.



See [`iedit-mode`](https://github.com/victorhge/iedit). 
