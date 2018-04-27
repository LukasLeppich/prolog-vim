> # This plugin is not ready for production use!



# VIM-Plugin for prolog

This plugin is the result of a master internship at the University of Würzburg.
It uses the [Language Server Protocol v2.x](https://github.com/Microsoft/language-server-protocol/blob/master/versions/protocol-2-x.md) to provide a simple autocompletion for prolog in vim.

The plugin is splitted into three parts:

* The VIM plugin in `autoload`, `ftdetect` and `ftplugin`. It handles the filetype, starts the python scripts and sets the `omnifunc` for autocompletion. 
* The python part of the plugin in `script`. It is used start the language server and handles the communication.
* The prolog language server in `script/server`. The server is used to analyze the prolog source files and provide a simple autocomplete functionality.



## Instalation
Copy the directory to `~.vim/bundle/` and add the following line to your `~.vimrc`:
```
set runtimepath+=~/.vim/bundle/prolog-vim
```
You can also use a plugin manager like [Vundle](https://github.com/VundleVim/Vundle.vim) to install the plugin.

```
Plugin LukasLeppich/prolog-vim
```