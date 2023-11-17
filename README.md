# fpc-vim -- a narrowly focused Pascal mode for vim

Most support for modern Pascal seems wedded to the Delphi and Lazarus environments, combining language extensions and forms based development with large visual IDEs. Those are all excellent tools, but I just want to use Object Pascal (`mode:objfpc`) from the Free Pascal Compiler (`fpc`) for non-forms based programming. I like terminal and command line interfaces for a more old school feel. After several attempts to get the default Pascal support in base Vim working for me, I decided to bite the bullet, learn Vimscript, and roll my own language plugin.

Being retired, I have no project managers yelling at me to meet deadlines and stop writing tools instead of product. It's a satisfying life and one that allows me to take time to learn something old that's new to me. It also allows me to start over...more than once.

>> The management question, therefore, is not whether to build a pilot system and throw it away. You will do that...Hence plan to throw one away; you will, anyhow. -- Fred Brooks, _The Mythical Man Month_


## Plugin Features

The Standard Pascal and Object Pascal core languages are small but powerful. This plugin really only supports the core language syntax and basic compilation. Convention and defaults are preferred over configuration. When a setting is needed, say for tabsize, the value from Vim is used.

The following are the planned features of this plugin:

- Syntax highlighting for "standard" Pascal and common language extensions such as units, exceptions, common data types, and structured programming helpers such as `break`, `exit`, and `continue`.
- Indenting source
  - On demand via `gg=G` to invoke the `=` pipe filter.
  - Off while in insert mode, instead always take the prior line's indent and allow the user to indent or outdent. In Vim terms `set autoindent copyindent nocindent`. This is less disruptive to my flow state.
- Reformatting inspired by [gofmt](https://go.dev/blog/gofmt) but with more flexibility. This is distinct from indenting but should follow the same layout rules.
- Tags and navigation within a project's source.
- Compile and possibly GDB invocation from within Vim.


## Bugs

- [ ] Destroys formatting of lucid or literate function/procedure headers. Use the indent off and on directives to circumvent this.
- [ ] Indenting after single statement's under various statements is not restored correctly. `for ... do <nl> statement1; <nl> statement2; ` does not align `statement2` even with `for`. Gratuitous use of `begin ... end;` circumvents this.
- [ ] Indenting with a reserved word such as `for` in a trailing comment on a line containing other code can cause subsequent lines to indent as if they were under a `for`.
- [x] Does not indent names under `uses` directive.
- [ ] `then`, `else`, and `do` indent incorrectly under `if` and `for` respectively.
- [ ] not consistently recognizing  multiline block comments.


## Sources, Resources, and Credit Where Credit is Due

In addition to the base code in the Vim runtime, I have looked to Mattia72's vim-delphi plugin as a place to mine for code and ideas. While Delphi specific, his plugin is complete, well written, and well documented. See the [vim-delphi](https://github.com/mattia72/vim-delphi) repository on github.

Another complete, well written, and well documented language plugin can be found in JuliaEditorSupport's [julia-vim](https://github.com/JuliaEditorSupport/julia-vim) repository. Julia is not a language I expect to use, but the Julia community is clearly top notch.

Anyone wanting a guide on writing their own Vim plugins should read Steve Losh's [Learn Vimscript the Hard Way](https://learnvimscriptthehardway.stevelosh.com/) (which I refer to as lvsthw in my notes). Lua is all the rage, but Vimscript is still viable in Neovim, and there's a lot of useful Vimscript out there. 

"Standard Pascal" is the Pascal of _Pascal User Manual and Report_ by Jensen and Wirth, and ISO 7185 as in _Standard Pascal User Reference Manual_ by Cooper. While not mentioned in these standards, I consider `unit`s for separate compilation to be a _de facto_ standard.

"Object Pascal" is more difficult to pin down but I lean toward the OBJFPC mode of Free Pascal where there is support for objects, inheritance, properties, and methods. The line between language and library can be blurry in references, but the _The Object Pascal Handbook_ by Cantù and the Free Pascal documentation are good sources. 


## Authorship, Licensing, and Copyright

Vim and its runtime are released under the terms of the [Vim License](https://vimdoc.sourceforge.net/htmldoc/uganda.html#license). While I regard anything I write these days to be public domain, the terms of the Vim License apply to this plugin as well.


Troy Brumley  
blametroi@gmail.com  
So let it be written. So let it be done.  

