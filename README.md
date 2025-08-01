# c2lp (C to List Prolog)
Converts C algorithms to List Prolog algorithms

# Prerequisites

* Use a search engine to find the Homebrew (or other) Terminal install command for your platform and install it, and search for the Terminal command to install swipl using Homebrew and install it or download and install SWI-Prolog for your machine at <a href="https://www.swi-prolog.org/build/">SWI-Prolog</a>.

# Mac, Linux and Windows (with Linux commands installed): Prepare to run swipl

* In Terminal settings (Mac), make Bash the default shell:

```
/bin/bash
```

* In Terminal, edit the text file `~/.bashrc` using the text editor Nano:

```
nano ~/.bashrc
```

* Add the following to the file `~/.bashrc`:

```
export PATH="$PATH:/opt/homebrew/bin/"
```

* Check if `usr/local/bin` exists

```
ls -ld /usr/local/bin
```

* Create the directory if missing

```
sudo mkdir -p /usr/local/bin
```

* Link to swipl in Terminal

```
sudo ln -s /opt/homebrew/bin/swipl /usr/local/bin/swipl
```

# 1. Install manually

Download <a href="http://github.com/luciangreen/c2lp/">this repository</a> and the <a href="https://github.com/luciangreen/listprologinterpreter">List Prolog interpreter</a> and its dependencies.

# 2. Or Install from List Prolog Package Manager (LPPM)

* Download the <a href="https://github.com/luciangreen/List-Prolog-Package-Manager">LPPM Repository</a>:

```
mkdir GitHub
cd GitHub/
git clone https://github.com/luciangreen/List-Prolog-Package-Manager.git
cd List-Prolog-Package-Manager
swipl
['lppm'].
lppm_install("luciangreen","c2lp").
../
halt.
```

# Running Prolog to List Prolog

* In Shell:
`cd c2lp`
`swipl`
`['c2lpconverter.pl'].`

Run:

Convert Prolog code to List Prolog code by copying Prolog algorithm into `test1.pl` and running: `c2lpconverter(S1),pp0(S1,S2),writeln(S2).`

e.g.
```
p(*grid1,*grid2){add_to_grid(*grid1,*grid3);}

p(*grid1,*grid2)
{
 add_to_grid(*grid1,*grid3);
 add_to_grid(*grid1,*grid3);
}

[
[[n,p],[[v,grid1],[v,grid2]],":-",
[
	[[n,add_to_grid],[[v,grid1],[v,grid3]]]
]],
[[n,p],[[v,grid1]],":-",
[
	[[n,add_to_grid],[[v,grid3]]]
]]
]
```


