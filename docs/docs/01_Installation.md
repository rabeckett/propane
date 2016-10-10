The easiest way to install the Propane compiler and toolchain is to **download the pre-packaged Virtualbox image** (see below). The Virtualbox VM comes already installed with the Propane compiler and integration with the CORE network emulator.

## Virtualbox Image (Recommended)

Download the Virtualbox image for Propane [here](http://www.cs.princeton.edu/~rbeckett/Propane.ova).

Next, load the VM into Virtualbox by going to `File > Import Appliance` and selecting the Propane VM image (.ova file).

The **username and password** for the VM are both `core`

## From Source (Advanced)

Propane can also be installed from source. Note this has not been tested thoroughly on linux. Requirements:

  * .NET platform for [Windows](https://www.microsoft.com/en-us/download/details.aspx?id=49981), [Mac](http://www.mono-project.com/download/#download-mac), or [Linux](http://www.mono-project.com/download/#download-lin) (version 4.0 or later)
  * The F# language [runtime and compiler](http://fsharp.org/) (version 4.0 or later)
  * For testing, download the [CORE network emulator](http://www.nrl.navy.mil/itd/ncs/products/core) (The pre-packaged VM works best)

#### Download

First, download the Propane source code from the git repository:

```
git clone https://github.com/rabeckett/propane.git
```

#### Build

There should now be a `propane` directory. In this directory, there will be a solution file: `propane.sln`.
First grab any missing project dependencies with the following command: 

```
nuget restore propane.sln
```

Next, build the executable by running the `xbuild` command (Linux/Mac), or the `msbuild` command (Windows). For example, on Linux/Mac you would run:

```
xbuild propane.sln
```

The Propane compiler binary will now be located at `<main dir>/src/bin/Release/propane.exe`.

#### Add an alias

Next, add an alias for the compiler. For example, on Mac you would add the following line to `~/.bashrc`: 

`alias propane="mono <main dir>/src/bin/Release/propane.exe"`

You can test to see if Propane is working now by running the following command:

``` 
propane --help
```