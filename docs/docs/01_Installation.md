The easiest way to install the Propane compiler and toolchain is to **download the pre-packaged Virtualbox image** (see below). The Virtualbox VM comes already installed with the Propane compiler and integration with the CORE network emulator.

## Virtualbox Image (Recommended)

Download the Virtualbox image for Propane [here](http://www.cs.princeton.edu/~rbeckett/Propane.ova).

Next, load the VM into Virtualbox by going to `File > Import Appliance` and selecting the Propane VM image (.ova file).


## From Source (Advanced)

Propane can also be installed from source. Note this has not been tested thoroughly on linux. Requirements:

  * .NET platform (download for [Mac](http://www.mono-project.com/download/#download-mac) or [Linux](http://www.mono-project.com/download/#download-lin) )
  * The F# language [runtime and compiler](http://fsharp.org/)
  * Optionally download [Visual Studio](https://beta.visualstudio.com/vs/community/) for Windows or [Xamarin Studio](https://www.xamarin.com/download) for Mac/Linux
  * For testing, the [CORE network emulator](http://www.nrl.navy.mil/itd/ncs/products/core)

#### Download

First, download the Propane source code from the git repository:

```
git clone https://github.com/rabeckett/propane.git
```

#### Build

In the main directory, open the `propane.sln` file in either Visual Studio or Xamarin Studio.

Run the build command for the project (e.g., `Build > Build All`). 

Alternatively, you can run `xbuild propane.sln` from the command line.

The Propane compiler binary will now be located at `<main dir>/src/bin/Release/propane.exe`.

#### Add an alias

Next, add an alias for the compiler. For example, on Mac you would add the following line to `~/.bash_profile`: 

`alias propane="mono <main dir>/src/bin/Release/propane.exe"`

You can test to see if Propane is working now by running the following command:

``` 
propane --help
```