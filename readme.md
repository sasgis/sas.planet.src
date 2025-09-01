
SAS.Planet is a free, open-source Geographic Information System (GIS) software designed for viewing, downloading, and managing high-resolution satellite imagery and conventional maps from various online sources.

### Key Features

* **Access to Multiple Map Services:** It aggregates maps and satellite images from numerous providers, including Google Maps, Bing Maps, OpenStreetMap, Yandex.Maps, and others. This allows users to compare different map layers for the same location.
* **Offline Use:** A primary feature is its ability to download and cache map tiles, enabling users to view maps offline without an internet connection. This is particularly useful for navigation and analysis in areas with limited or no connectivity.
* **Data Management:** Users can create and manage placemarks, measure distances and areas, and plot routes. It also supports the import and export of data in various formats (e.g., KML, GPX).
* **Functionality:** Beyond simple viewing, SAS.Planet can stitch together downloaded tiles to create a single large map image. It can also be connected to a GPS receiver to function as a navigation tool.

---

### Building from Source

For those who wish to compile the program themselves, the process involves setting up a specific environment and combining files from several repositories.

#### Build Process Overview

1. **Set Up Environment:** Prepare your development environment by following the instructions in the [sas.requires](https://github.com/sasgis/sas.requires/blob/master/readme.md) repository.
2. **Clone Source Code:** Clone the main source code repository: `git clone https://github.com/sasgis/sas.planet.src`.
3. **Add Binaries:** Download and extract the **[latest release](https://github.com/sasgis/sas.planet.src/releases/latest)** and then the **[latest nightly build](https://github.com/sasgis/sas.planet.src/releases/tag/nightly)** files into the `.bin\win32` (`.bin\win64` for x64 target) subdirectory of the source code folder.
4. **Download Maps:** Run the `Update.cmd` script located in the `Maps\sas.maps` folder to get the latest map definitions.
5. **Compile:** Open the Delphi project file `SASPlanet.dproj` in your IDE and compile the application.

#### Related Repositories

The project is modular and relies on several related repositories:

* **[sas.requires](https://github.com/sasgis/sas.requires):** Contains third-party open-source libraries and components needed for the build.
* **[sas.planet.bin](https://github.com/sasgis/sas.planet.bin):** Holds precompiled external DLLs and other necessary binary resources.
* **[sas.translate](https://github.com/sasgis/sas.translate)** and **[sas.translate.dev](https://github.com/sasgis/sas.translate.dev):** Manages localization and language files for the user interface.
* **[sas.nightly](https://github.com/sasgis/sas.nightly):** Includes scripts and utilities used to automate the assembly of nightly builds and official releases.
* **[sasgis.github.io](https://github.com/sasgis/sasgis.github.io):** Documentation.