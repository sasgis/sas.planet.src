
#### How to compile SAS.Planet project

1. Set up the environment following by this instruction: [sas.requires/readme.md](https://github.com/sasgis/sas.requires/blob/master/readme.md)

2. Clone this repository to any place on your disk:

    `git clone https://github.com/sasgis/sas.planet.src`
    
3. Download and extract all files from [latest release](https://bitbucket.org/sas_team/sas.planet.bin/downloads/) into *sas.planet.src\\.bin*

4. Download and extract all files from [latest nightly build](https://bitbucket.org/sas_team/sas.planet.bin/downloads/) into *sas.planet.src\\.bin* (overwrite existing files)

5. Update maps: execute *sas.planet.src\\.bin\Maps\sas.maps\Update.cmd*

6. Now you can open `*.dproj` file (for example: *sas.planet.src\SASPlanet.Rio.dproj*) and compile it!

---

#### Links to the related repositories

- [sas.requires](https://github.com/sasgis/sas.requires) - third party open source libraries and components used in this project
- [sas.translate](https://github.com/sasgis/sas.translate) and [sas.translate.dev](https://github.com/sasgis/sas.translate.dev) - localization (based on gnugettext)
- [sas.planet.bin](https://github.com/sasgis/sas.planet.bin) - precompiled external dll's and other binary resources
- [sas.nightly](https://github.com/sasgis/sas.nightly) - a set of utilities and scripts to automate the assembly of releases and nightly versions