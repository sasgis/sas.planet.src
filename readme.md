
#### How to compile SAS.Planet project

1. Set up the environment following this instruction: [sas.requires/readme.md](https://bitbucket.org/sas_team/sas.requires/src/default/readme.md)

2. Clone this repository to any place on your disk:

    `hg clone https://bitbucket.org/sas_team/sas.planet.src`
    
3. Download and extract all files from [latest release](https://bitbucket.org/sas_team/sas.planet.bin/downloads/) into *sas.planet.src\\.bin*

4. Download and extract all files from [latest nightly build](https://dl.bintray.com/zed/SASPlanet/) into *sas.planet.src\\.bin* (overwrite existing files)

5. Update maps: execute *sas.planet.src\\.bin\Maps\sas.maps\Update.cmd* and *sas.planet.src\\.bin\Maps\sas.plus.maps\UpdatePlus.cmd*

6. Now you can open `*.dproj` file (for example: *sas.planet.src\SASPlanet.Rio.dproj*) and compile it!

---

#### Links to the related repositories

- [sas.requires](https://bitbucket.org/sas_team/sas.requires) - third party open source libraries and components used in this project
- [sas.translate](https://bitbucket.org/sas_team/sas.translate) and [sas.translate.dev](https://bitbucket.org/sas_team/sas.translate.dev) - localization (based on gnugettext)
- [sas.planet.bin](https://bitbucket.org/sas_team/sas.planet.bin) - precompiled external dll's and other binary resources
- [sas.maps](https://bitbucket.org/sas_team/sas.maps) and [sas.plus.maps](https://bitbucket.org/sas_team/sas.plus.maps)
- [sas.nightly](https://bitbucket.org/sas_team/sas.nightly) - a set of utilities and scripts to automate the assembly of releases and nightly versions