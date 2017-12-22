# msc-phygeo-class-of-2017-creuden

Whenever you run in trouble open an issue
https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/issues
describe the problem with source code and add your sessionInfo() output


# Requirements
For running the examples your platform needs a bunch of third party software. The most comfortable way to fulfill most of them is to install QGIS, GRASS- and SAGA-GIS In Addition you need the Fusion tools for the fusion processing chain of LiDAR data. Following the [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will ensure a smooth working environment.

The [Fusion toolset](http://forsys.sefs.uw.edu/fusion/fusionlatest.html) is available at the developer homepage. Please download it and install it as usual. Note you have to adapt the Installation folder in the ``controlFusion.txt`` file. Running a default installation on Windows you do not need to change the path. Running Fusion tools under Linux you need to install wine ``sudo apt-get install wine winetricks``. Then you have to adapt the path in line 21. The usual case is to change the user home directory 

``"LC_CTYPE=de_DE.utf8 wine /home/$USER/.wine/dosdevices/c:/FUSION/"``

To install the functions you need to [download](https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/archive/master.zip) and unpack the master zip.


