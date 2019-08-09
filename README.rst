==============================================
STRUCT Toolbox 
==============================================

|Git| |Bioconda| |Build Status (Travis)| |License| |Coverage| |AppVeyor| |mybinder|


------------
Install
------------


Github
------------

.. code-block:: r

  library(devtools)
  library(testthat)
  install_github('computational-metabolomics/structToolbox')

Conda
------------

.. code-block:: command

   conda create -n structtoolbox bioconductor-structtoolbox -c conda-forge -c bioconda -c computational-metabolomics
   source activate structtoolbox

------------
References
------------


.. |Build Status (Travis)| image:: https://img.shields.io/travis/computational-metabolomics/structToolbox/master.svg?label=Travis
   :target: https://travis-ci.org/computational-metabolomics/structToolbox

.. |Build Status (AppVeyor)| image:: https://ci.appveyor.com/api/projects/status/github/computational-metabolomics/structToolbox?branch=master&svg=true
   :target: https://ci.appveyor.com/project/computational-metabolomcis/structToolbox

.. |Git| image:: https://img.shields.io/badge/repository-GitHub-blue.svg?style=flat&maxAge=3600
   :target: https://github.com/computational-metabolomics/structToolbox

.. |Bioconda| image:: https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat&maxAge=3600
   :target: https://bioconda.github.io/recipes/bioconductor-structtoolbox/README.html

.. |License| image:: https://img.shields.io/badge/licence-GNU_v3-teal.svg?style=flat&maxAge=3600
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. |Coverage| image:: https://codecov.io/gh/computational-metabolomics/structToolbox/branch/master/graph/badge.svg
   :target: https://codecov.io/gh/computational-metabolomics/structToolbox

.. |AppVeyor| image:: https://ci.appveyor.com/api/projects/status/github/computational-metabolomics/structToolbox?branch=master&svg=true
   :target: https://ci.appveyor.com/project/RJMW/structToolbox

.. |mybinder| image:: https://mybinder.org/badge_logo.svg
   :target: https://mybinder.org/v2/gh/computational-metabolomics/structToolbox/master?filepath=notebooks
