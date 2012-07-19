Logical Model
=================

The aim of this project is to improve interoperability between logical modeling tools,
motivated by the [CoLoMoTo](www.colomoto.org) discussion group.
For this, we define some core interfaces for the definition of Logical Models,
as well as some tools for their manipulation.

For now it is fairly limited, but we aim to add export capabilities to various formats,
notably SBML-qual and GINsim's GINML. Contributions to support more formats are welcome!

We also want to allow tight integration in two forms:
* Model editors can support this data structure and use directly the provided tools and exports.
* Some analysis tools could provide bridges, allowing to be used from such editors.
* Let us know what you would like to do with it...



How to use it?
--------------

As this is still work in progress, we do not have real releases yet,
so you will have to compile it, but it is fairly easy!
To compile it you will need is java6 JDK and [maven](http://maven.apache.org/).

* grab the source from github
* run "mvn package" to compile and package it
* you can use the jar in the "target" subdirectory. It needs the "lib" folder that maven filled for you.

For now, the included command-line tool only does model conversion as follow:

    java -jar LogicalModel-0.2.jar <your_input_file.sbml>  <your_output_file.ginml>

It will guess the desired formats based on file extensions.
For now it supports SBML import/export and GINML export, but more formats can be added.
Adding new filters is easy as writing the code and a simple format declaration file to integrate it.

It relies on [JSBML](http://sbml.org/Software/JSBML) for SBML-qual import/export, 
and on a small [MDD manipulation toolkit](https://github.com/aurelien-naldi/mddlib).


This is integrated into GINsim and an adaptation as cytoscape plugin is ongoing.
In both cases, the external tool will provide a model editor, some specific tools and thin wrapper around
these common tools and exports. Further work will be needed to share some pieces of the graphical interface.

[Javadoc snapshots](http://aurelien-naldi.github.com/gh-documentation/) are also available.


Where does it come from?
------------------------

Most of this comes from a refactoring of [GINsim](http://gin.univ-mrs.fr/GINsim) to cleanup its core and enable more code sharing.
Discussions among [CoLoMoTo](www.colomoto.org) participants showed a growing interest for improved interoperability.
This motivated the [qualitative extension](http://sbml.org/Community/Wiki/SBML_Level_3_Proposals/Qualitative_Models)
for SBML and this common toolbox.



Licence
-------

This code is available under LGPL v2+/CeCILL-C.


Authors
-------

Aurelien Naldi   
Duncan Berenguier  
Add your name here ;)   

The rest of the GINsim crew provided insight:   
Pedro Monteiro  
Claudine Chaouiya  
Denis Thieffry  
