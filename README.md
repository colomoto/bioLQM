bioLQM: Logical Qualitative Models of biological networks
=========================================================

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

    java -jar bioLQM-0.4.jar <your_input_file.sbml>  <your_output_file.ginml>

It will guess the desired formats based on file extensions.
Launch it without arguments to see the available formats and extra options.
For now it supports the following formats (< and > denote import/export capabilities):
  * <> [SBML-qual](http://sbml.org/Community/Wiki/SBML_Level_3_Proposals/Qualitative_Models)
  * <> raw logical functions
  * <> boolsim
  *  > [GINML](http://doc.ginsim.org/format-ginml.html)
  *  > [GNA](http://ibis.inrialpes.fr/article122.html)
  *  > Petri net (INA, PNML, APNN)
  *  > [Pint](http://loicpauleve.name/pint)
  * <> [Truth table](http://doc.ginsim.org/format-truthtable.html)

The integration of a filter only requires a simple format declaration class.
These classes are automatically detected using some java annotations.
(see subpackages of org.colomoto.biolqm.io)


It relies on [JSBML](http://sbml.org/Software/JSBML) for SBML-qual import/export, 
and on a small [MDD manipulation toolkit](https://github.com/colomoto/mddlib).


This is also integrated into GINsim, which provides a model editor,
some specific tools and thin wrapper around the common tools and exports.

[Javadoc snapshots](http://colomoto.github.com/gh-documentation/) are also available.


Where does it come from?
------------------------

Most of this comes from a refactoring of [GINsim](http://www.ginsim.org) to cleanup its core and enable more code sharing.
Discussions among [CoLoMoTo](http://www.colomoto.org) participants showed a growing interest for improved interoperability.
This motivated the [qualitative extension](http://sbml.org/Community/Wiki/SBML_Level_3_Proposals/Qualitative_Models)
for SBML and this common toolbox.



Licence
-------

This code is available under LGPL v3+/CeCILL-C.


Authors
-------

Aurelien Naldi   
Pedro Monteiro  
Duncan Berenguier  
Loïc Pauleve  
Francisco Plana  
Rui Henriques  
Julien Dorier  
Gautier Stoll  
Add your name here ;)   

The rest of the GINsim crew provided insight:   
Claudine Chaouiya  
Denis Thieffry  

