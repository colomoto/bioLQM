# BioLQM: Logical Qualitative Models of biological networks

BioLQM is a toolkit for the manipulation and analysis of Logical (Qualitative) Models
of biological networks. A model is composed of a list of components, associated to Boolean
or multi-valued activity levels and to dynamical rules driving changes in their activity
levels.

BioLQM provides a collection of *import/export formats*, enabling the exchange of models
between complementary tools (motivated by the [CoLoMoTo](www.colomoto.org) discussion
group) and the design of complex analysis workflows.
It also allows to define *model modifications*, to represent biological modifications 
(mutations) or to facilitate further analysis (model reduction, Boolean mapping of
multi-valued models).
Finally, it provides some analysis tools, focussing on the identification of attractors
and on building-blocks for model simulation.


## How to use it?


It can be used either as a standalone command-line tool, or as a programming interface:
the Java API can be integrated in other software or used through scripts.
Documentation and further information are available on http://colomoto.org/biolqm/.

* Install instructions: http://colomoto.org/biolqm/doc/install.html.


## Licence and authors

This code is available under LGPL v3+/CeCILL-C.

The code started as a refactoring of [GINsim](http://www.ginsim.org) to cleanup its core and enable more code sharing.
Discussions among [CoLoMoTo](http://www.colomoto.org) participants showed a growing interest for improved interoperability.
This motivated the [qualitative extension](http://sbml.org/Community/Wiki/SBML_Level_3_Proposals/Qualitative_Models)
for SBML and this toolkit.


Aurelien Naldi designed and maintains the project, with feedback and contributions from:

Claudine Chaouiya  
Denis Thieffry  
Duncan Berenguier  
Francisco Plana  
Gautier Stoll  
Julien Dorier  
Loïc Pauleve  
Pedro Monteiro  
Celine Hernandez  
Rui Henriques  
Martin Boutroux  

