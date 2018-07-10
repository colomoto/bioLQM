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

## Install

BioLQM is distributed as cross-platform binaries, relying on the Java Runtime (JRE), see
the github releases and http://colomoto.org/biolqm/.
While most features are implemented in Java and should work without further installation,
some analysis rely on the clingo ASP solver, available on https://potassco.org/.

After downloading the jar file for a release, run:

    java -jar bioLQM-0.6.1.jar


### Conda package

BioLQM is included in the conda package for GINsim. In a working conda environment, the
following command will to install GINsim, BioLQM, and their dependencies.

    conda install -c colomoto ginsim

A *bioLQM* command should then be available in your environment.
These packages are tested on Linux (x64), but should also work on Mac OSX and Windows platforms.

These conda packages are used to build a consistent and reproducible environment integrating
several software tools for the analysis of qualitative models, as described in
https://colomoto.github.io/colomoto-docker/.


### Build from source

Requirements: java 8 (JDK) and [maven](http://maven.apache.org/).

* grab the source from github (from git or a release archive)
* run "mvn package" to compile and package it
* you can then use the jar in the "target" subdirectory. If you want move the jar file 
to another folder, you should also take the "lib" folder (which contains dependencies).


### Maven repository

To integrate BioLQM in your Java software, you can use our Maven repository as follows:

      <repositories>
        <repository>
           <id>ginsim_snapshot</id>
           <name>GINsim snapshot repository</name>
           <snapshots><enabled>true</enabled></snapshots>
           <releases><enabled>false</enabled></releases>
           <url>http://ginsim.org/maven/snapshot</url>
        </repository>
        <repository>
           <id>ginsim_stable</id>
           <snapshots><enabled>false</enabled></snapshots>
           <releases><enabled>true</enabled></releases>
           <name>GINsim deps</name>
           <url>http://ginsim.org/maven/stable</url>
        </repository>
      </repositories>
    
      <dependencies>
        <dependency>
          <groupId>org.colomoto</groupId>
          <artifactId>bioLQM</artifactId>
          <version>0.6-SNAPSHOT</version>
        </dependency>
      </dependencies>



## Licence and authors

This code is available under LGPL v3+/CeCILL-C.

The code started as a refactoring of [GINsim](http://www.ginsim.org) to cleanup its core and enable more code sharing.
Discussions among [CoLoMoTo](http://www.colomoto.org) participants showed a growing interest for improved interoperability.
This motivated the [qualitative extension](http://sbml.org/Community/Wiki/SBML_Level_3_Proposals/Qualitative_Models)
for SBML and this toolkit.


Aurelien Naldi   
Pedro Monteiro  
Duncan Berenguier  
Loïc Pauleve  
Francisco Plana  
Rui Henriques  
Julien Dorier  
Gautier Stoll  

The rest of the GINsim crew provided insight:   
Claudine Chaouiya  
Denis Thieffry  

