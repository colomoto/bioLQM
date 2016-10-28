package org.colomoto.logicalmodel.io.avatar;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Reads an Avatar Document from an .AVATAR file
 *  
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
public final class AvatarReader {

	/**
	 * Creates and populates an Avatar Document from an .AVATAR file
	 * @param f the .AVATAR file
	 * @return the populated Avatar Document (with the associated variables, clauses, initializations and oracles)
	 * @throws NumberFormatException
	 * @throws IOException
	 * @throws AvatarLogicalModelException
	 */
	public static AvatarDocument loadAvatarDocumentFromFile(File f) throws NumberFormatException, IOException, AvatarLogicalModelException {
		AvatarDocument document = new AvatarDocument();
		//System.out.println("Loading Avatar document from file");
		BufferedReader br = new BufferedReader(new FileReader(f));
		
	    String section="", currentComponent="", line;
	    List<AvatarClause> definitionClauses = null; 
	    
	    while((line=br.readLine())!=null){
	    	if(line.startsWith("--")||line.matches("[ ]*")) continue;

		    if(line.startsWith("INIT")){
		    	String[] parts = line.split(" |;")[1].split("=");
		    	String component = parts[0];
		    	int state = Integer.valueOf(parts[1]);
		    	if(!document.hasComponent(component)) throw new AvatarLogicalModelException("Cannot specify value of unknown component named "+component); 
		    	else if(state>=document.getNumberStates(component)) throw new AvatarLogicalModelException("Cannot initialize component named "+component+" (with "+document.getNumberStates(component)+" states) in state "+state);
		    	document.addInitRestriction(component,state);
		    	continue;
		    }
		    if(line.matches("[A-Z]+")){ //find section for following parts
		    	section = line;
		    	continue;
		    }		    
		    if(section.contains("VAR")){ //VAR, IVAR, FROZENVAR
		    	line=line.replace(" ","").replace("{|}|;","");
		    	String[] parts=line.split(":");
		    	String component = parts[0];
		    	int nrStates = parts[1].split(",").length;
		    	if(section.startsWith("VAR")) document.addVar(component, nrStates);
		    	else if(section.startsWith("IVAR")) document.addIVar(parts[0],parts[1].split(",").length);
	        } 
	        
		    /** Definitions **/
		    else if(section.startsWith("DEFINE")){
	        	if(line.contains(":=")){
	        		String component = line.split("_")[0];
			    	if(!document.hasComponent(component)) throw new AvatarLogicalModelException("Cannot specify a definition for the unknown component "+component); 
			    	definitionClauses = new ArrayList<AvatarClause>();
	        		currentComponent = component;
	        	}
	        	else if(line.contains("case")) continue;
	        	else if(line.contains("esac")){
	        		List<AvatarClause> clone = new ArrayList<AvatarClause>(); 
	        		clone.addAll(definitionClauses);
	        		document.addDefinition(currentComponent,clone);
	        	} 
	        	else {
		 	    	line=line.replace(" ","").replace("(","").replace(")","").replace(";","");
		            String[] parts = line.split(":");
		            AvatarClause clause = new AvatarClause();
		            clause.addOutput(Integer.valueOf(parts[1]));
	            	if(parts[0].equals("TRUE")){
		            	int state = Integer.valueOf(parts[1]);
				    	if(state>=document.getNumberStates(currentComponent)) throw new AvatarLogicalModelException("Cannot assign default state {"+state+"} to component named "+currentComponent+" (with "+document.getNumberStates(currentComponent)+" states)");
				    	clause.addRestriction("DEFAULT",state);
	            	} else {
			            for(String assign : parts[0].split("&")){
			            	String component = assign.split("=")[0];
			            	int state = Integer.valueOf(assign.split("=")[1]);
					    	if(!document.hasComponent(component)) throw new AvatarLogicalModelException("Cannot specify value of unknown component named "+component); 
					    	else if(state>=document.getNumberStates(component)) throw new AvatarLogicalModelException("Cannot initialize component named "+component+" (with "+document.getNumberStates(component)+" states) in state "+state);
			            	clause.addRestriction(component, state);
			            }
		            }
	            	definitionClauses.add(clause);
		        }
		    }
		    
		    /** Attractors and Oracles **/
	        else if(section.startsWith("ATTRACTORS")||section.startsWith("ORACLES")){
	        	//optionally define an Oracle identifying a member of a complex attractor via a pattern
		        if(line.contains(":=")) continue;
		        String simpleLine=line.replace(" ","").replace("|","").replace(";","");
	            Map<String,Integer> assignment = new HashMap<String,Integer>();
		        for(String assign : simpleLine.split("&")){
		        	String component = assign.split("=")[0];
		            int state = Integer.valueOf(assign.split("=")[1]);
				    if(!document.hasComponent(component)) throw new AvatarLogicalModelException("Cannot specify oracle for unknown component named "+component); 
				    else if(state>=document.getNumberStates(component)) throw new AvatarLogicalModelException("Cannot initialize component named "+component+" (with "+document.getNumberStates(component)+" states) in state "+state+" as part of an oracle!");
		            assignment.put(component, state);
		        }
		    	document.addOraclePart(assignment);
		    }
	    }
		System.out.println("Avatar document successfully loaded from file");
	    return document;
	}

	/**
	 * Checks the soundness of the syntax of an .AVATAR file
	 * @param f the .AVATAR file
	 * @throws AvatarLogicalModelException
	 * @throws FileNotFoundException
	 */
	public static void checkFileSyntax(File f) throws AvatarLogicalModelException, FileNotFoundException {

		String section="", line="";
		BufferedReader br = new BufferedReader(new FileReader(f));
		
		try {
		    while((line=br.readLine())!=null){
		    	if(line.startsWith("--")||line.matches("[ ]*")) continue;
	
			    if(line.startsWith("INIT")){
			    	String[] parts = line.split(" |;")[1].split("=");
			    	Integer.valueOf(parts[1]);
			    	continue;
			    }
			    if(line.matches("[A-Z]+")){ //find section for following parts
			    	section = line;
			    	continue;
			    }		    
			    if(section.contains("VAR")){ //VAR, IVAR, FROZENVAR
			    	line=line.replace(" ","").replace("{|}|;","");
			    	String[] parts=line.split(":");
			    	int test = parts[1].split(",").length;
		        } 
			    else if(section.startsWith("DEFINE")){
		        	if(line.contains(":=")){ 
		        		String test = line.split("_")[0];
		        	} else if(line.contains("case")) continue;
		        	else if(line.contains("esac")) continue; 
		        	else {
			 	    	line=line.replace(" ","").replace("(","").replace(")","").replace(";","");
			            String[] parts = line.split(":");
			            Integer.valueOf(parts[1]);
		            	if(parts[0].equals("TRUE")) Integer.valueOf(parts[1]);
		            	else for(String assign : parts[0].split("&")) Integer.valueOf(assign.split("=")[1]);
			        }
			    }
		        else if(section.startsWith("ATTRACTORS")||section.startsWith("ORACLES")){
			        if(line.contains(":=")) continue;
			        String simpleLine=line.replace(" ","").replace("|","").replace(";","");
			        for(String assign : simpleLine.split("&")) Integer.valueOf(assign.split("=")[1]);
			    }
		    }
		} catch(Exception e) {
			throw new AvatarLogicalModelException("Syntax error on line: \""+line+"\"");
		}
	}

}
