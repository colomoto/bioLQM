package org.colomoto.logicalmodel.io.rawfunctions;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.logicalfunction.FunctionNode;
import org.colomoto.mddlib.logicalfunction.FunctionParser;
import org.colomoto.mddlib.logicalfunction.OperandFactory;
import org.colomoto.mddlib.logicalfunction.SimpleOperandFactory;

/**
 * Simple import reading logical functions in a text file.
 * 
 * @author Aurelien Naldi
 */
public class RawFunctionImport {
	
	private final List<String> nodes = new ArrayList<String>();
	private final Map<String, String> m_functions = new HashMap<String, String>();
	
	public void parse(File f) throws FileNotFoundException {
		Scanner scanner = new Scanner(f);
		while(scanner.hasNext()) {
		    String curLine = scanner.nextLine();
		    int pos = curLine.indexOf(':');
		    String s_id = curLine.substring(0, pos).trim();
		    String s_function = curLine.substring(pos+1).trim();
		    
		    if (m_functions.containsKey(s_id)) {
		    	System.err.println("Duplicates ID, new function will be discarded");
		    } else {
		    	nodes.add(s_id);
		    	m_functions.put(s_id, s_function);
		    }
		}

		scanner.close();
	}
	
	public LogicalModel getModel() {
		
		List<NodeInfo> modelNodes = new ArrayList<NodeInfo>();
		for (String s: nodes) {
			modelNodes.add( new NodeInfo(s));
		}
		
		FunctionParser parser = new FunctionParser();
		OperandFactory opFactory = new SimpleOperandFactory<NodeInfo>(modelNodes);
		MDDManager ddmanager = opFactory.getMDDManager();
		
		int[] functions = new int[nodes.size()];
		int i=0;
		for (String s: nodes) {
			String s_function = m_functions.get(s);
			if (s_function != null) {
				FunctionNode fnode = parser.compile(opFactory, s_function);
				functions[ i ] = fnode.getMDD(ddmanager);;
			}
			i++;
		}
		
		return new LogicalModelImpl(modelNodes, ddmanager, functions);
	}
}
