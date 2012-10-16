package org.colomoto.logicalmodel.io.boolsim;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

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
 * Import boolsim models.
 * 
 * @author Aurelien Naldi
 * @author Julien Dorier
 */
public class BoolSimImport {

	private final List<String> nodes = new ArrayList<String>();
	private final Map<String, String[]> m_functions = new HashMap<String, String[]>();

	public void parse(File f) throws FileNotFoundException {
		Scanner scanner = new Scanner(f);
		Set<String> knownNodes = new TreeSet<String>();
		while(scanner.hasNext()) {
			String curLine = scanner.nextLine();

			// sanity check for character that would mess up with the function parser
			char[] invalidChars = {'|', '(', ')', '!'};
			for (char c: invalidChars) {
				if (curLine.indexOf(c) >= 0) {
					throw new RuntimeException("Invalid character '"+c+"' in: "+curLine);
				}
			}

			// find the attribution symbol and sign of the function
			int positive = 0;
			int pos = curLine.indexOf("->");
			if (pos < 0) {
				pos = curLine.indexOf("-|");
				if (pos < 0) {
					System.err.println("Invalid line: "+curLine);
				}
				positive = 1;
			}
			
			String s_function = curLine.substring(0, pos).trim();
			String s_id = curLine.substring(pos+2).trim();
			
			// find all identifiers: some may not have a defined function
			String[] t_nodes = s_function.split("&");
			for (String node: t_nodes) {
				node = node.replace("^","").trim();
				knownNodes.add(node);
			}
			
			// transform syntax to be recognised by the internal parser
			s_function = s_function.replace("^","!");
			s_function = "("+s_function+")";
			String[] cur_functions = m_functions.get(s_id);
			if (cur_functions == null) {
				knownNodes.add(s_id);
				cur_functions = new String[2];
				m_functions.put(s_id, cur_functions);
			}

			if (cur_functions[positive] == null) {
				cur_functions[positive] = s_function;
			} else {
				cur_functions[positive] += " | "+s_function;
			}
		}

		scanner.close();
		nodes.addAll(knownNodes);
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
			String[] t_function = m_functions.get(s);
			if (t_function == null) {
				continue;
			}
			
			// turn positive and negative terms into a single function
			String s_function = t_function[0];
			if (s_function == null) {
				s_function = "!("+t_function[1]+")";
			}
			else if (t_function[1] != null){
				s_function = "("+s_function+") & !("+t_function[1]+")";
			}

			if (s_function != null) {
				FunctionNode fnode = parser.compile(opFactory, s_function);
				functions[ i ] = fnode.getMDD(ddmanager);;
			}
			i++;
		}

		return new LogicalModelImpl(modelNodes, ddmanager, functions);
	}
}
