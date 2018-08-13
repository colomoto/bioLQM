package org.colomoto.biolqm.io.truthtable;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.BaseLoader;
import org.colomoto.biolqm.io.StreamProvider;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

/**
 * Imports a file in the Truth Table format into a LogicalModel.
 * 
 * @author Pedro T. Monteiro
 */
public class TruthTableImport extends BaseLoader {

	private final String SEPARATOR = "\\s+";

	public TruthTableImport(StreamProvider streams) {
		super(streams);
	}

	/**
	 * From a BufferedReader it gets the next valid line, ignoring empty lines
	 * and comments starting with the character '#'.
	 * 
	 * @param br
	 * @return
	 * @throws IOException
	 */
	private String getNextValidLine(BufferedReader br) throws IOException {
		String line;
		while ((line = br.readLine()) != null) {
			line = line.trim();
			if (!line.startsWith("#") && !line.isEmpty()) {
				break;
			}
		}
		return line;
	}

	private boolean isHeader(String line) {
		return line.matches(".*[a-zA-Z].*");
	}

	private List<NodeInfo> getNodes(StreamProvider streams) throws IOException {
		Reader fr = new InputStreamReader(streams.input());
		BufferedReader br = new BufferedReader(fr);

		// Get Header node names
		List<String> nodeNames = new ArrayList<String>();
		String line = this.getNextValidLine(br);
		String[] saLine = line.split(SEPARATOR);
		if (!this.isHeader(line)) {
			// No header
			for (int i = 0; i < saLine[0].length(); i++)
				nodeNames.add("G" + i);
		} else {
			// Has header
			for (String id : saLine)
				nodeNames.add(id);
			line = this.getNextValidLine(br);
		}
		int nLines = 1;

		// Get node max values
		saLine = line.split(SEPARATOR);
		int iN = saLine[0].length();
		byte[] bMax = new byte[iN];
		do {
			for (int i = 0; i < iN; i++) {
				byte val = (byte) Character
						.getNumericValue(saLine[0].charAt(i));
				if (bMax[i] < val)
					bMax[i] = val;
			}
			line = this.getNextValidLine(br);
			if (line != null && !line.isEmpty()) {
				nLines++;
				saLine = line.split(SEPARATOR);
			}
		} while (line != null);

		int totalLines = 1;
		for (int i = 0; i < bMax.length; i++) {
			totalLines *= (bMax[i] + 1);
		}
		if (nLines < totalLines) {
			System.err.println("TruthTable incomplete: found " + nLines + " ("
					+ totalLines + " expected).\nMissing target "
					+ "states assumed to go towards null state.");
		}

		List<NodeInfo> nodeOrder = new ArrayList<NodeInfo>();
		for (int i = 0; i < bMax.length; i++) {
			nodeOrder.add(new NodeInfo(nodeNames.get(i), bMax[i]));
		}

		br.close();
		fr.close();
		return nodeOrder;
	}

	@Override
	public LogicalModel doGetResult() throws IOException {
		List<NodeInfo> nodeOrder = this.getNodes(streams);

		// Create the MDDManager
		byte max = 0;
		MDDVariableFactory mvf = new MDDVariableFactory();
		for (NodeInfo node : nodeOrder) {
			mvf.add(node, (byte) (node.getMax() + 1));
			if (node.getMax() > max)
				max = node.getMax();
		}
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf, (max + 1));

		// Fill in the MDDs
		Reader reader = new InputStreamReader(streams.input());
		BufferedReader br = new BufferedReader(reader);

		int[] kMDDs = new int[nodeOrder.size()];
		String line;
		while ((line = this.getNextValidLine(br)) != null) {
			if (this.isHeader(line))
				continue;
			String[] saLine = line.split(SEPARATOR);

			// Get the MDD for that line (source state - saLine[0])
			int[] state = new int[nodeOrder.size()];
			for (int j = 0; j < nodeOrder.size(); j++) {
				state[j] = Character.getNumericValue(saLine[0].charAt(j));
			}

			// Update the components (target state - saLine[1])
			for (int i = 0; i < nodeOrder.size(); i++) {
				int leaf = Character.getNumericValue(saLine[1].charAt(i));
				if (leaf > 0) {
					int pathMDD = buildPathMDD(ddmanager, state, leaf);
					kMDDs[i] = MDDBaseOperators.OR.combine(ddmanager, kMDDs[i],
							pathMDD);
				}
			}
		}
		br.close();
		reader.close();
		return new LogicalModelImpl(nodeOrder, ddmanager, kMDDs);
	}

	private int buildPathMDD(MDDManager ddmanager, int[] state, int leaf) {
		MDDVariable[] ddVariables = ddmanager.getAllVariables();
		int mddPath = leaf;
		for (int i = ddVariables.length - 1; i >= 0; i--) {
			int[] children = new int[ddVariables[i].nbval];
			children[state[i]] = mddPath;
			mddPath = ddVariables[i].getNode(children);
		}
		return mddPath;
	}
}
