package org.colomoto.logicalmodel.io.truthtable;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDManagerFactory;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.MDDVariableFactory;
import org.colomoto.mddlib.operators.MDDBaseOperators;

public final class TruthTableImport {

	private final String SEPARATOR = "\\s+";
	private int maxLines;

	private byte[] getMaxValues(FileReader fr) throws IOException {
		BufferedReader br = new BufferedReader(fr);
		String line;
		byte bMax[] = null;
		int nLines = 0;
		maxLines = 0;

		// 1st - Checks for max values only
		do {
			line = br.readLine();
		} while (line != null && line.trim().isEmpty());
		nLines++;
		String[] saLine = line.split(SEPARATOR);
		int iN = saLine[0].length();
		bMax = new byte[iN];
		do {
			if (!line.trim().isEmpty()) {
				saLine = line.split(SEPARATOR);
				// Looks only at the departing state
				for (int i = 0; i < iN; i++) {
					byte val = (byte) Character.getNumericValue(saLine[0]
							.charAt(i));
					if (bMax[i] < val)
						bMax[i] = val;
				}
				nLines++;
			}
		} while ((line = br.readLine()) != null);
		br.close();

		if (bMax.length > 0) {
			maxLines = 1;
			for (int i = 0; i < bMax.length; i++) {
				maxLines *= (bMax[i] + 1);
			}
			if (nLines < maxLines) {
				System.err.println("TruthTable incomplete: found " + nLines
						+ " (" + maxLines + " expected).\nMissing target "
						+ "states assumed to go towards null state.");
			}
		}
		return bMax;
	}

	public LogicalModel getModel(File file) throws IOException {
		FileReader fr = new FileReader(file);
		byte[] bMax = getMaxValues(fr);
		fr.close();
		fr = new FileReader(file);
		BufferedReader br = new BufferedReader(fr);

		List<NodeInfo> nodeOrder = new ArrayList<NodeInfo>();
		byte max = 0;
		for (int i = 0; i < bMax.length; i++) {
			nodeOrder.add(new NodeInfo("g" + i, bMax[i]));
			if (bMax[i] > max)
				max = bMax[i];
		}

		MDDVariableFactory mvf = new MDDVariableFactory();
		for (NodeInfo node : nodeOrder) {
			mvf.add(node, (byte) (node.getMax() + 1));
		}
		MDDManager ddmanager = MDDManagerFactory.getManager(mvf, (max + 1));

		int[] kMDDs = new int[nodeOrder.size()];
		String line;
		while ((line = br.readLine()) != null) {
			if (line.trim().isEmpty())
				continue;
			String[] saLine = line.split(SEPARATOR);

			// Get the MDD for that line (source state - saLine[0])
			int[] state = new int[nodeOrder.size()];
			for (int j = 0; j < nodeOrder.size(); j++) {
				state[j] = Character.getNumericValue(saLine[0]
						.charAt(j));
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
		
		return new LogicalModelImpl(nodeOrder, ddmanager, kMDDs);
	}

	private int buildPathMDD(MDDManager ddmanager, int[] state, int leaf) {
		MDDVariable[] ddVariables = ddmanager.getAllVariables();
		int mddPath = leaf; 
		for (int i = ddVariables.length - 1; i >= 0 ; i--) {
			int[] children = new int[ddVariables[i].nbval];
			children[state[i]] = mddPath;
			mddPath = ddVariables[i].getNode(children);
		}
		return mddPath;
	}
}
