package org.colomoto.biolqm.io.truthtable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

/**
 * Exports a logical model into the Truth Table format.
 * 
 * @author Pedro T. Monteiro
 */
public class TruthTableExport {

	/**
	 * Exports a logical model into a truth table.
	 * 
	 * @param model the model to export
	 * @param out an opened output stream to save the result
	 * @throws IOException if writing fails
	 */
	public static void export(LogicalModel model, OutputStream out) throws IOException {
		Writer writer = new OutputStreamWriter(out);

		List<NodeInfo> nodeOrder = model.getComponents();
		// BEGIN Header
		String stmp = "";
		for (NodeInfo node : nodeOrder) {
			stmp += node.getNodeID() + " ";
		}
		writer.write(stmp.trim() + "\n");
		// END Header
		byte[] state = new byte[nodeOrder.size()];
		exportStatesFromDim(writer, model, state, 0, 0);
		writer.close();
	}

	private static void exportStatesFromDim(Writer writer, LogicalModel m,
			byte[] state, int i, int j) throws IOException {
		if (j == state.length) {
			writer.append(state2String(state) + " ");
			writer.append(state2String(getStateImage(m, state))
					+ "\n");
		} else {
			NodeInfo node = m.getComponents().get(j);
			for (byte b = 0; b <= node.getMax(); b++) {
				state[j] = b;
				exportStatesFromDim(writer, m, state, i, j + 1);
			}
			for (int k = j; k < state.length - 1; k++) {
				state[k] = 0;
			}
		}
	}

	private static String state2String(byte[] state) {
		String tmp = "";
		for (int i = 0; i < state.length; i++) {
			tmp += state[i];
		}
		return tmp;
	}

	private static byte[] getStateImage(LogicalModel m, byte[] state) {
		byte[] nextState = new byte[state.length];
		for (int i = 0; i < state.length; i++) {
			nextState[i] = m.getTargetValue(i, state);
		}
		return nextState;
	}
}
