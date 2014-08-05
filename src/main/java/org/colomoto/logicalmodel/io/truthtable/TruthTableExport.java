package org.colomoto.logicalmodel.io.truthtable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;

/**
 * Exports a logical model into the Truth Table format.
 * 
 * @author Pedro T. Monteiro
 */
public class TruthTableExport {

	/**
	 * Exports a logical model into a truth table.
	 * 
	 * @param model
	 * @param out
	 * @throws IOException
	 */
	public void export(LogicalModel model, OutputStream out) throws IOException {
		Writer writer = new OutputStreamWriter(out);

		List<NodeInfo> nodeOrder = model.getNodeOrder();
		// BEGIN Header
		String stmp = "";
		for (NodeInfo node : nodeOrder) {
			stmp += node.getNodeID() + " ";
		}
		writer.write(stmp.trim() + "\n");
		// END Header
		byte[] state = new byte[nodeOrder.size()];
		this.exportStatesFromDim(writer, model, state, 0, 0);
		writer.close();
	}

	private void exportStatesFromDim(Writer writer, LogicalModel m,
			byte[] state, int i, int j) throws IOException {
		if (j == state.length) {
			writer.append(this.state2String(state) + " ");
			writer.append(this.state2String(this.getStateImage(m, state))
					+ "\n");
		} else {
			NodeInfo node = m.getNodeOrder().get(j);
			for (byte b = 0; b <= node.getMax(); b++) {
				state[j] = b;
				exportStatesFromDim(writer, m, state, i, j + 1);
			}
			for (int k = j; k < state.length - 1; k++) {
				state[k] = 0;
			}
		}
	}

	private String state2String(byte[] state) {
		String tmp = "";
		for (int i = 0; i < state.length; i++) {
			tmp += state[i];
		}
		return tmp;
	}

	private byte[] getStateImage(LogicalModel m, byte[] state) {
		byte[] nextState = new byte[state.length];
		for (int i = 0; i < state.length; i++) {
			nextState[i] = m.getTargetValue(i, state);
		}
		return nextState;
	}
}
