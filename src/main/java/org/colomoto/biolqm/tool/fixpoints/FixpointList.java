package org.colomoto.biolqm.tool.fixpoints;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.settings.state.StateList;
import org.colomoto.mddlib.MDDManager;

public class FixpointList extends ArrayList<byte[]> implements StateList {

	private final NodeInfo[] nodes;
	private final NodeInfo[] extraNodes;
	private final NodeInfo[] mergedNodes;
	private final int[] extraFunctions;
	private MDDManager ddmanager;
	private List<byte[]> extra;
	private boolean isExtra = false;

	public FixpointList(LogicalModel model) {
		this(model.getComponents(), model.getExtraComponents(), model.getMDDManager(), model.getExtraLogicalFunctions());
	}

	public FixpointList(List<NodeInfo> nodes) {
		this(nodes, null, null, null);
	}

	public FixpointList(List<NodeInfo> nodes, List<NodeInfo> extraNodes, MDDManager ddmanager, int[] extraFunctions) {
		this.nodes = extractIDs(nodes);
		this.extraNodes = extractIDs(extraNodes);
		if (this.extraNodes == null) {
			this.mergedNodes = this.nodes;
		} else {
			this.mergedNodes = new NodeInfo[ this.nodes.length + this.extraNodes.length];
			System.arraycopy(this.nodes, 0, this.mergedNodes, 0, this.nodes.length);
			System.arraycopy(this.extraNodes, 0, this.mergedNodes, this.nodes.length, this.extraNodes.length);
		}
		this.ddmanager = ddmanager;
		this.extraFunctions = extraFunctions;
	}

	private static NodeInfo[] extractIDs(List<NodeInfo> list) {
		if (list == null) {
			return null;
		}
		NodeInfo[] array = new NodeInfo[list.size()];
		for (int i=0 ; i<array.length ; i++) {
			array[i] = list.get(i);
		}
		return array;
	}

	@Override
	public NodeInfo[] getComponents() {
		if (isExtra) {
			return mergedNodes;
		}

		return nodes;
	}

	@Override
	public int get(int row, int col) {
		if (col < nodes.length) {
			return get(row)[col];
		}
		return extra.get(row)[col - nodes.length];
	}

	@Override
	public void setExtra(boolean extra) {
		if (extra == isExtra) {
			return;
		}
		this.isExtra = extra;

		if (!isExtra || extraNodes == null || extraNodes.length == 0) {
			this.extra = null;
			return;
		}

		this.extra = new ArrayList<>(this.size());
		for (byte[] state: this) {
			byte[] cur = new byte[extraFunctions.length];
			for (int idx=0 ; idx<extraFunctions.length ; idx++) {
				cur[idx] = ddmanager.reach( extraFunctions[idx], state);
			}
			this.extra.add(cur);
		}
	}

	public List<byte[]> getExtraData() {
		setExtra(true);
		return extra;
	}

	public byte[] rawByteArray() {
		int x = 0;
		int l = nodes.length;
		byte[] content = new byte[size()*l];
		for (byte[] d: this) {
			System.arraycopy(d, 0, content, x, l);
			x += l;
		}
		return content;
	}

	public byte[] fillState(byte[] state, int index) {
		int l = getComponents().length;
		if (state == null || state.length != l) {
			state = new byte[l];
		}

		byte[] innerstate = get(index);
		int ofset = innerstate.length;
		System.arraycopy(innerstate, 0, state, 0, ofset);
		if (isExtra && extra != null) {
			innerstate = extra.get(index);
			System.arraycopy(innerstate, 0, state, ofset, innerstate.length);
		}

		return state;
	}
}
