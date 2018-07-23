package org.colomoto.biolqm.tool.fixpoints;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;

public class FixpointList extends ArrayList<byte[]> {

	public final List<NodeInfo> nodes;
	private List<NodeInfo> extraNodes;
	private String[] tnodes = null;
	private String[] extratnodes = null;
	private int[] extraFunctions;
	private MDDManager ddmanager;
	private List<byte[]> extra;

	public FixpointList(LogicalModel model) {
		this.nodes = model.getComponents();
		this.extraNodes = model.getExtraComponents();
		this.extraFunctions = model.getExtraLogicalFunctions();
		this.ddmanager = model.getMDDManager();
	}

	public List<byte[]> fillExtraNodes() {
		if (extraNodes == null || extraNodes.size() == 0) {
			extra = null;
			return null;
		}
		extra = new ArrayList<>(this.size());
		for (byte[] state: this) {
			byte[] cur = new byte[extraFunctions.length];
			for (int idx=0 ; idx<extraFunctions.length ; idx++) {
				cur[idx] = ddmanager.reach( extraFunctions[idx], state);
			}
			extra.add(cur);
		}
		return getExtraValues();
	}

	public List<byte[]> getExtraValues() {
		return extra;
	}

	public String[] getNodeIDs() {
		if (tnodes == null) {
			tnodes = new String[nodes.size()];
			for (int i=0 ; i<tnodes.length ; i++) {
				tnodes[i] = nodes.get(i).getNodeID();
			}
		}
		return tnodes;
	}
	public String[] getExtraNodeIDs() {
		if (tnodes == null) {
			tnodes = new String[nodes.size()];
			for (int i=0 ; i<tnodes.length ; i++) {
				tnodes[i] = nodes.get(i).getNodeID();
			}
		}
		return tnodes;
	}

	public byte[] rawByteArray() {
		int x = 0;
		int l = nodes.size();
		byte[] content = new byte[size()*l];
		for (byte[] d: this) {
			System.arraycopy(d, 0, content, x, l);
			x += l;
		}
		return content;
	}
}
