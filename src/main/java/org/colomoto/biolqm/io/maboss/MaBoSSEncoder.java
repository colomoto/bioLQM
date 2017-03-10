package org.colomoto.biolqm.io.maboss;

import java.io.IOException;
import java.io.Writer;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.PathSearcher;

/**
 * Export a Boolean(ized) LogicalModel into MaBoSS format
 * 
 * @author Aurelien Naldi
 */
public class MaBoSSEncoder {

	private final LogicalModel model;
	private final MDDManager ddmanager;
	private final PathSearcher searcher;
	private final List<NodeInfo> nodes;
	private final List<NodeInfo> extraNodes;
	
	public MaBoSSEncoder(LogicalModel model) {
		this.model = model;
		this.ddmanager = model.getMDDManager();
		this.nodes = model.getComponents();
		this.extraNodes = model.getExtraComponents();
		this.searcher = new PathSearcher(ddmanager, 1);
	}
	
	public void write(Writer out) throws IOException {
		
		writeFunctions(nodes, model.getLogicalFunctions(), out);
		writeFunctions(model.getExtraComponents(), model.getExtraLogicalFunctions(), out);
	}

	public void writeConfig(Writer out) throws IOException {
		writeParams(nodes, out);
		writeParams(extraNodes, out);

		out.write("\n");
		writeInit(nodes, out);
		writeInit(extraNodes, out);
		
		// write generic config
		out.write("\n");
		out.write("time_tick = 0.5;\n");
		out.write("max_time = 1000;\n");
		out.write("sample_count = 10000;\n");
		out.write("discrete_time = 0;\n");
		out.write("use_physrandgen = 1;\n");
		out.write("seed_pseudorandom = 0;\n");

		out.write("display_traj = 0;\n");
		out.write("statdist_traj_count = 0;\n");
		out.write("statdist_cluster_threshold = 1;\n");
		out.write("thread_count = 1;\n");
		out.write("statdist_similarity_cache_max_size = 20000;\n");
	}

	private void writeParams(List<NodeInfo> nodes, Writer out) throws IOException {
		for (NodeInfo ni: nodes) {
			String name = ni.getNodeID();
			out.write("$u_" + name + " = 1;\n");
			out.write("$d_" + name + " = 1;\n");
		}
	}

	private void writeInit(List<NodeInfo> nodes, Writer out) throws IOException {
		Set<String> handledgroups = new HashSet<String>();
		for (NodeInfo ni: nodes) {
			NodeInfo[] group = ni.getBooleanizedGroup();
			if (group != null) {
				String groupuid = group[0].getNodeID();
				if (handledgroups.contains(groupuid)) {
					continue;
				}
				handledgroups.add(groupuid);
				int l = group.length;
				String names = "";
				for (NodeInfo gni: group) {
					names += "," + gni.getNodeID();
				}
				names = names.substring(1);
				out.write( "["+names+"].istate = 1 ["+ writeValues(l,0)+"]");
				for (int i=1 ; i<=l ; i++) {
					out.write(", 0 [" +writeValues(l,i)+ "]");
				}
				out.write(";\n");
			} else {
				out.write("["+ni.getNodeID() + "].istate = 1 [0] , 0 [1];\n");
			}
		}
	}

	private String writeValues(int n, int k) {
		if (n < 1) {
			return "";
		}

		String ret = "";
		for (int i=0 ; i<n ; i++) {
			if (i < k) {
				ret += ",1";
			} else {
				ret += ",0";
			}
		}

		return ret.substring(1);
	}

	
	private void writeFunctions(List<NodeInfo> nodes, int[] functions, Writer out) throws IOException {

		for (int i=0 ; i<functions.length ; i++) {
			NodeInfo ni = nodes.get(i);
			int func = functions[i];
			String name = ni.getNodeID();
			
			if (ni.getMax() > 1) {
				throw new RuntimeException("Multivalued nodes not supported");
			}
			
			out.write("Node "+name+" {\n");
			
			if (ddmanager.isleaf(func)) {
				if (func == 0) {
					out.write("  rate_up = 0;\n");
					out.write("  rate_down = $u_" + name + ";\n");
				} else if (func == 1) {
					out.write("  rate_up = $u_" + name + ";\n");
					out.write("  rate_down = 0;\n");
				} else {
					throw new RuntimeException("Multivalued models not supported");
				}
			} else {
				out.write("  logic = "+getFunctionString(func) + ";\n");
				out.write("  rate_up = @logic ? $u_" + name + " : 0;\n");
				out.write("  rate_down = @logic ? 0 : $d_" + name + ";\n");
			}
			
			out.write("}\n\n");
		}
	}
	
	private String getFunctionString(int f) {
		
		StringBuffer sb = null;;
		int[] path = searcher.setNode(f);
		for (int leaf: searcher) {
			if (sb == null) {
				sb = new StringBuffer("(");
			} else {
				// add "or" term
				sb.append(" | (");
			}
			
			// add all terms
			boolean first = true;
			for (int i=0 ; i<path.length ; i++) {
				int val_i = path[i];
				if (val_i < 0) {
					continue;
				}
				
				if (first) {
					first = false;
				} else {
					sb.append(" & ");
				}
				
				String name_i = nodes.get(i).getNodeID();
				if (val_i == 0) {
					sb.append("!"+name_i);
				} else {
					sb.append(name_i);
				}
			}
			sb.append(")");
		}
		return sb.toString();
	}
}
