package org.colomoto.logicalmodel.io.avatar;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.logicalmodel.StatefulLogicalModelImpl;
import org.colomoto.mddlib.PathSearcher;

/**
 * Writes a logical model to an AVATAR file
 *  
 * @author Pedro Monteiro, Rui Henriques
 * @version 1.0
 */
public final class AvatarWriter {

	private static List<String> avatarReserved = Arrays.asList("MODULE", "DEFINE", 
		"MDEFINE", "CONSTANTS", "VAR", "IVAR", "FROZENVAR", "INIT", "TRANS", "INVAR",
		"SPEC", "CTLSPEC", "LTLSPEC", "PSLSPEC", "COMPUTE", "NAME", "TRUE", "FALSE",
		"INVARSPEC", "FAIRNESS", "JUSTICE", "COMPASSION", "ISA", "ASSIGN",
		"CONSTRAINT", "SIMPWFF", "CTLWFF", "LTLWFF", "PSLWFF", "COMPWFF",
		"IN", "MIN", "MAX", "MIRROR", "PRED", "PREDICATES", "CASE", "ESAC");

	/**
	 * Exports a model to a AVATAR file
	 * @param model the logical model (MDD)
	 * @param out the writer receiving the encoded model description
	 * @throws IOException 
	 */
	public static void write(LogicalModel model, Writer out) throws AvatarLogicalModelException, IOException {
		List<NodeInfo> coreNodes = model.getNodeOrder();
		List<NodeInfo> outputNodes = model.getExtraComponents();
		if(coreNodes.isEmpty() && outputNodes.isEmpty()) 
			throw new AvatarLogicalModelException("AVATAR does not support empty graphs");
		if (!hasCoreNodes(coreNodes)) 
			throw new AvatarLogicalModelException("AVATAR needs at least one core (non-input/non-output) node");
			
		DateFormat dateformat = DateFormat.getDateTimeInstance(DateFormat.LONG,	DateFormat.LONG);
		out.write("-- " + dateformat.format(new Date()) + "\n");
		out.write("-- GINsim export for Avatar/Firefront\n");
		out.write("-- Inspired on the NuSMV v2.1+ syntax\n");

		NodeInfo[] aNodeOrder = new NodeInfo[coreNodes.size()];
		boolean hasInputVars = false;
		for(int i=0; i < aNodeOrder.length; i++) {
			NodeInfo node = model.getNodeOrder().get(i);
			aNodeOrder[i] = node;
			if(node.isInput()) hasInputVars = true;
		}

		//System.out.println("Size0:"+coreNodes.size());
		if(hasInputVars) {
			out.write("\nIVAR\n-- Input variables declaration\n");
			for(int i = 0; i < coreNodes.size(); i++) {
				if(!coreNodes.get(i).isInput()) continue;
				String s_levels = "0";
				for (int j = 1; j <= coreNodes.get(i).getMax(); j++) s_levels += ", " + j;
				out.write("  " + avoidAvatarNames(coreNodes.get(i).getNodeID()) + " : { " + s_levels + "};\n");
			}
		}
		out.write("\nVAR\n-- State variables declaration\n");
		for(int i = 0; i < coreNodes.size(); i++) {
			if(coreNodes.get(i).isInput()) continue;
			String s_levels = "0";
			for (int j = 1; j <= coreNodes.get(i).getMax(); j++) s_levels += ", " + j;
			out.write("  " + avoidAvatarNames(coreNodes.get(i).getNodeID()) + " : {" + s_levels + "};\n");
		}
		
		// Nodes actual logical rules
		out.write("\nDEFINE\n-- Variable next level regulation\n");
		int[] kMDDs = model.getLogicalFunctions();
		for(int i = 0; i < coreNodes.size(); i++) {
			NodeInfo node = coreNodes.get(i);
			if(node.isInput()) continue;
			out.write(avoidAvatarNames(node.getNodeID()) + "_focal :=\n  case\n");
			nodeRules2Avatar(out, model, kMDDs[i], coreNodes, node);
			out.write("  esac;\n");
		}
		out.write("\n-- Declaration of output variables\n");
		if (outputNodes.size() > 0) {
			kMDDs = model.getExtraLogicalFunctions();
			for (int i = 0; i < outputNodes.size(); i++) {
				NodeInfo node = outputNodes.get(i);
				out.write(avoidAvatarNames(node.getNodeID()) + " :=\n  case\n");
				nodeRules2Avatar(out, model, kMDDs[i], coreNodes, node);
				out.write("  esac;\n");
			}
		} else out.write("-- Empty !\n");
		out.write("\n");

		if(model instanceof StatefulLogicalModelImpl){
			out.write("\n-- Declaration of core variables restriction list\n");
			//config.getInitialState().keySet().iterator() new ArrayList<NamedState>()
			//out.write(writeStateList(aNodeOrder, null, false));
			byte[] initState = ((StatefulLogicalModelImpl)model).getInitialStates().get(0);
			//System.out.println("Size:"+initState.length);
			for(int i=0, l=initState.length; i<l; i++){
				if(initState[i]!=-1 && !coreNodes.get(i).isInput()){
					out.write("INIT "+avoidAvatarNames(coreNodes.get(i).getNodeID())+"="+initState[i]+"\n");
				}
			}
		
			if(hasInputVars) {
				out.write("\n-- Declaration of input variables restriction list\n");
				//config.getInputState().keySet().iterator()
				//out.write(writeStateList(aNodeOrder, null, true));
				for(int i=0, l=initState.length; i<l; i++){
					if(initState[i]!=-1 && coreNodes.get(i).isInput()){
						out.write("INIT "+avoidAvatarNames(coreNodes.get(i).getNodeID())+"="+initState[i]+"\n");
					}
				}
			}
		}
		out.close();
	}

	private static void nodeRules2Avatar(Writer out, LogicalModel model, int nodeMDD, List<NodeInfo> coreNodeOrder, NodeInfo node) throws IOException {
		//System.out.println("Max value="+node.getMax()+" Node MDD="+nodeMDD);
		PathSearcher searcher = new PathSearcher(model.getMDDManager(), 1, node.getMax());
		int[] path = searcher.getPath();
		searcher.setNode(nodeMDD);
	
		int leafValue = 0;
		String s = "";
		for (int l : searcher) {
			//System.out.println(AvatarUtils.toString(path));
			boolean bWrite = false;
			for (int i = 0; i < path.length; i++) {
				if (path[i] != -1) {
					if (!bWrite) s += "    ";
					else s += " & ";
					s += "("+ avoidAvatarNames(coreNodeOrder.get(i).getNodeID())+ " = " + path[i] + ")";
					bWrite = true;
				}
			}
			if (!s.isEmpty()) s += " : " + l + ";\n";
			else leafValue = l;
		}
		out.write(s+"    TRUE : " + leafValue + ";\n");
	}

	private static String avoidAvatarNames(String keyword) {
		if(keyword == null) return keyword;
		if(keyword.length()==1) return "_" + keyword;
		if(avatarReserved.contains(keyword.toUpperCase())) return "_" + keyword;
		return keyword;
	}

	private static boolean hasCoreNodes(List<NodeInfo> nodes) {
		boolean hasCore = false;
		if (nodes == null) return hasCore;
		for (NodeInfo node : nodes) {
			if (!node.isInput()) {
				hasCore = true;
				break;
			}
		}
		return hasCore;
	}

}
