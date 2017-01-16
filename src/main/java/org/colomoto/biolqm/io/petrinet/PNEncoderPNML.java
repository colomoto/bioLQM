package org.colomoto.biolqm.io.petrinet;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;

import org.colomoto.common.xml.XMLWriter;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;


/**
 * Export a regulatory graph to Petri net (PNML format).
 * The core of the translation is in <code>GsPetriNetExport</code>.
 *
 * <p>PNML tools/format:
 * <ul>
 *  <li>PNML: http://www.informatik.hu-berlin.de/top/pnml/about.html</li>
 *  <li>PIPE2: http://pipe2.sourceforge.net/</li>
 * </ul>
 */
public class PNEncoderPNML extends AbstractPNEncoder {

	public PNEncoderPNML(LogicalModel model) {
		super(model);
	}

	@Override
	protected void doExport( String netName, List<NodeInfo> v_no, List[] t_transition, byte[][] t_markup, OutputStreamWriter fout) throws IOException {

        XMLWriter out = new XMLWriter(fout, null);
		int len = t_transition.length;

        out.openTag("pnml");
        out.openTag("net", new String[] {"id",netName , "type","P/T net"});
        
        // places data
        for (int i=0 ; i<len ; i++) {
        	addPlace(out,  ""+v_no.get(i), t_markup[i][0],  50,(10+80*i));
        	addPlace(out, "-"+v_no.get(i), t_markup[i][1], 100,(10+80*i));
        }
        
        // transitions data
        for (int i=0 ; i<t_transition.length ; i++) {
        	List v_transition = t_transition[i];
            String s_node = v_no.get(i).toString();
            int max = v_no.get(i).getMax();
            int c = 0;
            if (v_transition != null) {
                for (int j=0 ; j<v_transition.size() ; j++) {
                    TransitionData td = (TransitionData)v_transition.get(j);
                    
                    if (td.value > 0 && td.minValue < td.value) {
                    	addTransition(out, "t_"+s_node+"_"+j+"+", 200+80*c, 10+80*i);
                        c++;
                    }
                    if (td.value < max && td.maxValue > td.value) {
                    	addTransition(out, "t_"+s_node+"_"+j+"-", 200+80*c, 10+80*i);
                        c++;
                    }
                }
            }
        }
        
        // arcs
        for (int i=0 ; i<t_transition.length ; i++) {
        	List v_transition = t_transition[i];
            String s_node = v_no.get(i).toString();
            int max = v_no.get(i).getMax();
            if (v_transition != null) {
                for (int j=0 ; j<v_transition.size() ; j++) {
                    
                    TransitionData td = (TransitionData)v_transition.get(j);
                    if (td.value > 0 && td.minValue < td.value) {
                        String s_transition = "t_"+s_node+"_"+j+"+";
                        String s_src = v_no.get(td.nodeIndex).toString();
                        if (td.minValue == 0) {
                        	addArc(out, s_transition, s_src, 1);
                        } else {
                        	addArc(out, s_src, s_transition, td.minValue);
                        	addArc(out, s_transition, s_src, td.minValue+1);
                        }
                        int a = td.value <= td.maxValue ?  max-td.value+1 : max-td.maxValue;
                    	addArc(out, "-"+s_src, s_transition, a);
                        if (a > 1) {
                        	addArc(out, s_transition, "-"+s_src, a-1);
                        }
                        if (td.t_cst != null) {
                            for (int ti=0 ; ti< td.t_cst.length ; ti++) {
                                int index = td.t_cst[ti][0]; 
                                if (index == -1) {
                                    break;
                                }
                                int lmin = td.t_cst[ti][1];
                                int lmax = td.t_cst[ti][2];
                                s_src = v_no.get(index).toString();
                                if (lmin != 0) {
                                	addArc(out, s_src, s_transition, lmin);
                                	addArc(out, s_transition, s_src, lmin);
                                }
                                if (lmax != 0) {
                                	addArc(out, "-"+s_src, s_transition, lmax);
                                	addArc(out, s_transition, "-"+s_src, lmax);
                                }
                            }
                        }
                    }
                    if (td.value < max && td.maxValue > td.value) {
                        String s_transition = "t_"+s_node+"_"+j+"-";
                        String s_src = v_no.get(td.nodeIndex).toString();
                        if (td.maxValue == max) {
                        	addArc(out, s_transition, "-"+s_src, 1);
                        } else {
                        	addArc(out, "-"+s_src, s_transition, td.maxValue);
                        	addArc(out, s_transition, "-"+s_src, td.maxValue+1);
                        }
                        int a = td.value >= td.minValue ?  td.value+1 : td.minValue;
                        addArc(out, s_src, s_transition, a);
                        if (a > 1) {
                            addArc(out, s_transition, s_src, a-1);
                        }
                        if (td.t_cst != null) {
                            for (int ti=0 ; ti< td.t_cst.length ; ti++) {
                                int index = td.t_cst[ti][0]; 
                                if (index == -1) {
                                    break;
                                }
                                int lmin = td.t_cst[ti][1];
                                int lmax = td.t_cst[ti][2];
                                s_src = v_no.get(index).toString();
                                if (lmin != 0) {
                                	addArc(out, s_src, s_transition, lmin);
                                	addArc(out, s_transition, s_src, lmin);
                                }
                                if (lmax != 0) {
                                	addArc(out, "-"+s_src, s_transition, lmax);
                                	addArc(out, s_transition, "-"+s_src, lmax);
                                }
                            }
                        }
                    }
                }
            }
        }
		// Close the file
        out.closeTag();
        out.closeTag();
		fout.close();
	}
	
	private void addPlace(XMLWriter out, String id, int markup, int x, int y) throws IOException {
    	out.openTag("place", new String[] {"id", id});
    	out.openTag("graphics");
    	out.addTag("position", new String[] {"x",""+x , "y",""+y});
    	out.closeTag();
    	out.openTag("name");
    	out.addTagWithContent("value", id);
    	out.closeTag();
    	out.openTag("initialMarking");
    	out.addTagWithContent("value", ""+markup);
    	out.closeTag();
        out.closeTag(); // place
	}
	
	
	private void addTransition(XMLWriter out, String id, int x, int y) throws IOException {
		out.openTag("transition", new String[] {"id", id});
		out.openTag("graphics");
		out.addTag("position", new String[] {"x",""+x , "y",""+y});
		out.closeTag();
		out.openTag("name");
		out.addTagWithContent("value", id);
		out.closeTag();
		out.openTag("orientation");
		out.addTagWithContent("value", "0");
		out.closeTag();
		out.openTag("rate");
		out.addTagWithContent("value", "1.0");
		out.closeTag();
		out.openTag("timed");
		out.addTagWithContent("value", "false");
		out.closeTag();
	    out.closeTag();
	}

	private void addArc(XMLWriter out, String src, String target, int value) throws IOException {
		out.openTag("arc");
		out.addAttr("id", "a_"+src+"_-"+target);
		out.addAttr("source", src);
		out.addAttr("target", target);
		out.openTag("inscription");
		out.addTagWithContent("value", ""+value);
		out.closeTag();
		out.closeTag();
	}

}
