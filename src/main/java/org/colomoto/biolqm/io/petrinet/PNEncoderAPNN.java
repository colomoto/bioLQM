package org.colomoto.biolqm.io.petrinet;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.io.StreamProvider;


/**
 * Export a regulatory graph to petri net (APNN format).
 * The core of the translation is in <code>GsPetriNetExport</code>.
 *
 * <p>petri net tools/format:
 * <ul>
 *  <li>APNN http://www4.cs.uni-dortmund.de/APNN-TOOLBOX/</li>
 *  <li>CHARLIE: http://www.informatik.tu-cottbus.de/~ms/charlie/</li>
 * </ul>
 */
public class PNEncoderAPNN extends AbstractPNEncoder {

	public PNEncoderAPNN(LogicalModel model) {
		super(model);
	}

	@Override
	protected void doExport( String netName, List<NodeInfo> v_no, List[] t_transition, byte[][] t_markup, OutputStreamWriter out) throws IOException {
        
        // NET
        out.write("\\beginnet{"+netName+"}"+"\n\n");
		int len = t_transition.length;
        
        // PLACE
        /* Comments: We add the property CAPACITY defined by \capacity{INTEGER} which represents the maximum capacity of a place. 
           But we manage already this property and there is no possibility to overtake this value.*/
        
        for (int i=0 ; i<len ; i++) 
        {
            out.write("\\place{"+v_no.get(i)+"}"+"{\\name{"+v_no.get(i)+"} \\init{"+t_markup[i][0]+"} \\capacity{"+(t_markup[i][0]+t_markup[i][1])+"} \\coords{"+50+" "+(10+80*i)+"}}\n");
            out.write("\\place{-"+v_no.get(i)+"}"+"{\\name{-"+v_no.get(i)+"} \\init{"+t_markup[i][1]+"} \\capacity{"+(t_markup[i][0]+t_markup[i][1])+"} \\coords{"+100+" "+(10+80*i)+"}}\n\n");
        }
        
        // TRANSITION
        for (int i=0 ; i<t_transition.length ; i++) 
        {
        	List v_transition = t_transition[i];
            String s_node = v_no.get(i).toString();
            int max = v_no.get(i).getMax();
            int c = 0;
            
            if (v_transition != null) 
            {
                for (int j=0 ; j<v_transition.size() ; j++) 
                {
                    TransitionData td = (TransitionData)v_transition.get(j);
                    
                    if (td.value > 0 && td.minValue < td.value) 
                    {
                        out.write("\\transition{t_"+s_node+"_"+j+"+}{\\name{t_"+s_node+"_"+j+"+} \\coords{"+(200+80*c)+" "+(10+80*i)+"}}\n");       
                        c++;
                    }
                    if (td.value < max && td.maxValue > td.value) 
                    {
                        out.write("\\transition{t_"+s_node+"_"+j+"-}{\\name{t_"+s_node+"_"+j+"-} \\coords{"+(200+80*c)+" "+(10+80*i)+"}}\n");
                        c++;
                    }
                }
            }
        }
        
        // ARC
        for (int i=0 ; i<t_transition.length ; i++) 
        {
        	List v_transition = t_transition[i];
            String s_node = v_no.get(i).toString();
            int max = v_no.get(i).getMax();
            
            if (v_transition != null) 
            {
                for (int j=0 ; j<v_transition.size() ; j++) 
                {           	
                	TransitionData td = (TransitionData)v_transition.get(j);
                    if (td.value > 0 && td.minValue < td.value) 
                    {
                        String s_transition = "t_"+s_node+"_"+j+"+";
                        String s_src = v_no.get(td.nodeIndex).toString();
                        if (td.minValue == 0) 
                        {
                            out.write("\\arc{a_"+s_transition+"_"+s_src+"}{\\from{"+s_transition+"} \\to{"+s_src+"} \\weight{1} \\type{ordinary}}\n");
            
                        } 
                        else 
                        {
                            out.write("\\arc{a_"+s_src+"_"+s_transition+"}{\\from{"+s_src+"} \\to{"+s_transition+"} \\weight{"+td.minValue+"} \\type{ordinary}}\n"); 		
                            out.write("\\arc{a_"+s_transition+"_"+s_src+"}{\\from{"+s_transition+"} \\to{"+s_src+"} \\weight{"+(td.minValue+1)+"} \\type{ordinary}}\n"); 		
                        }
                        int a = td.value <= td.maxValue ?  max-td.value+1 : max-td.maxValue;
                        out.write("\\arc{a_-"+s_src+"_"+s_transition+"}{\\from{-"+s_src+"} \\to{"+s_transition+"} \\weight{"+a+"} \\type{ordinary}}\n");
                        if (a > 1) 
                        {
                            out.write("\\arc{a_"+s_transition+"_-"+s_src+"}{\\from{"+s_transition+"} \\to{-"+s_src+"} \\weight{"+(a-1)+"} \\type{ordinary}}\n");
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
                                if (lmin != 0) 
                                {
                                    out.write("\\arc{a_"+s_src+"_"+s_transition+"}{\\from{"+s_src+"} \\to{"+s_transition+"} \\weight{"+lmin+"} \\type{ordinary}}\n"); 		
                                    out.write("\\arc{a_"+s_transition+"_"+s_src+"}{\\from{"+s_transition+"} \\to{"+s_src+"} \\weight{"+lmin+"} \\type{ordinary}}\n");
                                }
                                if (lmax != 0) {
                                    out.write("\\arc{a_-"+s_src+"-_"+s_transition+"}{\\from{-"+s_src+"} \\to{"+s_transition+"} \\weight{"+lmax+"} \\type{ordinary}}\n");
                                    out.write("\\arc{a_"+s_transition+"_-"+s_src+"}{\\from{"+s_transition+"} \\to{-"+s_src+"} \\weight{"+lmax+"} \\type{ordinary}}\n");
                                }
                            }
                        }
                    }
                    if (td.value < max && td.maxValue > td.value) {
                        String s_transition = "t_"+s_node+"_"+j+"-";
                        String s_src = v_no.get(td.nodeIndex).toString();
                        if (td.maxValue == max) 
                        {
                            out.write("\\arc{a_"+s_transition+"_-"+s_src+"}{\\from{"+s_transition+"} \\to{-"+s_src+"} \\weight{1} \\type{ordinary}}\n");
                        } else
                          {
                            out.write("\\arc{a_-"+s_src+"_"+s_transition+"}{\\from{-"+s_src+"} \\to{"+s_transition+"} \\weight{"+td.maxValue+"} \\type{ordinary}}\n");
                            out.write("\\arc{a_"+s_transition+"_-"+s_src+"}{\\from{"+s_transition+"} \\to{-"+s_src+"} \\weight{"+(td.maxValue+1)+"} \\type{ordinary}}\n");
                          }
                      int a = td.value >= td.minValue ?  td.value+1 : td.minValue;
                      
                        out.write("\\arc{a_"+s_src+"_"+s_transition+"}{\\from{"+s_src+"} \\to{"+s_transition+"} \\weight{"+a+"} \\type{ordinary}}\n");
                        if (a > 1) 
                        {
                            out.write("\\arc{a_"+s_transition+"_"+s_src+"}{\\from{"+s_transition+"} \\to{"+s_src+"} \\weight{"+(a-1)+"} \\type{ordinary}}\n");
                        }
                        if (td.t_cst != null) 
                        {
                            for (int ti=0 ; ti< td.t_cst.length ; ti++) 
                            {
                                int index = td.t_cst[ti][0]; 
                                if (index == -1) 
                                {
                                    break;
                                }
                                int lmin = td.t_cst[ti][1];
                                int lmax = td.t_cst[ti][2];
                                s_src = v_no.get(index).toString();
                                if (lmin != 0)
                                {
                                    out.write("\\arc{a_"+s_src+"_"+s_transition+"}{\\from{"+s_src+"} \\to{"+s_transition+"} \\weight{"+lmin+"} \\type{ordinary}}\n");
                                    out.write("\\arc{a_"+s_transition+"_"+s_src+"}{\\from{"+s_transition+"} \\to{"+s_src+"} \\weight{"+lmin+"} \\type{ordinary}}\n");
                                }
                                if (lmax != 0) 
                                {
                                    out.write("\\arc{a_-"+s_src+"_"+s_transition+"}{\\from{-"+s_src+"} \\to{"+s_transition+"} \\weight{"+lmax+"} \\type{ordinary}}\n");
                                    out.write("\\arc{a_"+s_transition+"_-"+s_src+"}{\\from{"+s_transition+"} \\to{-"+s_src+"} \\weight{"+lmax+"} \\type{ordinary}}\n");
                                }
                            }
                        }
                    }
                }
            }
        }
		// ENDNET
        out.write("\\endnet\n");
		out.close();
	}
}
