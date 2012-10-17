package org.colomoto.logicalmodel.io.petrinet;

import java.util.HashMap;
import java.util.Map;

import org.colomoto.logicalmodel.LogicalModel;


public class PNConfig {

	int[][] t_priorities = null;
//	public PNFormat format;
//	Map m_init = new HashMap();
//	Map m_input = new HashMap();
//
//	public PNConfig( ) {
//		// FIXME: change default when the format choice is restored
//		format = new PetriNetExportINA();
//	}
//	
//	public Map getInitialState() {
//		return m_init;
//	}
//
//    public Map getInputState() {
//        return m_input;
//    }

	
	// old code to prepare the priority metadata
	/*		
	if (priorities != null) {
		t_priorities = new int[len][2];
		int[][] t_pclass = config.priorities.getPclass(nodeOrder);
		for (int i=0 ; i<t_pclass.length ; i++) {
			int[] t_class = t_pclass[i];
			int priority = t_class[0];
			for (int j=2 ; j<t_class.length ; j++) {
				int index = t_class[j++];
				LogManager.trace( "priority of "+priority+" for "+index+" ("+t_class[j]+")");
				switch (t_class[j]) {
					case 1:
						t_priorities[index][0] = priority;
						break;
					case -1:
						t_priorities[index][1] = priority;
						break;
					default:
						t_priorities[index][0] = priority;
						t_priorities[index][1] = priority;
				}
			}
		}
	}
*/		

}
