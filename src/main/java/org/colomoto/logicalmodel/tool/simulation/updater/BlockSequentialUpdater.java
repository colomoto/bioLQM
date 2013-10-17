package org.colomoto.logicalmodel.tool.simulation.updater;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.mddlib.MDDManager;

/**
 * Updater for a block-sequential scheme: there are groups of nodes updated in 
 * synchronous way, but the different groups are updated in sequential way, one after
 * another.
 * 
 * The semantics of the Integer array provided by the user is the following: 
 * The length of the array is equal to the number of nodes in the model
 * 
 * order[i]==j iff the node i is updated in the time j
 * 
 * many nodes could be updated in time j, but each node is updated
 * exactly once in one update sequence given by the block-sequential scheme
 * 
 * the nodes are updated in ascending order: if MIN corresponds to the minimum
 * value of the array, the code first update (synchronously) the nodes with 
 * time MIN, then the nodes with time MIN+1, and so on 
 * 
 * @author Francisco Plana
 */

public class BlockSequentialUpdater extends AbstractUpdater {
	
	private Integer [] sizes;
	private final List<List <Integer> > scheme = new ArrayList<List <Integer>>();
	private int length;

	public BlockSequentialUpdater(LogicalModel model, Integer [] o) {
		super(model);
		generate_scheme(o);
	}

	/*
	 * this method converts the Integer Array provided by the user
	 * into a list of lists, which every sublist stores the IDs of
	 * nodes updated synchronously at some time, the sublists
	 * are ordered in an ascendent way according to the updating time
	 * 
	 * the array sizes stores the size of every sublist
	 * 
	 * the int length is the amount of blocks (sublists) in the list scheme
	 */
	
	private void generate_scheme(Integer[] o) {
		
		List<Integer> L = Arrays.asList(o);
		
		/* 
		 * The following part gets the maximum and minimum time 
		 * of the array o, these are the limits that time points
		 * can take during 1 update sequence; 
		 */ 
		
		Integer MAX = Collections.max(L);
		Integer MIN = Collections.min(L);
		
		int size = MAX - MIN + 1;
		sizes = new Integer [size];
		
		int abs_index, rel_index;
		List <Integer> aux;
		
		// this counts the amount of nodes in every sublist
		int counter;
		
		// Iterate over the update time values in ascendent order
		for (int i = MIN ; i <= MAX ; i++) {
			
			// create the sublist of nodes updated at time i
			List<Integer> s_list = new ArrayList<Integer>();
			
			counter = 0;
			
			// The following part gets every appearance of index i in L
			
			abs_index = 0;
			aux = L;
			if (L.indexOf(i) == -1)
				continue;
			else {
				while(true) {
			
					rel_index = aux.indexOf(i);
					
					// abs_index+rel_index is the index of the current node updated at time i 
				
					s_list.add( abs_index + rel_index );
					counter++;
				
					// update the indexes to continue processing other node (or time)
					
					abs_index++;
					abs_index += rel_index;
					aux=L.subList( abs_index , L.size() );
					if (aux.contains(i))
						continue;
					else break;
				}
			}
			
			this.sizes[i-MIN] = counter;
			this.scheme.add(s_list);
			
		}
		
		this.length = scheme.size();
		
	}

	@Override
	public byte[] buildNext() {
		// only build one successor
		if (status != 0) {
			return null;
		}
		status = 1;
		
		/*
		 * this the state that will be returned
		 * here it is initialized to be current basal state
		 */
		byte[] nextstate = state.clone();
		
		/*
		 * this is an auxiliar state vector used to store 
		 * the previous state over which the updates in the following
		 * group of nodes of the scheme are applied
		 */
		byte[] prev = nextstate;
		
		List <Integer> aux;
		int k_aux;
		
		boolean changed = false;
		
		// Iterate over the blocks (update times) in the list scheme
		
		for(int i = 0 ; i <this.length ; i++) {
			
			aux = scheme.get(i); 
			
			// Iterate over the nodes in the block i
			for(int j=0 ; j< this.sizes[i] ; j++) {
				
				k_aux=aux.get(j);
				
				/*
				 * if there is a change in the previous state, we apply it on the 
				 * next state
				 */
				
				int change = nodeChange(prev, k_aux);
				if (change != 0) {
					nextstate[k_aux] += change;
					changed = true;
				}
				
			}
			
			/*
			 * The current nextstate is the previous state when updating
			 * nodes belonging to the next group of nodes
			 */
			prev = nextstate;
		}
		
		if (changed) {
			return nextstate;
		}
		return null;
		
	}

}
