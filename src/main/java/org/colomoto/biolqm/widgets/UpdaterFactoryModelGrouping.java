package org.colomoto.biolqm.widgets;

import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicPriorityUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.PCRankGroupsVars;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.CompleteUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

public class UpdaterFactoryModelGrouping {
	
	
	public static String[] getSupportedUpdaters(boolean multi, boolean single) {
		ResourceBundle bundle = ResourceBundle.getBundle("Updaters");
		List<String> updaters = new ArrayList<String>();
		if (multi) {
			if (bundle.containsKey("MULTI_SUCCESSOR_UPDATERS")) {
				String[] upd = bundle.getString("MULTI_SUCCESSOR_UPDATERS").split(",");
				for (String updater : upd)
					updaters.add(updater);
			}
		} if (single) {
			if (bundle.containsKey("SINGLE_SUCCESSOR_UPDATERS")) {
				String[] upd = bundle.getString("SINGLE_SUCCESSOR_UPDATERS").split(",");
				for (String updater : upd)
					updaters.add(updater);
			}
		}
		String[] updatersArray = new String[updaters.size()];
		for (int i = 0; i < updaters.size(); i++)
			updatersArray[i] = updaters.get(i);
		

		return updatersArray;
	}

	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater) {

	        if (updater.equalsIgnoreCase("Synchronous")) {
	            return new SynchronousUpdater(model);
	        }
	        if (updater.equalsIgnoreCase("Sequential")) {
	                return new SequentialUpdater(model);    
	                
	        } if (updater.equalsIgnoreCase("Asynchronous")) {
	            return new AsynchronousUpdater(model);
	            
	        } else if (updater.equalsIgnoreCase("Complete")) {
	            return new CompleteUpdater(model);
	            
	        } else if (updater.equalsIgnoreCase("Random uniform")) {
		        return new RandomUpdaterWrapper(new AsynchronousUpdater(model));
		        
	        } else if (updater.equalsIgnoreCase("Random non uniform")) {
		        return new RandomUpdaterWithRates(model);
		        
	        } else  if (updater.equalsIgnoreCase("Block Sequential")) {
	            return new BlockSequentialUpdater(new PCRankGroupsVars(model));
	            
	        } else if (updater.equalsIgnoreCase("Deterministic Priority")) {
	            return new DeterministicPriorityUpdater(new PCRankGroupsVars(model));
	   
	        } else if (updater.equalsIgnoreCase("priority")) {
	            return new PriorityUpdater(new PCRankGroupsVars(model));
	        }

	        throw new RuntimeException("Unrecognized updater: " + updater);
	    }
	 
	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater, String order) {

		 	if (updater.equalsIgnoreCase("Sequential")) {
	        	return new SequentialUpdater(model, order); 	
	        } else if (updater.equalsIgnoreCase("Block Sequential")) {
	            return new BlockSequentialUpdater(model, order);
	        } else if (updater.equalsIgnoreCase("Deterministic Priority")) {
	            return new DeterministicPriorityUpdater(model, order);
	        }

	        throw new RuntimeException("Unrecognized updater: " + updater);
	    }
	 
	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater, PCRankGroupsVars mpc) {

		 	if (updater.equalsIgnoreCase("Block Sequential")) {
	            return new BlockSequentialUpdater(mpc);	            	         
	        } else if (updater.equalsIgnoreCase("Deterministic Priority")) {
	            return new DeterministicPriorityUpdater(mpc);	   
	        } else if (updater.equalsIgnoreCase("Priority")) {
	            return new PriorityUpdater(mpc);
	        }
	        throw new RuntimeException("Unrecognized updater: " + updater);
	    }
	 
	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater, Double[] rates) {
		 	if (updater.equalsIgnoreCase("Random non uniform")) 
		 		return  new RandomUpdaterWithRates(model, rates);
		 	
	        throw new RuntimeException("Unrecognized updater: " + updater);
		 
	 }
	
}
