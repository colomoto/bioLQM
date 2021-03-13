package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicPriorityUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.CompleteUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

public class UpdaterFactory2 {

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
	            
	        } else if (updater.equalsIgnoreCase("Random Asynchronous")) {
		        return new RandomUpdaterWrapper(new AsynchronousUpdater(model));
		        
	        } else  if (updater.equalsIgnoreCase("Block Sequential")) {
	            return new BlockSequentialUpdater(new ModelGrouping(model));
	            
	        } else if (updater.equalsIgnoreCase("Deterministic Priority")) {
	            return new DeterministicPriorityUpdater(new ModelGrouping(model));
	   
	        } else if (updater.equalsIgnoreCase("priority")) {
	            return new PriorityUpdater(new ModelGrouping(model));
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
	 
	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater, ModelGrouping mpc) {

		 	if (updater.equalsIgnoreCase("Block Sequential")) {
	            return new BlockSequentialUpdater(mpc);	            	         
	        } else if (updater.equalsIgnoreCase("Deterministic Priority")) {
	            return new DeterministicPriorityUpdater(mpc);	   
	        } else if (updater.equalsIgnoreCase("Priority")) {
	            return new PriorityUpdater(mpc);
	        }
	        throw new RuntimeException("Unrecognized updater: " + updater);
	    }
	 
	 public static LogicalModelUpdater getUpdater(LogicalModel model, String updater, double[] rates) {
		 	if (updater.equalsIgnoreCase("Random non uniform")) 
		 		return  new RandomUpdaterWithRates(model, rates);
		 	
	        throw new RuntimeException("Unrecognized updater: " + updater);
		 
	 }
	
}
