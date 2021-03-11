package org.colomoto.biolqm.tool.simulation.grouping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ResourceBundle;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.BlockSequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SequentialUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.CompleteUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.PriorityUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

public class testReadUp {
	
	private final static Class<?>[] availableUpdaters = new Class<?>[] {SynchronousUpdater.class, SequentialUpdater.class, 
			BlockSequentialUpdater.class, RandomUpdaterWithRates.class, RandomUpdaterWrapper.class,
			AsynchronousUpdater.class, CompleteUpdater.class, PriorityUpdater.class};
	
		
	public static List getSupportedUpdaters(boolean multi, boolean single) {
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
					
		return updaters;
	}
	
	public static LogicalModelUpdater getUpdater(String updaterName, LogicalModel m) {
		return getUpdater(updaterName, m, null);
	}

	public static LogicalModelUpdater getUpdater(String updaterName, LogicalModel m, double[] rates) {
		
		if (updaterName == "Random non uniform") {
			return new RandomUpdaterWithRates(m, rates);
		}
		
		for (Class<?> updaterClass : availableUpdaters) {
			try {
				Method method = updaterClass.getMethod("getUpdaterName");
				try {
					String name = (String) method.invoke(null);
					
					if (name.equals(updaterName)) {
						try {
							BaseUpdater newUpdater = (BaseUpdater) updaterClass.
									getConstructor(LogicalModel.class).newInstance(m);
							return newUpdater;
						} catch (InstantiationException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				} catch (IllegalAccessException e) {
					System.out.println("FAILED");
					e.printStackTrace();
				} catch (IllegalArgumentException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InvocationTargetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}		
			} catch (NoSuchMethodException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	
		return null;
		
	}
	
	
}
