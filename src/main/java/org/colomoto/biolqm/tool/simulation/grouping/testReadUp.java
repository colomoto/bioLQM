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
	
	public static LogicalModelUpdater getUpdater(String updaterName, LogicalModel m) {
		return getUpdater(updaterName, m, null);
	}

	public static LogicalModelUpdater getUpdater(String updaterName, LogicalModel m, Double[] rates) {
		
		if (updaterName == "Random non uniform") {
			return new RandomUpdaterWithRates(m, rates);
		}
		
		for (Class<?> updaterClass : availableUpdaters) {
				Method method = null;
				try {
					method = updaterClass.getMethod("getUpdaterClassName");
				} catch (NoSuchMethodException | SecurityException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
					String name = null;
					try {
						name = (String) method.invoke(null);
					} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					if (name.equals(updaterName)) {
						BaseUpdater newUpdater = null;
						try {
							newUpdater = (BaseUpdater) updaterClass.
										getConstructor(LogicalModel.class).newInstance(m);
						} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
								| InvocationTargetException | NoSuchMethodException | SecurityException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						return newUpdater;

					}
		}
	
		return null;
		
	}
	
	
}
