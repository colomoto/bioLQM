package org.colomoto.biolqm.tool.simulation.avatar;

import java.util.HashSet;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorSimulation;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;

public class FirefrontSimulation extends MultipleSuccessorSimulation {

	public HashSet<byte[]> hs;
	
	public FirefrontSimulation(LogicalModel model, byte[] initialState) {
		super(new AsynchronousUpdater(model));
		hs = new HashSet<byte[]>();
		addState(initialState);
	}
	
	@Override
	public void addState(byte[] state) {
		if ( !contains(state) ) {
			hs.add(state);
			enqueue(state);
		}
	}

	// Not optimal solution, but an hashcode of a byte[] is not well defined
	boolean contains(byte[] a) {
		boolean found = false;
		for(byte[] b: hs) {
			boolean idem = true;
			for(int i = 0; i < b.length; i++)
				idem = idem && b[i] == a[i];
			found = found || idem;
		}
		return found;
	}

	@Override
	public void addTransition(byte[] from, byte[] to) {}

	public FirefrontSimulation getAsyncSuccessors(byte[] next) {
		// TODO Auto-generated method stub
		return null;
	}

	public List<byte[]> getAsyncSuccessors() {
		// TODO Auto-generated method stub
		return null;
	}

}
