package org.colomoto.logicalmodel.tool.simulation;

import java.util.Stack;
import java.util.EmptyStackException;

public abstract class Simulation {
	
	protected Stack<byte[]> queue; // exploration queue
	MultipleSuccessorsUpdater updater;

	public Simulation(MultipleSuccessorsUpdater updater) {
		this.updater = updater;
		queue = new Stack<byte[]>();
	}

	public void addState(byte[] state_to_visit) { // states that still have to be visited
		queue.push(state_to_visit);
	}

	public void runSimulation() {
		byte[] state = null;
		
		while (true) {
			try {
				state = queue.pop();
			}
			catch(EmptyStackException e) {
				break;
			}
			for(byte[] child : updater.getSuccessors(state)) {
				if (addTransition(state, child)) {
					addState(child);
				}
			}
		}
	}

	public abstract boolean addTransition(byte[] from, byte[] to);
}

