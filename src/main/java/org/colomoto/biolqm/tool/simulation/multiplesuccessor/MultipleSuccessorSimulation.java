package org.colomoto.biolqm.tool.simulation.multiplesuccessor;

import java.util.*;

/**
 * Common structure for simulations with updaters yielding multiple successors.
 * This base class only deals with the queue of states to visit.
 *
 * @author Lucas Baudin
 * @author Aurelien Naldi
 */
public abstract class MultipleSuccessorSimulation {

	// exploration queue
	private final LinkedList<QueuedState> queue;
	private final MultipleSuccessorsUpdater updater;
	private final SimulationStrategy strategy;

	private int max_depth = -1;
	private int current_depth = 0;


	public MultipleSuccessorSimulation(MultipleSuccessorsUpdater updater) {
		this(updater, SimulationStrategy.DEPTH_FIRST);
	}

	public MultipleSuccessorSimulation(MultipleSuccessorsUpdater updater, SimulationStrategy strategy) {
		this.updater = updater;
		this.strategy = strategy;
		queue = new LinkedList<QueuedState>();
	}

	public void runSimulation(Iterator<byte[]> initialStates) {
		while (initialStates.hasNext()) {
			addState(initialStates.next());
			runSimulation();
		}
	}
	
	public void runSimulation() {
		while (!queue.isEmpty()) {
			QueuedState queued;
			if (this.strategy == SimulationStrategy.BREADTH_FIRST) {
				queued = queue.removeFirst();
			} else {
				queued = queue.removeLast();
			}

			byte[] state = queued.state;
			current_depth = queued.depth + 1;

			if (max_depth > 0 && current_depth > max_depth) {
				continue;
			}

			for(byte[] child : updater.getSuccessors(state)) {
				this.addState(child);
				addTransition(state, child);
			}
		}
	}

	protected void enqueue(byte[] state) {
		this.queue.addLast( new QueuedState(state, current_depth) );
	}

	/**
	 * Add a state to the result of this simulation.
	 * If the state is new, it should be enqueued
	 *
	 * @param state the added state
	 */
	public abstract void addState(byte[] state);

	/**
	 * Add a transition
	 *
	 * @param from source state
	 * @param to target state
	 */
	public abstract void addTransition(byte[] from, byte[] to);
}

/**
 * Associate a queued state with its depth
 */
class QueuedState {
	public final byte[] state;
	public final int depth;

	public QueuedState(byte[] state, int depth) {
		this.state = state;
		this.depth = depth;
	}
}
