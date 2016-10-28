package org.colomoto.logicalmodel.avatar;

import java.util.HashSet;
import java.util.Iterator;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.avatar.AvatarUtils;
import org.colomoto.logicalmodel.tool.simulation.SingleSuccessorSimulation;
import org.colomoto.logicalmodel.tool.simulation.updater.SequentialUpdater;

public class AvatarSimulation extends SingleSuccessorSimulation {

	public static int MAX_STEPS = 100;
	public HashSet<byte[]> hs;

	public AvatarSimulation(LogicalModel model, byte[] initialState) {
		super(new SequentialUpdater(model), initialState, MAX_STEPS);
		hs = new HashSet<byte[]>();
	}
	
	public void runSimulation() {
		int k=0;
		Iterator<byte[]> it = iterator();
		while (it.hasNext() && k++<100) {
			byte[] state = it.next();
			System.out.println("Succ:"+AvatarUtils.toString(state));			
		}
		System.out.println("END("+k+")");
	}
}
