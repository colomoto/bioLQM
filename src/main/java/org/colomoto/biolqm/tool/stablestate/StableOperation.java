package org.colomoto.biolqm.tool.stablestate;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.NodeRelation;
import org.colomoto.mddlib.operators.AbstractFlexibleOperator;


/**
 * MDD operation used to build a stability condition.
 * The operation needs the current node and merges an existing condition with its function.
 * <p/><b>Warning</b>: it is NOT thread-safe. Use multiple instances of this class for multiple threads.
 * 
 * @author Aurelien Naldi
 */
public class StableOperation extends AbstractFlexibleOperator {
	
	MDDVariable ref;
	byte nbVal;
	
	public StableOperation() {
		super(MergeAction.ASKME);
		for (NodeRelation rel: NodeRelation.values()) {
			setAction(rel, MergeAction.ASKME);
		}
	}

	/**
	 * Build the stability condition for a node, and merge it with the existing condition.
	 * Note: Freeing "known" and "f" is left under the caller responsibility.
	 * 
	 * @param factory the MDD factory
	 * @param known ID of the existing result in factory
	 * @param f ID of the function of the considered node in the factory
	 * @param var the corresponding variable in the factory
	 * 
	 * @return the ID of the merged stability condition
	 */
	public int getStable(MDDManager factory, int known, int f, MDDVariable var) {
		this.ref = var;
		this.nbVal = var.nbval;
		int ret = combine(factory, known, f);
		return ret;
	}

	@Override
	public MergeAction ask(MDDManager factory, NodeRelation type, int first, int other) {
		switch (type) {
			case LL:
				if (first == 0) {
					return MergeAction.THIS;
				}
				return MergeAction.CUSTOM;
			case NL:
			case NN:
			case NNn:
				if (ref.after( factory.getNodeVariable(first) )) {
					return MergeAction.RECURSIVE;
				}
				return MergeAction.CUSTOM;

			case LN:
				if (first == 0) {
					return MergeAction.THIS;
				}
			case NNf:
				if (ref.after( factory.getNodeVariable(other) )) {
					return MergeAction.RECURSIVE;
				}
				return MergeAction.CUSTOM;
		}
		System.err.println("DEBUG: Stable ask should not come here!");
		return MergeAction.ASKME;
	}

	@Override
	public int custom(MDDManager factory, NodeRelation type, int first, int other) {
		int[] children = new int[nbVal];
		MDDVariable firstVar = factory.getNodeVariable(first);
		switch (type) {
		case LL:
		case NL:
			if ( ref.equals( firstVar)) {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = (i==other) ? factory.getChild(first, i) : 0;
				}
			} else {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = (i==other) ? first : 0;
				}
			}
			return ref.getNode(children);
		case LN:
		case NN:
		case NNf:
		case NNn:
			if (type == NodeRelation.NNn && ref.equals(firstVar)) {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = IsStableOperation.getOp(i).combine(factory, factory.getChild(first, i), other);
				}
			} else if (type == NodeRelation.NN && ref.equals(firstVar)) {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = IsStableOperation.getOp(i).combine(factory, factory.getChild(first, i), factory.getChild(other, i));
				}
			} else if (type != NodeRelation.NNn && ref.equals(factory.getNodeVariable(other))) {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = IsStableOperation.getOp(i).combine(factory, first, factory.getChild(other, i));
				}
			} else {
				for (int i=0 ; i<nbVal ; i++) {
					children[i] = IsStableOperation.getOp(i).combine(factory, first, other);
				}
			}
			return ref.getNode(children);
		}
		System.err.println("DEBUG: Stable custom should not come here!");
		return -1;
	}
}

/**
 * MDD operation to build stability condition.
 * The operation knows the value of the current node and merges an existing condition with its function.
 * It only acts once a leaf is reached in the function of the current node.
 * It should be used by the StableOperation operation, not directly.
 * 
 * @author Aurelien Naldi
 */
class IsStableOperation extends AbstractFlexibleOperator {
	private static List<IsStableOperation> OPERATIONS = new ArrayList<IsStableOperation>();
	static {
		for (int i=0 ; i<2 ; i++) {
			OPERATIONS.add(new IsStableOperation(i));
		}
	}

	public static IsStableOperation getOp(int value) {
		if (value<0) {
			return new IsStableOperation(value);
		}
		if (value >= OPERATIONS.size()) {
			for (int i=OPERATIONS.size() ; i<=value ; i++) {
				OPERATIONS.add(new IsStableOperation(i));
			}
		}
		return OPERATIONS.get(value);
	}

	private final int value;

	private IsStableOperation(int value) {
		super(MergeAction.CUSTOM);
		setAction(NodeRelation.LN, MergeAction.ASKME);
		setAction(NodeRelation.NL, MergeAction.CUSTOM);
		this.value = value;
		lock();
	}

	@Override
	public MergeAction ask(MDDManager factory, NodeRelation type, int first, int other) {
		switch (type) {
		case LN:
			if (first == 0) {
				return MergeAction.THIS;
			}
			return MergeAction.RECURSIVE;
		}
		System.err.println("DEBUG: isStable ask should not come here!");
		return null;
	}

	@Override
	public int custom(MDDManager factory, NodeRelation type, int first, int other) {
		switch (type) {
		case LL:
		case NL:
			if (other == value) {
				return factory.use(first);
			}
			return 0;
		}
		System.err.println("DEBUG: isStable custom should not come here!");
		return -1;
	}
}
