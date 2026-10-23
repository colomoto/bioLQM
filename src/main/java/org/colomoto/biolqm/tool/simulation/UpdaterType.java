package org.colomoto.biolqm.tool.simulation;

public enum UpdaterType {

	SYNC("Synchronous"),
	ASYNC("Asynchronous"),
	RAND_ASYNC("Random asynchronous"),
	RAND_NON_UNIF("Random non uniform"),
	COMPLETE("Complete"),
	SEQUENTIAL("Sequential"),
	BLOCK_SEQ("Block sequential"),
	DETERM_PRI("Deterministic priority"),
	PRIORITIES("Priorities")
	;

	private String description;

	private UpdaterType(String description) {
		this.description = description;
	}

	public String toString() {
		return this.description;
	}
}
