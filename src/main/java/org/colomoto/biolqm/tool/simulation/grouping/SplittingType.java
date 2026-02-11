package org.colomoto.biolqm.tool.simulation.grouping;

public enum SplittingType {

	MERGED(""), POSITIVE("[+]"), NEGATIVE("[-]");

	private String description;

	private SplittingType(String description) {
		this.description = description;
	}

	public String toString() {
		return this.description;
	}

}
