package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.AnnotationTarget;

/**
 * One instance per model opened to store the different types of qualifiers available for each component
 * When a URI involving a new collection is added, the list of collections useful for the relevant qualifier is updated
 *
 * @author Martin Boutroux
 */
public class QualifiersAvailable {

	static private final QualifierGroup BQMODEL, BQBIOL;

	static {
		ClassLoader cl = QualifiersAvailable.class.getClassLoader();
		BQMODEL = new QualifierGroup("bqmodel", cl.getResourceAsStream("bqmodel.yaml"));
		BQBIOL = new QualifierGroup("bqbiol", cl.getResourceAsStream("bqbiol.yaml"));
	}

	private final QualifierGroup model;
	private final QualifierGroup species;
	private final QualifierGroup transition;
	
	public QualifiersAvailable() {
		this.model = new QualifierGroup("custom");
		this.species = new QualifierGroup("custom");
		this.transition = new QualifierGroup("custom");
	}

	public Qualifier ensureQualifier(AnnotationTarget target, String qualifier) {
		switch (target) {
			case Model:
				return this.ensureQualifier(BQMODEL, this.model, qualifier);
			case Component:
				return this.ensureQualifier(BQBIOL, this.species, qualifier);
			case Interaction:
				return this.ensureQualifier(BQBIOL, this.transition, qualifier);
		}

		// Unreachable
		return null;
	}

	private Qualifier ensureQualifier(QualifierGroup g1, QualifierGroup g2, String name) {
		Qualifier q = g1.get(name);
		if (q != null) {
			return q;
		}
		return g2.ensure(name);
	}
}