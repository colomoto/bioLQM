package org.colomoto.logicalmodel.io.sbml;

import org.sbml.jsbml.Model;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.ext.qual.QualitativeModel;

/**
 * Simple wrapper to get together a SBMLDocument, its enclosed model and qualitative model.
 *  
 * @author Aurelien Naldi
 */
public class SBMLQualBundle {

	public final SBMLDocument document;
	public final Model model;
	public final QualitativeModel qmodel;
	
	
	public SBMLQualBundle(SBMLDocument document, Model model, QualitativeModel qmodel) {
		this.document = document;
		this.model = model;
		this.qmodel = qmodel;
	}
	
}
