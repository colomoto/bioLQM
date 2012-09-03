package org.colomoto.logicalmodel.io.sbml;

import java.io.File;
import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.sbml.jsbml.Model;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.ext.SBasePlugin;
import org.sbml.jsbml.ext.layout.ExtendedLayoutModel;
import org.sbml.jsbml.ext.layout.LayoutConstants;
import org.sbml.jsbml.ext.qual.QualConstant;
import org.sbml.jsbml.ext.qual.QualitativeModel;
import org.sbml.jsbml.xml.stax.SBMLReader;

/**
 * Helper to create new SBML documents from scratch or from files using JSBML.
 * It will create a bundle with a convenient access to the document,
 * its enclosed model and the qualitative part of the model.
 * 
 * @author Aurelien Naldi
 */
public class SBMLqualHelper {

	public static SBMLQualBundle loadFile(File f) throws IOException, XMLStreamException {
		return getQualitativeModel( parseFile(f));
	}
	
	public static SBMLDocument parseFile(File f) throws IOException, XMLStreamException {
		
		return new SBMLReader().readSBML(f);
	}
	
	/**
	 * Create a new Bundle, with an empty qualitative model
	 * 
	 * @return a new SBML document
	 */
	public static SBMLQualBundle newBundle() {
		// init SBML document
		SBMLDocument sdoc = new SBMLDocument(3,1);
		sdoc.addNamespace(QualConstant.shortLabel, "xmlns", QualConstant.namespaceURI);
		sdoc.addNamespace(LayoutConstants.shortLabel, "xmlns", LayoutConstants.namespaceURI);

		// create the main SBML model
		Model smodel = sdoc.createModel("model_id");
		
		// add qual and layout extensions
		QualitativeModel qmodel = new QualitativeModel(smodel);
		smodel.addExtension(QualConstant.namespaceURI, qmodel);
		ExtendedLayoutModel lmodel = new ExtendedLayoutModel(smodel);
		smodel.addExtension(LayoutConstants.namespaceURI, lmodel);

		return new SBMLQualBundle(sdoc, smodel, qmodel, lmodel);
	}
	
	
	private static SBMLQualBundle getQualitativeModel(SBMLDocument sdoc) {
		Model smodel = sdoc.getModel();
		
		// Warning: how will we deal with multiple namespace versions?
		QualitativeModel qmodel = null;
		SBasePlugin plugin = smodel.getExtension(QualConstant.namespaceURI);
		if (plugin instanceof QualitativeModel) {
			qmodel = (QualitativeModel)plugin;
		}

		ExtendedLayoutModel lmodel = null;
		plugin = smodel.getExtension(LayoutConstants.namespaceURI);
		if (plugin instanceof ExtendedLayoutModel) {
			lmodel = (ExtendedLayoutModel)plugin;
		}

		return new SBMLQualBundle(sdoc, smodel, qmodel, lmodel);
	}
}
