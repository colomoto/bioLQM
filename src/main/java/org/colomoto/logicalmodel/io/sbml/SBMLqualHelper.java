package org.colomoto.logicalmodel.io.sbml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLStreamException;

import org.sbml.jsbml.Model;
import org.sbml.jsbml.SBMLDocument;
import org.sbml.jsbml.ext.SBasePlugin;
import org.sbml.jsbml.ext.layout.LayoutModelPlugin;
import org.sbml.jsbml.ext.layout.LayoutConstants;
import org.sbml.jsbml.ext.qual.QualConstants;
import org.sbml.jsbml.ext.qual.QualModelPlugin;
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
	
	public static SBMLQualBundle parseInputStream(InputStream in) throws XMLStreamException {
		return getQualitativeModel( new SBMLReader().readSBMLFromStream(in));
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
		return newBundle(false);
	}
	public static SBMLQualBundle newBundle(boolean addLayout) {
		// init SBML document
		SBMLDocument sdoc = new SBMLDocument(3,1);
		sdoc.addNamespace(QualConstants.shortLabel, "xmlns", QualConstants.namespaceURI);
		if (addLayout) {
			sdoc.addNamespace(LayoutConstants.shortLabel, "xmlns", LayoutConstants.namespaceURI);
		}

		// create the main SBML model
		Model smodel = sdoc.createModel("model_id");
		
		// add qual and layout extensions
		QualModelPlugin qmodel = new QualModelPlugin(smodel);
		smodel.addExtension(QualConstants.namespaceURI, qmodel);
		// Add the "required" attributes for the extensions (should be automated later)
		sdoc.getSBMLDocumentAttributes().put(QualConstants.shortLabel + ":required", "true");

		LayoutModelPlugin lmodel = null;
		if (addLayout) {
			lmodel = new LayoutModelPlugin(smodel);
			smodel.addExtension(LayoutConstants.namespaceURI, lmodel);
			sdoc.getSBMLDocumentAttributes().put(LayoutConstants.shortLabel + ":required", "false");
		}

		return new SBMLQualBundle(sdoc, smodel, qmodel, lmodel);
	}
	
	
	private static SBMLQualBundle getQualitativeModel(SBMLDocument sdoc) {
		Model smodel = sdoc.getModel();
		
		// Warning: how will we deal with multiple namespace versions?
		QualModelPlugin qmodel = null;
		SBasePlugin plugin = smodel.getExtension(QualConstants.namespaceURI);
		if (plugin instanceof QualModelPlugin) {
			qmodel = (QualModelPlugin)plugin;
		} else {
			System.out.println("Failed creating the qual model plugin");
		}

		LayoutModelPlugin lmodel = null;
		plugin = smodel.getExtension(LayoutConstants.namespaceURI);
		if (plugin instanceof LayoutModelPlugin) {
			lmodel = (LayoutModelPlugin)plugin;
		}

		return new SBMLQualBundle(sdoc, smodel, qmodel, lmodel);
	}
}
