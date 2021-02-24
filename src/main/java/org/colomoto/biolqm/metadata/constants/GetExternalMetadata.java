package org.colomoto.biolqm.metadata.constants;

import org.colomoto.biolqm.metadata.annotations.URI;
import org.colomoto.biolqm.metadata.annotations.JsonReader;

import org.colomoto.biolqm.metadata.constants.ModelConstants;

import org.json.JSONObject;
import org.json.JSONArray;

import java.io.IOException;


/**
 * Class used to interrogate the Crossref API in a separate thread (cause it's a slow API)
 *
 * @author Martin Boutroux
 */
public class GetExternalMetadata extends Thread {
	
	// variables
	private ModelConstants modelConstants;
	private String collection;
	private String identifier;
	
	// constructors
	public GetExternalMetadata(ModelConstants newModelConstants, String newCollection, String newIdentifier) {
		this.modelConstants = newModelConstants;
		this.collection = newCollection;
		this.identifier = newIdentifier;
	}
	
	public void run() {
		try {
			URI uri = new URI(this.collection, this.identifier);
			String url = "https://api.crossref.org/works/"+this.identifier;
			
			JSONObject json = JsonReader.readJsonFromUrl(url);
			JSONObject jsonMessage = json.getJSONObject("message");
			
			String title = null;
			Integer year = null;
			if (jsonMessage.has("title") && !jsonMessage.isNull("title")) {
				title = jsonMessage.getJSONArray("title").getString(0).toString();
			}
			if (jsonMessage.has("created") && !jsonMessage.isNull("created")) {
				year = jsonMessage.getJSONObject("created").getJSONArray("date-parts").getJSONArray(0).getInt(0);
			}
			
			if (title != null && year != null) {
				this.modelConstants.getInstanceOfExternalMetadata().updateExternalMetadata(uri, title, String.valueOf(year));
			}
			else {
				System.err.println("Error retrieving the metadata of the doi: at least one of the characteristics couldn't be fetched." + "\n");
			}
		} catch (IOException e) {
			System.err.println("Error retrieving the metadata of the doi." + "\n");
		}
	}
}
