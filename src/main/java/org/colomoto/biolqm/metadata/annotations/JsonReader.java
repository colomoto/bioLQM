package org.colomoto.biolqm.metadata.annotations;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.json.JSONException;
import org.json.JSONObject;

public class JsonReader {

	private static String readAll(Reader rd) throws IOException {
		StringBuilder sb = new StringBuilder();
		int cp;
		while ((cp = rd.read()) != -1) {
			sb.append((char) cp);
		}
		return sb.toString();
	}
  
	public static JSONObject readInputStream(InputStream is) throws IOException, JSONException {
		try {
			BufferedReader rd = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8));
			String jsonText = readAll(rd);
			
			JSONObject json = new JSONObject(jsonText);
			return json;
		} finally {
			is.close();
		}
	}

	public static JSONObject readJsonFromUrl(String url) throws MalformedURLException, IOException {
		InputStream is = new URL(url).openStream();
		return readInputStream(is);
	}
	
	public static JSONObject readJsonFromFile(String filename) throws MalformedURLException, IOException {
		File initialFile = new File(filename);
		InputStream is = new FileInputStream(initialFile);
		return readInputStream(is);
	}
}
