package org.colomoto.biolqm.helper.clingo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parse and store individual results from a clingo run.
 * 
 * @author Aurelien Naldi
 */
public class ClingoResult {

	private static final Pattern p = Pattern.compile("(?<id>[a-zA-Z]\\w*)([(](?<params>[^)]*)[)])?\\s*");
	private static final Pattern p2 = Pattern.compile("(\"(?<id>[^\"]*)\"|(?<val>[0-9]+))\\s*,?\\s*");

	private final Map<String, List<String[]>> content = new HashMap<String, List<String[]>>();
	
	public ClingoResult(String line) {
		Matcher m = p.matcher(line);
		int idx = 0;
		while (m.find(idx)) {
			idx = m.end();
			String id = m.group("id");
			String params = m.group("params");
			if (params == null) {
				add(id);
				continue;
			}
			List<String> args = new ArrayList<String>();
			Matcher m2 = p2.matcher(params);
			int sidx = 0;
			while (m2.find(sidx)) {
				sidx = m2.end();
				String s = m2.group("id");
				if (s != null) {
					args.add(s);
				} else {
					args.add(m2.group("val"));
				}
			}
			String[] content = new String[args.size()];
			for (int i=0 ; i<content.length ; i++) {
				content[i] = args.get(i);
			}
			add(id, content);
		}
	}
	
	private void add(String data) {
		this.add("", new String[] {data});
	}
	
	private void add(String key, String[] args) {
		if (!content.containsKey(key)) {
			content.put(key, new ArrayList<String[]>());
		}
		content.get(key).add(args);
	}
	
	public List<String[]> get(String group) {
		return content.get(group);
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (String key: content.keySet()) {
			sb.append(key+": ");
			for (String[] item: content.get(key)) {
				String prefix = "";
				for (String s: item) {
					sb.append(prefix+s);
					prefix = ",";
				}
				sb.append(" ");
			}
			sb.append("    ");
		}
		return sb.toString();
	}
	
}
