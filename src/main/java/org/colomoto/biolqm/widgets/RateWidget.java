package org.colomoto.biolqm.widgets;
import java.util.List;

import javax.swing.JTextField;

public class RateWidget {
	
	public String node;
	public JTextField jtf = null;
	
	RateWidget(String node, JTextField jtf) {
		this.node = node;
		this.jtf = jtf;
	}
	
	RateWidget(String node) {
		this.node = node;
	}
	
	@Override
	public String toString() {
		return this.node;
	}
}
	

