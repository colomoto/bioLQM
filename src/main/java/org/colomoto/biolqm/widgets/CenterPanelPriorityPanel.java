package org.colomoto.biolqm.widgets;

import javax.swing.JPanel;

public class CenterPanelPriorityPanel extends JPanel {

	@Override
	public java.awt.Dimension getPreferredSize() {
		   return new java.awt.Dimension(super.getPreferredSize().width,
				   super.getSize().height);
	}
}
