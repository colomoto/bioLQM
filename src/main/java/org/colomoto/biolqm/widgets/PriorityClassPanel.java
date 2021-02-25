package org.colomoto.biolqm.widgets;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

public class PriorityClassPanel extends JPanel {
	private static final long serialVersionUID = -6249588129185682333L;
	public static final Color LIGHT_RED = new Color(255, 120, 120);


	private final int GROUP_WIDTH = 90;
	private final int CLASS_SPACING = 15;
	private boolean guiMultipSuc;

	private List<List<JList<String>>> guiClasses;
	private ModelGrouping mpc;

	private JPanel jpCenter;
	private JButton jbIncClass;
	private JButton jbDecClass;
	private JButton jbIncGroup;
	private JButton jbDecGroup;

	private JButton getNoMargins(String text) {
		JButton jb = new JButton(text);
		jb.setMargin(new Insets(0, 0, 0, 0));
		return jb;
	}

	public PriorityClassPanel(ModelGrouping mpc, boolean guiMultipSuc) {
		this.mpc = mpc;
		this.guiMultipSuc = true; //
		
		this.setLayout(new BorderLayout());

		this.guiClasses = new ArrayList<List<JList<String>>>();
		this.jbIncClass = this.getNoMargins("←");
		this.jbDecClass = this.getNoMargins("→");
		this.jbIncGroup = this.getNoMargins("↑");
		this.jbDecGroup = this.getNoMargins("↓");

		JPanel jpTopCenter = new JPanel(new BorderLayout());

		// CENTER PANEL
		this.jpCenter = new JPanel(new GridBagLayout());
		jpTopCenter.add(this.jpCenter, BorderLayout.CENTER);
		this.add(jpTopCenter, BorderLayout.CENTER);

		// SOUTH PANEL
		JPanel jpSouthCenter = new JPanel(new FlowLayout());
		JButton jbSplit = this.getNoMargins("Split");
		jbSplit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				splitSelVars();
				updateGUI();
			}
		});
		jpSouthCenter.add(jbSplit);
		JButton jbUnsplit = this.getNoMargins("Unsplit");
		jbUnsplit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				unsplitSelVars();
				updateGUI();
			}
		});
		jpSouthCenter.add(jbUnsplit);

		this.jbIncClass.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				incPriorityOfSelVars();
				updateGUI();
			}
		});
		jpSouthCenter.add(this.jbIncClass);

		if (this.guiMultipSuc) {
			this.jbIncGroup.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					incGroupOfSelVars();
					updateGUI();
				}
			});
			jpSouthCenter.add(this.jbIncGroup);

			this.jbDecGroup.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					decGroupOfSelVars();
					updateGUI();
				}
			});
			jpSouthCenter.add(this.jbDecGroup);
		}

		this.jbDecClass.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				decPriorityOfSelVars();
				updateGUI();
			}
		});
		jpSouthCenter.add(this.jbDecClass);
 
		JButton jbSingle = this.getNoMargins("Collapse All");
		jbSingle.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				collapseAll();
				updateGUI();
			}
		});
		jpSouthCenter.add(jbSingle);
		this.add(jpSouthCenter, BorderLayout.SOUTH);
		jpSouthCenter.setBorder(BorderFactory.createLineBorder(Color.GRAY));
	}

	public void updatePriorityList() { 
		this.guiClasses.clear(); 
		this.jpCenter.removeAll();
		GridBagConstraints gdcCenter = new GridBagConstraints();
		gdcCenter.anchor = GridBagConstraints.FIRST_LINE_START;
		gdcCenter.gridx = 0;
		gdcCenter.gridy = 0;
		
		Map<JComboBox<String>, int[]> pcIdxGroup = new HashMap<JComboBox<String>, int[]>();
		Map<JTextField, int[]> idxJtf = new HashMap<JTextField, int[]>();


		for (int idxPC = 0; idxPC < mpc.size(); idxPC++) { 
			this.guiClasses.add(new ArrayList<JList<String>>());
			JPanel jpPClass = new JPanel(new BorderLayout());
			jpPClass.setAlignmentY(TOP_ALIGNMENT);
			JPanel jpPClassHeader = new JPanel(new BorderLayout());
			jpPClass.add(jpPClassHeader, BorderLayout.NORTH);

			JLabel jlTmp = new JLabel("   Rank " + (idxPC + 1) + "   ", SwingConstants.CENTER);
			jlTmp.setToolTipText(idxPC == 0 ? "Fastest class" : (idxPC == (mpc.size() - 1) ? "Slowest class" : ""));
			jpPClassHeader.add(jlTmp, BorderLayout.CENTER);

			// Increase Class if not first
			JButton jbInc = this.getNoMargins("←");
			jbInc.setActionCommand("" + idxPC);
			jbInc.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					JButton jb = (JButton) e.getSource();
					int pos = Integer.parseInt(jb.getActionCommand());
					mpc.switchClasses(pos, pos - 1);
					fireActionEvent();
					updatePriorityList();
				}
			});
			jpPClassHeader.add(jbInc, BorderLayout.LINE_START);
			if (idxPC == 0) {
				jbInc.setEnabled(false);
			}

			// Decrease Class if not last
			JButton jbDec = this.getNoMargins("→");
			jbDec.setActionCommand("" + idxPC);
			jbDec.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					JButton jb = (JButton) e.getSource();
					int pos = Integer.parseInt(jb.getActionCommand());
					mpc.switchClasses(pos, pos + 1);
					fireActionEvent();
					updatePriorityList();
				}
			});
			jpPClassHeader.add(jbDec, BorderLayout.LINE_END);
			if ((idxPC + 1) == mpc.size()) {
				jbDec.setEnabled(false);
			}

			// Groups Panel inside a class
			JPanel jpGroups = new JPanel(new GridBagLayout());
			GridBagConstraints gbcG = new GridBagConstraints();
			gbcG.insets = new Insets(3, 3, 3, 3);
			gbcG.gridx = 0;
	
			
			// Inside a given group of variables
			List<List<String>> lGrpVars = mpc.getClassVars(idxPC);
			
			// g += 2
			// When g is pair it adds a ComboBox, when g is impair it adds the groups
			// so the group index is g/2 

			for (int g = 0 ; g < lGrpVars.size()*2; g += 2) {
				int idxGroup = g/2;
				// get supported updaters
				JComboBox<String> jcbUpdaters = new JComboBox(ModelGrouping.getUpdatersAvailable());
	
				// get the updaterName from that group
				String updaterName = mpc.getGroupUpdaterName(idxPC, idxGroup);
				jcbUpdaters.setSelectedItem(updaterName);
				
				// save updater with idxPC and idxGroup !!!!!!!!!!!!!!!!
				pcIdxGroup.put(jcbUpdaters, new int[] {idxPC, idxGroup});
				
				
				jcbUpdaters.addActionListener(new ActionListener() {
					@Override
		            public void actionPerformed(ActionEvent e) {
		                String up = (String) jcbUpdaters.getSelectedItem();
	                	LogicalModelUpdater updater = null;

	                	// get the PC index and Group index
	                	int[] idx = pcIdxGroup.get(e.getSource());
	                	Map<JTextField, String> textfields = null;

		                switch (up) {
		                    case "Synchronous":
		                		updater = new SynchronousUpdater(mpc.getModel());
		                		mpc.addUpdater(idx[0], idx[1], updater);
		                        break;
		                        
		                    case "Random uniform":
		                    	MultipleSuccessorsUpdater MultiUpdater = new AsynchronousUpdater(mpc.getModel());
		                    	updater = new RandomUpdaterWrapper(MultiUpdater);

		                		mpc.addUpdater(idx[0], idx[1], updater);
		                        break;
		                        
		                    case "Random non uniform":
		                    	validateTextRates(idx[0],idx[1], textfields);
		                        break;
		                    default:
		                        break;
		                }
						fireActionEvent();
						updatePriorityList();	
		            }
		        });
		
				// get the group variables
				List<String> vars = lGrpVars.get(idxGroup);
								
				if (updaterName.equals("Random uniform") ||
						updaterName.equals("Random non uniform")) {
					
					// if random uniform or random non uniform, save (node string, rate) and (textfield, node string)
					Map<String, Double> rates = new HashMap<String, Double>();
					Map<JTextField, String> textfields = new HashMap<JTextField, String>();
					textfields.clear();

					//updaterName = "Random non uniform";
					if (updaterName.equals("Random uniform")) {
						for (int e = 0; e < vars.size(); e++) {
							 rates.put(vars.get(e), 1.0); 
							 }
					} else {

						Map<String, Double> upRates = mpc.getRates(idxPC, idxGroup, vars);

						// put (node string, rate)
						for (int e = 0; e < vars.size(); e++) {
							String var = vars.get(e);
							rates.put(var, upRates.get(var)); 
						}
					}
						
					 JPanel ratesPanel = new JPanel(new GridBagLayout()); 
					 GridBagConstraints gbcR = new GridBagConstraints(); gbcR.gridx = 0;
					 
					 // -- Order variables alphabetically 
					 Collections.sort(vars,String.CASE_INSENSITIVE_ORDER);
					
					 Map<String, Double> nodes = new HashMap<String, Double>();
					 for (int d = 0; d < rates.keySet().size(); d++) { 
						 String node = vars.get(d);
						 
						 Double rate = rates.get(node);
						 if (rate == null)
							 rate = 1.0;
						 
						 String nodeRate = Double.toString(rate);
						 nodes.put(node, rate);
						 
					 
						 JTextField jtf = new JTextField(nodeRate); 
						 idxJtf.put(jtf, new int[] {idxPC, idxGroup});
						 jtf.setToolTipText(node);
						 jtf.addKeyListener(new KeyListener() {
							 @Override
							 public void keyTyped(KeyEvent e) {
							 }
							 
							 @Override
							 public void keyReleased(KeyEvent e) {
								 int[] idx = idxJtf.get(e.getSource());
								 validateTextRates(idx[0], idx[1], textfields);
							}
							@Override
							public void keyPressed(KeyEvent e) {
							}
				
						});
					 
						 // put(textfield, node string) 
						 textfields.put(jtf, node);
	
						 jtf.setColumns(3); 
						 if (updaterName.equals("Random uniform")) 
							 jtf.disable();
						 
						 ratesPanel.add(jtf, gbcR); 
					 }
					 
					//JPanel ratesPanel = ratesPanel(vars, rates, textfields, updaterName);
					
                	gbcG.gridx = 1;
                	gbcG.gridy = g + 1;
					jpGroups.add(ratesPanel, gbcG);
					
				}
				
				DefaultListModel<String> lModel = new DefaultListModel<String>();
				for (String var : vars) {
					lModel.addElement(var);
				}
							
				JList<String> jList = new JList<String>(lModel);
				jList.setBorder(BorderFactory.createLoweredBevelBorder());
				jList.setFixedCellWidth(this.GROUP_WIDTH);
				jList.setFixedCellHeight(20);

				jList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);				
					
				jList.addMouseListener(new MouseListener() {
					@Override
					public void mouseReleased(MouseEvent e) {
						@SuppressWarnings("unchecked")
						JList<String> selJList = (JList<String>) e.getSource();
						for (List<JList<String>> lClass : guiClasses) {
							for (JList<String> lGroup : lClass) {
								if (!lGroup.equals(selJList)) {
									lGroup.clearSelection();
								}
							}
						}
					}

					@Override
					public void mousePressed(MouseEvent e) {
					}

					@Override
					public void mouseExited(MouseEvent e) {
					}

					@Override
					public void mouseEntered(MouseEvent e) {
					}

					@Override
					public void mouseClicked(MouseEvent e) {
					}
				});
				jList.addMouseMotionListener(new MouseMotionListener() {
					@Override
					public void mouseMoved(MouseEvent e) {
						JList<String> l = (JList<String>) e.getSource();
						DefaultListModel<String> m = (DefaultListModel<String>) l.getModel();
						int index = l.locationToIndex(e.getPoint());
						if (index > -1) {
							l.setToolTipText(m.getElementAt(index).toString());
						}
					}

					@Override
					public void mouseDragged(MouseEvent e) {
					}
				});
				
							    
				gbcG.gridx = 0;
				this.guiClasses.get(idxPC).add(jList);
				// add first comboBox
				gbcG.gridy = g;
				jpGroups.add(jcbUpdaters, gbcG);
				// then add group
				gbcG.gridy = g + 1 ;
				jpGroups.add(jList, gbcG);

			}

			JScrollPane jScroll = new JScrollPane(jpGroups);
			jScroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
			jpPClass.add(jScroll, BorderLayout.CENTER);

			if (this.guiMultipSuc) {
				JPanel jpTmp = new JPanel(new BorderLayout());
				jpTmp.add(new JLabel("Groups", SwingConstants.CENTER), BorderLayout.CENTER);
				// Expand groups -- Async class
				JButton jbExpand = this.getNoMargins("E");
				jbExpand.setToolTipText("Expand - one component per group (Asynchronous)");
				jbExpand.setActionCommand("" + idxPC);
				jbExpand.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						JButton jb = (JButton) e.getSource();
						int pos = Integer.parseInt(jb.getActionCommand());
						mpc.groupExpand(pos);
						fireActionEvent();
						updatePriorityList();
					}
				});
				jpTmp.add(jbExpand, BorderLayout.LINE_START);
				// Collapse groups -- Sync class
				JButton jbCollapse = this.getNoMargins("C");
				jbCollapse.setToolTipText("Collapse - All components in single group (Synchronous)");
				jbCollapse.setActionCommand("" + idxPC);
				jbCollapse.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						JButton jb = (JButton) e.getSource();
						int pos = Integer.parseInt(jb.getActionCommand());
						mpc.groupCollapse(pos);
						fireActionEvent();
						updatePriorityList();
					}
				});
				jpTmp.add(jbCollapse, BorderLayout.LINE_END);

				jpPClass.add(jpTmp, BorderLayout.SOUTH);
			}

			jpPClass.setMinimumSize(new Dimension( 2 * GROUP_WIDTH, 5 * GROUP_WIDTH)); // FIXME
			this.jpCenter.add(jpPClass, gdcCenter);
			gdcCenter.gridx++;
			this.jpCenter.add(Box.createRigidArea(new Dimension(this.CLASS_SPACING, 10)), gdcCenter);
			gdcCenter.gridx++;
		}
		updateGUI();
	}

	private void splitSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					for (String var : values)
						mpc.split(i, g, var);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}

	private void unsplitSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					for (String var : values)
						mpc.unsplit(i, g, var);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}

	private void incPriorityOfSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					mpc.incPriorities(i, g, values);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}

	private void decPriorityOfSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					mpc.decPriorities(i, g, values);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}

	private void incGroupOfSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					mpc.incGroup(i, g, values);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}

	private void decGroupOfSelVars() {
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					mpc.decGroup(i, g, values);
					fireActionEvent();
					break all;
				}
			}
		}
		this.updatePriorityList();
	}
	

	/*
	 * private JPanel ratesPanel(List<String> vars, Map<String, Double> rates,
	 * Map<JTextField, String> textfields, String updaterName) { JPanel ratesPanel =
	 * new JPanel(new GridBagLayout()); GridBagConstraints gbcR = new
	 * GridBagConstraints(); gbcR.gridx = 0;
	 * 
	 * // -- Order variables alphabetically Collections.sort(vars,
	 * String.CASE_INSENSITIVE_ORDER);
	 * 
	 * for (int d = 0; d < rates.keySet().size(); d++) { String node = vars.get(d);
	 * String nodeRate = Double.toString(rates.get(node));
	 * 
	 * JTextField jtf = new JTextField(nodeRate); jtf.setToolTipText(node);
	 * 
	 * // put(textfield, node string) textfields.put(jtf, node);
	 * 
	 * jtf.setColumns(3); if (updaterName.equals("Random uniform")) jtf.disable();
	 * ratesPanel.add(jtf, gbcR); }
	 * 
	 * return ratesPanel;
	 * 
	 * }
	 */
	
	private void validateTextRates(int idxPC, int idxGrp, Map<JTextField, String> textfields) {
		
		Map<String, Double> nodeRates = new HashMap<String, Double>();
		if (textfields == null) {
     		mpc.addUpdater(idxPC, idxGrp, nodeRates);
		} else {
			int i = 0;
			Boolean valid = true;
			for (JTextField jtf : textfields.keySet()) {
			
				String text = jtf.getText();
				String node = jtf.getToolTipText();
				try {
					Double rate = Double.parseDouble(text);
					jtf.setBackground(Color.white);
					nodeRates.put(node, rate);
				}
				catch(NumberFormatException er) {
					jtf.setBackground(LIGHT_RED);
					valid = false;
					break;
				}
				i++;
			}
			fireActionEvent();
			mpc.addUpdater(idxPC, idxGrp, nodeRates);
			} 
		}
	

	private void collapseAll() {
		this.mpc.collapseAll();
		fireActionEvent();
		this.updatePriorityList();
	}

	private void updateGUI() {
		this.jpCenter.revalidate();
		this.jpCenter.repaint();
	}

	// Listeners
	public void addActionListener(PanelChangedEventListener listener) {
		this.listenerList.add(PanelChangedEventListener.class, listener);
	}

	public void removeActionListener(PanelChangedEventListener listener) {
		this.listenerList.remove(PanelChangedEventListener.class, listener);
	}

	private void fireActionEvent() {
		Object[] listeners = this.listenerList.getListenerList();
		for (int i = 0; i < listeners.length; i += 2) {
			if (listeners[i] == PanelChangedEventListener.class) {
				((PanelChangedEventListener) listeners[i + 1]).panelChangedOccurred();
			}
		}
	}
}