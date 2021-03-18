package org.colomoto.biolqm.widgets;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EmptyBorder;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.testReadUp;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.AsynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;



public class PriorityClassPanel extends JPanel {
	private static final long serialVersionUID = -6249588129185682333L;
	public static final Color LIGHT_RED = new Color(255, 120, 120);

	private final int GROUP_WIDTH = 83;
	// HEIGHT = 19 because it needs to match the (rates) textbox height
	private final int GROUP_HEIGHT = 19;
	private final int CLASS_SPACING = 15;

	private boolean guiMultipSuc;
	private boolean multiUpdater;
	private boolean singleUpdater;

	private Map<String, Double> ratesCache;
	private List<List<JList<String>>> guiClasses;
	private ModelGrouping mpc;

	private JPanel jpCenter;
	private JButton jbIncClass;
	private JButton jbDecClass;
	private JButton jbIncGroup;
	private JButton jbDecGroup;
	// if guiMultipSuc  = true
	private JButton jbSingle;
	private JButton jbSplit;
	private JButton jbUnsplit;
	private JButton jbCollapse;
	private JButton jbExpand;


	private JButton getNoMargins(String text) {
		JButton jb = new JButton(text);
		jb.setMargin(new Insets(0, 0, 0, 0));
		return jb;
	}

	public PriorityClassPanel(ModelGrouping mpc, boolean guiMultipSuc) {
		this(mpc, guiMultipSuc, true, false);
	}
	
	public PriorityClassPanel(ModelGrouping mpc, boolean guiMultipSuc,
			 boolean singleUpdater, boolean multiUpdater) {
		this.mpc = mpc;
		this.guiMultipSuc = guiMultipSuc; //
		this.singleUpdater = singleUpdater;
		this.multiUpdater = multiUpdater;
		this.ratesCache = new HashMap<String, Double>();
		
		this.setLayout(new BorderLayout());

		this.guiClasses = new ArrayList<List<JList<String>>>();
		this.jbIncClass = this.getNoMargins("←");
		this.jbDecClass = this.getNoMargins("→");
		this.jbIncGroup = this.getNoMargins("↑");
		this.jbDecGroup = this.getNoMargins("↓");


		// CENTER PANEL
		
		this.jpCenter = new CenterPanelPriorityPanel();
		this.jpCenter.setLayout(new GridBagLayout());
		this.jpCenter.addMouseListener(new MouseListener() {

			@Override
			public void mouseClicked(MouseEvent e) {
			}
			@Override
			public void mousePressed(MouseEvent e) {
				for (int pc = 0; pc < guiClasses.size(); pc ++) {
					for (JList<String> lGroup : guiClasses.get(pc)) 
						lGroup.clearSelection();
					}
			}
			@Override
			public void mouseReleased(MouseEvent e) {
			}
			@Override
			public void mouseEntered(MouseEvent e) {			
			}
			@Override
			public void mouseExited(MouseEvent e) {
			}
		});
		
        JScrollPane scrollPane = new JScrollPane(this.jpCenter,
        		ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
        		ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.add(scrollPane , BorderLayout.CENTER);
		

		// SOUTH PANEL
		JPanel jpSouthCenter = new JPanel(new BorderLayout());
		
		JPanel jpSouth= new JPanel(new GridBagLayout());
		GridBagConstraints gbcC = new GridBagConstraints();
        gbcC.insets = new Insets(4, 4, 4, 4);
        gbcC.gridx = 0;
        gbcC.gridy = 0;

		jpSouth.setBorder(BorderFactory.createTitledBorder("Components"));
		
		
		JPanel jpMove= new JPanel();
		jpMove.setBorder(BorderFactory.createTitledBorder("Move"));
//		GridBagConstraints gbcM = new GridBagConstraints();
//        gbcM.insets = new Insets(4, 4, 4, 4);
//        gbcM.gridx = 0;
//        gbcM.gridy = 0;
        

		this.jbIncClass.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				incPriorityOfSelVars();
				updateGUI();
			}
		});
		

		jpMove.add(this.jbIncClass);
		
//		gbcM.gridy ++;

		this.jbDecClass.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				decPriorityOfSelVars();
				updateGUI();
			}
		});
		
		jpMove.add(this.jbDecClass);

		JPanel jpMerge= new JPanel();
		jpMerge.setBorder(BorderFactory.createTitledBorder("Group"));
//		GridBagConstraints gbcMm = new GridBagConstraints();
//        gbcMm.insets = new Insets(4, 4, 4, 4);
//        gbcMm.gridx = 0;
//        gbcMm.gridy = 0;
        
		if (this.guiMultipSuc) {
			
//			gbcM.gridx ++;
//			gbcM.gridy --;
			
			this.jbIncGroup.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					incGroupOfSelVars();
					updateGUI();
				}
			});
			jpMove.add(this.jbIncGroup);
			
//			gbcM.gridy ++;

			this.jbDecGroup.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					decGroupOfSelVars();
					updateGUI();
				}
			});
			jpMove.add(this.jbDecGroup);
			

			// Collapse groups -- Sync class
			this.jbCollapse = this.getNoMargins("Group");
			jbCollapse.setToolTipText("Group selected components in a group)");
			jbCollapse.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					collapseRankVars();
				}
			});
			
			jpMerge.add(jbCollapse);
//			gbcMm.gridy ++;
			
			// Expand groups -- Async class
			this.jbExpand = this.getNoMargins("Ungroup");
			jbExpand.setToolTipText("Separate selected components, one per group");
			jbExpand.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					expandRankVars();
				}
			});
			
			jpMerge.add(jbExpand);

		}
		
		JPanel jpSplit = new JPanel();
		jpSplit.setBorder(BorderFactory.createTitledBorder("Split"));
//		GridBagConstraints gbcS = new GridBagConstraints();
//        gbcS.insets = new Insets(4, 4, 4, 4);
//        gbcS.gridx = 0;
//        gbcS.gridy = 0;
        
		this.jbSplit = this.getNoMargins("Split");
		jbSplit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				splitSelVars();
				updateGUI();
			}
		});
		
		jpSplit.add(jbSplit);
//		gbcS.gridy++;
		
		this.jbUnsplit = this.getNoMargins("Unsplit");
		jbUnsplit.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				unsplitSelVars();
				updateGUI();
			}
		});

		jpSplit.add(jbUnsplit);
		jpSouth.add(jpMove);
		
//		gbcC.gridx ++;
		jpSouth.add(jpSplit);
//		gbcC.gridx ++;
		if (this.guiMultipSuc)
			jpSouth.add(jpMerge);
		
		jpSouthCenter.add(jpSouth, BorderLayout.CENTER);
		
 
		JPanel jpSouthLeft = new JPanel(new GridBagLayout());
		jpSouthLeft.setBorder(BorderFactory.createTitledBorder("Ranks"));
		
		this.jbSingle = this.getNoMargins("Collapse Ranks");
		jbSingle.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if (guiMultipSuc) {
					collapseRanks();
				} else {
					collapseAll();
				}
				updateGUI();
			}
		});

		
		jpSouthLeft.add(jbSingle);
		jpSouthCenter.add(jpSouthLeft, BorderLayout.WEST);
		this.add(jpSouthCenter, BorderLayout.SOUTH);
		jpSouthCenter.setBorder(BorderFactory.createLineBorder(Color.GRAY));
	}

	public void updatePriorityList() { 
		
		// CLASS PANEL
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
			JPanel jpPClass = new JPanel();
			jpPClass.setLayout(new BorderLayout());
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
				
				JPanel groupHeader = new JPanel();
				
				// get supported updaters
				JComboBox<String> jcbUpdaters = new JComboBox<String>(testReadUp.getSupportedUpdaters(multiUpdater, singleUpdater));
				if (this.guiMultipSuc) {
					JButton moveUp = this.getNoMargins("↑");
					moveUp.setActionCommand("" + idxPC + "," + idxGroup);
					moveUp.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							JButton jb = (JButton) e.getSource();
							String[] idxs = jb.getActionCommand().split(",");
						
							int idxPC = Integer.parseInt(idxs[0]);
							int idxGrp = Integer.parseInt(idxs[1]);
						
							mpc.switchGroups(idxPC, idxGrp, idxGrp - 1);
							fireActionEvent();
							updatePriorityList();
						}
					});
					if (idxGroup == 0) 
						moveUp.setEnabled(false);	
		
					JButton moveDown = this.getNoMargins("↓");
					moveDown.setActionCommand("" + idxPC + "," + idxGroup);
					moveDown.addActionListener(new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							JButton jb = (JButton) e.getSource();
							String[] idxs = jb.getActionCommand().split(",");
						
							int idxPC = Integer.parseInt(idxs[0]);
							int idxGrp = Integer.parseInt(idxs[1]);
						
							mpc.switchGroups(idxPC, idxGrp, idxGrp + 1);
							fireActionEvent();
							updatePriorityList();
						}
					});
					if (idxGroup + 1 == lGrpVars.size()) 
						moveDown.setEnabled(false);	
				
					groupHeader.add(moveUp);
					groupHeader.add(moveDown);
				}
				groupHeader.add(jcbUpdaters);
				groupHeader.setMinimumSize(getPreferredSize());
	
				// get the updaterName from that group
				String updaterName = ((LogicalModelUpdater) mpc.getUpdater(idxPC, idxGroup)).getUpdaterName();
//				String updaterName = mpc.getGroupUpdaterName(idxPC, idxGroup);
				jcbUpdaters.setSelectedItem(updaterName);
				
				// save updater with idxPC and idxGroup !
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
		                    case "Random non uniform":
		                    	validateTextRates(idx[0],idx[1], textfields);
		                        break;
		                    default:
		                    	updater = UpdaterFactoryModelGrouping.getUpdater(mpc.getModel(), up);
		                		mpc.addUpdater(idx[0], idx[1], updater);
		                        break;
		                }
						fireActionEvent();
						updatePriorityList();	
		            }
		        });
		
				// get the group variables
				List<String> vars = lGrpVars.get(idxGroup);
			
				DefaultListModel<String> lModel = new DefaultListModel<String>();
				for (String var : vars) {
					lModel.addElement(var);
				}
							
				JList<String> jList = new JList<String>(lModel);
				jList.setBorder(BorderFactory.createLoweredBevelBorder());
				jList.setFixedCellWidth(this.GROUP_WIDTH);
				jList.setFixedCellHeight(this.GROUP_HEIGHT);


				jList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);				
					
				jList.addMouseListener(new MouseListener() {
					@Override
					public void mouseReleased(MouseEvent e) {
						@SuppressWarnings("unchecked")
	
						JList<String> selJList = (JList<String>) e.getSource();
						for (int pc = 0; pc < guiClasses.size(); pc ++) {
							if(guiClasses.get(pc).indexOf(selJList) == -1) {
								for (JList<String> lGroup : guiClasses.get(pc)) 
									lGroup.clearSelection();
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
				
				this.guiClasses.get(idxPC).add(jList);

				// add first comboBox
				gbcG.gridx = 0;
				gbcG.gridy = g;
				gbcG.gridwidth = 6;
				gbcG.anchor = GridBagConstraints.CENTER;
				gbcG.fill = GridBagConstraints.HORIZONTAL;
				jpGroups.add(groupHeader, gbcG);
				
				// empty space
				gbcG.gridy = g + 1;
				gbcG.gridx = 0;
				gbcG.gridwidth = 4;
				jpGroups.add(Box.createHorizontalStrut(this.GROUP_WIDTH/2), gbcG);


				// then add group
				gbcG.gridy = g + 1;
				gbcG.gridx = 4;
				gbcG.gridwidth = 1;
				gbcG.anchor = GridBagConstraints.LINE_END;
				gbcG.fill = GridBagConstraints.NONE;
				jpGroups.add(jList, gbcG);
				
				if (updaterName.equals("Random non uniform")) {
					JPanel ratesPanel = this.ratesPanel(idxPC, idxGroup, vars);
					 
					//JPanel ratesPanel = ratesPanel(vars, rates, textfields, updaterName);
                	gbcG.gridy = g + 1;
    				gbcG.gridx = 5;
    				gbcG.gridwidth = 1;
  
    				gbcG.anchor = GridBagConstraints.CENTER;
    				gbcG.fill = GridBagConstraints.NONE;
                	jpGroups.add(ratesPanel, gbcG);
				} 
				;
			}

			JScrollPane jScroll = new JScrollPane(jpGroups);
			jScroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
			jScroll.setHorizontalScrollBar(null);
			jpPClass.add(jScroll, BorderLayout.CENTER);

			if (this.guiMultipSuc) {
				JPanel jpTmpAll = new JPanel(new GridBagLayout());
				GridBagConstraints gbcT = new GridBagConstraints();
				gbcT.gridx = 0;
				
				
				JButton jbAllGrp = this.getNoMargins("Select all");
				jbAllGrp.setToolTipText("Select all vars");
				jbAllGrp.setActionCommand("" + idxPC);
				jbAllGrp.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						JButton jb = (JButton) e.getSource();
						int pos = Integer.parseInt(jb.getActionCommand());
						
						for (int pc = 0; pc < guiClasses.size(); pc ++) {
							if(pc != pos) {
								for (JList<String> lGroup : guiClasses.get(pc)) 
									lGroup.clearSelection();
								}
						}
						List<JList<String>> rankGroups = guiClasses.get(pos);
						for (int g = 0; g < rankGroups.size(); g++) {
							JList<String> group = rankGroups.get(g);
							group.setSelectionInterval(0, group.getModel().getSize() - 1);
						}
					}
				});
				jpTmpAll.add(jbAllGrp);
				
				JButton jbDGrp = this.getNoMargins("Deselect all");
				jbDGrp.setToolTipText("Deselect all vars");
				jbDGrp.setActionCommand("" + idxPC);
				jbDGrp.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						JButton jb = (JButton) e.getSource();
						int pos = Integer.parseInt(jb.getActionCommand());
						
						for (int pc = 0; pc < guiClasses.size(); pc ++) {
							if(pc == pos) {
								for (JList<String> lGroup : guiClasses.get(pc)) 
									lGroup.clearSelection();
								}
						}	
					}
				});
				gbcT.gridx ++;
				gbcT.gridx ++;

				jpTmpAll.add(jbDGrp);				
				jpPClass.add(jpTmpAll, BorderLayout.SOUTH);
			}

			jpPClass.setMinimumSize(new Dimension(jpPClass.getPreferredSize().width,
					jpCenter.getHeight())); // FIXME
			
//			jpPClass.setMaximumSize(new Dimension(jpPClass.getPreferredSize().width,
//					jpCenter.getHeight()));

			this.jpCenter.add(jpPClass, gdcCenter);
			gdcCenter.gridx++;
			this.jpCenter.add(Box.createRigidArea(new Dimension(this.CLASS_SPACING, 10)), gdcCenter);
			gdcCenter.gridx++;
			
		}
		updateGUI();
	}
	
	private JPanel ratesPanel(int idxPC, 	int idxGroup, List<String> vars) {
		// if random uniform or random non uniform, save (node string, rate) and (textfield, node string)
		
		Map<String, Double> rates = new HashMap<String, Double>();
		Map<JTextField, String> textfields = new HashMap<JTextField, String>();
		textfields.clear();

		Map<String, Double> upRates = mpc.getRates(idxPC, idxGroup, vars);

		// put (node string, rate)
		for (int e = 0; e < vars.size(); e++) {
			String var = vars.get(e);
			rates.put(var, upRates.get(var)); 
		}
				
		 JPanel ratesPanel = new JPanel(new GridBagLayout()); 
		 GridBagConstraints gbcR = new GridBagConstraints(); gbcR.gridx = 0;
		 
		
		 Map<String, Double> nodes = new HashMap<String, Double>();
		 Map<JTextField, int[]>  idxJtf = new HashMap<JTextField, int[]>();
		 for (int d = 0; d < rates.keySet().size(); d++) { 
			 String node = vars.get(d);
			 
			 Double rate = rates.get(node);
			 if (rate == null)
				 rate = 1.0;
			 
			 nodes.put(node, rate);
			 this.ratesCache.put(node, rate);
	 
			 JTextField jtf = new JTextField(Double.toString(rate)); 
			 
			 idxJtf.put(jtf, new int[] {idxPC, idxGroup});
			 jtf.setToolTipText(node);
			 // put(textfield, node string) 
			 textfields.put(jtf, node);
			 jtf.setColumns(3); 
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
			 
			 ratesPanel.add(jtf, gbcR); 
		 }
		return ratesPanel;
	}

	private void splitSelVars() {
		
		boolean pc = false;
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					pc = true;
					for (String var : values)
						mpc.split(i, g, var);
					if (!this.guiMultipSuc) {
						fireActionEvent();
						break all;
						}
				}
			} 
			if (pc) { 
				fireActionEvent();
				break all;
			}
		}
		this.updatePriorityList();
	}

	private void unsplitSelVars() {
		boolean pc = false;
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					pc = true;
					for (String var : values)
						mpc.unsplit(i, g, var);
					if (!this.guiMultipSuc) {
						fireActionEvent();
						break all;
						}

				}
			}
			if (pc) { 
				fireActionEvent();
				break all;
			}
		}
		this.updatePriorityList();
	}

	private void incPriorityOfSelVars() {
		
		// groups, strings
		Map<Integer, List<String>> groupsSel = new HashMap<Integer, List<String>>();
		 
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					List<String> vars = new ArrayList<String>();
					vars.addAll(values);
					groupsSel.put(g, vars);
					if (!this.guiMultipSuc) {
						mpc.incPriorities(i, groupsSel, this.guiMultipSuc);
						break all;
						}
				}
			} if (!groupsSel.isEmpty()) { 
				mpc.incPriorities(i, groupsSel, this.guiMultipSuc);
				break all;
			}
		}
		fireActionEvent();
		this.updatePriorityList();
	}

	private void decPriorityOfSelVars() {
			// groups, strings
				Map<Integer, List<String>> groupsSel = new HashMap<Integer, List<String>>();
				 
				all: for (int i = 0; i < this.guiClasses.size(); i++) {
					for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
						List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
						if (!values.isEmpty()) {
							List<String> vars = new ArrayList<String>();
							vars.addAll(values);
							groupsSel.put(g, vars);
							if (!this.guiMultipSuc) {
								mpc.decPriorities(i, groupsSel, this.guiMultipSuc);
								break all;
								}
						}
					} if (!groupsSel.isEmpty()) { 
						mpc.decPriorities(i, groupsSel, this.guiMultipSuc);
						break all;
					}
				}
				fireActionEvent();
				this.updatePriorityList();
	}

	private void incGroupOfSelVars() {
		boolean pc = false;
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					pc = true;
					mpc.incGroup(i, g, values);
				}
			}
			if (pc) { 
				fireActionEvent();
				break all;
			}
		}
		this.updatePriorityList();
	}

	private void decGroupOfSelVars() {
		boolean pc = false;
		all: for (int i = 0; i < this.guiClasses.size(); i++) {
			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
				if (!values.isEmpty()) {
					pc = true;
					mpc.decGroup(i, g, values);
				}
			}
			if (pc) { 
				fireActionEvent();
				break all;
			}
		} 
		this.updatePriorityList();
	}
	
	
	private void validateTextRates(int idxPC, int idxGrp, Map<JTextField, String> textfields) {
		
		Map<String, Double> nodeRates = new HashMap<String, Double>();
		if (textfields == null) {
			List<String> vars = mpc.getClassVars(idxPC).get(idxGrp);
			Set<String> cacheVars = this.ratesCache.keySet();
			
			boolean allVars = true;
			for (String var : vars) {
				if (!cacheVars.contains(var)) {
					allVars = false;
					break;
				}
			}
			if (allVars) {
				for (String var : vars)
					nodeRates.put(var, this.ratesCache.get(var));				
			}
			
     		mpc.addUpdater(idxPC, idxGrp, nodeRates);
     		
		} else {
			int i = 0;
			for (JTextField jtf : textfields.keySet()) {
			
				String text = jtf.getText();
				String node = jtf.getToolTipText();
				try {
					Double rate = Double.parseDouble(text);
					jtf.setBackground(Color.white);
					nodeRates.put(node, rate);
					this.ratesCache.put(node, rate);
				}
				catch(NumberFormatException er) {
					jtf.setBackground(LIGHT_RED);
					break;
				}
				i++;
			}
			fireActionEvent();
			mpc.addUpdater(idxPC, idxGrp, nodeRates);
			} 
		}

//	private void enableButtons() {
//		
//		if (mpc.size() == 1) 
//			jbSingle.setEnabled(false);
//		
//		
//		Map<Integer, List<String>> groupsSel = new HashMap<Integer, List<String>>();
//		 
//		all: for (int i = 0; i < this.guiClasses.size(); i++) {
//			for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
//				List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
//				if (!values.isEmpty()) {
//					List<String> vars = new ArrayList<String>();
//					vars.addAll(values);
//					groupsSel.put(g, vars);
//					if (!this.guiMultipSuc) {
//						mpc.incPriorities(i, groupsSel, this.guiMultipSuc);
//						break all;
//						}
//				}
//			} if (!groupsSel.isEmpty()) { 
//				mpc.incPriorities(i, groupsSel, this.guiMultipSuc);
//				break all;
//			}
//		}
//		
//		jbSplit.setEnabled(false);
//		jbUnsplit.setEnabled(false);
//		jbCollapse.setEnabled(false);
//		jbExpand.setEnabled(false);
//		jbIncGroup.setEnabled(false);
//		jbDecGroup.setEnabled(false);
//		jbIncClass.setEnabled(false);
//		jbDecClass.setEnabled(false);
//		
//		if (groupsSel.size() == 1) {
//			jbIncGroup.setEnabled(true);
//			jbDecGroup.setEnabled(true);
//			jbIncClass.setEnabled(true);
//			jbDecClass.setEnabled(true);
//
//		}
//	
//	}
	
	private void collapseAll() {
		this.mpc.collapseAll();
		fireActionEvent();
		this.updatePriorityList();
	}
	
	private void collapseRanks() {
		this.mpc.collapseRanks();
		fireActionEvent();
		this.updatePriorityList();
	}
	
	private void collapseRankVars(){
		// groups, strings
			Map<Integer, List<String>> groupsSel = new HashMap<Integer, List<String>>();
			int idxPC = -1;
			 
			all: for (int i = 0; i < this.guiClasses.size(); i++) {
				for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
					List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
					if (!values.isEmpty()) {
						List<String> vars = new ArrayList<String>();
						vars.addAll(values);
						groupsSel.put(g, vars);
					}
				} if (!groupsSel.isEmpty()) { 
					idxPC = i;
					break all;
				}
			}
			if (idxPC != -1)
				mpc.groupCollapse(idxPC, groupsSel);
			
			fireActionEvent();
			this.updatePriorityList();
	}
	
	private void expandRankVars(){
		// groups, strings
			Map<Integer, List<String>> groupsSel = new HashMap<Integer, List<String>>();
			int idxPC = -1;
			 
			all: for (int i = 0; i < this.guiClasses.size(); i++) {
				for (int g = 0; g < this.guiClasses.get(i).size(); g++) {
					List<String> values = this.guiClasses.get(i).get(g).getSelectedValuesList();
					if (!values.isEmpty()) {
						List<String> vars = new ArrayList<String>();
						vars.addAll(values);
						groupsSel.put(g, vars);
					}
				} if (!groupsSel.isEmpty()) { 
					idxPC = i;
					break all;
				}
			}
			
			if (idxPC != - 1)
				mpc.groupExpand(idxPC, groupsSel);
			
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
	
	public class CenterPanelPriorityPanel extends JPanel {

		private static final long serialVersionUID = 1L;

		@Override
		public java.awt.Dimension getPreferredSize() {
			   return new java.awt.Dimension(super.getPreferredSize().width,
					   super.getSize().height);
		}
	}
	
	
}