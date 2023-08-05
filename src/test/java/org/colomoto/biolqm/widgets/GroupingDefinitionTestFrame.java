package org.colomoto.biolqm.widgets;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.ReferenceModels;
import org.colomoto.biolqm.tool.simulation.grouping.PCRankGroupsVars;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class GroupingDefinitionTestFrame {

    public static void main(String[] args) throws Exception {

        LogicalModel model = ReferenceModels.getModel(ReferenceModels.getNames()[0]);
        PCRankGroupsVars mpc = new PCRankGroupsVars(model);

        PriorityClassPanel pcp = new PriorityClassPanel(mpc, true);
        pcp.updatePriorityList();

        JButton b = new JButton("Print");
        b.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                System.out.println(mpc);
            }
        });

        JPanel panel = new JPanel();
        panel.setLayout( new GridBagLayout() );

        GridBagConstraints cst = new GridBagConstraints();
        cst.weighty = cst.weightx = 1;
        cst.fill = GridBagConstraints.BOTH;
        panel.add(pcp, cst);

        cst = new GridBagConstraints();
        cst.gridy++;
        panel.add(b, cst);

        JFrame f = new JFrame("Test Grouping");
        f.setSize(500,300);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setContentPane(panel);
        f.setVisible(true);
    }
}
