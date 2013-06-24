/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.preference;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;

import static org.pdfsam.gui.view.Views.GAP;

/**
 * Panel used in the preferences frame with some common layout setting
 * 
 * @author Andrea Vacondio
 * 
 */
public class PreferencePanel extends JPanel {

    private JPanel innerPanel = new JPanel();

    PreferencePanel(String title) {
        setBackground(Color.WHITE);
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createTitledBorder(title));
        innerPanel.setBackground(Color.WHITE);
        innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.Y_AXIS));

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 3;
        c.gridheight = 3;
        c.insets = new Insets(GAP, GAP, GAP, GAP);
        c.weightx = 1.0;
        c.weighty = 1.0;
        add(innerPanel, c);
    }

    /**
     * Add a preference component to this panel
     * 
     * @param component
     */
    void addPeferenceComponent(JComponent component) {
        component.setAlignmentX(Component.LEFT_ALIGNMENT);
        innerPanel.add(component);
        innerPanel.add(Box.createRigidArea(new Dimension(0, GAP)));
    }
}
