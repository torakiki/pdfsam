/*
 * Created on 14/giu/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.support;

import javax.swing.GroupLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import static javax.swing.GroupLayout.Alignment.TRAILING;

/**
 * Utility class to create components
 * 
 * @author Andrea Vacondio
 * 
 */
public final class Components {

    public static final int GAP = 5;

    private Components() {
        // hide
    }

    /**
     * 
     * @param component
     * @param labelText
     * @return a panel horizontally aligned with the label and the component
     */
    public static JPanel newLabeledComponent(JComponent component, String labelText) {
        JPanel panel = new JPanel();
        JLabel label = new JLabel(labelText);
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setHorizontalGroup(layout.createSequentialGroup().addComponent(label).addGap(GAP)
                .addComponent(component));
        layout.setVerticalGroup(layout.createSequentialGroup().addGroup(
                layout.createParallelGroup(TRAILING, false).addComponent(label).addComponent(component)));
        return panel;
    }
}
