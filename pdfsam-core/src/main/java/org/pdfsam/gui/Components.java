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
package org.pdfsam.gui;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.gui.balloon.BalloonUtils;

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
     * @param component
     * @param labelText
     * @param tooltip
     * @return a panel horizontally aligned with the label and the component
     */
    public static JPanel newLabeledComponent(JComponent component, String labelText, String tooltip) {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        JLabel label = new JLabel(labelText);
        BalloonUtils.createBalloonFor(component, tooltip);
        BalloonUtils.createBalloonFor(label, tooltip);
        panel.add(label);
        panel.add(Box.createRigidArea(new Dimension(GAP, 0)));
        panel.add(component);
        panel.add(Box.createHorizontalGlue());
        return panel;
    }

    /**
     * 
     * @param component
     * @param labelText
     * @param tooltip
     * @return a panel horizontally aligned with the label and the component and white background
     */
    public static JPanel newLabeledComponentWhiteBackground(JComponent component, String labelText, String tooltip) {
        JPanel panel = newLabeledComponent(component, labelText, tooltip);
        panel.setBackground(Color.WHITE);
        return panel;
    }

    /**
     * @param title
     * @return a title panel with white background
     */
    public static JPanel newTitledWhitePanel(String title) {
        JPanel panel = newTitledPanel(title);
        panel.setBackground(Color.WHITE);
        return panel;
    }

    /**
     * @param title
     * @return a title panel
     */
    public static JPanel newTitledPanel(String title) {
        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createTitledBorder(title));
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        return panel;
    }
}
