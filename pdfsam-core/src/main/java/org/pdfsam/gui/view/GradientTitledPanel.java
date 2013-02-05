/*
 * Created on 05/feb/2013
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
package org.pdfsam.gui.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.gui.view.GradientPanel.GradientOrientation;

/**
 * A panel using a gradient to display the title. This panel uses a {@link BorderLayout} where PAGE_START is used for the title and typically another panel is added as CENTER.
 * 
 * @author Andrea Vacondio
 * 
 */
public class GradientTitledPanel extends JPanel {

    public GradientTitledPanel(String title, GradientOrientation orientation) {
        setLayout(new BorderLayout());
        add(buildTitlePanel(title, orientation), BorderLayout.PAGE_START);
    }

    private Component buildTitlePanel(String title, GradientOrientation orientation) {
        GradientPanel titlePanel = new GradientPanel(orientation);
        JLabel titleLabel = new JLabel(title);
        titlePanel.setLayout(new BoxLayout(titlePanel, BoxLayout.LINE_AXIS));
        titlePanel.add(Box.createRigidArea(new Dimension(2, 2)));
        titlePanel.add(titleLabel);
        titlePanel.add(Box.createHorizontalGlue());
        return titlePanel;
    }

}
