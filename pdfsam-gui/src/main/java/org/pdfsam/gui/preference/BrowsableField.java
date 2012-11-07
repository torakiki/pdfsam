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
package org.pdfsam.gui.preference;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.gui.balloon.BalloonUtils;

import static javax.swing.GroupLayout.Alignment.TRAILING;
import static org.pdfsam.support.Components.GAP;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Preference field where the value of the field can be browsed.
 * 
 * @author Andrea Vacondio
 * 
 */
abstract class BrowsableField extends JPanel {

    private JTextField field = new JTextField();
    private StringUserPreference preference;

    BrowsableField(String labelText, StringUserPreference preference) {
        this.preference = preference;
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        JLabel label = new JLabel(labelText);

        JPanel fieldPanel = new JPanel();
        GroupLayout layout = new GroupLayout(fieldPanel);
        fieldPanel.setLayout(layout);
        JButton browse = new JButton(new BrowseAction());
        layout.setHorizontalGroup(layout.createSequentialGroup().addComponent(field).addGap(GAP).addComponent(browse));
        layout.setVerticalGroup(layout.createSequentialGroup().addGroup(
                layout.createParallelGroup(TRAILING).addComponent(field).addComponent(browse)));

        add(label);
        add(Box.createRigidArea(new Dimension(GAP, GAP)));
        add(fieldPanel);
    }

    void setBalloonTooltip(String tooltip) {
        BalloonUtils.createBalloonFor(this, tooltip);
    }

    void setDefaultFieldValue(String workspace) {
        field.setText(workspace);
    }

    /**
     * @return a {@link JFileChooser} initialized for the component.
     */
    abstract JFileChooser getChooser();

    /**
     * Browse action
     * 
     * @author Andrea Vacondio
     * 
     */
    private class BrowseAction extends AbstractAction {

        BrowseAction() {
            super("Browse");
        }

        public void actionPerformed(ActionEvent e) {
            JFileChooser chooser = getChooser();
            if (isNotBlank(field.getText())) {
                chooser.setCurrentDirectory(new File(field.getText()));
            }
            int retVal = chooser.showOpenDialog(BrowsableField.this);

            if (retVal == JFileChooser.APPROVE_OPTION) {
                BrowsableField.this.field.setText(chooser.getSelectedFile().getAbsolutePath());
                DefaultUserContext.getInstance().setStringPreference(BrowsableField.this.preference,
                        BrowsableField.this.field.getText());
            }
        }
    }
}
