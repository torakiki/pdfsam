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

import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.ParallelGroup;
import javax.swing.GroupLayout.SequentialGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.apache.commons.lang3.StringUtils;

import static javax.swing.GroupLayout.Alignment.TRAILING;
import static org.pdfsam.gui.view.Views.GAP;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Abstract implementation of a field where the value can be browsed with a {@link JFileChooser}. Implementors have to specify a {@link JFileChooser}
 * 
 * @author Andrea Vacondio
 * 
 */
public abstract class BaseBrowsableField extends JPanel {
    private JTextField field = new JTextField();
    private JLabel label;

    /**
     * Creates a browsable field without label
     */
    protected BaseBrowsableField() {
        this(StringUtils.EMPTY);
    }

    /**
     * Creates a browsable field with label
     */
    protected BaseBrowsableField(String labelText) {
        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);

        if (StringUtils.isNotBlank(labelText)) {
            label = new JLabel(labelText);
        }

        JButton browse = new JButton(new BrowseAction());

        ParallelGroup pGroup = layout.createParallelGroup();
        if (label != null) {
            pGroup = pGroup.addGroup(layout.createSequentialGroup().addComponent(label)).addGap(GAP);
        }
        layout.setHorizontalGroup(pGroup.addGroup(layout.createSequentialGroup().addComponent(field).addGap(GAP)
                .addComponent(browse)));

        SequentialGroup sGroup = layout.createSequentialGroup();
        if (label != null) {
            sGroup = sGroup.addGroup(layout.createSequentialGroup().addComponent(label)).addGap(2);
        }
        layout.setVerticalGroup(sGroup.addGroup(layout.createParallelGroup(TRAILING).addComponent(field)
                .addComponent(browse)));
    }

    public void setBalloonTooltip(String tooltip) {
        this.setToolTipText(tooltip);
        field.setToolTipText(tooltip);
    }

    public void setDefaultFieldValue(String defaultValue) {
        field.setText(defaultValue);
    }

    public String getFieldValue() {
        return field.getText();
    }

    /**
     * @return a {@link JFileChooser} initialized for the component.
     */
    protected abstract JFileChooser getChooser();

    /**
     * Callback executed when a file is selected
     * 
     * @param selected
     */
    protected abstract void onFileSelected(File selected);

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
            int retVal = chooser.showOpenDialog(BaseBrowsableField.this);

            if (retVal == JFileChooser.APPROVE_OPTION) {
                File selectedFile = chooser.getSelectedFile();
                BaseBrowsableField.this.field.setText(selectedFile.getAbsolutePath());
                onFileSelected(selectedFile);
            }
        }
    }
}
