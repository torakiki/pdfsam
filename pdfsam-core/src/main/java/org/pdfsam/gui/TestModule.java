/*
 * Created on 29/nov/2012
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

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JPanel;

import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.view.Views;
import org.pdfsam.gui.view.output.FilePdfDestinationPanel;
import org.pdfsam.gui.view.output.OutputViews;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.PdfsamModule;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * @author Andrea Vacondio
 * 
 */
@PdfsamModule
public class TestModule extends BaseTaskExecutionModule {

    private static final EventNamespace MERGE_NAMESPACE = EventNamespace.newRootInstance("Merge");

    /*
     * (non-Javadoc)
     * 
     * @see org.pdfsam.gui.Module#getDescriptor()
     */
    @Override
    public ModuleDescriptor getDescriptor() {
        return new ModuleDescriptor("IdModule", ModuleCategory.MERGE, "Merge", null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.pdfsam.gui.TaskExecutionModule#getParameters()
     */
    @Override
    protected TaskParameters getParameters() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.pdfsam.gui.TaskExecutionModule#getInnerPanel()
     */
    @Override
    protected JPanel getInnerPanel() {
        JPanel panel = new JPanel(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        JPanel top = new JPanel();
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.weightx = 1.0;
        c.insets = new Insets(5, 5, 5, 5); // top padding
        c.gridx = 0;
        c.gridy = 0;
        c.gridwidth = 3;
        c.gridheight = 1;
        panel.add(top, c);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0;
        c.gridy = 1;
        c.gridheight = 1;
        FilePdfDestinationPanel destination = new FilePdfDestinationPanel();
        destination.setEventNamespace(MERGE_NAMESPACE);
        panel.add(OutputViews.newFilePdfDestinationPanel(destination), c);

        JPanel inner = Views.newPrefixPanel(false);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridy = 2;
        c.gridheight = 1;
        panel.add(inner, c);
        return panel;
    }
}
