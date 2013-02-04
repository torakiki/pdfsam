/*
 * Created on 03/apr/2012
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

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.bushe.swing.event.annotation.ReferenceStrength;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.service.TaskExecutionRequestEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.TaskParameters;

import static org.pdfsam.gui.Components.GAP;

/**
 * Abstract implementation of a pdfsam module providing features common to every module whose purpose is to execute a pdf manipulation task.
 * 
 * @author Andrea Vacondio
 * 
 */
public abstract class TaskExecutionModule implements Module {

    private JButton runButton = new JButton(new RunAction());
    private JPanel modulePanel = new JPanel(new GridBagLayout());

    public TaskExecutionModule() {
        init();
        AnnotationProcessor.process(new RunButtonStatusHandler());
    }

    private void init() {
        GridBagConstraints c = new GridBagConstraints();
        c.ipady = GAP;
        c.ipadx = GAP;
        c.gridwidth = 3;
        c.gridheight = 2;
        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1;
        c.weighty = 1;
        c.fill = GridBagConstraints.BOTH;
        JScrollPane scroll = new JScrollPane();
        scroll.setViewportView(getInnerPanel());
        scroll.setBorder(BorderFactory.createEmptyBorder());
        modulePanel.add(scroll, c);

        c.ipady = 0;
        c.ipadx = 0;
        c.gridwidth = 3;
        c.gridheight = 1;
        c.gridx = 0;
        c.gridy = 2;
        c.weightx = 0;
        c.weighty = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        modulePanel.add(buttonPanel(), c);
    }

    private JPanel buttonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, GAP, GAP));
        buttonPanel.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, buttonPanel.getBackground().darker()));
        buttonPanel.add(runButton);
        return buttonPanel;
    }

    /**
     * @return the inner panel that allows the user to set options and preferences for this panel
     */
    protected abstract JPanel getInnerPanel();

    /**
     * @return parameters to be used to perform a pdf manipulation
     */
    protected abstract TaskParameters getParameters();

    public JPanel getModulePanel() {
        return modulePanel;
    }

    /**
     * Run action for the frame
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class RunAction extends AbstractAction {

        private RunAction() {
            super(DefaultI18nContext.getInstance().i18n("Run"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            // run
        }

    }

    /**
     * Handles the run button status by listening to events affecting its status and changing it accordingly.
     * 
     * @author Andrea Vacondio
     * 
     */
    final class RunButtonStatusHandler {

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void disableRunButtonIfTaskRequested(TaskExecutionRequestEvent event) {
            runButton.setEnabled(false);
        }

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void enableRunButtonOnTaskCompletion(TaskExecutionCompletedEvent event) {
            runButton.setEnabled(true);
        }

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void enableRunButtonOnTaskFailure(TaskExecutionFailedEvent event) {
            runButton.setEnabled(true);
        }

    }
}
