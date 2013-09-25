/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/apr/2012
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
package org.pdfsam.module;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.bushe.swing.event.annotation.ReferenceStrength;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.gui.view.Views;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.task.TaskExecutionRequestEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.parameter.base.TaskParameters;

import static org.pdfsam.gui.event.EnableDisableComponentCallback.disableComponent;
import static org.pdfsam.gui.event.EnableDisableComponentCallback.enableComponent;
import static org.pdfsam.gui.event.EventSubscriberTemplate.ifEvent;
import static org.pdfsam.gui.view.Views.GAP;

/**
 * Abstract implementation of a pdfsam module providing common features to every module whose purpose is to execute a pdf manipulation task.
 * 
 * @author Andrea Vacondio
 * 
 */
public abstract class BaseTaskExecutionModule implements Module, WithEventNamespace {

    private JButton runButton = new JButton(new RunAction());
    private JPanel modulePanel = new JPanel(new GridBagLayout());

    public BaseTaskExecutionModule() {
        init();
        AnnotationProcessor.process(new RunButtonStatusController());
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
        modulePanel.add(Views.newButtonsPanel(runButton), c);
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
            putValue(Action.SMALL_ICON, new ImageIcon(RunAction.class.getResource("/images/run.png")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            EventBus.publish(new TaskExecutionRequestEvent(getParameters()));
        }

    }

    /**
     * Handles the run button status by listening to events affecting its status and changing it accordingly.
     * 
     * @author Andrea Vacondio
     * 
     */
    final class RunButtonStatusController {

        // we give highest priority to be sure this is executed before the actual task.
        // we first want to disable the button and then execute the task
        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG, priority = Integer.MIN_VALUE)
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

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void disableRunButtonWhileLoadingDocuments(PdfLoadRequestEvent event) {
            // still loading
            ifEvent(event).routesTo(getEventNamespace()).execute(disableComponent(runButton));
        }

        @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
        public void enableRunButtonOnLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
            // I'm done loading documents
            ifEvent(event).routesTo(getEventNamespace()).execute(enableComponent(runButton));
        }

    }
}
