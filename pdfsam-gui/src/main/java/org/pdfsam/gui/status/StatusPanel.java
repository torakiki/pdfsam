/*
 * Created on 06/apr/2012
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
package org.pdfsam.gui.status;

import java.awt.Dimension;
import java.math.BigDecimal;
import java.text.DecimalFormat;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.update.UpdateAvailableEvent;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;

/**
 * Status panel showing the progress bar and the updates available icons.
 * 
 * @author Andrea Vacondio
 * 
 */
public class StatusPanel extends JPanel {

    private static final int MAX_VALUE = 1000;
    private JLabel updateAvailableIcon;
    private JProgressBar progressBar;

    public StatusPanel() {
        init();
        AnnotationProcessor.process(this);
    }

    private void init() {
        updateAvailableIcon = new JLabel(new ImageIcon(StatusPanel.class.getResource("/images/updates_available.png")));
        updateAvailableIcon.setVisible(false);
        progressBar = new JProgressBar(0, MAX_VALUE);
        progressBar.setOrientation(JProgressBar.HORIZONTAL);
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        add(Box.createHorizontalGlue());
        add(Box.createRigidArea(new Dimension(5, 0)));
        add(updateAvailableIcon);
        add(Box.createRigidArea(new Dimension(5, 0)));
        add(progressBar);
        add(Box.createRigidArea(new Dimension(5, 0)));
    }

    @EventSubscriber
    public void updateAvailable(UpdateAvailableEvent event) {
        updateAvailableIcon.setToolTipText(DefaultI18nContext.getInstance().i18n("New version {0} available",
                event.getAvailableVersion()));
        updateAvailableIcon.setVisible(true);
    }

    @EventSubscriber
    public void taskStarted(TaskExecutionStartedEvent event) {
        progressBar.setValue(0);
        progressBar.setStringPainted(false);
    }

    @EventSubscriber
    public void taskCompleted(TaskExecutionCompletedEvent event) {
        progressBar.setValue(MAX_VALUE);
    }

    @EventSubscriber
    public void taskPercentageDone(PercentageOfWorkDoneChangedEvent event) {
        if (event.isUndetermined()) {
            progressBar.setStringPainted(false);
            progressBar.setIndeterminate(true);
        } else {
            progressBar.setStringPainted(true);
            progressBar.setIndeterminate(false);
            progressBar.setValue(event.getPercentage().multiply(BigDecimal.TEN).intValue());
            System.out.println(event.getPercentage());
            progressBar.setString(new DecimalFormat("###.#").format(event.getPercentage()) + " %");
        }
    }
}
