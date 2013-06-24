/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/nov/2012
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
package org.pdfsam.update;

import javax.inject.Named;
import javax.swing.SwingWorker;

import org.apache.commons.lang3.StringUtils;
import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.bushe.swing.event.annotation.ReferenceStrength;
import org.pdfsam.configuration.PdfsamProperties;
import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller responding to updates related request.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class UpdateController {
    private static final Logger LOG = LoggerFactory.getLogger(UpdateController.class);
    private static final String URI = "http://www.pdfsam.org/check-version.php?version=basic&remoteversion="
            + PdfsamProperties.VERSION + "&branch=2";

    private UpdateChecker checker;

    public UpdateController() {
        checker = UpdateCheckers.newHttpUpdateChecker(URI);
        AnnotationProcessor.process(this);
    }

    @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
    public void checkForUpdates(UpdateCheckRequest event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Checking for updates"));
        doCheckForUpdatesAsync();
    }

    private void doCheckForUpdatesAsync() {
        SwingWorker<String, Void> worker = new AsyncUpdateChecker();
        worker.execute();
    }

    /**
     * Check for updates asynchronously
     * 
     * @author Andrea Vacondio
     * 
     */
    private class AsyncUpdateChecker extends SwingWorker<String, Void> {

        @Override
        public String doInBackground() {
            return StringUtils.defaultString(checker.getLatestVersion());
        }

        @Override
        protected void done() {
            String latest;
            try {
                latest = get();
                if (!PdfsamProperties.VERSION.equals(latest)) {
                    EventBus.publish(new UpdateAvailableEvent(latest));
                } else {
                    LOG.debug(DefaultI18nContext.getInstance().i18n("No update available"));
                }
            } catch (Exception e) {
                LOG.warn("An error occurred while checking for updates");
            }

        }

    }
}
