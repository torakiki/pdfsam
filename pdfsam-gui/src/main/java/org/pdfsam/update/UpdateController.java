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
package org.pdfsam.update;

import javax.swing.SwingWorker;

import org.apache.commons.lang3.StringUtils;
import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.bushe.swing.event.annotation.ReferenceStrength;
import org.pdfsam.Pdfsam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller responding to updates related request.
 * 
 * @author Andrea Vacondio
 * 
 */
public class UpdateController {
    private static final Logger LOG = LoggerFactory.getLogger(UpdateController.class);
    private static final String URI = "http://www.pdfsam.org/check-version.php?version=basic&remoteversion="
            + Pdfsam.VERSION + "&branch=2";

    private UpdateChecker checker;

    public UpdateController() {
        checker = UpdateCheckers.newHttpUpdateChecker(URI);
    }

    @EventSubscriber(referenceStrength = ReferenceStrength.STRONG)
    public void checkForUpdates(UpdateCheckRequest event) {
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
                if (!Pdfsam.VERSION.equals(latest)) {
                    EventBus.publish(new UpdateAvailableEvent(latest));
                } else {
                    LOG.debug("No update available");
                }
            } catch (Exception e) {
                LOG.warn("An error occurred while checking for updates");
            }

        }

    }
}
