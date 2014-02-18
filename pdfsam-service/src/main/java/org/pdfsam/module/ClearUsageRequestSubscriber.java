/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.module;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Named;

import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for a request to clean the modues usage statistics and performing the clean
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class ClearUsageRequestSubscriber {
    private static final Logger LOG = LoggerFactory.getLogger(ClearUsageRequestSubscriber.class);
    @Inject
    private UsageService service;

    public ClearUsageRequestSubscriber() {
        eventStudio().addAnnotatedListeners(this);
    }

    /**
     * Request to clear the modules usage stats
     * 
     * @param event
     */
    @EventListener(priority = Integer.MIN_VALUE)
    public void request(ClearUsageRequestEvent event) {
        LOG.debug("Clearing usage statistics");
        service.clear();
    }
}
