/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import org.pdfsam.injector.Auto;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for a request to clean the modules usage statistics and performing the clean
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class ClearUsageController {
    private static final Logger LOG = LoggerFactory.getLogger(ClearUsageController.class);

    private UsageService service;

    @Inject
    public ClearUsageController(UsageService service) {
        this.service = service;
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
