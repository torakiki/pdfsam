/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
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
package org.pdfsam.update;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.ConfigurableProperty.VERSION;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;

import org.pdfsam.Pdfsam;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component listening for updates related requests
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class UpdatesController {
    private static final Logger LOG = LoggerFactory.getLogger(UpdatesController.class);

    private Pdfsam pdfsam;
    private UpdateService service;

    @Inject
    UpdatesController(UpdateService service, Pdfsam pdfsam) {
        this.service = service;
        this.pdfsam = pdfsam;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void checkForUpdates(UpdateCheckRequest event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Checking for updates"));
        CompletableFuture.supplyAsync(service::getLatestVersion).thenAccept(current -> {
            if (isNotBlank(current)) {
                if (!pdfsam.property(VERSION).equals(current)) {
                    LOG.info(DefaultI18nContext.getInstance().i18n("PDFsam {0} is available for download", current));
                    eventStudio().broadcast(new UpdateAvailableEvent(current));
                } else if (event.nofityNoUpdates) {
                    eventStudio().broadcast(new NoUpdateAvailable());
                }
            }
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to find the latest available version."), e);
            }
        });
    }
}
