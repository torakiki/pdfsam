/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.service.update;

import jakarta.inject.Inject;
import org.pdfsam.core.AppBrand;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.update.NoUpdateAvailable;
import org.pdfsam.model.update.UpdateAvailableEvent;
import org.pdfsam.model.update.UpdateCheckRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CompletableFuture;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.BrandableProperty.VERSION;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Component listening for updates related requests
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class UpdatesController {
    private static final Logger LOG = LoggerFactory.getLogger(UpdatesController.class);

    private final AppBrand appBrand;
    private final UpdateService service;

    @Inject
    UpdatesController(UpdateService service, AppBrand appBrand) {
        this.service = service;
        this.appBrand = appBrand;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void checkForUpdates(UpdateCheckRequest event) {
        LOG.debug(i18n().tr("Checking for updates"));
        CompletableFuture.supplyAsync(service::getLatestVersion).thenAccept(current -> {
            if (isNotBlank(current)) {
                if (!appBrand.property(VERSION).equals(current)) {
                    LOG.info(i18n().tr("PDFsam {0} is available for download", current));
                    eventStudio().broadcast(new UpdateAvailableEvent(current));
                } else if (event.notifyIfNoUpdates()) {
                    eventStudio().broadcast(new NoUpdateAvailable());
                }
            }
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.warn(i18n().tr("Unable to find the latest available version."), e);
            }
        });
    }
}
