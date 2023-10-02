/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.premium;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.premium.FetchPremiumModulesRequest;
import org.pdfsam.model.premium.PremiumToolsResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@Auto
public class PremiumToolsController {
    private static final Logger LOG = LoggerFactory.getLogger(PremiumToolsController.class);

    private final PremiumToolsService service;

    @Inject
    PremiumToolsController(PremiumToolsService service) {
        this.service = service;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void fetchPremium(FetchPremiumModulesRequest event) {
        Thread.ofVirtual().name("premium-tools-thread").start(() -> {
            LOG.debug(i18n().tr("Fetching premium modules"));
            try {
                Optional.ofNullable(service.getPremiumTools()).filter(l -> !l.isEmpty()).map(PremiumToolsResponse::new)
                        .ifPresent(eventStudio()::broadcast);

            } catch (Exception e) {
                LOG.warn(i18n().tr("Unable to retrieve premium modules"), e);
            }
        });
    }
}
