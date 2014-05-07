/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/apr/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.net.URL;
import java.util.Map;

import javafx.application.Platform;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;

/**
 * Default JSON implementation for the update service
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class DefaultUpdateService implements UpdateService {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultUpdateService.class);
    private static final String UPDATES_URL = "http://www.pdfsam.org/current-version";
    private static final String CURRENT_VERSION_KEY = "currentVersion";

    @Inject
    @Named("appVersion")
    private String appVersion;

    public void checkForUpdates() {
        try {
            Map<String, Object> map = JSON.std.mapFrom(new URL(String.format("%s?c=%s", UPDATES_URL, appVersion)));
            String current = map.getOrDefault(CURRENT_VERSION_KEY, "").toString();
            if (!current.equals(appVersion)) {
                LOG.info(DefaultI18nContext.getInstance().i18n("PDFsam {0} is available for download", current));
                Platform.runLater(() -> eventStudio().broadcast(new UpdateAvailableEvent(current)));
            }
        } catch (IOException e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to find the latest available version."), e);
        }
    }

}
