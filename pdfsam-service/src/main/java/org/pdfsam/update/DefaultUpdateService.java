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

import static org.apache.commons.lang3.StringUtils.EMPTY;

import java.io.IOException;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
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
    private static final String CURRENT_VERSION_KEY = "currentVersion";

    private Object jsonSource;

    @Inject
    DefaultUpdateService(@Named("updatesUrl") Object jsonSource) {
        this.jsonSource = jsonSource;
    }

    @Override
    public String getLatestVersion() {
        try {
            return JSON.std.mapFrom(jsonSource).getOrDefault(CURRENT_VERSION_KEY, "").toString();
        } catch (IOException e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n("Unable to find the latest available version."), e);
        }
        return EMPTY;
    }

}
