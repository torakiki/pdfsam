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
package org.pdfsam.service.update;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.inject.Inject;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URL;
import java.util.Map;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Default JSON implementation for the update service
 *
 * @author Andrea Vacondio
 */
class DefaultUpdateService implements UpdateService {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultUpdateService.class);
    private static final String CURRENT_VERSION_KEY = "currentVersion";

    private final ObjectMapper objectMapper;

    private final AppBrand appBrand;

    @Inject
    DefaultUpdateService(AppBrand appBrand, ObjectMapper objectMapper) {
        this.appBrand = appBrand;
        this.objectMapper = objectMapper;
    }

    @Override
    public String getLatestVersion() {
        try {
            return objectMapper.readValue(
                            new URL(String.format(appBrand.property(BrandableProperty.CURRENT_VERSION_URL),
                                    appBrand.property(BrandableProperty.VERSION))), Map.class)
                    .getOrDefault(CURRENT_VERSION_KEY, "").toString();
        } catch (IOException e) {
            LOG.warn(i18n().tr("Unable to find the latest available version."), e);
        }
        return EMPTY;
    }

}
