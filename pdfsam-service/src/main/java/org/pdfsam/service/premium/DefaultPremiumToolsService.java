/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
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
package org.pdfsam.service.premium;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.inject.Inject;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.model.premium.PremiumTool;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * @author Andrea Vacondio
 */
public class DefaultPremiumToolsService implements PremiumToolsService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultPremiumToolsService.class);
    private final AppBrand appBrand;
    private final ObjectMapper mapper;

    @Inject
    DefaultPremiumToolsService(AppBrand appBrand, ObjectMapper mapper) {
        requireNotNullArg(appBrand, "Application info cannot be null");
        this.appBrand = appBrand;
        this.mapper = mapper;
    }

    @Override
    public List<PremiumTool> getPremiumTools() {
        try {
            return Arrays.asList(mapper.readValue(new URL(appBrand.property(BrandableProperty.PREMIUM_TOOLS_URL)),
                    PremiumTool[].class));
        } catch (IOException e) {
            LOG.warn(i18n().tr("Unable to retrieve premium features description"), e);
        }
        return Collections.emptyList();
    }

}
