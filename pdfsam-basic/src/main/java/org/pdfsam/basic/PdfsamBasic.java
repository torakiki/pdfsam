/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
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
package org.pdfsam.basic;

import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfsamBasic implements AppBrand {
    private final Properties properties = new Properties();

    public PdfsamBasic() throws IOException {
        try (InputStream stream = this.getClass().getResourceAsStream("/pdfsam.properties")) {
            properties.load(stream);
        }
    }

    @Override
    public String property(BrandableProperty prop, String defaultValue) {
        return properties.getProperty(prop.prop, defaultValue);
    }

    @Override
    public String property(BrandableProperty prop) {
        return properties.getProperty(prop.prop, EMPTY);
    }
}
