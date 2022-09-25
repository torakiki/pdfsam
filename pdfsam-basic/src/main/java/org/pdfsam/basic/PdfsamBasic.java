/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
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
package org.pdfsam.basic;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.pdfsam.core.BrandableProperty;
import org.pdfsam.core.AppBrand;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfsamBasic implements AppBrand {
    private Properties properties = new Properties();
    private String name;
    private String shortName;

    public PdfsamBasic(String name, String shortName) throws IOException {
        requireNotBlank(name, "Application name cannot be blank");
        requireNotBlank(shortName, "Application short name cannot be blank");
        this.name = name;
        this.shortName = shortName;
        try (InputStream stream = this.getClass().getResourceAsStream("/pdfsam.properties")) {
            properties.load(stream);
        }
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public String shortName() {
        return shortName;
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
