/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
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
package org.pdfsam.community;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.pdfsam.support.RequireUtils.requireNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;

import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;
import org.springframework.core.env.Environment;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfsamCommunity implements Pdfsam {
    private Environment env;
    private String name;
    private String shortName;

    public PdfsamCommunity(String name, String shortName, Environment env) {
        requireNotBlank(name, "Application name cannot be blank");
        requireNotBlank(shortName, "Application short name cannot be blank");
        requireNotNull(env, "Environment cannot be null");
        this.env = env;
        this.name = name;
        this.shortName = shortName;
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
    public String property(ConfigurableProperty prop, String defaultValue) {
        return env.getProperty(prop.prop, defaultValue);
    }

    @Override
    public String property(ConfigurableProperty prop) {
        return env.getProperty(prop.prop, EMPTY);
    }
}
