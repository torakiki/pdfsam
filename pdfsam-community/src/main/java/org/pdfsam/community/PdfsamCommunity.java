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

import org.pdfsam.Pdfsam;
import org.pdfsam.PdfsamEdition;
import org.springframework.core.env.Environment;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfsamCommunity implements Pdfsam {
    private Environment env;
    private String name;

    public PdfsamCommunity(String name, Environment env) {
        requireNotBlank(name, "Application name cannot be blank");
        requireNotNull(env, "Environment cannot be null");
        this.env = env;
        this.name = name;
    }

    public PdfsamEdition edition() {
        return PdfsamEdition.COMMUNITY;
    }

    public String name() {
        return name;
    }

    public String property(org.pdfsam.ConfigurableProperty prop, String defaultValue) {
        return env.getProperty(prop.prop, defaultValue);
    }

    public String property(org.pdfsam.ConfigurableProperty prop) {
        return env.getProperty(prop.prop, EMPTY);
    }

}
