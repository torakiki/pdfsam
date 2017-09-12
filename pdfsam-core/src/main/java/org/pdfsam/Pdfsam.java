/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam;

/**
 * Information about the current running version of PDFsam
 * 
 * @author Andrea Vacondio
 *
 */
public interface Pdfsam {

    /**
     * @return application name
     */
    String name();

    /**
     * @return application short name
     */
    String shortName();

    /**
     * @param prop
     * @param defaultValue
     * @return a configurable property value
     */
    public String property(ConfigurableProperty prop, String defaultValue);

    /**
     * @param prop
     * @param defaultValue
     * @return a configurable property value or blank if no value is available
     */
    public String property(ConfigurableProperty prop);
}
