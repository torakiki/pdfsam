/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/lug/2014
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
package org.pdfsam.context;

/**
 * Event to notify that the application Locale should be changed
 * 
 * @author Andrea Vacondio
 *
 */
public class SetLocaleEvent {
    private String localeString;

    public SetLocaleEvent(String localeString) {
        this.localeString = localeString;
    }

    public String getLocaleString() {
        return localeString;
    }

}
