/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
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
package org.pdfsam.tools.splitbysize;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Unit of measure of the size to split at
 * 
 * @author Andrea Vacondio
 *
 */
public enum SizeUnit {
    MEGABYTE(i18n().tr("Megabytes"), i18n().tr("MB")) {
        @Override
        public long toBytes(int raw) {
            return KILOBYTE.toBytes(raw) * 1024;
        }
    },
    KILOBYTE(i18n().tr("Kilobytes"), i18n().tr("KB")) {
        @Override
        public long toBytes(int raw) {
            return raw * 1024L;
        }
    };

    private final String name;
    private final String symbol;

    SizeUnit(String name, String symbol) {
        this.name = name;
        this.symbol = symbol;
    }

    public String friendlyName() {
        return name;
    }

    public String symbol() {
        return symbol;
    }

    /**
     * Convert the raw integer to the corresponding bytes for the unit
     * 
     * @return number of bytes
     */
    public abstract long toBytes(int raw);
}
