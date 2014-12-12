/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
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
package org.pdfsam.splitbysize;

import org.pdfsam.i18n.DefaultI18nContext;

/**
 * Unit of measure of the size to split at
 * 
 * @author Andrea Vacondio
 *
 */
public enum SizeUnit {
    KILOBYTE(DefaultI18nContext.getInstance().i18n("Kilobytes"), DefaultI18nContext.getInstance().i18n("KB")) {
        @Override
        public long toBytes(int raw) {
            return raw * 1024;
        }
    },
    MEGABYTE(DefaultI18nContext.getInstance().i18n("Megabytes"), DefaultI18nContext.getInstance().i18n("MB")) {
        @Override
        public long toBytes(int raw) {
            return KILOBYTE.toBytes(raw) * 1024;
        }
    };

    private String name;
    private String symbol;

    private SizeUnit(String name, String symbol) {
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
     * @param raw
     * @return number of bytes
     */
    public abstract long toBytes(int raw);
}
