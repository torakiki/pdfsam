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
package org.pdfsam.model.premium;

import java.beans.ConstructorProperties;

/**
 * Premium tools data
 *
 * @author Andrea Vacondio
 */
public record PremiumTool(int id, String name, String description, String url, PremiumProduct product) {

    //@ConstructorProperties used by jackson to know the constructor params names
    @ConstructorProperties({ "id", "name", "description", "url", "product" })
    public PremiumTool(int id, String name, String description, String url, String product) {
        this(id, name, description, url, productFrom(product));
    }

    private static PremiumProduct productFrom(String product) {
        try {
            return PremiumProduct.valueOf(product.toUpperCase());
        } catch (IllegalArgumentException e) {
            return PremiumProduct.OTHER;
        }

    }
}
