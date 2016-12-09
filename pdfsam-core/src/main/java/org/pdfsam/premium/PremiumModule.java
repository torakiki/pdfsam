/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
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
package org.pdfsam.premium;

/**
 * Premium module data
 * 
 * @author Andrea Vacondio
 *
 */
public class PremiumModule {
    private int id;
    private String name;
    private String description;
    private String url;
    private PremiumProduct product;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public PremiumProduct getProduct() {
        return product;
    }

    public void setProduct(String product) {
        try {
            this.product = PremiumProduct.valueOf(product.toUpperCase());
        } catch (IllegalArgumentException e) {
            this.product = PremiumProduct.OTHER;
        }
    }
}
