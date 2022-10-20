/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23 nov 2016
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
package org.pdfsam.model.premium;

import javafx.scene.Node;
import javafx.scene.image.ImageView;

import static java.util.Optional.ofNullable;

/**
 * Types of premium products
 *
 * @author Andrea Vacondio
 */
public enum PremiumProduct {

    VISUAL("/org/pdfsam/model/images/visual32.png", "visual-category"),
    ENHANCED("/org/pdfsam/model/images/enhanced32.png", "enhanced-category"),
    OTHER(null, "other");

    private final String imageUrl;
    private final String styleClass;

    PremiumProduct(String imageUrl, String styleClass) {
        this.imageUrl = imageUrl;
        this.styleClass = styleClass;
    }

    public Node graphic() {
        return ofNullable(imageUrl).map(ImageView::new).orElse(null);
    }

    /**
     * @return the style class for this category
     */
    public String styleClass() {
        return styleClass;
    }
}
