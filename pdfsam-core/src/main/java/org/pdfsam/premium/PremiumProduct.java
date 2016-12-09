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

import javafx.scene.Node;
import javafx.scene.image.ImageView;

/**
 * Types of premium products
 * 
 * @author Andrea Vacondio
 *
 */
public enum PremiumProduct {
    VISUAL {
        @Override
        public Node graphic() {
            return new ImageView("images/visual32.png");
        }
    },
    ENHANCED {
        @Override
        public Node graphic() {
            return new ImageView("images/enhanced32.png");
        }
    },
    OTHER;

    public Node graphic() {
        return null;
    }
}
