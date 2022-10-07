/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.gui.components.dashboard;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

/**
 * Represents an item for the Dashboard
 * 
 * @author Andrea Vacondio
 */
public interface DashboardItem {

    String id();

    String name();

    Pane pane();

    /**
     * @return the graphic node for this {@link Pane}.
     */
    Node graphic();

    /**
     * @return the priority for this item. It affects the order the items have in the quickbar
     */
    int priority();

    /**
     * @return true if this item has been disabled
     */
    default boolean disabled() {
        return false;
    }

}
