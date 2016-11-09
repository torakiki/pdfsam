/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/set/2014
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
package org.pdfsam.test;

import org.pdfsam.ui.dashboard.DashboardItem;

import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultPriorityDashboardItem implements DashboardItem {
    public static final String ID = "test.item";

    @Override
    public String id() {
        return ID;
    }

    @Override
    public String name() {
        return "name";
    }

    @Override
    public Pane pane() {
        return new VBox();
    }

    @Override
    public Node graphic() {
        return null;
    }

    @Override
    public int priority() {
        return 0;
    }

}
