package org.pdfsam.test;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import org.pdfsam.model.ui.ContentItem;

/**
 * @author Andrea Vacondio
 */
public class TestContentItem implements ContentItem {

    public static final String ID = "TEST";

    @Override
    public String id() {
        return ID;
    }

    @Override
    public String name() {
        return "Test";
    }

    @Override
    public String description() {
        return "A test item";
    }

    @Override
    public Pane panel() {
        return null;
    }

    @Override
    public Node graphic() {
        return new Circle(10, Color.CORAL);
    }
}
