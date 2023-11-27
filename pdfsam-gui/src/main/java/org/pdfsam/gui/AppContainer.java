package org.pdfsam.gui;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/01/23
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

import jakarta.inject.Inject;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import org.pdfsam.gui.components.sidebar.VerticalSidebar;
import org.pdfsam.ui.components.support.Style;

/**
 * @author Andrea Vacondio
 */
public class AppContainer extends BorderPane {
    private final ScrollPane center = new ScrollPane();

    @Inject
    AppContainer(VerticalSidebar sidebar) {
        setId("app-container");
        center.getStyleClass().addAll(Style.CONTAINER.css());
        center.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
        center.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED);
        center.setFitToWidth(true);
        center.setFitToHeight(true);
        setCenter(center);
        setLeft(sidebar);
    }

    public void setContent(Pane panel) {
        if (!panel.getStyleClass().contains("content-pane")) {
            panel.getStyleClass().add("content-pane");
        }
        center.setContent(panel);
    }

    public Node getContent() {
        return center.getContent();
    }
}
