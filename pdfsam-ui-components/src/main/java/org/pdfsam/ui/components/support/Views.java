/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.support;

import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import javafx.util.Duration;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;

import static org.pdfsam.ui.components.support.Style.TITLED_PANE;

/**
 * General utility methods related to views
 *
 * @author Andrea Vacondio
 */
public final class Views {
    private Views() {
        // hide
    }

    /**
     * @param titleString
     * @param node
     * @return a {@link TitledPane} around the given node with the give title
     */
    public static TitledPane titledPane(String titleString, Node node) {
        return titledPane(titleString, node, TITLED_PANE.css());
    }

    public static TitledPane titledPane(String titleString, Node node, String... styleClass) {
        var pane = new TitledPane(titleString, node);
        pane.getStyleClass().addAll(styleClass);
        pane.setMinHeight(Region.USE_PREF_SIZE);
        return pane;
    }

    /**
     * Creates an help icon that shows a popup with the given message
     *
     * @param text
     * @return the help icon
     */
    public static Text helpIcon(String text) {
        var tooltip = new Tooltip(text);
        var icon = FontIcon.of(UniconsLine.QUESTION_CIRCLE);
        icon.getStyleClass().add("help-icon");
        icon.setAccessibleRole(AccessibleRole.TEXT);
        icon.setAccessibleText(text);
        icon.setFocusTraversable(true);
        tooltip.setHideDelay(Duration.millis(100));
        tooltip.setShowDelay(Duration.millis(0));
        tooltip.setShowDuration(Duration.seconds(10));
        Tooltip.install(icon, tooltip);
        return icon;
    }
}
