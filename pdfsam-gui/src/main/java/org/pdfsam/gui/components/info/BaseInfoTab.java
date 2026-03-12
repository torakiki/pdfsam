/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/mag/2014
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
package org.pdfsam.gui.components.info;

import javafx.geometry.HPos;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Tab;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import org.apache.commons.lang3.StringUtils;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Base class for {@link Tab} in the info panel
 * 
 * @author Andrea Vacondio
 *
 */
class BaseInfoTab extends Tab {
    private final GridPane grid = new GridPane();

    BaseInfoTab() {
        setClosable(false);
        grid.getStyleClass().add("info-props");
        grid.setAlignment(Pos.TOP_CENTER);
        ColumnConstraints column1 = new ColumnConstraints();
        column1.setPercentWidth(25);
        ColumnConstraints column2 = new ColumnConstraints();
        column2.setPercentWidth(75);
        grid.getColumnConstraints().addAll(column1, column2);
        grid.setAccessibleRoleDescription(i18n().tr("Properties table"));
        ScrollPane scroll = new ScrollPane(grid);
        scroll.setFitToHeight(true);
        scroll.setFitToWidth(true);
        scroll.setAccessibleText(i18n().tr("Properties"));
        setContent(scroll);
    }

    protected GridPane grid() {
        return grid;
    }

    protected static Label createTitleLabel(String text, Label titleFor) {
        var label = new Label(i18n().tr(text) + ":");
        label.getStyleClass().add("info-property");
        label.setLabelFor(titleFor);
        GridPane.setHalignment(label, HPos.RIGHT);
        GridPane.setValignment(label, VPos.TOP);
        return label;
    }

    protected static Label createValueLabel() {
        var label = new Label();
        label.textProperty().addListener((obs, oldVal, newVal) -> {
            if (StringUtils.isNotBlank(newVal)) {
                label.setAccessibleText(newVal);
            } else {
                label.setAccessibleText(i18n().tr("Empty"));
            }
        });
        label.getStyleClass().add("info-property-value");
        label.setWrapText(true);
        return label;
    }
}
