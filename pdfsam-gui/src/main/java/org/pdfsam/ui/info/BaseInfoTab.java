/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/mag/2014
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
package org.pdfsam.ui.info;

import javafx.geometry.HPos;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Tab;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Base class for {@link Tab} in the info panel
 * 
 * @author Andrea Vacondio
 *
 */
class BaseInfoTab extends Tab {
    private GridPane grid = new GridPane();

    BaseInfoTab() {
        setClosable(false);
        grid.getStyleClass().add("info-props");
        grid.setAlignment(Pos.TOP_CENTER);
        ColumnConstraints column1 = new ColumnConstraints();
        column1.setPercentWidth(25);
        ColumnConstraints column2 = new ColumnConstraints();
        column2.setPercentWidth(75);
        grid.getColumnConstraints().addAll(column1, column2);
        ScrollPane scroll = new ScrollPane(grid);
        scroll.setFitToHeight(true);
        scroll.setFitToWidth(true);
        setContent(scroll);
    }

    protected GridPane grid() {
        return grid;
    }

    protected static Label createTitleLabel(String text) {
        Label ret = new Label(DefaultI18nContext.getInstance().i18n(text) + ":");
        ret.getStyleClass().add("info-property");
        GridPane.setHalignment(ret, HPos.RIGHT);
        GridPane.setValignment(ret, VPos.TOP);
        return ret;
    }

    protected static Label createValueLabel() {
        Label ret = new Label();
        ret.getStyleClass().add("info-property-value");
        ret.setWrapText(true);
        return ret;
    }
}
