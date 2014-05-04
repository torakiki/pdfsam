/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.commons;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.text.TextAlignment;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.support.Style;

/**
 * Panel with a "Close" button to be used as bottom of closeable Stage.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ClosePane extends HBox {

    public ClosePane() {
        setAlignment(Pos.CENTER_RIGHT);
        getStyleClass().addAll(Style.CONTAINER.css());
        Button closeButton = new Button(DefaultI18nContext.getInstance().i18n("Close"));
        closeButton.getStyleClass().addAll(Style.BUTTON.css());
        closeButton.setTextAlignment(TextAlignment.CENTER);
        closeButton.setOnAction((e) -> this.getScene().getWindow().hide());
        getChildren().add(closeButton);
    }
}
