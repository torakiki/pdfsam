/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.commons;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.text.TextAlignment;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * {@link BorderPane} where the bottom area is occupied by a panel providing a "Close" button that hides the hiding panel
 *
 * @author Andrea Vacondio
 */
public class HidingPane extends BorderPane {

    public HidingPane() {
        Button closeButton = new Button(i18n().tr("Close"));
        closeButton.getStyleClass().addAll(Style.BUTTON.css());
        closeButton.setTextAlignment(TextAlignment.CENTER);
        closeButton.setOnAction(e -> this.setVisible(false));
        HBox bottom = new HBox(closeButton);
        bottom.setAlignment(Pos.CENTER_RIGHT);
        bottom.getStyleClass().addAll(Style.CONTAINER.css());
        super.setBottom(bottom);
    }
}
