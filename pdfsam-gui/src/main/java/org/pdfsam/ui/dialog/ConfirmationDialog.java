/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03 dic 2015
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
package org.pdfsam.ui.dialog;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.ui.commons.HideOnEscapeHandler;
import org.pdfsam.ui.support.Style;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.stage.Window;

/**
 * A generic Ok/Cancel confirmation dialog
 * 
 * @author Andrea Vacondio
 *
 */
class ConfirmationDialog extends Stage {

    private ConfirmationDialogContent dialogContent;
    private boolean response = false;

    public ConfirmationDialog(StylesConfig styles, DialogStyle style, String positiveButtonText,
            String negativeButtonText) {
        initModality(Modality.WINDOW_MODAL);
        initStyle(StageStyle.UTILITY);
        setResizable(false);
        this.dialogContent = new ConfirmationDialogContent(style.icon);
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.getStyleClass().addAll("-pdfsam-dialog", style.style);
        containerPane.setCenter(dialogContent);
        HBox buttons = new HBox(buildButton(positiveButtonText, true), buildButton(negativeButtonText, false));
        buttons.getStyleClass().add("-pdfsam-dialog-buttons");
        containerPane.setBottom(buttons);
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles.styles());
        scene.setOnKeyReleased(new HideOnEscapeHandler(this));
        setScene(scene);
    }

    public void setOwner(Window owner) {
        initOwner(owner);
    }

    ConfirmationDialog title(String title) {
        setTitle(title);
        return this;
    }

    ConfirmationDialog messageTitle(String title) {
        dialogContent.messageTitle(title);
        return this;
    }

    ConfirmationDialog messageContent(String title) {
        dialogContent.messageContent(title);
        return this;
    }

    public boolean response() {
        showAndWait();
        return response;
    }

    private Button buildButton(String text, boolean response) {
        Button button = new Button(text);
        button.getStyleClass().addAll(Style.BUTTON.css());
        button.setOnAction(e -> {
            this.response = response;
            hide();
        });
        return button;
    }
}
