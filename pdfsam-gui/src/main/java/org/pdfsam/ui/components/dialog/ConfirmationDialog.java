/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03 dic 2015
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
package org.pdfsam.ui.components.dialog;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.ui.components.support.Style;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
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
public class ConfirmationDialog extends Stage {

    private ConfirmationDialogContent dialogContent;
    private boolean response = false;

    public ConfirmationDialog(StylesConfig styles, DialogStyle style, String positiveButtonText,
            String negativeButtonText) {
        initModality(Modality.WINDOW_MODAL);
        initStyle(StageStyle.UTILITY);
        setResizable(false);
        this.dialogContent = new ConfirmationDialogContent(style.icon);
        VBox containerPane = new VBox();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.getStyleClass().addAll("-pdfsam-dialog", style.style);
        HBox buttons = new HBox(buildPositiveButton(positiveButtonText, true),
                buildCancelButton(negativeButtonText, false));
        buttons.getStyleClass().add("-pdfsam-dialog-buttons");
        containerPane.getChildren().addAll(dialogContent, buttons);
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles.styles());
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

    private Button buildPositiveButton(String text, boolean response) {
        Button button = buildButton(text, response);
        button.setDefaultButton(true);
        return button;
    }

    private Button buildCancelButton(String text, boolean response) {
        Button button = buildButton(text, response);
        button.setCancelButton(true);
        return button;
    }

}
