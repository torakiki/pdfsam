/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/ott/2014
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

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.stage.Window;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.StylesConfig;
import org.pdfsam.ui.commons.HideOnEscapeHandler;
import org.pdfsam.ui.support.Style;

/**
 * Dialog asking the user to confirm for the output file overwrite
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class OverwriteConfirmationDialog extends Stage {

    private boolean overwrite = false;
    private OverwriteConfirmationDialogContent dialogContent = new OverwriteConfirmationDialogContent();

    @Inject
    public OverwriteConfirmationDialog(StylesConfig styles) {
        initModality(Modality.WINDOW_MODAL);
        initStyle(StageStyle.UTILITY);
        setResizable(false);
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.getStyleClass().addAll("-pdfsam-dialog", "-pdfsam-warning-dialog");
        containerPane.setCenter(dialogContent);
        HBox buttons = new HBox(buildButton(DefaultI18nContext.getInstance().i18n("Overwrite"), true), buildButton(
                DefaultI18nContext.getInstance().i18n("Cancel"), false));
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

    OverwriteConfirmationDialog title(String title) {
        setTitle(title);
        return this;
    }

    OverwriteConfirmationDialog messageTitle(String title) {
        dialogContent.messageTitle(title);
        return this;
    }

    OverwriteConfirmationDialog messageContent(String title) {
        dialogContent.messageContent(title);
        return this;
    }

    public boolean shouldOverwrite() {
        showAndWait();
        return overwrite;
    }

    private Button buildButton(String text, boolean result) {
        Button button = new Button(text);
        button.getStyleClass().addAll(Style.BUTTON.css());
        button.setOnAction(e -> {
            this.overwrite = result;
            hide();
        });
        return button;
    }
}
