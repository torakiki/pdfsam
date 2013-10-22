/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui;

import java.io.IOException;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Panel with a "Close" button to be used as bottom of closeable Stage.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ClosePane extends HBox {
    @FXML
    private Button closeButton;

    public ClosePane() {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/fxml/ClosePane.fxml"));
        fxmlLoader.setRoot(this);
        fxmlLoader.setController(this);

        try {
            fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }
        closeButton.setText(DefaultI18nContext.getInstance().i18n("Close"));
    }

    /**
     * Hides the parent Window
     */
    public void hideWindow() {
        this.getScene().getWindow().hide();
    }
}
